;;; structured-log.el --- view structured log messages

;; Author:  Fang Lungang <lungang.fang@mail.com>
;; Maintainer: Fang Lungang
;; Created: 2024
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.2") json-ts-mode treesit)
;; Keywords: tree-sitter, treesit, log, json

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a way to view log files in the JSON Lines format, i.e.
;; each log entry is a json object.

;; Assumptions:
;; - The log file is a JSON Lines file.
;; - Each log entry is relatively short.

;;; Code:

(require 'json-ts-mode)
(require 'json)
(require 'treesit)

(defvar structlog--to-hide '("{" "}" "[" "]" "\"" ":" ","))
(defvar structlog--overlay "structlog-overlay")

(defun structlog--get-overlay (node)
  "Get the structured log overlay of the NODE."
  (delq nil (mapcar
             (lambda (overlay) (and (member structlog--overlay
                                   (overlay-properties overlay)) overlay))
             (overlays-in (treesit-node-start node) (treesit-node-end node)))))

(defun structlog--hide-node (node)
    "Hide the NODE."
    (let* ((beg (treesit-node-start node))
           (end (treesit-node-end node))
           (overlay (if (structlog--get-overlay node) nil ; already hidden
                      (make-overlay beg end)))
           )
      (when overlay
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'intangible t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'before-string " ")
        (overlay-put overlay 'category structlog--overlay)
        )))

(defun structlog--show-node (node)
  "Show the NODE."
  (interactive)
  (let* ((begin (treesit-node-start node))
         (end (treesit-node-end node))
         (overlays (structlog--get-overlay node)))
    (mapcar 'delete-overlay overlays)
    ))

(defun structlog-should-hide (node)
  "The default predicate function to determine if NODE should be hidden."
    (or (string-equal (treesit-node-field-name node) "key")
                     (member (treesit-node-type node) structlog--to-hide)))

(defun structlog--window-end ()
  "Get the end of the current window."
    (save-excursion
      (goto-char (window-end nil t))
      ;; window-end is not reliable when called in the `window-scroll-functions`
      ;; hook. Hence move 1/5 window further down or to the buffer end.
      (forward-line (/ (window-size nil nil) 5))
      (line-beginning-position))
  )

(defun structlog-hide-nodes-in-window (hide)
  "Hide all the nodes in the current window if HIDE is non-nil, else show them."
  (let* ((root (treesit-buffer-root-node))
         (node (treesit-node-first-child-for-pos root (window-start)))
         (pred #'structlog-should-hide)
         (process-fn (if hide #'structlog--hide-node #'structlog--show-node))
         )
    (while node
      (treesit-induce-sparse-tree node pred process-fn)
      (if (< (treesit-node-end node) (structlog--window-end))
          (setq node (treesit-node-next-sibling node))
        (setq node nil)
        ))))

(defvar structlog--currently-hidding nil)
(make-variable-buffer-local 'structlog--currently-hidding)

(defun structlog-hide ()
  "Hide the keys etc."
  (interactive)
  (setq structlog--currently-hidding t)
  (structlog-hide-nodes-in-window structlog--currently-hidding))

(defun structlog-show ()
  "Show the original line."
  (interactive)
  (setq structlog--currently-hidding nil)
  (structlog-hide-nodes-in-window structlog--currently-hidding))

(defvar structured-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h") 'structlog-hide)
    (define-key map (kbd "C-c C-s") 'structlog-show)
    map)
  "Keymap for `structured-log-mode'.")

(defun structlog--after-scroll (window start)
  "Adapter of `structlog-hide-nodes-in-window', WINDOW & START are not used."
  (structlog-hide-nodes-in-window structlog--currently-hidding))

(defvar structlog--main-buffer-name nil)
(defvar structlog--side-buffer-name "*structured-log*")
(defvar structlog--timer nil)
(defvar structlog--timer-delay 0.5)
(defvar structlog--prev-line nil)
(defvar structlog--truncate-lines-original-value nil)
(make-variable-buffer-local 'structlog--truncate-lines)

(defun structlog--get-buffer-create ()
  "Get the structured log buffer."
  (or (get-buffer structlog--side-buffer-name)
      (let ((buffer (get-buffer-create structlog--side-buffer-name)))
        (with-current-buffer buffer (json-ts-mode))
        buffer)))

(defun structlog--update-side-buffer ()
  "Update the structured log buffer."
  (when (eq (buffer-name) structlog--main-buffer-name)
    ;; update side buffer only when the main buffer is the current buffer
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (line (buffer-substring-no-properties beg end))
           )
      (unless (equal line structlog--prev-line)
        (setq structlog--prev-line line)
        (with-current-buffer (structlog--get-buffer-create)
          (erase-buffer)
          (insert line)
          (json-pretty-print-buffer)
          ))
      ))
  )

(defun structlog--cancel-timer ()
  "Stop the structured log timer."
  (when structlog--timer
    (cancel-timer structlog--timer)
    (setq structlog--timer nil)
     ))

(defun structlog--start-timer ()
  "Start the structured log timer."
    (setq structlog--timer
          (run-with-idle-timer structlog--timer-delay
                               t
                               #'structlog--update-side-buffer))
    (add-hook 'kill-buffer-hook 'structlog--cancel-timer nil t)
    )

;;;###autoload
(define-minor-mode structured-log-mode
  "Displays JSON lines in a more human-friendly format."
  :global nil
  :lighter ""
  :keymap structured-log-mode-map

  (setq structlog--currently-hidding structured-log-mode)
  (structlog-hide-nodes-in-window structlog--currently-hidding)
  (add-hook 'window-scroll-functions 'structlog--after-scroll 100 t)
  (display-buffer-in-side-window (structlog--get-buffer-create)
                                 '((side . right)))
  (if structured-log-mode
      (progn
        (setq structlog--main-buffer-name (buffer-name))
        (setq structlog--truncate-lines-original-value truncate-lines)
        (toggle-truncate-lines t)
        (structlog--start-timer)
        )

    (toggle-truncate-lines structlog--truncate-lines-original-value)
    (let ((side-window (get-buffer-window structlog--side-buffer-name)))
      (when side-window (delete-window side-window)))
    (structlog--cancel-timer)
    (setq structlog--main-buffer-name nil)
    ))

(provide 'structured-log)
;;; structured-log.el ends here

;;; structured-log-mode.el --- View structured (JSON Lines) log files -*- lexical-binding: t; -*-

;; Author:  Fang Lungang <lungang.fang@mail.com>
;; Maintainer: Fang Lungang
;; Created: 2024
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
(require 'seq)

(defvar structlog--to-hide '("{" "}" "[" "]" "\"" ":" ","))

(defconst structlog--replacement " "
  "Display string for hidden syntax.
A single shared object: adjacent hidden nodes get `eq' display
values, so each run of hidden text renders as one space.")

(defun structlog--hide-node (node)
  "Display NODE as a space via a text property."
  (put-text-property (treesit-node-start node) (treesit-node-end node)
                     'display structlog--replacement))

(defun structlog--unhide-region (beg end)
  "Remove our display properties between BEG and END.
Display properties set by other packages are left alone."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'display nil end)))
        (when (eq (get-text-property pos 'display) structlog--replacement)
          (remove-text-properties pos next '(display nil)))
        (setq pos next)))))

(defun structlog--should-hide (node)
  "The default predicate function to determine if NODE should be hidden."
    (or (string-equal (treesit-node-field-name node) "key")
                     (member (treesit-node-type node) structlog--to-hide)))

(defvar-local structlog--our-parser nil "The parser created by us.")

(defun structlog--update-parser-range (ranges)
  "Set RANGES for our own parser, which is a list of cons cells.

Setting range allows us to handle huge files. If there is already
a JSON parser, then the file isn't that big. Don't bother with
range."
  (when structlog--our-parser
    (treesit-parser-set-included-ranges structlog--our-parser ranges)))

(defun structlog--create-parser-if-needed ()
  "Create a parser if needed."
  (unless (delq nil (mapcar
                     (lambda (parser)
                       (when (eq (treesit-parser-language parser) 'json) parser))
                     (treesit-parser-list)))
    (setq structlog--our-parser (treesit-parser-create 'json))))


(defvar-local structlog--hiding nil
  "Non-nil when JSON keys and punctuation are being hidden.")

(defun structlog--jit-hide (beg end)
  "Hide JSON keys and punctuation between BEG and END.
Registered with `jit-lock-register'.  When `structlog--hiding' is
nil, unhides the region instead.  Returns the jit-lock bounds of
the region actually processed (extended to whole lines)."
  (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
        (end (save-excursion (goto-char end) (line-end-position))))
    (with-silent-modifications
      (structlog--unhide-region beg end)
      (when structlog--hiding
        (structlog--update-parser-range (list (cons beg end)))
        (let ((node (treesit-node-first-child-for-pos
                     (treesit-buffer-root-node) beg)))
          (while (and node (< (treesit-node-start node) end))
            (treesit-induce-sparse-tree
             node #'structlog--should-hide #'structlog--hide-node)
            (setq node (treesit-node-next-sibling node))))))
    (cons 'jit-lock-bounds (cons beg end))))

(defun structlog--hide-show (hide)
  "Hide the keys etc. if HIDE is non-nil, else show them."
  (setq structlog--hiding hide)
  (jit-lock-refontify))

(defun structlog-hide ()
  "Hide the keys etc."
  (interactive)
  (structlog--hide-show t))

(defun structlog-show ()
  "Show the original line."
  (interactive)
  (structlog--hide-show nil))

(defvar structured-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h") 'structlog-hide)
    (define-key map (kbd "C-c C-s") 'structlog-show)
    map)
  "Keymap for `structured-log-mode'.")

(defvar structlog--side-buffer-name "*structured-log*")
(defvar structlog--timer nil
  "Shared idle timer updating the side buffer; one for all mode buffers.")
(defvar structlog--prev-line nil
  "The line content currently rendered in the shared side buffer.")
(defvar-local structlog--truncate-lines-original-value nil
  "Value of `truncate-lines' before the mode was enabled, per buffer.")

(defvar structlog-timer-delay 0.3
  "Delay (in seconds) before updating the side window.")

(defun structlog--get-buffer-create ()
  "Get the structured log buffer."
  (or (get-buffer structlog--side-buffer-name)
      (let ((buffer (get-buffer-create structlog--side-buffer-name)))
        (with-current-buffer buffer (json-ts-mode))
        buffer)))

(defun structlog--update-side-buffer ()
  "Update the structured log buffer."
  (when structured-log-mode
    ;; update side buffer only when the current buffer has the mode enabled
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

(defun structlog--ensure-timer ()
  "Start the shared idle timer unless it is already running."
  (unless structlog--timer
    (setq structlog--timer
          (run-with-idle-timer structlog-timer-delay t
                               #'structlog--update-side-buffer))))

(defun structlog--teardown-shared-maybe ()
  "Release shared resources when this is the last structlog buffer.
Cancels the idle timer and deletes the side window unless some
other live buffer still has `structured-log-mode' enabled.  Safe
to call from the mode's disable path and from `kill-buffer-hook'
\(the current buffer is excluded from the check in both cases)."
  (unless (seq-some (lambda (buf)
                      (and (not (eq buf (current-buffer)))
                           (buffer-local-value 'structured-log-mode buf)))
                    (buffer-list))
    (when structlog--timer
      (cancel-timer structlog--timer)
      (setq structlog--timer nil))
    (let ((side-window (get-buffer-window structlog--side-buffer-name)))
      (when side-window (delete-window side-window)))))

;;;###autoload
(define-minor-mode structured-log-mode
  "Displays JSON lines in a more human-friendly format."
  :global nil
  :lighter ""
  :keymap structured-log-mode-map

  (if structured-log-mode
      (progn
        (structlog--create-parser-if-needed)
        (setq structlog--hiding t)
        (jit-lock-register #'structlog--jit-hide)
        (display-buffer-in-side-window (structlog--get-buffer-create)
                                       '((side . right)))
        (setq structlog--truncate-lines-original-value truncate-lines)
        (setq truncate-lines t)
        (structlog--ensure-timer)
        (add-hook 'kill-buffer-hook #'structlog--teardown-shared-maybe nil t))
    (jit-lock-unregister #'structlog--jit-hide)
    (with-silent-modifications
      (structlog--unhide-region (point-min) (point-max)))
    (when structlog--our-parser
      (treesit-parser-delete structlog--our-parser)
      (setq structlog--our-parser nil))
    (setq truncate-lines structlog--truncate-lines-original-value)
    (remove-hook 'kill-buffer-hook #'structlog--teardown-shared-maybe t)
    (structlog--teardown-shared-maybe)))

(provide 'structured-log-mode)
;;; structured-log-mode.el ends here

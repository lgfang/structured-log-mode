;;; structured-log.el --- view structured log messages

;; Author:  Fang Lungang <lungang.fang@mail.com>
;; Maintainer: Fang Lungang
;; Created: 2024
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.2"))
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

(require 'treesit)

(defvar structlog--to-hide '("{" "}" "[" "]" "\"" ":" ","))
(defvar structlog--overlay "structlog-overlay")

(defun structlog--get-overlay (node)
  "Get the structured log overlay of the NODE."
  (delq nil
        (mapcar (lambda (overlay)
                  (and (member structlog--overlay (overlay-properties overlay))
                       overlay))
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
        (overlay-put overlay 'before-string "")
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
   
(defun structlog-hide-nodes-in-window (hide)
  "Hide all the nodes in the current window if HIDE is non-nil, else show them."
  (let* ((root (treesit-buffer-root-node))
         (node (treesit-node-first-child-for-pos root (window-start)))
         (pred #'structlog-should-hide)
         (process-fn (if hide #'structlog--hide-node #'structlog--show-node))
         )
    (while node
      (treesit-induce-sparse-tree node pred process-fn)
      (if (< (treesit-node-end node) (window-end))
          (setq node (treesit-node-next-sibling node))
        (setq node nil)
        ))))

;; (defvar structured-log-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-h") 'structlog-hide)
;;     (define-key map (kbd "C-c C-s") 'structlog-show)
;;     map)
;;   "Keymap for `structured-log-mode'.")

;; define a buffer local var to store the hidden nodes
(defvar structlog--currently-hidding nil)
(make-variable-buffer-local 'structlog--currently-hidding)

(defun structlog--after-scroll (window start)
  "Adapter of `structlog-hide-nodes-in-window', WINDOW & START are not used."
  (structlog-hide-nodes-in-window structlog--currently-hidding))

(define-minor-mode structured-log-mode
  "A simple minor mode."
  :global nil
  :lighter ""
  (setq structlog--currently-hidding structured-log-mode)
  (structlog-hide-nodes-in-window structlog--currently-hidding)
  (add-hook 'window-scroll-functions 'structlog--after-scroll 100 t)
  )

(provide 'structured-log)
;;; structured-log.el ends here

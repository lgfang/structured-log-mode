;;; structured-log-mode.el --- View structured (JSON Lines) log files -*- lexical-binding: t; -*-

;; Author:  Fang Lungang <lungang.fang@mail.com>
;; Maintainer: Fang Lungang
;; Created: 2024
;; Version: 0.2.0
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

(defgroup structured-log nil
  "View JSON Lines log files in a human-friendly way."
  :group 'tools
  :prefix "structlog-")

(defcustom structlog-hide-node-types '("{" "}" "[" "]" "\"" ":" ",")
  "Tree-sitter node types to hide, in addition to object keys."
  :type '(repeat string))

(defcustom structlog-side-window-side 'right
  "Which side of the frame shows the pretty-printed log entry."
  :type '(choice (const left) (const right) (const top) (const bottom)))

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
                     (member (treesit-node-type node) structlog-hide-node-types)))

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

(defun structlog-toggle-hiding ()
  "Toggle hiding of JSON keys and punctuation."
  (interactive)
  (setq structlog--hiding (not structlog--hiding))
  (jit-lock-refontify))

(defvar structured-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") #'structlog-toggle-hiding)
    map)
  "Keymap for `structured-log-mode'.")

(defvar structlog--side-buffer-name "*structured-log*")
(defvar structlog--timer nil
  "Shared idle timer updating the side buffer; one for all mode buffers.")
(defvar structlog--prev-line nil
  "The line content currently rendered in the shared side buffer.")
(defvar-local structlog--truncate-lines-original-value nil
  "Value of `truncate-lines' before the mode was enabled, per buffer.")

(defcustom structlog-timer-delay 0.3
  "Idle delay (in seconds) before updating the side window."
  :type 'number)

(defun structlog--get-buffer-create ()
  "Get the structured log buffer."
  (or (get-buffer structlog--side-buffer-name)
      (let ((buffer (get-buffer-create structlog--side-buffer-name)))
        (with-current-buffer buffer (json-ts-mode))
        buffer)))

(defvar structured-log-mode)            ; defined by `define-minor-mode' below

(defun structlog--update-side-buffer ()
  "Render the current line, pretty-printed, into the side buffer.
Runs from the shared idle timer; does nothing unless the current
buffer has `structured-log-mode' enabled.  Lines that fail to
parse as JSON are shown raw."
  (when structured-log-mode
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (unless (equal line structlog--prev-line)
        (setq structlog--prev-line line)
        (with-current-buffer (structlog--get-buffer-create)
          (erase-buffer)
          (insert line)
          (condition-case nil
              (json-pretty-print-buffer)
            (error nil)))))))

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
                                       `((side . ,structlog-side-window-side)))
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

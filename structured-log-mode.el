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
  :prefix "structured-log-")

(defcustom structured-log-hide-node-types '("{" "}" "[" "]" "\"" ":" ",")
  "Tree-sitter node types to hide, in addition to object keys."
  :type '(repeat string))

(defcustom structured-log-side-window-side 'right
  "Which side of the frame shows the pretty-printed log entry."
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom structured-log-level-key "s"
  "JSON key whose value holds the log level of an entry."
  :type 'string)

(defcustom structured-log-level-faces
  '(("W" . warning) ("E" . error) ("F" . error))
  "Alist mapping log level values to faces used to highlight the line.
Levels not listed here (e.g. \"I\") are not highlighted."
  :type '(alist :key-type string :value-type face))

(defconst structured-log--replacement " "
  "Display string for hidden syntax.
A single shared object: adjacent hidden nodes get `eq' display
values, so each run of hidden text renders as one space.")

(defun structured-log--hide-node (node)
  "Display NODE as a space via a text property."
  (put-text-property (treesit-node-start node) (treesit-node-end node)
                     'display structured-log--replacement))

(defun structured-log--unhide-region (beg end)
  "Remove our display properties between BEG and END.
Display properties set by other packages are left alone."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'display nil end)))
        (when (eq (get-text-property pos 'display) structured-log--replacement)
          (remove-text-properties pos next '(display nil)))
        (setq pos next)))))

(defun structured-log--should-hide (node)
  "The default predicate function to determine if NODE should be hidden."
    (or (string-equal (treesit-node-field-name node) "key")
                     (member (treesit-node-type node) structured-log-hide-node-types)))

(defvar-local structured-log--our-parser nil "The parser created by us.")

(defun structured-log--update-parser-range (ranges)
  "Set RANGES for our own parser, which is a list of cons cells.

Setting range allows us to handle huge files. If there is already
a JSON parser, then the file isn't that big. Don't bother with
range."
  (when structured-log--our-parser
    (treesit-parser-set-included-ranges structured-log--our-parser ranges)))

(defun structured-log--create-parser-if-needed ()
  "Create a parser if needed."
  (unless (delq nil (mapcar
                     (lambda (parser)
                       (when (eq (treesit-parser-language parser) 'json) parser))
                     (treesit-parser-list)))
    (setq structured-log--our-parser (treesit-parser-create 'json))))


(defun structured-log--node-level (node)
  "Return the log level string of the entry NODE, or nil if none.
The level is the value of the `structured-log-level-key' member of
NODE, with string quotes stripped."
  (let ((key-text (concat "\"" structured-log-level-key "\"")))
    (catch 'level
      (dolist (pair (treesit-node-children node t))
        (let ((key (treesit-node-child-by-field-name pair "key")))
          (when (and key (equal (treesit-node-text key t) key-text))
            (let* ((value (treesit-node-child-by-field-name pair "value"))
                   (content (and value (treesit-node-child value 0 t))))
              (throw 'level
                     (and value (treesit-node-text (or content value) t))))))))))

(defun structured-log--highlight-node (node)
  "Apply the matching level face, if any, to the whole entry NODE."
  (let* ((level (structured-log--node-level node))
         (face (cdr (assoc level structured-log-level-faces))))
    (when face
      (let ((beg (treesit-node-start node))
            (end (treesit-node-end node)))
        (put-text-property beg end 'face face)
        (put-text-property beg end 'structured-log--level-face face)))))

(defun structured-log--unhighlight-region (beg end)
  "Remove our level faces between BEG and END."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'structured-log--level-face nil end)))
        (when (get-text-property pos 'structured-log--level-face)
          (remove-text-properties
           pos next '(face nil structured-log--level-face nil)))
        (setq pos next)))))

(defvar-local structured-log--hiding nil
  "Non-nil when JSON keys and punctuation are being hidden.")

(defun structured-log--jit-hide (beg end)
  "Hide JSON syntax and highlight log levels between BEG and END.
Registered with `jit-lock-register'.  Keys and punctuation are
hidden only when `structured-log--hiding' is non-nil; log-level
line highlighting is applied regardless.  Returns the jit-lock
bounds of the region actually processed (extended to whole
lines)."
  (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
        (end (save-excursion (goto-char end) (line-end-position))))
    (with-silent-modifications
      (structured-log--unhide-region beg end)
      (structured-log--unhighlight-region beg end)
      (structured-log--update-parser-range (list (cons beg end)))
      (let ((node (treesit-node-first-child-for-pos
                   (treesit-buffer-root-node) beg)))
        (while (and node (< (treesit-node-start node) end))
          (when structured-log--hiding
            (treesit-induce-sparse-tree
             node #'structured-log--should-hide #'structured-log--hide-node))
          (structured-log--highlight-node node)
          (setq node (treesit-node-next-sibling node)))))
    (cons 'jit-lock-bounds (cons beg end))))

(defun structured-log-toggle-hiding ()
  "Toggle hiding of JSON keys and punctuation."
  (interactive)
  (setq structured-log--hiding (not structured-log--hiding))
  (jit-lock-refontify))

(defvar structured-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") #'structured-log-toggle-hiding)
    map)
  "Keymap for `structured-log-mode'.")

(defvar structured-log--side-buffer-name "*structured-log*")
(defvar structured-log--timer nil
  "Shared idle timer updating the side buffer; one for all mode buffers.")
(defvar structured-log--prev-line nil
  "The line content currently rendered in the shared side buffer.")
(defvar-local structured-log--truncate-lines-original-value nil
  "Value of `truncate-lines' before the mode was enabled, per buffer.")

(defcustom structured-log-timer-delay 0.3
  "Idle delay (in seconds) before updating the side window."
  :type 'number)

(defun structured-log--get-buffer-create ()
  "Get the structured log buffer."
  (or (get-buffer structured-log--side-buffer-name)
      (let ((buffer (get-buffer-create structured-log--side-buffer-name)))
        (with-current-buffer buffer (json-ts-mode))
        buffer)))

(defvar structured-log-mode)            ; defined by `define-minor-mode' below

(defun structured-log--update-side-buffer ()
  "Render the current line, pretty-printed, into the side buffer.
Runs from the shared idle timer; does nothing unless the current
buffer has `structured-log-mode' enabled.  Lines that fail to
parse as JSON are shown raw."
  (when structured-log-mode
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (unless (equal line structured-log--prev-line)
        (setq structured-log--prev-line line)
        (with-current-buffer (structured-log--get-buffer-create)
          (erase-buffer)
          (insert line)
          (condition-case nil
              (json-pretty-print-buffer)
            (error nil)))))))

(defun structured-log--ensure-timer ()
  "Start the shared idle timer unless it is already running."
  (unless structured-log--timer
    (setq structured-log--timer
          (run-with-idle-timer structured-log-timer-delay t
                               #'structured-log--update-side-buffer))))

(defun structured-log--teardown-shared-maybe ()
  "Release shared resources when this is the last structured-log buffer.
Cancels the idle timer and deletes the side window unless some
other live buffer still has `structured-log-mode' enabled.  Safe
to call from the mode's disable path and from `kill-buffer-hook'
\(the current buffer is excluded from the check in both cases)."
  (unless (seq-some (lambda (buf)
                      (and (not (eq buf (current-buffer)))
                           (buffer-local-value 'structured-log-mode buf)))
                    (buffer-list))
    (when structured-log--timer
      (cancel-timer structured-log--timer)
      (setq structured-log--timer nil))
    (let ((side-window (get-buffer-window structured-log--side-buffer-name)))
      (when side-window (delete-window side-window)))))

;;;###autoload
(define-minor-mode structured-log-mode
  "Displays JSON lines in a more human-friendly format."
  :global nil
  :lighter ""
  :keymap structured-log-mode-map

  (if structured-log-mode
      (progn
        (structured-log--create-parser-if-needed)
        (setq structured-log--hiding t)
        (jit-lock-register #'structured-log--jit-hide)
        ;; `jit-lock-register' prepends, so `font-lock-fontify-region'
        ;; would run after us and wipe our level faces when it
        ;; unfontifies; reposition ourselves to run last.
        (remove-hook 'jit-lock-functions #'structured-log--jit-hide t)
        (add-hook 'jit-lock-functions #'structured-log--jit-hide 99 t)
        (display-buffer-in-side-window (structured-log--get-buffer-create)
                                       `((side . ,structured-log-side-window-side)))
        (setq structured-log--truncate-lines-original-value truncate-lines)
        (setq truncate-lines t)
        (structured-log--ensure-timer)
        (add-hook 'kill-buffer-hook #'structured-log--teardown-shared-maybe nil t))
    (jit-lock-unregister #'structured-log--jit-hide)
    (with-silent-modifications
      (structured-log--unhide-region (point-min) (point-max))
      (structured-log--unhighlight-region (point-min) (point-max)))
    (when structured-log--our-parser
      (treesit-parser-delete structured-log--our-parser)
      (setq structured-log--our-parser nil))
    (setq truncate-lines structured-log--truncate-lines-original-value)
    (remove-hook 'kill-buffer-hook #'structured-log--teardown-shared-maybe t)
    (structured-log--teardown-shared-maybe)))

(provide 'structured-log-mode)
;;; structured-log-mode.el ends here

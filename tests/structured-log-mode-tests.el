;;; structured-log-mode-tests.el --- ERT tests for structured-log-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L . -l tests/structured-log-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'structured-log-mode)

(ert-deftest structured-log-test-feature-provided ()
  "The file provides a feature matching its file name."
  (should (featurep 'structured-log-mode)))

(ert-deftest structured-log-test-truncate-save-is-buffer-local ()
  "The saved truncate-lines value is buffer-local, not shared."
  (should (local-variable-if-set-p 'structured-log--truncate-lines-original-value)))

(defmacro structured-log-tests--with-log-buffer (&rest body)
  "Run BODY in a temp buffer containing two JSON log lines.
Point starts at buffer beginning; the buffer is shown in the
selected window.  Side-window creation is stubbed out so tests
work identically in batch mode."
  `(let ((buf (generate-new-buffer " *structured-log-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert "{\"a\":1}\n{\"b\":\"x\"}\n")
           (goto-char (point-min))
           (set-window-buffer (selected-window) buf)
           (cl-letf (((symbol-function 'display-buffer-in-side-window)
                      #'ignore))
             ,@body))
       (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest structured-log-test-disable-tears-down ()
  "Disabling the mode unregisters jit-lock, unhides text, restores settings."
  (structured-log-tests--with-log-buffer
   (structured-log-mode 1)
   (should truncate-lines)
   (structured-log--jit-hide (point-min) (point-max))
   (should (eq (get-text-property 1 'display) structured-log--replacement))
   (structured-log-mode -1)
   (should-not (memq #'structured-log--jit-hide jit-lock-functions))
   (should-not truncate-lines)
   (should-not structured-log--timer)
   (should-not structured-log--our-parser)
   ;; every hidden char is visible again
   (should-not (text-property-any (point-min) (point-max)
                                  'display structured-log--replacement))))

(ert-deftest structured-log-test-jit-hide-hides-keys-and-punctuation ()
  "Keys, quotes, braces, and colons get the display property; values don't.
Buffer content: {\"a\":1}\\n{\"b\":\"x\"}\\n
Positions: 1={ 2-4=\"a\" 5=: 6=1 7=}   10-12=\"b\" 14=\" 15=x 16=\""
  (structured-log-tests--with-log-buffer
   (structured-log-mode 1)
   (structured-log--jit-hide (point-min) (point-max))
   (dolist (hidden-pos '(1 2 3 4 5 7 10 14 16))
     (should (eq (get-text-property hidden-pos 'display)
                 structured-log--replacement)))
   (dolist (visible-pos '(6 15))
     (should-not (get-text-property visible-pos 'display)))))

(ert-deftest structured-log-test-jit-unhides-when-hiding-off ()
  "With hiding toggled off, the jit function strips our properties only."
  (structured-log-tests--with-log-buffer
   ;; a foreign display property on the visible `1' must survive
   (put-text-property 6 7 'display "FOREIGN")
   (structured-log-mode 1)
   (structured-log--jit-hide (point-min) (point-max))
   (setq structured-log--hiding nil)
   (structured-log--jit-hide (point-min) (point-max))
   (should-not (text-property-any (point-min) (point-max)
                                  'display structured-log--replacement))
   (should (equal (get-text-property 6 'display) "FOREIGN"))))

(ert-deftest structured-log-test-multi-buffer-timer-lifecycle ()
  "The shared timer survives until the last mode buffer disables."
  (let ((buf1 (generate-new-buffer " *structured-log-1*"))
        (buf2 (generate-new-buffer " *structured-log-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   #'ignore))
          (with-current-buffer buf1
            (insert "{\"a\":1}\n") (structured-log-mode 1))
          (with-current-buffer buf2
            (insert "{\"b\":2}\n") (structured-log-mode 1))
          (should structured-log--timer)
          (with-current-buffer buf1 (structured-log-mode -1))
          (should structured-log--timer)     ; buf2 still needs it
          (should (buffer-local-value 'structured-log-mode buf2))
          (with-current-buffer buf2 (structured-log-mode -1))
          (should-not structured-log--timer))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest structured-log-test-truncate-restored-per-buffer ()
  "Each buffer restores its own original truncate-lines value."
  (let ((buf1 (generate-new-buffer " *structured-log-1*"))
        (buf2 (generate-new-buffer " *structured-log-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   #'ignore))
          (with-current-buffer buf1
            (insert "{\"a\":1}\n")
            (setq truncate-lines nil)
            (structured-log-mode 1))
          (with-current-buffer buf2
            (insert "{\"b\":2}\n")
            (setq truncate-lines t)
            (structured-log-mode 1))
          (with-current-buffer buf1
            (structured-log-mode -1)
            (should-not truncate-lines))
          (with-current-buffer buf2
            (structured-log-mode -1)
            (should truncate-lines)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest structured-log-test-killing-last-buffer-cancels-timer ()
  "Killing a mode buffer (without disabling first) also releases the timer."
  (let ((buf (generate-new-buffer " *structured-log-kill*")))
    (cl-letf (((symbol-function 'display-buffer-in-side-window) #'ignore))
      (with-current-buffer buf
        (insert "{\"a\":1}\n")
        (structured-log-mode 1))
      (should structured-log--timer)
      (kill-buffer buf)
      (should-not structured-log--timer))))

(ert-deftest structured-log-test-side-buffer-survives-malformed-json ()
  "A non-JSON line must not signal; the side buffer shows it raw."
  (structured-log-tests--with-log-buffer
   (structured-log-mode 1)
   (goto-char (point-max))
   (insert "not json at all\n")
   (forward-line -1)                       ; point on the malformed line
   (setq structured-log--prev-line nil)
   (structured-log--update-side-buffer)         ; must not signal
   (should (equal (with-current-buffer (structured-log--get-buffer-create)
                    (buffer-string))
                  "not json at all"))))

(ert-deftest structured-log-test-toggle-hiding ()
  "The toggle command flips hiding on and off."
  (structured-log-tests--with-log-buffer
   (structured-log-mode 1)
   (should structured-log--hiding)
   (structured-log-toggle-hiding)
   (should-not structured-log--hiding)
   (structured-log-toggle-hiding)
   (should structured-log--hiding)))

(ert-deftest structured-log-test-customs-exist ()
  "User options are defined via defcustom."
  (dolist (sym '(structured-log-hide-node-types
                 structured-log-timer-delay
                 structured-log-side-window-side))
    (should (custom-variable-p sym))))

(ert-deftest structured-log-test-level-highlighting ()
  "Lines get whole-line faces per log level; disabling removes them.
Buffer lines (20 chars each incl. newline): I at 1, W at 21, E at 41."
  (let ((buf (generate-new-buffer " *structured-log-hl*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "{\"s\":\"I\",\"msg\":\"a\"}\n"
                  "{\"s\":\"W\",\"msg\":\"b\"}\n"
                  "{\"s\":\"E\",\"msg\":\"c\"}\n")
          (goto-char (point-min))
          (set-window-buffer (selected-window) buf)
          (cl-letf (((symbol-function 'display-buffer-in-side-window)
                     #'ignore))
            (structured-log-mode 1)
            (structured-log--jit-hide (point-min) (point-max))
            (should-not (get-text-property 1 'face))         ; I: no highlight
            (should (eq (get-text-property 21 'face) 'warning))
            (should (eq (get-text-property 41 'face) 'error))
            ;; highlight covers the whole line, not just the level field
            (should (eq (get-text-property 39 'face) 'warning))
            (structured-log-mode -1)
            (should-not (get-text-property 21 'face))
            (should-not (get-text-property 41 'face))))
      (kill-buffer buf))))

(ert-deftest structured-log-test-level-highlight-respects-custom-key ()
  "The level is read from `structured-log-level-key'."
  (let ((buf (generate-new-buffer " *structured-log-hl-key*"))
        (structured-log-level-key "level"))
    (unwind-protect
        (with-current-buffer buf
          (insert "{\"level\":\"E\",\"s\":\"I\"}\n")
          (goto-char (point-min))
          (set-window-buffer (selected-window) buf)
          (cl-letf (((symbol-function 'display-buffer-in-side-window)
                     #'ignore))
            (structured-log-mode 1)
            (structured-log--jit-hide (point-min) (point-max))
            (should (eq (get-text-property 1 'face) 'error))
            (structured-log-mode -1)))
      (kill-buffer buf))))

(ert-deftest structured-log-test-highlight-survives-font-lock ()
  "Line faces survive a font-lock-style pass that strips `face'.
Font-lock registers its jit-lock function before the mode is
enabled and wipes the `face' property of the region it fontifies;
our function must therefore run after it."
  (let ((buf (generate-new-buffer " *structured-log-fl*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "{\"s\":\"W\",\"msg\":\"b\"}\n")
          (goto-char (point-min))
          (set-window-buffer (selected-window) buf)
          (cl-letf (((symbol-function 'display-buffer-in-side-window)
                     #'ignore))
            ;; stand-in for `font-lock-fontify-region'
            (add-hook 'jit-lock-functions
                      (lambda (beg end)
                        (remove-text-properties beg end '(face nil)))
                      nil t)
            (structured-log-mode 1)
            (run-hook-with-args 'jit-lock-functions (point-min) (point-max))
            (should (eq (get-text-property 1 'face) 'warning))))
      (kill-buffer buf))))

(provide 'structured-log-mode-tests)
;;; structured-log-mode-tests.el ends here

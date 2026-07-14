;;; structured-log-mode-tests.el --- ERT tests for structured-log-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L . -l tests/structured-log-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'structured-log-mode)

(ert-deftest structlog-test-feature-provided ()
  "The file provides a feature matching its file name."
  (should (featurep 'structured-log-mode)))

(ert-deftest structlog-test-truncate-save-is-buffer-local ()
  "The saved truncate-lines value is buffer-local, not shared."
  (should (local-variable-if-set-p 'structlog--truncate-lines-original-value)))

(defmacro structlog-tests--with-log-buffer (&rest body)
  "Run BODY in a temp buffer containing two JSON log lines.
Point starts at buffer beginning; the buffer is shown in the
selected window.  Side-window creation is stubbed out so tests
work identically in batch mode."
  `(let ((buf (generate-new-buffer " *structlog-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert "{\"a\":1}\n{\"b\":\"x\"}\n")
           (goto-char (point-min))
           (set-window-buffer (selected-window) buf)
           (cl-letf (((symbol-function 'display-buffer-in-side-window)
                      #'ignore))
             ,@body))
       (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest structlog-test-disable-tears-down ()
  "Disabling the mode unregisters jit-lock, unhides text, restores settings."
  (structlog-tests--with-log-buffer
   (structured-log-mode 1)
   (should truncate-lines)
   (structlog--jit-hide (point-min) (point-max))
   (should (eq (get-text-property 1 'display) structlog--replacement))
   (structured-log-mode -1)
   (should-not (memq #'structlog--jit-hide jit-lock-functions))
   (should-not truncate-lines)
   (should-not structlog--timer)
   (should-not structlog--our-parser)
   ;; every hidden char is visible again
   (should-not (text-property-any (point-min) (point-max)
                                  'display structlog--replacement))))

(ert-deftest structlog-test-jit-hide-hides-keys-and-punctuation ()
  "Keys, quotes, braces, and colons get the display property; values don't.
Buffer content: {\"a\":1}\\n{\"b\":\"x\"}\\n
Positions: 1={ 2-4=\"a\" 5=: 6=1 7=}   10-12=\"b\" 14=\" 15=x 16=\""
  (structlog-tests--with-log-buffer
   (structured-log-mode 1)
   (structlog--jit-hide (point-min) (point-max))
   (dolist (hidden-pos '(1 2 3 4 5 7 10 14 16))
     (should (eq (get-text-property hidden-pos 'display)
                 structlog--replacement)))
   (dolist (visible-pos '(6 15))
     (should-not (get-text-property visible-pos 'display)))))

(ert-deftest structlog-test-jit-unhides-when-hiding-off ()
  "With hiding toggled off, the jit function strips our properties only."
  (structlog-tests--with-log-buffer
   ;; a foreign display property on the visible `1' must survive
   (put-text-property 6 7 'display "FOREIGN")
   (structured-log-mode 1)
   (structlog--jit-hide (point-min) (point-max))
   (setq structlog--hiding nil)
   (structlog--jit-hide (point-min) (point-max))
   (should-not (text-property-any (point-min) (point-max)
                                  'display structlog--replacement))
   (should (equal (get-text-property 6 'display) "FOREIGN"))))

(provide 'structured-log-mode-tests)
;;; structured-log-mode-tests.el ends here

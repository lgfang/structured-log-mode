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
  "Disabling the mode removes hook, overlays, timer, and restores settings."
  (structlog-tests--with-log-buffer
   (structured-log-mode 1)
   (should (memq 'structlog--after-scroll window-scroll-functions))
   (should truncate-lines)
   (structured-log-mode -1)
   (should-not (memq 'structlog--after-scroll window-scroll-functions))
   (should-not truncate-lines)
   (should-not structlog--timer)
   (should-not structlog--our-parser)
   (should-not (seq-some (lambda (ov)
                           (eq (overlay-get ov 'category) structlog--overlay))
                         (overlays-in (point-min) (point-max))))))

(provide 'structured-log-mode-tests)
;;; structured-log-mode-tests.el ends here

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

(provide 'structured-log-mode-tests)
;;; structured-log-mode-tests.el ends here

;;; test-helper.el --- Helpers for porthole-test.el

;;; HACK: Allow loading package files
(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (add-to-list 'load-path source-directory))

;;; test-helper.el ends here

(defvar porthole--tests-directory
  (file-name-directory (or load-file-name
                           buffer-file-name))
 "The location of the porthole `emacs-web-server'-based transport layer's tests.")


(push (expand-file-name
       (f-join porthole--tests-directory ".." ".."))
      load-path)


(load-file "../../porthole.el")


(defconst porthole-transport-layer-test-dir
  (file-name-directory
   (or load-file-name buffer-file-name)))


(defvar porthole-test-server-name "external-tests-server")


(defun porthole-stop-server-on-idle-timer ()
  "Stop the server on an idle timer.

Designed to be called by the test suite after all tests have been
run successfully."
  (run-with-idle-timer 0.01 nil (lambda ()
                                  (porthole-stop-server)))
  t)


(defun porthole-run-external-tests ()
  "Test the functionality of the transport layer.

This method spins up a test server and then runs a set of HTTP
requests using Python. The results will be displayed in a new
buffer."
  (interactive)
  ;; Stop the server if it's running.
  (ignore-errors (porthole-stop-server porthole-test-server-name))
  ;; Start the test on an automatically assigned port.
  (porthole-start-server porthole-test-server-name
                     ;; Have to expose a single method: `+'.
                     :exposed-functions '(+))
  ;; A little feedback.
  (message "Server started: %s" (porthole--get-server
                                 porthole-test-server-name))
  (let ((default-directory porthole-transport-layer-test-dir))
    (async-shell-command
     (format "nosetests %s" "test_external_calls.py")
     "*porthole nosetests*"))
  ;; TODO: Test server without authentication
  ;; TODO: Test server on specific port
  )


;; Tests will be run automatically when this buffer is evaluated directly.
(when (not load-file-name)
 (porthole-run-external-tests))


(provide 'porthole-test-remote-calls)
;;; porthole-test-remote-calls.el ends here

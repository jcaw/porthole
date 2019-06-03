(defvar http-rpc-server--tests-directory
  (file-name-directory (or load-file-name
                           buffer-file-name))
 "The location of the hrpc `emacs-web-server'-based transport layer's tests.")


(push (expand-file-name  (concat http-rpc-server--tests-directory ".."))
      load-path)


(load-file "../http-rpc-server.el")


(defconst hrpc-transport-layer-test-dir
  (file-name-directory
   (or load-file-name buffer-file-name)))


(defconst hrpc-test-port 0
  "For testing, we dynamically allocate a port.")


(defun hrpc-stop-server-on-idle-timer ()
  "Stop the server on an idle timer.

Designed to be called by the test suite after all tests have been
run successfully."
  (run-with-idle-timer 0.01 nil (lambda ()
                                  (hrpc-stop-server)))
  t)


(defun hrpc-run-external-tests ()
  "Test the functionality of the transport layer.

This method spins up a test server and then runs a set of HTTP
requests using Python. The results will be displayed in a new
buffer.

Note that this test will reset your `json-rpc-server'
configuration."
  (interactive)
  ;; Stop the server if it's running.
  (ignore-errors (hrpc-stop-server))
  ;; Have to expose a single method: `+'.
  (setq jrpc-exposed-functions '(+))
  ;; We use dynamic port 8006 for tests. These tests won't work if port 8006 is
  ;; already bound.
  (hrpc-start-server :port hrpc-test-port
                     :username "test_username"
                     :password "test_password")
  (let ((default-directory hrpc-transport-layer-test-dir))
    (async-shell-command
     (format "nosetests %s" "test_external_calls.py")
     "*http-rpc-server nosetests*")))


;; Tests will be run automatically when this buffer is evaluated directly.
(when (not load-file-name)
 (hrpc-run-external-tests))


(provide 'test-http-rpc-server)
;;; test-http-rpc-server.el ends here

(defvar http-rpc-server--tests-directory
  (file-name-directory (or load-file-name
                           buffer-file-name))
 "The location of the hrpc `emacs-web-server'-based transport layer's tests.")


(push (expand-file-name
       (f-join http-rpc-server--tests-directory ".." ".."))
      load-path)


(load-file "../../http-rpc-server.el")


(defconst hrpc-transport-layer-test-dir
  (file-name-directory
   (or load-file-name buffer-file-name)))


(defvar hrpc-test-server-name "external-tests-server")


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
buffer."
  (interactive)
  ;; Stop the server if it's running.
  (ignore-errors (hrpc-stop-server hrpc-test-server-name))
  ;; Start the test on an automatically assigned port.
  (hrpc-start-server hrpc-test-server-name
                     ;; Have to expose a single method: `+'.
                     :exposed-functions '(+))
  (message "server: %s" (hrpc--get-server hrpc-test-server-name))
  (let ((default-directory hrpc-transport-layer-test-dir))
    (async-shell-command
     (format "nosetests %s" "test_external_calls.py")
     "*http-rpc-server nosetests*"))
  ;; TODO: Test server without authentication
  ;; TODO: Test server on specific port
  )


;; Tests will be run automatically when this buffer is evaluated directly.
(when (not load-file-name)
 (hrpc-run-external-tests))


(provide 'test-http-rpc-server)
;;; test-http-rpc-server.el ends here

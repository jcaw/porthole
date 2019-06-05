;; Elnode doesn't work on Emacs 26+, because it sets both the `:server` and
;; `:nowait` flags. This is not possible since Emacs 26+ - it throws an error.
;;
;; Technically, this problem occurs elsewhere.
;;
;; Emacs 26+, on the other hand, formalises this by throwing an error. so
;; `:nowait` has to be dropped entirely.
;;
;; This patch is not being merged into Elnode, so we use a monkeypatch.
;;
;; We use `el-patch' to ensure the patch is future-proofed.


(require 'elnode)
(require 'el-patch)


;; Make sure the patch is applied *after* elnode is loaded.
(with-eval-after-load 'elnode
  ;; Two functions set `:nowait' to t. We have to patch both.

  (el-patch-defun elnode/make-service (host port service-mappings request-handler defer-mode)
    "Make an actual TCP server."
    (let ((an-buf (get-buffer-create "*elnode-webserver*")))
      (make-network-process
       :name "*elnode-webserver-proc*"
       :buffer an-buf
       :server t
       :nowait (el-patch-swap 't
                              (< emacs-major-version 26))
       :host (cond
              ((equal host "localhost") 'local)
              ((equal host "*") nil)
              (t host))
       :service port
       :coding '(raw-text-unix . raw-text-unix)
       :family 'ipv4
       :filter 'elnode--filter
       :sentinel 'elnode--sentinel
       :log 'elnode--log-fn
       :plist (list
               :elnode-service-map service-mappings
               :elnode-http-handler request-handler
               :elnode-defer-mode defer-mode))))

  (el-patch-defun elnode-find-free-service ()
  "Return a free (unused) TCP port.

The port is chosen randomly from the ephemeral ports. "
  (let (myserver
        (port 50000)) ; this should be ephemeral base
    (while
        (not
         (processp
          (condition-case sig
              (setq myserver
                    (make-network-process
                     :name "*test-proc*"
                     :server t
                     :nowait (el-patch-swap 't
                                            (< emacs-major-version 26))
                     :host 'local
                     :service port
                     :family 'ipv4))
            (file-error
             (if (equal
                  "Cannot bind server socket address already in use"
                  (mapconcat 'identity (cdr sig) " "))
                 (setq port (+ 50000 (random 5000)))))))))
    (delete-process myserver)
    port))
  )


(provide 'porthole-elnode-monkeypatch)
;;; porthole-elnode-monkeypatch.el ends here

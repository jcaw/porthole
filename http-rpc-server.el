(require 'cl-lib)
(require 'elnode)
(require 'json-rpc-server)


;; We have to monkeypatch Elnode before using it on Emacs 26, possibly other
;; Emacs too.
(require 'http-rpc-server-elnode-monkeypatch)


;;; Code


(defvar hrpc--current-server nil
  "The currently running hrpc server.

If the server is running, this will be an `hrpc--server' object.
If not, it will be nil")


(cl-defstruct hrpc--server
  "Structure representing an HTTP RPC server.

This struct is intended to be used as a record of the running RPC
server."
  (port :read-only t)
  (username nil :read-only t)
  (password nil :read-only t)
  (elnode-process :read-only t)
  )


(define-error 'hrpc-http-error "An error occurred processing the HTTP request")
(define-error 'hrpc-malformed-http-request
  "The request was malformed"
  'hrpc-http-error)
(define-error 'hrpc-invalid-http-request
  "The request was invalid"
  'hrpc-http-error)


(defun hrpc-xor (arg1 arg2)
  "Exclusive or of two parameters, `ARG1' and `ARG2'.

This is not a bitwise comparison. It uses the truthiness of the
arguments to evaluate the result."
  (and (or arg1 arg2)
       (not (and arg1 arg2))))


(defun hrpc--case-insensitive-comparison (string1 string2)
  "Check if two objects are identical strings, case insensitive.

Tolerates non-string input. Will simply return nil if a
non-string is supplied."
  (and (stringp string1) (stringp string2)
       (string= (downcase string1) (downcase string2))))


(defun hrpc--similar-keys (key1 key2)
  "Are two keys the same at a fundamental level?

This is a utility function for `hrpc-alist-get'. It compares
keys. Symbol keys can match string keys, and all keys are
compared case-insensitively.

See `htpc-alist-get' for usage examples."
  ;; Can only compare strings and symbols
  (when (and (or (symbolp key1) (stringp key1))
             (or (symbolp key2) (stringp key2)))
    ;; Convert symbols to strings before comparison
    (when (symbolp key1)
      (setq key1 (format "%s" key1)))
    (when (symbolp key2)
      (setq key2 (format "%s" key2)))
    ;; Compare the two strings
    (hrpc--case-insensitive-comparison key1 key2)))


(defun hrpc-alist-get (key alist)
  "Like `alist-get', but much more flexible.

`KEY' is the key to query.
`ALIST' is the alist to search.

Will match symbols against strings. Will also match strings
case-insensitively.

For example:

  - The symbol 'key will match the string \"key\"

  - The string \"key\" match the string \"Key\"

  - The symbol 'key will match the string \"KEY\"

  - The symbol 'key will match the symbol 'KEY

  - The symbol 'key will NOT match the symbol 'keys"
  (let ((pair (assoc key alist 'hrpc--similar-keys)))
    (and pair
         (eq (type-of pair) 'cons)
         (cdr pair))))


(defun hrpc--send-400 (httpcon message)
  (elnode-send-400 httpcon message))


(defun hrpc--extract-content (httpcon)
  "Extract the body of an HTTP request.

Ordinarily, Elnode provides no method to extract the raw content
from an HTTP request. This is an extension function to allow
this.

This is ripped from `elnode--http-post-to-alist', extracted into
its own function to enable custom body handling."
  (with-current-buffer (process-buffer httpcon)
    (buffer-substring
     ;; we might have to add 2 to this because of trailing \r\n
     (process-get httpcon :elnode-header-end)
     (point-max))))


(defun hrpc--handle-request (httpcon)
  "Handle a JSON-RPC request.

This method extracts the underlying JSON-RPC request and passes
it to the RPC layer to be executed. It then responds to the
client with the result."
  (condition-case err
      (let ((headers (elnode-http-headers httpcon)))
        (message "Headers: \n%s" headers)
        (message "Process Plist: \n%s" (process-plist httpcon))
        (progn
          (let ((content-type (hrpc-alist-get "Content-Type" headers)))
            (unless content-type
              (signal 'hrpc-invalid-http-request
                      (format "No `Content-Type` provided.")))
            (unless (hrpc--case-insensitive-comparison
                     (format "%s" content-type)
                     "application/json")
              (signal 'hrpc-invalid-http-request
                      (format
                       "`Content-Type` should be application/json. Was: %s"
                       content-type))))
          (let ((content (hrpc--extract-content httpcon)))
            (unless content
              (signal 'hrpc-invalid-http-request
                      "Content could not be extracted from the request."))
            (let ((hrpc-response (jrpc-handle content)))
              (elnode-http-start httpcon 200 '("Content-Type" . "application/json"))
              (elnode-http-return httpcon hrpc-response)))))
    (hrpc-http-error
     (hrpc--send-400 httpcon (cdr err)))
    (error
     (elnode-send-500 (format "An internal error occurred. Error: %s"
                              err)))))


(defun hrpc--server-port (elnode-server-instance)
  "Get the actual port an `elnode-server' is running on.

By default, `ws-server' objects store the port which was given as
input to create the server. This may not actually be the port the
server is running on. For example, if a server was created with
dynamic port allocation, the `ws-server' object may have the port
stored as \"0\" or t - even though the network process was
allocated a specific port.

This method bypasses the flawed `ws-server' implementation and
extract the actual port from the underlying network process."
  ;; (process-contact (oref ws-server-instance process) :service)
  (warn "Port getting not implemented"))


(defun hrpc--on-linux ()
  "Is this instance of Emacs running on Linux?"
  (eq system-type 'gnu/linux))


(defun hrpc--on-windows ()
  "Is this instance of Emacs running on Windows?"
  (eq (system-type 'windows-nt)))


(defun hrpc--on-mac ()
  "Is this instance of Emacs running on MacOS?"
  (eq (system-type 'darwin)))


(defconst hrpc--port-number-filename-only
  ".emacs-rpc-server-port"
  "Filename (sans directory) of the temporary port file.")


(defconst hrpc--port-number-temp-file
  (cond ((or (hrpc--on-linux)
             (hrpc--on-mac))
         (substitute-in-file-name
          (format "$HOME/%s" hrpc--port-number-filename-only)))
        ((hrpc--on-windows)
         (format "%s\\%s"
                 (or (getenv "USERPROFILE")
                     (concat (getenv "HOMEDRIVE")
                             (getenv "HOMEPATH")))
                 hrpc--port-number-filename-only))
        (t nil))
  "Temporary file used to communicate the port number to clients.")


(defun hrpc--publish-port (port)
  "Write the server's port number to a temporary file.

This file is used so clients can determine which port the server
was dynamically allocated at creation. It is not necessary if a
fixed port was used, but it can still be useful to reduce setup."
  (unless hrpc--port-number-temp-file
    (error "Port publishing not supported on this platform."))
  ;; Make at least some effort to clean up the port file when Emacs is closed.
  ;; This will only clean it up when `kill-emacs' is called, but it's better
  ;; than nothing.
  (add-hook 'kill-emacs-hook 'hrpc--erase-port-file)
  (unwind-protect
      (progn
        (find-file hrpc--port-number-temp-file)
        ;; Erase any existing port information.
        (erase-buffer)
        (insert (format "%s" port))
        (write-file hrpc--port-number-temp-file)
        (message (concat "JSON-RPC server port written to \"%s\". This file can "
                         "be used by clients to determine the port to connect to.")
                 hrpc--port-number-temp-file))
    (kill-current-buffer)))


(defun hrpc--find-free-port (host)
  "Get a dynamically allocated port. This port should be free.

Note that using this method will probably produce a small chance
of a race condition. The port could theoretically be claimed
between this method returning and another method trying to use
the port. The chance of this happening is small, but it should
still be protected against."
  (let* ((free-port-process (make-network-process
                             :name "*hrpc-free-port-finding-service*"
                             :host host
                             :service 0
                             :server t
                             :family 'ipv4))
         (port (process-contact free-port-process :service)))
    (delete-process free-port-process)
    port))


(defun hrpc--erase-port-file ()
  "Erase the port information file, if it exists."
  (if (file-exists-p hrpc--port-number-temp-file)
      (delete-file hrpc--port-number-temp-file nil)))


(cl-defun hrpc-start-server (&key
                             (port 0)
                             username
                             password
                             (publish-port t))
  "Start a new JSON-RPC 2.0 server.

JSON-RPC requests to the server should be sent in the body of
POST requests. They should be sent according to the JSON-RPC 2.0
specification, although the server will also tolerate JSON-RPC
1.x requests. JSON-RPC protocols >2.0 are not supported.

# Port

  The server will be allocated a random port when it is started.
  This will be printed to the message buffer. Use the keyword
  argument `:PORT' to specify a port to the server.

  If the port is dynamically allocated, clients need to be able
  to discover it. To achieve this, port information can be
  written to a known file in the user's home dir. The flag
  `:PUBLISH-PORT' controls whether this file is created or not.
  By default, it is t, meaning the port information will be
  published.

  See the README for a full explanation of how to configure
  clients.

# Authentication

  The server optionally supports Basic Access Authentication to
  authenticate RPC requests:

    https://en.wikipedia.org/wiki/Basic_access_authentication

  By default, the server will be started with no authentication.
  If you would like to use Basic Access Authentication, specify
  the keyword arguments `:USERNAME' or `:PASSWORD'. You do not
  need to provide both (although it is obviously recommended).
  For example, if you provide only a password, the empty string
  will be used for the username.

Note that this method will fail if the server is already
running."
  (when hrpc--current-server
    (user-error "RPC server already running for this instance of Emacs. "
                "Please call `hrpc-stop-server' before starting another."))
  (when (or username password)
    (error "Auth not currently implemented"))
  ;; We have to handle dynamic ports differently.
  (if (member port '("0" 0 t))
      ;; Get a dynamic port. Please note, this CAN PRODUCE A RACE CONDITION if the
      ;; port is grabbed between this check and starting the server.
      ;;
      ;; This is necessary because Elnode stores a server started on port 0, in...
      ;; The port 0 slot. So you can only start one server on port 0.
      ;;
      ;; Because this can produce a race condition, we try multiple times just in
      ;; case that race condition pops up.
      (let (
            ;; Try to claim a dynamic port this many times. The chance of the race
            ;; condition occurring 500 times is infinitesimally small.
            (max-attempts 500))
        (unless (catch 'server-started
                  (dotimes (i max-attempts)
                    (setq port (hrpc--find-free-port "localhost"))
                    (condition-case err
                        ;; Try and start the server with these parameters.
                        ;;
                        ;; If the port is taken, it will throw a file-error. If
                        ;; Elnode already has a server running on this port (it
                        ;; shouldn't, but just in case), the server creation
                        ;; process will fail silently, and return nil. Handle
                        ;; both cases.
                        (when (elnode-start
                               'hrpc--handle-request
                               :port port
                               :host "localhost")
                          ;; If the server was started successfully, we're done. We
                          ;; have a server - break out and continue.
                          (throw 'server-started t))
                      (file-error nil)
                      (error
                       (signal (car err) (cdr err)))
                      )
                    (throw 'server-started nil)))
          ;; If the server could not be started after many retries, we just raise an error.
          (error "%s" (concat (format "Tried to start on a free port %s times."
                                      max-attempts)
                              " Failed each time. Server could not be started."))))
    (when (alist-get port elnode-server-socket)
      (error "Elnode already has a server running on this port."))
    (elnode-start
     'hrpc--handle-request
     :port port
     :host "localhost"))
  ;; If we've reached this point, the server has started successfully.
  (setq hrpc--current-server
        (make-hrpc--server
         :port port
         :username username
         :password password
         ;; We store the actual Elnode server process too, in case we wish to
         ;; query it directly.
         :elnode-process (alist-get port elnode-server-socket)))
  (add-hook 'kill-emacs-hook 'hrpc--stop-server-safe)
  (message "JSON-RPC server running on port %s" port)
  (when publish-port
    (hrpc--publish-port port))
  hrpc--current-server)


(defun hrpc-stop-server ()
  "Stop the active JSON-RPC 2.0 server.

This method will fail if no server is running."
  ;; Erase the port file up front, just in case it exists when the server is
  ;; down.
  (hrpc--erase-port-file)
  (unless hrpc--current-server
    (error "Server not running."))
  (elnode-stop (hrpc--server-port hrpc--current-server))
  (setq hrpc--current-server nil))


(defun hrpc--stop-server-safe (&rest _)
  "Like `hrpc-stop-server', but this function will not raise errors.

For example, it can safely be attached to the kill-emacs-hook."
  (ignore-errors (hrpc-stop-server)))


(provide 'http-rpc-server)
;;; http-rpc-server.el ends here

(require 'web-server)
(require 'json-rpc-server)


;;; Code


(defvar hrpc--server nil
  "The currently running hrpc server.")


(define-error 'hrpc-http-error "An error occurred processing the HTTP request")
(define-error 'hrpc-malformed-http-request
  "The request was malformed"
  'hrpc-http-error)
(define-error 'hrpc-invalid-http-request
  "The request was invalid"
  'hrpc-http-error)

(define-error 'hrpc-missing-key "The specified key could not be found")


(defun hrpc-xor (arg1 arg2)
  "Exclusive or of two parameters, `ARG1' and `ARG2'.

This is not a bitwise comparison. It uses the truthiness of the
arguments to evaluate the result."
  (and (or arg1 arg2)
       (not (and arg1 arg2))))


(defun hrpc--extract-post-content (request)
  "Manually extract the content from a `ws-request'.

`web-server' doesn't add content to the response unless it's of a
specific type. This is a hack to extract it manually."
  ;; TODO: Ensure request is a POST

  ;; This method relies on pending having the full request content.
  (with-slots (pending) request
    ;; TODO: Handle each way things might be malformed here
    (let* ((content-length-string (condition-case nil
                                      (hrpc--extract-header request :CONTENT-LENGTH)
                                    (error nil))))
      (when (eq nil content-length-string)
        (signal 'hrpc-malformed-http-request
                "No `Content-Length` parameter"))
      (let ((content-length (condition-case nil
                                (string-to-number content-length-string)
                              (error nil)))
            (headers-end (string-match "\r\n\r\n" pending)))
        (unless (integerp content-length)
          (signal 'hrpc-malformed-http-request
                  "`Content-Length` was not an integer"))
        (when (eq nil headers-end)
          (signal 'hrpc-malformed-http-request
                  "No double line breaks found in POST request. Content not acceptable"))
        (let* (
               ;; Have to offset the content start. It will be four characters
               ;; after the end of the headers because of the double newline.
               (content-start (+ 4 headers-end))
               (content-end (+ content-start content-length)))
          (condition-case nil
              (substring pending content-start content-end)
            (error
             (signal 'hrpc-malformed-http-request
                     "POST content was shorter than `Content-Length` parameter"))))))))


(defun hrpc--case-insensitive-comparison (string1 string2)
  "Check if two objects are identical strings, case insensitive.

Tolerates non-string input. Will simply return nil if a
non-string is supplied."
  (and (stringp string1) (stringp string2)
       (string= (downcase string1) (downcase string2))))


(defun hrpc--extract-header (request header-key)
  "Extract a header named by `HEADER-KEY'."
  (with-slots (headers) request
    (condition-case nil
        (cdr (assoc header-key headers))
      (signal 'hrpc-missing-key "Key %s could not be found in headers" header-key))))


(defun hrpc--send-400 (request message)
  (with-slots (process) request
    (ws-response-header process 400 '("Content-type" . "html/plain"))
    (process-send-string process message)))


(defun hrpc--handle-request (request)
  "Handle a JSON-RPC request.

This method extracts the underlying JSON-RPC request and passes
it to the RPC layer to be executed. It then responds to the
client with the result."
  (with-slots (process headers context) request
    (condition-case err
        (progn
          (let ((content-type context))
            (unless content-type
              (signal 'hrpc-invalid-http-request (format "No `Content-Type` provided.")))
            (unless (hrpc--case-insensitive-comparison
                     (format "%s" content-type)
                     "application/json")
              (signal 'hrpc-invalid-http-request
                      (format
                       "`Content-Type` should be application/json. Was: %s"
                       content-type))))
          (let* ((content (hrpc--extract-post-content request))
                 (hrpc-response (jrpc-handle content)))
            (ws-response-header process 200 '("Content-type" . "application/json"))
            (process-send-string process hrpc-response)))
      ((hrpc-malformed-http-request hrpc-invalid-http-request)
       (hrpc--send-400 request (cdr err))))))


(defun hrpc--server-port (ws-server-instance)
  "Get the actual port a `ws-server' is running on.

By default, `ws-server' objects store the port which was given as
input to create the server. This may not actually be the port the
server is running on. For example, if a server was created with
dynamic port allocation, the `ws-server' object may have the port
stored as \"0\" or t - even though the network process was
allocated a specific port.

This method bypasses the flawed `ws-server' implementation and
extract the actual port from the underlying network process."
  (process-contact (oref ws-server-instance process) :service))


(defun hrpc--auth-handler (&optional username password)
  "Create a request handler that requires authentication.

This handler works the same as `hrpc--handle-request', but it
requires Basic Access Authentication for the request to be
processed.

`USERNAME' and `PASSWORD' are the authentication credentials to
use."
  (when (and username (not password))
    (setq password ""))
  (when (and password (not username))
    (setq username ""))
  (ws-with-authentication
   'hrpc--handle-request
   ;; Build an alist with just this user
   (list (cons username password)))
  ;; TODO: Meaningful responses on failed authentication, internal errors, etc.
  )


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


(defun hrpc--erase-port-file ()
  "Erase the port information file, if it exists."
  (if (file-exists-p hrpc--port-number-temp-file)
      (delete-file hrpc--port-number-temp-file nil)))


(cl-defun hrpc-start-server (&key
                             (port "0")
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
  (when hrpc--server
    (user-error "RPC server already running for this instance of Emacs. "
                "Please call `hrpc-stop-server' before starting another."))
  (setq hrpc--server
        (ws-start
         (if (or username password)
             (hrpc--auth-handler username password)
           'hrpc--handle-request)
         port))
  (add-hook 'kill-emacs-hook 'hrpc-stop-server)
  (let ((port (hrpc--server-port hrpc--server)))
    (message "JSON-RPC server running on port %s" port)
    (when publish-port
     (hrpc--publish-port port))))


(defun hrpc-stop-server ()
  "Stop the active JSON-RPC 2.0 server.

This method will fail if no server is running."
  ;; Erase the port file up front, just in case it exists when the server is
  ;; down.
  (hrpc--erase-port-file)
  (unless hrpc--server
    (error "Server not running."))
  (ws-stop hrpc--server)
  (setq hrpc--server nil))


(defun hrpc--stop-server-safe (&rest _)
  "Like `hrpc-stop-server', but this function will not raise errors.

For example, it can safely be attached to the kill-emacs-hook."
  (ignore-errors (hrpc-stop-server)))


(provide 'http-rpc-server)
;;; http-rpc-server.el ends here

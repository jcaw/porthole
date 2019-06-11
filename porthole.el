;;; porthole.el --- RPC Servers in Emacs            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  GitHub user "Jcaw"

;; Author:  GitHub user "Jcaw"
;; URL: https://github.com/jcaw/porthole
;; Version: 0.2.2
;; Keywords: comm, rpc, http, json
;; Package-Requires: ((emacs "26") (elnode "0.9.9.8") (f "0.19.0") (json-rpc-server "0.1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Porthole lets you start RPC servers in Emacs. These servers allow Elisp to be
;; invoked remotely via HTTP requests.

;; You can expose data that exists within Emacs, or control Emacs from an
;; external program. You can also execute Elisp functions on data (such as text)
;; that exists outside Emacs.

;; ---

;; Porthole servers are designed to "just work." All your server needs is a name
;; and clients will be able to find it automatically. Here's a typical workflow:

;; In Emacs:

;;   1. Pick a name and start your server.
;;   2. Tell it which functions you want to be available to RPC calls.

;;   Now, continue using Emacs.

;; In the Client:

;;   1. Load the connection information from your server's session file (this
;;      file has a known path).
;;   2. POST a JSON-RPC request to the server.

;;   *Emacs executes your RPC call and returns the result.*

;;   3. Parse the JSON-RPC 2.0 object you received.

;; There's even a Python Client to handle the client-side automatically.

;; See README.md for more information and usage examples.

;; ---

;; README.md:     https://github.com/jcaw/porthole

;; Python Client: https://github.com/jcaw/porthole-python-client



;;; Code:


(require 'cl-lib)
(require 'elnode)
(require 'f)
(require 'json-rpc-server)


(defgroup porthole nil
  "Group relating to Porthole RPC servers."
  :prefix "porthole-"
  :link `(url-link :tag "Send Bug Report"
                   "https://github.com/jcaw/porthole/issues")
  :link '(url-link :tag "Other Emacs packages by Jcaw"
                   "https://github.com/jcaw?utf8=%E2%9C%93&tab=repositories&q=&type=source&language=emacs+lisp")
  :link '(url-link :tag "Homepage"
                   "https://github.com/jcaw/porthole")
  :group 'comm)


(defvar porthole--running-servers '()
  "Alist of running Porthole servers.

Maps server names (strings) to their respective `porthole--server'
objects.")


(cl-defstruct porthole--server
  "Struct representing a Porthole server.

This struct is intended to be used as a record of a running RPC
server."
  (name nil
        :read-only t
        :type string)
  (port nil
        :read-only t)
  (exposed-functions '())
  (username nil
            :read-only t)
  (password nil
            :read-only t)
  (elnode-process nil
                  :read-only t)
  )


(defconst porthole--on-linux (eq system-type 'gnu/linux)
  "Is this instance of Emacs running on Linux?")


(defconst porthole--on-windows (eq system-type 'windows-nt)
  "Is this instance of Emacs running on Windows?")


(defconst porthole--on-mac (eq system-type 'darwin)
  "Is this instance of Emacs running on MacOS?")


(defun porthole--get-linux-temp-dir ()
  "Get a user-only temp dir on Linux, to store the server info in."
  ;; If the runtime dir isn't available, fall back to the home dir.
  (or (getenv "XDG_RUNTIME_DIR")
      (and (getenv "HOME")
           (f-join (getenv "HOME") "tmp"))
      (display-warning
       "porthole"
       (concat "Neither $XDG_RUNTIME_DIR nor $HOME could be read from "
               "the environment. Clients will not be able to automatically "
               "connect to servers by reading the temp file."))))


(defconst porthole--base-temp-dir
  (cond (porthole--on-linux
         (porthole--get-linux-temp-dir))
        (porthole--on-windows
         (getenv "TEMP"))
        (porthole--on-mac
         (substitute-in-file-name "$HOME/Library/"))
        (t
         ;; Use the same method as Linux on unknown systems.
         (display-warning
          "porthole"
          (concat "Unrecognised system type. Don't know where to find the "
                  "temp directory. Using the same method as Linux."))
         (porthole--get-linux-temp-dir)))
  "The base temp directory to use.

This will be dependent on the current system.")


(defconst porthole--session-info-dir
  (when porthole--base-temp-dir
    (f-join porthole--base-temp-dir "emacs-porthole"))
  "Directory in which to store Porthole servers.

This is a known name, so clients can also read it and gather
relevant information.")


(defconst porthole--session-file-name "session.json"
  "Local filename where server session information is stored.

This file is used to communicate session information to clients
automatically. It will be a JSON-encoded object mapping names to
values. Possible names are \"port\", \"username\" and
\"password\".")


(defun porthole--case-insensitive-comparison (string1 string2)
  "Check if two objects are identical strings, case insensitive.

Tolerates non-string input. Will simply return nil if a
non-string is supplied.

`STRING1' and `STRING2' are the strings to compare."
  (and (stringp string1) (stringp string2)
       (string= (downcase string1) (downcase string2))))


(defun porthole--similar-keys (key1 key2)
  "Are two keys the same at a fundamental level?

This is a utility function for `porthole--alist-get'. It compares
keys. Symbol keys can match string keys, and all keys are
compared case-insensitively.

See `htpc-alist-get' for usage examples.

`KEY1' and `KEY2' are the keys to compare."
  ;; Convert symbols to strings before comparison
  (unless (stringp key1)
    (setq key1 (format "%s" key1)))
  (unless (stringp key2)
    (setq key2 (format "%s" key2)))
  ;; Compare the two strings
  (porthole--case-insensitive-comparison key1 key2))


(defun porthole--alist-get (key alist)
  "Like `alist-get', but much more flexible.

Will match symbols against strings. Will also match strings
case-insensitively.

For example:

  - The symbol 'key will match the string \"key\"

  - The string \"key\" match the string \"Key\"

  - The symbol 'key will match the string \"KEY\"

  - The symbol 'key will match the symbol 'KEY

  - The symbol 'key will NOT match the symbol 'keys

Arguments:

`KEY' - the key to query.

`ALIST' - the alist to search."
  (let ((pair (assoc key alist 'porthole--similar-keys)))
    (and pair
         (eq (type-of pair) 'cons)
         (cdr pair))))


(defun porthole--alist-remove (key alist)
  "Remove all pairs referenced by `KEY' in `ALIST'.

Returns a new alist without those elements."
  (cl-delete key alist :key #'car :test #'porthole--similar-keys))


(defun porthole--random-sha256-key ()
  "Generate a random sha256 key."
  ;; Make 400 random int strings, join them, then hash the result. That should
  ;; be suitably unique.
  (let ((long-random-number
         (apply 'concat (mapcar (lambda (_)
                                  (format "%s" (random 9999999999999)))
                                (number-sequence 0 400)))))
    (secure-hash 'sha256 long-random-number)))


(defun porthole--extract-content (httpcon)
  "Extract the body of an HTTP request.

Ordinarily, Elnode provides no method to extract the raw content
from an HTTP request. This is an extension function to allow
this.

This is ripped from `elnode--http-post-to-alist', extracted into
its own function to enable custom body handling.

`HTTPCON' is the Elnode connection object."
  (with-current-buffer (process-buffer httpcon)
    (buffer-substring
     ;; we might have to add 2 to this because of trailing \r\n
     (process-get httpcon :elnode-header-end)
     (point-max))))


(defun porthole--authenticate (headers porthole-server)
  "Ensure a request has valid authentication.

If not, a 401 response trigger will be thrown.

Arguments:

`HEADERS' - an alist of an Elnode `httpcon' object's headers.

`PORTHOLE-SERVER' - the name of the server to check against."
  (let ((target-username (porthole--server-username porthole-server))
        (target-password (porthole--server-password porthole-server)))
    ;; Only perform authentication when a username or password are required.
    ;; Otherwise, it's authenticated by default.
    (when (or target-username target-password)
      (let ((authorization (porthole--alist-get "Authorization" headers)))
        (unless authorization
          (porthole--end-unauthenticated "authentication required"))
        ;; The code below that actually parses the authorization header is
        ;; ripped from `web-server.el' by Eric Schulte. See the method
        ;; `ws-parse' for more information.
        (string-match "\\([^[:space:]]+\\) \\(.*\\)$" authorization)
        (let ((protocol (match-string 1 authorization))
              (credentials (match-string 2 authorization)))
          (unless protocol
            (porthole--end-400 "Invalid authorization string"))
          (unless credentials
            (porthole--end-400 "No credentials provided"))
          (unless (porthole--case-insensitive-comparison
                   protocol "basic")
            ;; If they've supplied the wrong protocol, just tell them
            ;; authentication is required.
            (porthole--end-unauthenticated "authentication required"))
          (let ((decoded-credentials (base64-decode-string credentials)))
            ;; `integerp' because we might match at position 0.
            (if (integerp (string-match ":" decoded-credentials))
                (let ((provided-username (substring decoded-credentials 0
                                                    (match-beginning 0)))
                      (provided-password (substring decoded-credentials
                                                    (match-end 0))))
                  (unless (and (equal provided-username target-username)
                               (equal provided-password target-password))
                    (porthole--end-unauthenticated "invalid credentials")))
              (porthole--end-400 (format "bad credentials: \"%s\""
                                         decoded-credentials)))))))))


(defun porthole--server-from-port (port)
  "Get the `porthole--server' object running on `PORT'.

Throws an error if no server could be found running on that
port."
  (catch 'server-found
    ;; Check port against each running server
    (mapc (lambda (server-pair)
            (let* ((server (cdr server-pair))
                   (server-port (porthole--server-port server)))
              (when (eq port server-port)
                ;; Server matches! Return it.
                (throw 'server-found server))))
          porthole--running-servers)
    ;; No server was found. Raise an error.
    (error "%s" (format "No `porthole--server' could be found running on port %s"
                        port))))


(defun porthole--server-from-httpcon (httpcon)
  "Get the `porthole--server' that this `HTTPCON' is connecting to.

Returns a `porthole--server' object.

Throws an error if no object could be found."
  ;; We get the underlying Elnode server that's serving the connection, then we
  ;; get the `porthole--server' from that.
  ;;
  ;; We use the port as the key to extract the `porthole--server'. Why introduce the
  ;; extra step? Why not use the Elnode server as the key? Well, ports are
  ;; probably more robust. What if Elnode restarted its server for some reason,
  ;; such as rebooting after a fatal error? The ports should still match, even
  ;; if the servers don't.
  (or (porthole--server-from-port
       (let ((underlying-elnode-server (process-get httpcon :server)))
         (process-contact underlying-elnode-server :service)))
      ;; Block if no server found. Should never get here but just in case.
      (error "%s" "No `porthole--server' could be found for `HTTPCON'")))


(defun porthole--end (response-code
                      headers
                      content)
  "Respond to the request and stop the handler.

This method (or another method which calls it) should be used
when a response needs to be sent. It will throw a signal to the
top of the handler, telling the handler to respond to the client
and return immediately.

This is the lowest level method to end a handler. Ordinarily, a
higher-level handler should be used.

Arguments:

`RESPONSE-CODE' - the HTTP response code to send.

`HEADERS' - the headers to send in the HTTP response. This should
  include the Content-type.

`CONTENT' - the content of the HTTP response."
  (throw 'porthole-finish-handling `((response-code . ,response-code)
                                     (headers       . ,headers)
                                     (content       . ,content))))


(defun porthole--end-simple (response-code
                             content-type
                             content)
  "Respond to a porthole request and stop the handler.

This is a wrapper that provides a simpler interface than the
underlying `porthole-end'.

Arguments:

`RESPONSE-CODE' - the HTTP response code to send.

`CONTENT-TYPE' - the Content-Type of the response.

`CONTENT' - the content of the HTTP response."
  (porthole--end response-code
                 `(("Content-Type" . ,content-type))
                 content))


(defun porthole--end-400 (message)
  "End processing of a handler and respond with a 400 response.

`MESSAGE' is the message to send in the content."
  (porthole--end-simple 400 "text/html" message))


(defun porthole--end-unauthenticated (message)
  "End processing of a handler and respond with a 401 response.

`MESSAGE' is the message to send in the content."
  (porthole--end 401
                 '(("WWW-Authenticate" . "Basic realm=\"emacs-rpc-server\"")
                   ("Content-Type" . "text/html"))
                 message))


(defun porthole--end-success (json-response)
  "End processing of a handler with a successful JSON response.

`JSON-RESPONSE' is the string-encoded JSON content to send."
  (unless (stringp json-response)
    (error "`JSON-RESPONSE' should be string. Was: %s" (type-of json-response)))
  (porthole--end-simple 200 "application/json" json-response))


(defun porthole--end-error-with-info (err)
  "End the handler with a 500 and feed error information back to the client.

`ERR' is the error that was raised. The response will be a 500
response containing a JSON object that encapsulated this error."
  (porthole--end-simple 500 "application/json"
                        (json-encode
                         `((details . ((error-symbol ,(car err))
                                       (data ,(cdr err))))))))


(defun porthole--end-error-no-info ()
  "End the handler with a 500. Do not share error information with the client."
  (porthole--end-simple 500 "application/json"
                        (json-encode
                         `((details . :json-null)))))


(defun porthole--handle-authenticated (httpcon headers porthole-server)
  "Handle a request, after it's been authenticated.

The authentication process requires the `HEADERS' and the
`PORTHOLE-SERVER' to be extracted from the Elnode `HTTPCON'
object. Pass these down to this function to avoid duplication of
effort."
  (condition-case-unless-debug err
      (let ((content-type (porthole--alist-get "Content-Type" headers)))
        (unless content-type
          (porthole--end-400 "No `Content-Type` provided."))
        (unless (porthole--case-insensitive-comparison
                 (format "%s" content-type)
                 "application/json")
          (porthole--end-400
           (format "`Content-Type` should be application/json. Was: %s"
                   content-type)))
        (let ((content (porthole--extract-content httpcon)))
          (unless content
            (porthole--end-400 "Content could not be extracted from the request."))
          (let* ((exposed-functions (porthole--server-exposed-functions
                                     porthole-server))
                 (porthole-response (jrpc-handle content exposed-functions)))
            (porthole--end-success porthole-response))))
    ;; Catch unexpected errors.
    (error
     ;; Since the user is authenticated, we can share information about the
     ;; underlying error with the client.
     (porthole--end-error-with-info err))))


(defun porthole--handle-request (httpcon)
  "Handle a JSON-RPC request.

`HTTPCON' is the Elnode HTTP connection object. This method will
be called automatically by Elnode.

This method extracts the underlying JSON-RPC request and passes
it to the underlying RPC layer, `json-rpc-server', to be
executed. It then responds to the client with the result."
  (porthole--respond
   httpcon
   (catch 'porthole-finish-handling
     ;; Don't catch errors when debugging, so we can trigger the debugger.
     ;;
     ;; Note that this will cause the connection with the client to terminate
     ;; unexpectedly.
     ;;
     ;; TODO: Take a second look. Should debugging follow the method in
     ;;   `json-rpc-server'?
     (condition-case-unless-debug nil
         (let ((headers (elnode-http-headers httpcon))
               (porthole-server (porthole--server-from-httpcon httpcon)))
           ;; Authenticate first
           (porthole--authenticate headers porthole-server)
           (porthole--handle-authenticated httpcon headers porthole-server))
       ;; Catch unexpected errors.
       (error
        ;; Before authentication, we send no details about the internal error.
        (porthole--end-error-no-info))))))


(defun porthole--respond (httpcon response-alist)
  "Send a response to an HTTP request.

The details of the response should be specified in
`RESPONSE-ALIST'. This should be an alist thrown by
`porthole--end'.

`HTTPCON' is the Elnode HTTP connection object."
  (apply 'elnode-http-start
         (append (list httpcon)
                 (list (alist-get 'response-code response-alist))
                 (alist-get 'headers response-alist)))
  (elnode-http-return httpcon (alist-get 'content response-alist)))


(defun porthole--find-free-port (host)
  "Get a dynamically allocated port. This port should be free.

`HOST' is the hostname to use. It should probably be
\"localhost\".

Note that using this method, there's a small chance of a race
condition. The port could theoretically be claimed between this
method returning and another method trying to use the port. The
chance of this happening is tiny, but it should still be
protected against."
  (let* ((free-port-process (make-network-process
                             :name "*porthole-free-port-finding-service*"
                             :host host
                             :service 0
                             :server t
                             :family 'ipv4))
         (port (process-contact free-port-process :service)))
    (delete-process free-port-process)
    port))


(defun porthole--get-session-folder (name-of-server)
  "Get the session info folder for `NAME-OF-SERVER'."
  ;; Session file should be at:
  ;; <porthole-info-dir>/<name-of-server>/session.json
  (f-join porthole--session-info-dir name-of-server))


;;;###autoload
(defun porthole-get-session-file-path (name-of-server)
  "Get the path of the session file for server with name `NAME-OF-SERVER'."
  ;; Session file should be at:
  ;; <temp-dir>/emacs-porthole/<name-of-server>/session.json
  (f-join (porthole--get-session-folder name-of-server)
          porthole--session-file-name))


(cl-defun porthole--publish-session-file (name
                                          port
                                          username
                                          password
                                          &key
                                          (publish-port t)
                                          (publish-username t)
                                          (publish-password t))
  "Write the current session's info to the known session file."
  ;; TODO: Flesh out docstring.
  ;;
  ;; TODO: Maybe don't publish the file at all if none of the publishish flags
  ;;   are t? It shows other programs that the server is at least running.
  ;;
  ;; Session file should be at:
  ;; <temp-dir>/emacs-porthole/<name-of-server>/session.json
  (let ((info '()))
    (when publish-port
      (push `(port . ,port)
            info))
    (when publish-username
      (push `(username . ,username)
            info))
    (when publish-password
      (push `(password . ,password)
            info))
    (let ((info-as-json (json-encode info))
          (info-folder (porthole--get-session-folder name))
          (info-filename (porthole-get-session-file-path name)))
      (unless (f-dir-p info-folder)
        ;; Make the app and server directories, if they don't exist.
        (make-directory info-folder t))
      (with-temp-file info-filename
        (insert info-as-json))
      ;; Return the filename for the info file
      info-filename)))


(defun porthole--erase-session-file (name-of-server)
  "Delete a server's session file. `NAME-OF-SERVER' indicates the server.

Also deletes the server's session folder."
  (let ((info-folder (porthole--get-session-folder name-of-server))
        (info-filename (porthole-get-session-file-path name-of-server)))
    (when (f-file-p info-filename)
      (f-delete info-filename))
    (when (f-dir-p info-folder)
      (f-delete info-folder t))))


(defun porthole--assert-symbol (symbol)
  "Ensure that `SYMBOL' is actually a symbol.

This method does not allow nil or t, even though they are
technically symbols.

An error will be raised if `SYMBOL' is not a symbol."
  (unless (and (symbolp symbol)
               ;; Protect against nil
               symbol
               ;; Protect against t
               (not (eq t symbol)))
    (error "%s" (format "Not a symbol. Type: %s. Value: %s"
                        (type-of symbol) symbol))))


(defun porthole--running-server-names ()
  "Get the names of all running servers."
  (mapcar 'car porthole--running-servers))


;;;###autoload
(defun porthole-get-server (name-of-server)
  "Get the `porthole--server' with name `NAME-OF-SERVER'.

Returns nil if no server with this name is running."
  (porthole--alist-get name-of-server porthole--running-servers))


(defun porthole-server-running-p (name-of-server)
  "Return t if a server with `NAME-OF-SERVER' is already running.

Note that server names are case-insensitive."
  (if (porthole-get-server name-of-server) t nil))


(defun porthole--assert-server-running (name-of-server &optional message)
  "Ensure a server is running. If not, raise an error.

`NAME-OF-SERVER' is the name of the server to check"
  (unless (porthole-server-running-p name-of-server)
    (error "%s" (or message
                    (format "No server named \"%s\" is running" name-of-server)))))


(defun porthole--assert-server-not-running (name-of-server &optional message)
  "If a server with `NAME-OF-SERVER' is running, raise an error.

`NAME-OF-SERVER' is the name of the server to check."
  (when (porthole-server-running-p name-of-server)
    (error "%s" (or message
                    (format "A server with the name \"%s\" is already running"
                            name-of-server)))))


(defun porthole--assert-valid-server-name (name-of-server)
  "Ensure `NAME-OF-SERVER' is a valid server name. Raise an error if not.

Server names may only contain alphanumeric characters, and
dashes.

Valid:

  \"my-server-02\"

Invalid:

  \"a_server_with? punctuation\""
  (unless (string-match "^[a-zA-Z0-9-]+$" name-of-server)
    (error "Server names may only contain alphanumeric characters and dashes")))


;;;###autoload
(cl-defun porthole-start-server-safe (name &key (exposed-functions '()))
  "If the Porthole server with `NAME' is not running, start it.

This is a safe version of `porthole-start-server' that doesn't
raise an error when the server is already running. Use this to
start extensible servers. For example, you may have a client that
contacts a single server, but that server could be modular. Each
module can use this method to ensure the server is running after
it's loaded.

If the server *is* running, `EXPOSED-FUNCTONS' will be exposed in
addition to the currently exposed functions. If not, it will be
used to initialise a new server.

This function should only be used for automatic servers. Don't
use it for manually configured servers.

See `porthole-start-server' for more about starting servers."
  (unless (porthole-server-running-p name)
    (porthole-start-server name :exposed-function exposed-functions))
  (porthole-expose-functions name exposed-functions))


;;;###autoload
(cl-defun porthole-start-server (name &key (exposed-functions '()))
  "Start a new Porthole server.

`NAME' should be the name of the server.

This is the intended way to start a server. The server will be
started on a dynamically allocated port, with a random SHA-256
username and password. These credentials will be published to the
server's session file, which is accessible only to the user.

If you would like more control over the server (for example,
specifying the username, port or password) please refer to
`porthole-start-server-advanced'. The documentation for that
function also contains more information on how servers can be
used.

Arguments:

`NAME' - This is the name Porthole uses to identify your server.
  It should be simple, memorable and unique. Only alphanumeric
  characters and dashes are allowed

`:EXPOSED-FUNCTIONS' - A list of functions to expose immediately
  upon server creation. See `porthole-expose-function' for more.
  Default: nil"
  (let ((username (porthole--random-sha256-key))
        (password (porthole--random-sha256-key)))
    (porthole-start-server-advanced name
                                    :port 0
                                    :username username
                                    :password password
                                    :publish-port t
                                    :publish-username t
                                    :publish-password t
                                    :exposed-functions exposed-functions)))


;;;###autoload
(cl-defun porthole-start-server-advanced (name-of-server
                                          &key
                                          (port 0)
                                          (username nil)
                                          (password nil)
                                          (publish-port t)
                                          (publish-username t)
                                          (publish-password t)
                                          (exposed-functions '()))
  "Start a new Porthole server with advanced configuration.

JSON-RPC requests to the server should be sent in the body of
POST requests. They should be sent according to the JSON-RPC 2.0
specification, although the server will also tolerate JSON-RPC
1.x requests. JSON-RPC protocols >2.0 are not supported.

The server optionally supports Basic Access Authentication to
authenticate RPC requests:

  https://en.wikipedia.org/wiki/Basic_access_authentication

If you would like to use Basic Access Authentication, specify the
keyword arguments `:USERNAME' or `:PASSWORD'. You do not need to
provide both (although it is obviously recommended). For example,
if you provide only a password, the empty string will be used for
the username.

By default, session information is written to a known file so
local clients can connect automatically. This file is only
accessible to the current user. See the README for a full
explanation of how to configure clients. This behavior is
configurable - see the arguments for more.

Arguments:

`NAME-OF-SERVER' - This is the name Porthole uses to identify your
  server. It should be simple, memorable and unique. Server
  creation will fail if a server with the same name is already
  running. This argument is mandatory.

`:PORT' - the port the server will run on. Use a value of 0 to
  have the server start on a dynamic port. Default: 0

`:USERNAME' - The username to use for Basic Authentication. If
  this and the password are unspecified, no authentication will
  be used. Default: nil

`:PASSWORD' - The password to use for Basic Authentication. If
  this and the username are unspecified, no authentication will
  be used. Default: nil

`:PUBLISH-PORT' - Whether to publish the port to the server's
  session file. Default: t

`:PUBLISH-USERNAME' - Whether to publish the username to the
  server's session file. Default: t

`:PUBLISH-PASSWORD' - Whether to publish the password to the
  server's session file. Default: t

`:EXPOSED-FUNCTIONS' - A list of functions to expose immediately
  upon server creation. See `porthole-expose-function' for more.
  Default: nil"
  (porthole--assert-valid-server-name name-of-server)
  (porthole--assert-server-not-running name-of-server)
  ;; Every function name should be a symbol.
  (mapc 'porthole--assert-symbol exposed-functions)
  (let ((assigned-port))
    ;; We have to handle dynamic ports differently.
    (if (member port '("0" 0 t))
        (setq assigned-port (porthole--start-on-dynamic-port))
      ;; TODO: Maybe explicitly check to see if this port is free across the
      ;; board?
      (when (alist-get port elnode-server-socket)
        ;; Have to manually check that Elnode doesn't have a server on this port,
        ;; because it will fail silently otherwise.
        (error "Elnode already has a server running on this port"))
      ;; TODO: Find some way to name the server process so it doesn't have a
      ;;   generic Elnode name and looks like a porthole server?
      (unless (elnode-start
               'porthole--handle-request
               :port port
               :host "localhost")
        (error "The Elnode server was not started. Reason unknown"))
      (setq assigned-port port))
    ;; If we've reached this point, the Elnode server has started successfully.
    ;; Now create a `porthole--server' object to wrap up all the server
    ;; information and push it onto the list.
    (push (cons name-of-server (make-porthole--server
                             :name name-of-server
                             :port assigned-port
                             :username username
                             :password password
                             :exposed-functions exposed-functions
                             ;; We store the actual Elnode server process too, in case we
                             ;; wish to query it directly.
                             :elnode-process (alist-get port elnode-server-socket)))
          porthole--running-servers)
    (message "porthole: RPC server \"%s\" running on port %s"
             name-of-server assigned-port)
    (porthole--publish-session-file
     name-of-server assigned-port username password
     :publish-port publish-port
     :publish-username publish-username
     :publish-password publish-password)
    name-of-server))


(defun porthole--start-on-dynamic-port ()
  "Start an underlying Elnode server on a dynamic port.

This requires a different approach to starting on a specific
port. See the comments in this method for details. Please note,
this is an internal method intended for
`porthole-start-server-advanced'.

Returns the port that was assigned to the server."
  ;; Please note, this CAN PRODUCE A RACE CONDITION if the port is grabbed
  ;; between this check and starting the server.
  ;;
  ;; This is necessary because Elnode stores a server started on port 0,
  ;; in... The port 0 slot. So you can only start one server on port 0.
  ;; Rather than modifying the underlying Elnode dictionary (Elnode might
  ;; change its structure), we pre-allocate a specific port.
  ;;
  ;; Because this can produce a race condition, we try multiple times.
  (let (assigned-port
        ;; Try to claim a dynamic port this many times. The chance of the race
        ;; condition occurring 500 times is infinitesimally small.
        (max-attempts 500))
    (unless (catch 'server-started
              (dotimes (i max-attempts)
                (setq assigned-port (porthole--find-free-port "localhost"))
                (condition-case nil
                    ;; Try and start the server with these parameters.
                    ;;
                    ;; If the port is taken, it will throw a file-error. If
                    ;; Elnode already has a server running on this port (it
                    ;; shouldn't, but just in case), the server creation
                    ;; process will fail silently, and return nil. Handle
                    ;; both cases.
                    (when (elnode-start
                           'porthole--handle-request
                           :port assigned-port
                           :host "localhost")
                      ;; If the server was started successfully, we're done. We
                      ;; have a server - break out and continue.
                      (throw 'server-started t))
                  (file-error nil))
                ;; The server wasn't started succesfully - try again
                (throw 'server-started nil)))
      ;; If the server could not be started after many retries, we just raise an error.
      (error "%s" (format (concat
                           "Tried to start on a free port %s times."
                           " Failed each time. Server could not be started")
                          max-attempts)))
    ;; Return the assigned port
    assigned-port))


(defun porthole-stop-server (name-of-server)
  "Stop a porthole server.

`NAME-OF-SERVER' is the name of the server to stop.

This method will fail if no server with that name is running.

This is only meant to be used on your server. Please don't try to
stop servers started by other packages."
  ;; Erase the server info file up front - it may exist even though the server
  ;; isn't running.
  (porthole--erase-session-file name-of-server)
  (porthole--assert-server-running name-of-server)
  (let* ((server (porthole-get-server name-of-server))
         (port (porthole--server-port server)))
    ;; Stop the actual HTTP process
    (elnode-stop port)
    ;; Elnode will message stuff here that might be misleading - explain it
    ;; explicitly.
    (message
     (concat "porthole: Please ignore any messages that say \"found the "
             "server process - NOT deleting\" - this is just logging from "
             "Elnode (it can't be suppressed)."))
    (message "Porthole server \"%s\" stopped." name-of-server)
    ;; Remove the server from the list of running servers.
    (setq porthole--running-servers
          (porthole--alist-remove name-of-server porthole--running-servers))))


(defun porthole--stop-server-safe (name-of-server &rest _)
  "Like `porthole-stop-server', but this function will not raise errors."
  (ignore-errors (porthole-stop-server name-of-server)))


(defun porthole--stop-all-servers (&rest _)
  "Stop all running `porthole' servers.

Their session information files will be cleaned up.

This function is not intended to be used by the end-user. It
should only be called when, for example, Emacs is closing."
  (mapc 'porthole--stop-server-safe
        (porthole--running-server-names)))


(defun porthole-expose-functions (name-of-server funcs)
  "Expose a list of functions to RPC calls on one Porthole server.

Functions have to be exposed before they can be executed
remotely. This is just like `porthole-expose-functions', but it
allows you to expose many functions at once.

Example call:

  (porthole-expose-function \"pirate-server\"
                            '(insert delete-char point))

`NAME-OF-SERVER' is the name of the server on which the function
should be exposed.

`FUNCS' is a list of function symbols to expose. For example:

  '(insert magit-status)

Supply a symbol, not a string. Lambda functions are not
allowed (there would be no way for the client to reference them
by name)."
  (mapc (lambda (func)
          (porthole-expose-function name-of-server func))
        funcs))


(defun porthole-list-exposed-functions (name-of-server)
  "List the functions exposed by the server with `NAME-OF-SERVER'"
  (porthole--server-exposed-functions
   (porthole-get-server name-of-server)))


(defun porthole-expose-function (name-of-server func)
  "Expose a function to RPC calls on a particular Porthole server.

Functions have to be exposed before they can be executed
remotely.

Example call:

  (porthole-expose-function \"pirate-server\" 'insert)

`NAME-OF-SERVER' is the name of the server on which the function
should be exposed.

`FUNC' is the function symbol to expose. For example, `insert' or
`magit-status'. Supply a symbol, not a string. Lambda functions
are not allowed (there would be no way for the client to
reference them by name)."
  (unless (symbolp func)
    (error "`func' should be a symbol. Was type: %s. Value: %s"
           (type-of func) func))
  (porthole--assert-server-running name-of-server)
  (let* ((server (porthole-get-server name-of-server))
         (exposed-functions (porthole--server-exposed-functions server)))
    ;; We don't want duplicates
    (setf (porthole--server-exposed-functions server)
          (push func exposed-functions))))


(defun porthole-hide-function (name-of-server func)
  "Hide a function from remote procedure calls on a server.

`FUNC' is the function symbol to hide.

`NAME-OF-SERVER' is the name of the server on which the function
should be hidden.

This reverses `porthole-expose-function'."
  (unless (symbolp func)
    (error "`func' should be a symbol. Was type: %s. Value: %s"
           (type-of func) func))
  (porthole--assert-server-running name-of-server)
  (let ((server (porthole-get-server name-of-server)))
    (setf (porthole--server-exposed-functions server)
          (remove func (porthole--server-exposed-functions server)))))


;; Ensure all servers are stopped when Emacs is closed.
(add-hook 'kill-emacs-hook 'porthole--stop-all-servers)


;; Elnode is very chatty. It logs a lot but it provides no mechanism to turn off
;; logging for a specific server. The only way to turn it off is to disable it
;; globally.
(message "porthole: Disabling Elnode logging globally to prevent slowdown.")
;; This prevents logs from cluttering messages.
(setq elnode-error-log-to-messages nil)
;; TODO: Is this enough? It will still log to a buffer. Is that too much? Do we
;;   need to disable all logging full stop? Use for a while and see.
;; (setq elnode--do-error-logging nil)


(provide 'porthole)
;;; porthole.el ends here

;;; porthole-test.el --- Tests for porthole


(require 'elnode)
(require 'cl-lib)
(require 'f)
(require 'json)


(require 'porthole)


(ert-deftest test-porthole--assert-valid-server-name ()
  "Test for `porthole--assert-valid-server-name'"
  ;; This name is valid. This should pass without error.
  (porthole--assert-valid-server-name "test-name-1234")
  ;; This name contains an invalid character.
  (should-error (porthole--assert-valid-server-name "test_name"))
  ;; The empty name should also raise an error.
  (should-error (porthole--assert-valid-server-name ""))
  )


(ert-deftest test-porthole-get-server ()
  "Test for `porthole-get-server'"
  (porthole-start-server "test-server")
  (let ((server (porthole-get-server "test-server")))
    (should server)
    (should (porthole--server-p server)))
  (porthole-stop-server "test-server")
  )


(ert-deftest test-porthole--similar-keys ()
  "Test for `porthole--similar-keys'"
  (should (porthole--similar-keys "a_string" "a_string"))
  (should (porthole--similar-keys 'symbol 'symbol))
  (should (porthole--similar-keys "a_string" "A_STRING"))
  (should (porthole--similar-keys 'symbol 'SYMBOL))
  (should (porthole--similar-keys 'symbol "symbol"))
  (should (porthole--similar-keys "A-stRinG" 'a-stRinG))
  (should-not (porthole--similar-keys "string" "string_"))
  (should-not (porthole--similar-keys 'symbol 'sybmol))
  (should-not (porthole--similar-keys 'symbol "sybmol"))
  )


(ert-deftest test-porthole--alist-get ()
  "Test for `porthole--alist-get'"
  (should (eq (porthole--alist-get "key" '(("key" . value)))
              'value))
  (should (eq (porthole--alist-get "key" '((key . value)))
              'value))
  (should-not (porthole--alist-get "key" '(("nkey" . value))))
  )


(ert-deftest test-porthole--alist-remove ()
  "Test for `porthole--alist-remove'"
  (should (equal (porthole--alist-remove 'key2 '((key1 . value1)
                                                 (key2 . value2)))
                 '((key1 . value1))))
  ;; It shouldn't modify the original alist
  (let ((original-alist '((key1 . value1)
                          (key2 . value2))))
    (should (equal (porthole--alist-remove 'key1 original-alist)
                   '((key2 . value2))))
    ;; Ensure the original was unchanged
    (should (equal original-alist
                   '((key1 . value1)
                     (key2 . value2)))))

  ;; It shouldn't raise an error when the item doesn't exist
  (should (equal (porthole--alist-remove 'key1 nil)
                 nil))
  (should (equal (porthole--alist-remove 'key1 '((key2 . value2)))
                 '((key2 . value2))))
  )


(ert-deftest test-porthole--authenticate ()
  "Test for `porthole--authenticate'"
  (let ((test-server (make-porthole--server
                      :username "test_username"
                      :password "test_password")))
    ;; Successful authentication
    (should (catch 'porthole-finish-handling
              ;; This should simply run without problems
              (porthole--authenticate '((username . "test_username")
                                        (password . "test_password"))
                                      test-server)
              'did-not-catch))
    ;; Wrong username and password
    (should (catch 'porthole-finish-handling
              ;; Should terminate early because the authentication was invalid
              (porthole--authenticate '((username . "wrong")
                                        (password . "wrong"))
                                      test-server)
              ;; Should not reach here
              nil))
    ;; Just wrong password
    (should (catch 'porthole-finish-handling
              ;; Should terminate early because the authentication was invalid
              (porthole--authenticate '((username . "test_username")
                                        (password . "wrong"))
                                      test-server)
              ;; Should not reach here
              nil))
    ;; Just wrong username
    (should (catch 'porthole-finish-handling
              ;; Should terminate early because the authentication was invalid
              (porthole--authenticate '((username . "wrong")
                                        (password . "test_password"))
                                      test-server)
              ;; Should not reach here
              nil))
    ;; No username or password
    (should (catch 'porthole-finish-handling
              ;; Should terminate early because the authentication was invalid
              (porthole--authenticate '()
                                      test-server)
              ;; Should not reach here
              nil)))
  )


(ert-deftest test-porthole--find-free-port ()
  "Test for `porthole--find-free-port'"
  ;; This isn't a thorough test. Just check a port is found, and that it is not
  ;; 0.
  (let ((port (porthole--find-free-port "localhost")))
    (should (integerp port))
    (should (> port 0)))
  )


(ert-deftest test-porthole--publish-session-file ()
  "Test for `porthole--publish-session-file'"
  (unwind-protect
      (let ((info-filename (porthole--publish-session-file "test-server-2"
                                                           22222
                                                           "test_username_2"
                                                           "test_password_2"
                                                           :publish-port t
                                                           :publish-username t
                                                           :publish-password t)))
        (should info-filename)
        (should (f-exists-p info-filename))
        ;; This isn't perfect. Manually inspect the file if you really want to be
        ;; sure on a new system.
        (should (equal (porthole-get-session-file-path "test-server-2")
                       info-filename))
        (let ((decoded-file (json-read-file info-filename)))
          (should (json-alist-p decoded-file))
          ;; The decoded file should only have three keys
          (should (eq (length decoded-file) 3))
          (should (eq (alist-get 'port decoded-file) 22222))
          (should (equal (alist-get 'username decoded-file) "test_username_2"))
          (should (equal (alist-get 'password decoded-file) "test_password_2"))))
    ;; Make sure to clean up after the test.
    (porthole--erase-session-file "test-server-2"))

  ;; Test not publishing anything
  (unwind-protect
      (let ((info-filename (porthole--publish-session-file "test-server-3"
                                                           33333
                                                           "test_username_3"
                                                           "test_password_3"
                                                           :publish-port nil
                                                           :publish-username nil
                                                           :publish-password nil)))
        (should (file-exists-p info-filename))
        ;; This isn't perfect. Manually inspect the file if you really want to be
        ;; sure on a new system.
        (should (equal (porthole-get-session-file-path "test-server-3")
                       info-filename))
        ;; Make sure nothing was published
        (should (equal (json-read-file info-filename) '())))
    ;; Make sure to clean up after the test.
    (porthole--erase-session-file "test-server-3")))


(ert-deftest test-porthole--erase-session-file ()
  "Test for `porthole--erase-session-file'"
  ;; Just create an empty session file
  (let ((info-filename (porthole--publish-session-file "test-server-4"
                                                       nil nil nil
                                                       :publish-port nil
                                                       :publish-username nil
                                                       :publish-password nil)))
    ;; Ensure the file exists before we delete it
    (should (file-exists-p info-filename))
    (porthole--erase-session-file "test-server-4")
    (should-not (file-exists-p info-filename))
    ;; Also ensure the server's directory was removed.
    (should-not (f-dir-p (f-dirname info-filename)))))


(ert-deftest test-porthole--assert-symbol ()
  "Test for `porthole--assert-symbol'"
  ;; The should run fine
  (porthole--assert-symbol 'symbol)
  (porthole--assert-symbol :keyword-symbol)
  ;; Non-symbols raise errors
  (should-error (porthole--assert-symbol "string"))
  ;; Check the tricky symbols
  (should-error (porthole--assert-symbol nil))
  (should-error (porthole--assert-symbol t))
  )


;;; porthole-test.el ends here

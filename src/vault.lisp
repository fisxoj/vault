(in-package :cl-user)
(defpackage vault
  (:use :cl)
  (:export #:*host*
           #:*port*
           #:*token*

           #:secret))
(in-package :vault)

(defparameter +api-path+ "/v1/")
(defparameter *port* 8200)
(defparameter *host* "localhost")
(defparameter *token* nil)

(define-condition vault-condition ()
  ()
  (:documentation "A base condition that all vault conditions descend from."))

(define-condition vault-request-error (dex:http-request-failed)
  ()
  (:documentation "Indicates an error in the http transport layer (dexador)."))

;; REF: https://www.vaultproject.io/docs/http/
(defun secret (key)
  (request-secret key))

(defun (setf secret) (key value)
  (dex:post (secret-url key) :content (json-value value)))

(defun request-secret (key)
  (assert (stringp *token*)
          () "A vault token is required for fetching secrets.  Set `vault:*token*`.")

  (dex:get (secret-url key) :headers '((:x-vault-token . *token*))))

(defun secret-url (key)
  (concatenate 'string "http://" *host* ":" (format nil "~a" *port*) +api-path+ key))

(defun json-value (value)
  (jonathan:with-output-to-string*
    (jonathan:with-object
      (jonathan:write-key-value "value" value))))

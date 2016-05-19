(in-package :cl-user)
(defpackage vault
  (:use :cl)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:export #:*host*
           #:*port*
           #:*token*

           #:secret
           #:delete-secret

           #:list-path

           #:sealed-p
           #:seal
           #:unseal))
(in-package :vault)

;;; Configuration/constants

(defparameter +api-path+ "/v1/")
(defparameter *host* "http://localhost:8200")
(defparameter *token* nil)

;;;; Conditions

(define-condition vault-condition ()
  ()
  (:documentation "A base condition that all vault conditions descend from."))

(define-condition vault-request-error (dex:http-request-failed)
  ()
  (:documentation "Indicates an error in the http transport layer (dexador)."))

(define-condition vault-unauthorized (dex:http-request-unauthorized)
  ()
  (:documentation "Indicates that the `*token*` didn't have permissions adequate to do something.  E.g. it doesn't have write permission.")
  )

;;;; The Vault API
;; REF: https://www.vaultproject.io/docs/http/
(defun secret (key)
  (awhen (request :get (secret-url key) :json-field :|data|)
    (getf it :|value|)))

(defun (setf secret) (value key)
  (request :post (secret-url key) :content (json-value value)))

(defun delete-secret (key)
  (request :delete (secret-url key)))

(defun list-path (path)
  (awhen (request :get (concatenate 'string (secret-url path) "?list=true") :json-field :|data|)
    (getf it :|keys|)))

(defun sealed-p ()
  (request :get (sys-url "seal-status") :json-field :|sealed|))

;;;; Request handling

(defun request (method uri &rest args &key (token-header-p t) (json-field t) &allow-other-keys)
  (assert (or (not token-header-p) (stringp *token*))
          () "A vault token is required for fetching secrets.  Set `vault:*token*`.")

  (remf args :json-field)
  (remf args :token-header-p)

  (handler-case
      (multiple-value-bind (resp status-code)
          (apply #'dex:request
                 uri
                 :method method
                 :headers (when token-header-p (token-headers))
                 args)
        (case status-code
          (200
           (let ((decoded-response (jonathan:parse resp)))
             (cond
               ((symbolp json-field) (getf decoded-response json-field))
               ((eq t json-field) decoded-response)
               (t t))))

          (204 t)

          ;; FIXME: Handle errors better
          (t nil)))))

(defun token-headers ()
  `((:x-vault-token . ,*token*)))

(defun secret-url (key)
  (concatenate 'string *host* +api-path+ "secret/" key))

(defun sys-url (key)
  (concatenate 'string *host* +api-path+ "sys/" key))

(defun json-value (value)
  (jonathan:with-output-to-string*
    (jonathan:with-object
      (jonathan:write-key-value "value" value))))

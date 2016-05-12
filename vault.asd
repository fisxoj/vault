#|
  This file is a part of vault project.
|#

(in-package :cl-user)
(defpackage vault-asd
  (:use :cl :asdf))
(in-package :vault-asd)

(defsystem vault
  :version "0.1"
  :author ""
  :license "MIT"
  :depends-on (:dexador
               :jonathan)
  :components ((:module "src"
                :components
                ((:file "vault"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op vault-test))))

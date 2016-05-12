#|
  This file is a part of vault project.
|#

(in-package :cl-user)
(defpackage vault-test-asd
  (:use :cl :asdf))
(in-package :vault-test-asd)

(defsystem vault-test
  :author ""
  :license "MIT"
  :depends-on (:vault
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "vault"))))
  :description "Test system for vault"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

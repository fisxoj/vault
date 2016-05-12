(in-package :cl-user)
(defpackage vault-test
  (:use :cl
        :vault
        :prove))
(in-package :vault-test)

(plan 3)

(subtest "secret-url"

  ;; Use some sane default values
  (let ((vault:*host* "pizza-host")
        (vault:*port* 8200))

    (is (vault::secret-url "production/database-password")
        "http://pizza-host:8200/v1/production/database-password"
        "Secret urls are generated correctly.")))

(subtest "request-key"

  ;; Use some sane default values
  (let ((vault:*host* "pizza-host")
        (vault:*port* 8200)
        (vault:*token* nil))

    (is-error (vault::request-secret "production/database-password")
              error
              "An error is raised when a request for a secret is made with no token.")))

(subtest "json-value"
  (is (vault::json-value "potato") "{\"value\":\"potato\"}"
      "Values are formatted to correct json."))

(finalize)

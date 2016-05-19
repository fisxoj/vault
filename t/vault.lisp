(in-package :cl-user)
(defpackage vault-test
  (:use :cl
        :vault
        :prove))
(in-package :vault-test)

(plan 4)

(subtest "Reading and writing secrets"
  (let ((vault:*host* "http://localhost:8200")
        (vault:*token* "feb2b356-a659-24f0-0caf-cc92872d23cc")
        (key "producton/database-uri")
        (value "postgres://something:secret@pg-server/"))

    (ok (setf (vault:secret key) value)
        "No errors raised setting a secret.")

    (is (vault:secret key) value
        "The secret was readable.")

    (ok (vault:delete-secret key))))

(subtest "secret-url"

  ;; Use some sane default values
  (let ((vault:*host* "http://pizza-host:8200"))

    (is (vault::secret-url "production/database-password")
        "http://pizza-host:8200/v1/secret/production/database-password"
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

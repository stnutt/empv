(require 'empv-json)
(require 'ert)

(ert-deftest empv-json-test-read ()
  "Verify an object read from a JSON-encoded string."
  (should (equal (empv-json-read-from-string "{\"array\":[null,false]}")
                 '((array nil nil)))))

(ert-deftest empv-json-test-read-invalid ()
  "Verify nil read from invalid JSON string."
  (should-not (empv-json-read-from-string "{")))

(ert-deftest empv-json-test-encode ()
  "Verify a string JSON-encoded from an object."
  (should (string-match-p "\\`{\"array\":\\[null, *false\\]}\\'"
                          (empv-json-encode '(:array (nil :json-false))))))

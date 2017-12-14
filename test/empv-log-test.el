(require 'empv-log)
(require 'ert)

(ert-deftest empv-log-test-source-p ()
  "Verify the predicate accurately reflects configured log sources."
  (should-not (empv-log-source-p 'test))
  (let ((empv-log-sources t))
    (should (empv-log-source-p 'test)))
  (let ((empv-log-sources '(test)))
    (should (empv-log-source-p 'test)))
  (let ((empv-log-sources '(foo)))
    (should-not (empv-log-source-p 'test))))

(ert-deftest empv-log-test-message ()
  "Verify messages are logged according to the configured sources."
  (ignore-errors (kill-buffer "*empv-log*"))
  (empv-log-message 'test "message")
  (should-not (get-buffer "*empv-log*"))
  (let ((empv-log-sources '(test)))
    (empv-log-message 'test "message"))
  (with-current-buffer "*empv-log*"
    (should (re-search-backward "\\`[- .:0-9]+ \\[test\\] message\\'" nil t))))

(ert-deftest empv-log-test-filter ()
  "Verify process output is logged by the filter."
  (ignore-errors (kill-buffer "*empv-log*"))
  (let ((empv-log-sources '(test)))
    (let ((process (make-process :name "empv-test"
                                 :command '("echo" "message")
                                 :filter 'empv-log-filter)))
      (while (process-live-p process)
        (sleep-for 1))
      (accept-process-output)))
  (with-current-buffer "*empv-log*"
    (should (re-search-backward "\\`[- .:0-9]+ \\[test\\] message\\'" nil t))))

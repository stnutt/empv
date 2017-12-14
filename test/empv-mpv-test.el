(require 'empv-mpv)
(require 'ert)

(ert-deftest empv-mpv-test-start ()
  "Verify mpv process is started with correct parameters."
  (unwind-protect
      (cl-letf* ((empv-mpv-test-process nil)
                 (empv-mpv-default-directory "/")
                 (empv-mpv-options '("--option1" "--option2"))
                 ((symbol-function 'start-file-process)
                  (lambda (name buffer program &rest args)
                    (should (equal default-directory "/"))
                    (should (equal name "empv-mpv"))
                    (should (member "--input-ipc-server=/ipc" args))
                    (should (member "--option1" args))
                    (should (member "--option2" args))
                    (setq empv-mpv-test-process
                          (make-process :name name
                                        :command '("sleep" "1000"))))))
        (should (eq (empv-mpv-start "/ipc") empv-mpv-test-process))
        (should empv-mpv-test-process))
    (ignore-errors (delete-process (get-process "empv-mpv")))))

(ert-deftest empv-mpv-test-running ()
  "Verify running state of mpv is correctly reported."
  (unwind-protect
      (progn
        (should-not (empv-mpv-running-p))
        (make-process :name "empv-mpv" :command '("sleep" "1000"))
        (should (empv-mpv-running-p))
        (should-not (empv-mpv-start "/ipc"))
        (should-not (get-process "empv-mpv<1>")))
    (ignore-errors (delete-process (get-process "empv-mpv")))))

(ert-deftest empv-mpv-test-file-host-error ()
  "Verify an error occurs when mpv files reside on different hosts."
  (let ((empv-mpv-default-directory "/"))
    (should-error (empv-mpv-start "/ssh:remote:/ipc") :type 'user-error)
    (should-not (get-process "empv-mpv"))))

(ert-deftest empv-mpv-test-log ()
  "Verify mpv process output is logged when the source is configured."
  (unwind-protect
      (let ((empv-log-sources '(mpv)))
        (cl-letf (((symbol-function 'start-file-process)
                   (lambda (name buffer program &rest args)
                     (make-process :name name :command '("sleep" "1000")))))
          (should (eq (process-filter (empv-mpv-start "/ipc"))
                      'empv-log-filter))))
    (ignore-errors (delete-process (get-process "empv-mpv")))))

;;; empv-log.el --- empv logging functions           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Stephen Nutt

;; Author: Stephen Nutt <stnutt@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for logging messages for empv

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar empv-log-sources nil
  "A list of sources for which to perform logging.

If nil, then no logging is performed.  If t, then messages from
all sources are logged.")

(defun empv-log-source-p (source)
  "Return non-nil if messages from SOURCE will be logged."
  (or (eq empv-log-sources t) (memq source empv-log-sources)))

(defun empv-log-message (source string &rest objects)
  "Log a message from SOURCE, formatted from STRING and OBJECTS."
  (when (stringp source)
    (setq source (intern (string-remove-prefix "empv-" source))))
  (when (empv-log-source-p source)
    (let* ((default-directory "~")
           (inhibit-read-only t)
           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%6N"))
           (lines (mapcar (apply-partially 'format "%s [%s] %s" timestamp source)
                          (split-string (if objects
                                            (apply 'format string objects)
                                          string)
                                        "\n" t "[ \t\n\r]+"))))
      (with-current-buffer (get-buffer-create "*empv-log*")
        (read-only-mode 1)
        (when (prog1 (eobp)
                (save-excursion
                  (goto-char (point-max))
                  (when (> (buffer-size) 0)
                    (insert "\n"))
                  (insert (mapconcat 'identity lines "\n"))))
          (goto-char (point-max)))))))

(defun empv-log-filter (process output)
  "A filter for PROCESS that logs all OUTPUT."
  (empv-log-message (process-name process) output))

(provide 'empv-log)
;;; empv-log.el ends here

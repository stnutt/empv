;;; empv-mpv.el --- empv mpv process management      -*- lexical-binding: t; -*-

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

;; Functions for managing the empv mpv process

;;; Code:

(require 'empv-log)

(defgroup empv-mpv nil
  "mpv process management."
  :group 'empv
  :prefix "empv-mpv-")

(defcustom empv-mpv-default-directory "~/"
  "Directory in which to start the mpv process."
  :group 'empv-mpv
  :type 'directory)

(defcustom empv-mpv-options nil
  "Additional mpv command options."
  :group 'empv-mpv
  :type '(repeat (string :tag "Option")))

(defun empv-mpv-running-p ()
  "Return non-nil if mpv is running."
  (process-live-p (get-process "empv-mpv")))

(defun empv-mpv-start (ipc-file)
  "Start the mpv process listening on IPC-FILE.

Return nil if mpv is already running, or the mpv process if it
was started."
  (unless (empv-mpv-running-p)
    (unless (equal (file-remote-p empv-mpv-default-directory)
                   (file-remote-p ipc-file))
      (user-error
       "The mpv default directory and ipc file must reside on the same host"))
    (setq ipc-file (expand-file-name ipc-file))
    (setq ipc-file (or (file-remote-p ipc-file 'localname) ipc-file))
    (make-directory (file-name-directory ipc-file) t)
    (let* ((default-directory empv-mpv-default-directory)
           (options `("--idle"
                      ,(concat "--input-ipc-server=" ipc-file)
                      ,@(if (empv-log-source-p 'mpv)
                            '("--msg-level=all=v"
                              "--no-input-terminal"
                              "--no-msg-color"
                              "--quiet")
                          '("--no-terminal"))
                      ,@empv-mpv-options))
           (process (apply 'start-file-process "empv-mpv" nil "mpv" options)))
      (set-process-query-on-exit-flag process nil)
      (when (empv-log-source-p 'mpv)
        (set-process-filter process 'empv-log-filter)
        (set-process-sentinel process 'empv-log-filter))
      process)))

(provide 'empv-mpv)
;;; empv-mpv.el ends here

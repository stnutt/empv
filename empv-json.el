;;; empv-json.el --- empv JSON functions             -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Stephen Nutt

;; Author: Stephen Nutt <stnutt@gmail.com>
;; Keywords: multimedia

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

;; Functions for reading and encoding JSON for empv

;;; Code:

(require 'json)

(defun empv-json-read-from-string (string)
  "Return the object for the JSON-encoded STRING."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (ignore-errors
      (json-read-from-string string))))

(defun empv-json-encode (object)
  "Return the JSON-encoded string for the OBJECT."
  (let ((json-encoding-separator ",")
        (json-encoding-pretty-print nil)
        (json-encoding-object-sort-predicate nil))
    (json-encode object)))

(provide 'empv-json)
;;; empv-json.el ends here

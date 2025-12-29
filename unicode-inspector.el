;;; unicode-inspector.el --- Text to inspect Unicode properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 29 Dec 2025
;; Keywords: text
;; Homepage: https://github.com/zonuexe/unicode-inspector.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (vui "1.0.0"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unicode Inspector provides an interactive UI to inspect Unicode
;; properties for characters entered by the user.  Run:
;;
;;   M-x unicode-inspector
;;
;; Then type one or more characters; each line is inspected and shown
;; with codepoint, UTF-8/UTF-16LE bytes, name, category, and block.

;;; Code:
(require 'browse-url)
(require 'emoji)
(require 'unicode-inspector-blocks)
(require 'vui)
(eval-when-compile
  (require 'cl-lib)
  (require 'pcase))

(defgroup unicode-inspector nil
  "Text to inspect Unicode properties."
  :group 'text)

(defvar unicode-inspector--buffer-name "*Unicode Inspector*"
  "Buffer name for the Unicode Inspector UI.")

(defun unicode-inspector--bytes-to-hex (bytes)
  "Format BYTES as a space-separated hex string."
  (mapconcat (lambda (byte) (format "%02X" byte)) bytes " "))

(defun unicode-inspector--encode-hex (char coding)
  "Return hex bytes for CHAR encoded with CODING."
  (unicode-inspector--bytes-to-hex
   (string-to-list (encode-coding-string (string char) coding))))

(defun unicode-inspector--char-name (char)
  "Return Unicode name for CHAR or a fallback string."
  (or (get-char-code-property char 'name) "UNASSIGNED"))

(defun unicode-inspector--char-block (char)
  "Return a block cell for CHAR with a chart PDF link."
  (let ((range (unicode-inspector-blocks-range-for char)))
    (if (not range)
        "No_Block"
      (pcase-let ((`(,start ,_ ,name) range))
        (vui-button name
                    :on-click (lambda ()
                                (browse-url (unicode-inspector-blocks-url start)))
                    :help-echo (unicode-inspector-blocks-url start)
                    :no-decoration t)))))

(defun unicode-inspector--char-category (char)
  "Return general category for CHAR."
  (symbol-name (or (get-char-code-property char 'general-category) 'Cn)))

(defun unicode-inspector--char-row (char)
  "Build a table row for character CHAR."
  (list (string char)
        (format "%04X" char)
        (unicode-inspector--encode-hex char 'utf-8)
        (unicode-inspector--encode-hex char 'utf-16le)
        (unicode-inspector--char-name char)
        (unicode-inspector--char-category char)
        (unicode-inspector--char-block char)))

(defun unicode-inspector--rows-from-input (text)
  "Return table rows for TEXT split by lines."
  (let ((lines (split-string (or text "") "\n" nil)))
    (cl-loop for line in lines
             when (not (string= line ""))
             append (mapcar #'unicode-inspector--char-row
                            (string-to-list line)))))

(defun unicode-inspector--table (text)
  "Render the inspector table for TEXT."
  (let ((rows (unicode-inspector--rows-from-input text)))
    (if rows
        (vui-table
         :columns '((:header "Char" :min-width 9)
                    (:header "Code" :min-width 4)
                    (:header "UTF-8" :min-width 8)
                    (:header "UTF-16 LE" :min-width 9)
                    (:header "Name" :min-width 18)
                    (:header "Cat" :min-width 3)
                    (:header "Block" :min-width 12))
         :rows rows)
      (vui-text "Enter characters above to inspect Unicode details."
        :face 'shadow))))

(vui-defcomponent unicode-inspector--app ()
  "Unicode Inspector UI."
  :state ((input ""))
  :render
  (vui-vstack :spacing 1
              (vui-text "Unicode Inspector" :face 'bold)
              (vui-text "Input (one or more characters per line):")
              (vui-field :size 48
                         :value input
                         :on-change (lambda (new-value)
                                      (vui-set-state :input new-value)))
              (unicode-inspector--table input)))

;;;###autoload
(defun unicode-inspector ()
  "Open the Unicode Inspector UI."
  (interactive)
  (vui-mount (vui-component 'unicode-inspector--app)
             unicode-inspector--buffer-name))

(provide 'unicode-inspector)
;;; unicode-inspector.el ends here

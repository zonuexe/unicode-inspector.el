;;; unicode-inspector.el --- Text to inspect Unicode properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 29 Dec 2025
;; Keywords: text
;; Homepage: https://github.com/zonuexe/unicode-inspector.el
;; Version: 0.0.2
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
(require 'subr-x)
(require 'unicode-inspector-blocks)
(require 'vui)
(require 'nerd-icons nil t)
(eval-when-compile
  (require 'cl-lib)
  (require 'pcase))

(defgroup unicode-inspector nil
  "Text to inspect Unicode properties."
  :group 'text)

(defcustom unicode-inspector-unique-input nil
  "When non-nil, inspect each character only once."
  :type 'boolean
  :group 'unicode-inspector)

(defcustom unicode-inspector-char-face 'emoji
  "Face for characters shown in the Char column.
When nil, no face is applied."
  :type '(choice (const :tag "None" nil) face)
  :group 'unicode-inspector)

(defcustom unicode-inspector-block-table-char-face nil
  "Face for characters shown in block table buffers.
When nil, no face is applied."
  :type '(choice (const :tag "None" nil) face)
  :group 'unicode-inspector)

(defvar unicode-inspector--pdf-icon-cache nil
  "Cached (label . face) for the PDF button.")

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

(defun unicode-inspector--char-cell (char)
  "Return the Char column cell for CHAR."
  (if unicode-inspector-char-face
      (vui-text (string char) :face unicode-inspector-char-face)
    (string char)))

(defun unicode-inspector--pdf-icon ()
  "Return cached (label . face) for PDF buttons."
  (or unicode-inspector--pdf-icon-cache
      (setq unicode-inspector--pdf-icon-cache
            (let* ((icon (when (and (featurep 'nerd-icons)
                                    (fboundp 'nerd-icons-icon-for-extension))
                           (nerd-icons-icon-for-extension "pdf")))
                   (face (and (stringp icon) (get-text-property 0 'face icon)))
                   (label (or icon "[PDF]")))
              (cons label face)))))

(defun unicode-inspector--char-block (char)
  "Return a block cell for CHAR with a chart PDF link."
  (if-let* ((range (unicode-inspector-blocks-range-for char)))
      (pcase-let* ((`(,start ,end ,name) range)
                   (`(,pdf-label . ,pdf-face) (unicode-inspector--pdf-icon)))
        (vui-hstack
         :spacing 1
         (vui-button name
           :on-click (lambda ()
                       (unicode-inspector--open-block-table start end name))
           :no-decoration t)
         (vui-button pdf-label
           :face pdf-face
           :on-click (lambda ()
                       (browse-url (unicode-inspector-blocks-url start)))
           :help-echo (unicode-inspector-blocks-url start)
           :no-decoration t)))
    "No_Block"))

(define-derived-mode unicode-inspector-block-table-mode special-mode "Unicode-Block-Table"
  "Major mode for Unicode block table buffers."
  (setq-local truncate-lines t))

(defun unicode-inspector--block-table-buffer-name (name)
  "Return a buffer name for block table NAME."
  (format "*Unicode Block Table* %s" name))

(defun unicode-inspector--block-table-columns (start end)
  "Return column bases between START and END."
  (let ((start-col (ash start -4))
        (end-col (ash end -4)))
    (cl-loop for col from start-col to end-col collect col)))

(defun unicode-inspector--block-table-cell (codepoint)
  "Return the cell vnode for CODEPOINT."
  (if (characterp codepoint)
      (if unicode-inspector-block-table-char-face
          (vui-text (string codepoint) :face unicode-inspector-block-table-char-face)
        (string codepoint))
    " "))

(defun unicode-inspector--block-table-heading (name start end)
  "Return the heading vnode for block table NAME START END."
  (let* ((label (format "%s (%04X..%04X)" name start end))
         (pdf (unicode-inspector--pdf-icon))
         (pdf-label (car pdf))
         (pdf-face (cdr pdf)))
    (vui-hstack
     :spacing 1
     (vui-text label :face 'bold)
     (vui-button pdf-label
                 :face pdf-face
                 :on-click (lambda ()
                             (browse-url (unicode-inspector-blocks-url start)))
                 :help-echo (unicode-inspector-blocks-url start)
                 :no-decoration t))))

(defun unicode-inspector--block-table-vnode (start end name)
  "Return a VUI table vnode for START..END with block NAME."
  (let* ((row-bases (unicode-inspector--block-table-columns start end))
         (col-nibbles (number-sequence 0 15))
         (columns (cons (list :header "" :width 6)
                        (mapcar (lambda (col)
                                  (list :header (format "%X" col) :width 4 :align :center))
                                col-nibbles)))
         (rows (mapcar (lambda (row-base)
                         (cons (format "%04X" row-base)
                               (mapcar (lambda (col)
                                         (let ((codepoint (+ (ash row-base 4) col)))
                                           (if (<= start codepoint end)
                                               (unicode-inspector--block-table-cell codepoint)
                                             "")))
                                       col-nibbles)))
                       row-bases)))
    (vui-vstack
     :spacing 1
     (unicode-inspector--block-table-heading name start end)
     (vui-table :columns columns :rows rows))))

(defun unicode-inspector--open-block-table (start end name)
  "Open a Unicode block table for START..END with block NAME."
  (let ((buf (get-buffer-create (unicode-inspector--block-table-buffer-name name))))
    (with-current-buffer buf
      (vui-render (unicode-inspector--block-table-vnode start end name)))
    (pop-to-buffer buf)))

(defun unicode-inspector--char-category (char)
  "Return general category for CHAR."
  (symbol-name (or (get-char-code-property char 'general-category) 'Cn)))

(defun unicode-inspector--char-row (char)
  "Build a table row for character CHAR."
  (list (unicode-inspector--char-cell char)
        (format "%04X" char)
        (unicode-inspector--encode-hex char 'utf-8)
        (unicode-inspector--encode-hex char 'utf-16le)
        (unicode-inspector--char-name char)
        (unicode-inspector--char-category char)
        (unicode-inspector--char-block char)))

(defun unicode-inspector--rows-from-input (text)
  "Return table rows for TEXT split by lines."
  (let ((lines (split-string (or text "") "\n" nil))
        (seen (when unicode-inspector-unique-input (make-hash-table :test 'eq))))
    (cl-loop for line in lines
             when (not (string= line ""))
             append (cl-loop for char across line
                             when (or (not seen)
                                      (unless (gethash char seen)
                                        (puthash char t seen)))
                             collect (unicode-inspector--char-row char)))))

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

;;; unicode-inspector.el --- Text to inspect Unicode properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 29 Dec 2025
;; Keywords: text
;; Homepage: https://github.com/zonuexe/unicode-inspector.el
;; Version: 0.0.3
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

(defcustom unicode-inspector-show-trailing-whitespace :hide
  "Override `show-trailing-whitespace' in Unicode Inspector buffers."
  :type '(choice (const :tag "Use default" nil)
                 (const :tag "Show" t)
                 (const :tag "Hide" :hide))
  :group 'unicode-inspector)

(defcustom unicode-inspector-control-replacements
  '((?\t . "␉"))
  "Alist of character replacements used for display.
Uses direct replacement (not `display' properties) to avoid table misalignment."
  :type '(alist :key-type character :value-type string)
  :group 'unicode-inspector)

(defcustom unicode-inspector-display-replacements
  '((?\s . "␠")
    (#x00AD . "[SHY]")
    (#x034F . "[CGJ]")
    (#x180E . "[MVS]")
    (#x200B . "[ZWSP]")
    (#x200C . "[ZWNJ]")
    (#x200D . "[ZWJ]")
    (#x200E . "[LRM]")
    (#x200F . "[RLM]")
    (#x2028 . "[LS]")
    (#x2029 . "[PS]")
    (#x202A . "[LRE]")
    (#x202B . "[RLE]")
    (#x202C . "[PDF]")
    (#x202D . "[LRO]")
    (#x202E . "[RLO]")
    (#x202F . "[NNBSP]")
    (#x2060 . "[WJ]")
    (#x2061 . "[f()]")
    (#x2062 . "[×]")
    (#x2063 . "[,]")
    (#x2064 . "[+]")
    (#x2066 . "[LRI]")
    (#x2067 . "[RLI]")
    (#x2068 . "[FSI]")
    (#x2069 . "[PDI]")
    (#xFEFF . "[BOM]"))
  "Alist of display replacements using `display' properties."
  :type '(alist :key-type character :value-type string)
  :group 'unicode-inspector)

(defvar unicode-inspector--pdf-icon-cache nil
  "Cached (label . face) for the PDF button.")

(defvar unicode-inspector--buffer-name "*Unicode Inspector*"
  "Buffer name for the Unicode Inspector UI.")

(defvar-keymap unicode-inspector-mode-map
  :doc "Keymap for `unicode-inspector-mode'."
  "q" #'quit-window)

(define-minor-mode unicode-inspector-mode
  "Minor mode for Unicode Inspector buffers."
  :init-value nil
  :lighter " Unicode-Inspector"
  :keymap unicode-inspector-mode-map
  (if unicode-inspector-mode
      (when unicode-inspector-show-trailing-whitespace
        (setq show-trailing-whitespace
              (not (eq unicode-inspector-show-trailing-whitespace :hide))))
    (setq show-trailing-whitespace (default-value 'show-trailing-whitespace))))

(defun unicode-inspector--bytes-to-hex (bytes)
  "Format BYTES as a space-separated hex string."
  (mapconcat (lambda (byte) (format "%02X" byte)) bytes " "))

(defun unicode-inspector--encode-hex (char coding)
  "Return hex bytes for CHAR encoded with CODING."
  (unicode-inspector--bytes-to-hex
   (string-to-list (encode-coding-string (string char) coding))))

(defun unicode-inspector--char-name (char)
  "Return Unicode name for CHAR or a fallback string."
  (or (cdr (assq char unicode-inspector-ascii-control-names))
      (get-char-code-property char 'name)
      "UNASSIGNED"))

(defun unicode-inspector--display-char (char)
  "Return display string for CHAR, mapping replacements if configured."
  (let ((base (string char)))
    (let ((display (cdr (assq char unicode-inspector-display-replacements)))
          (direct (cdr (assq char unicode-inspector-control-replacements))))
      (cond
       (display (propertize base 'display display))
       (direct direct)
       (t base)))))

(defun unicode-inspector--display-face (label default-face)
  "Return face for LABEL, overriding DEFAULT-FACE when LABEL is long."
  (let* ((display (and (stringp label) (get-text-property 0 'display label)))
         (effective (if (stringp display) display label)))
    (if (>= (length effective) 3)
        'font-lock-escape-face
      default-face)))

(defun unicode-inspector--char-cell (char)
  "Return the Char column cell for CHAR."
  (let* ((label (unicode-inspector--display-char char))
         (face (unicode-inspector--display-face label unicode-inspector-char-face)))
    (if face
        (vui-text label :face face)
      label)))

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

(defun unicode-inspector--block-table-cell (codepoint start end name)
  "Return the cell vnode for CODEPOINT within START..END NAME."
  (if (characterp codepoint)
      (let* ((label (unicode-inspector--display-char codepoint))
             (face (unicode-inspector--display-face
                    label unicode-inspector-block-table-char-face)))
        (vui-button label
                    :face face
                    :on-click (lambda ()
                                (unicode-inspector--open-block-list
                                 start end name (unicode-inspector--char-name codepoint)))
                    :help-echo (unicode-inspector--char-name codepoint)
                    :no-decoration t))
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
                                               (unicode-inspector--block-table-cell
                                                codepoint start end name)
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
    (unicode-inspector--enable-mode buf)
    (pop-to-buffer buf)))

(defun unicode-inspector--block-list-buffer-name (name)
  "Return a buffer name for block list NAME."
  (format "*Unicode Block List* %s" name))

(defun unicode-inspector--blocks-alist ()
  "Return an alist of block NAME to (START END NAME)."
  (cl-loop for (start end name) in unicode-inspector-blocks
           collect (cons name (list start end name))))

(defun unicode-inspector--read-block (prompt)
  "Read a Unicode block with PROMPT and return (START END NAME)."
  (let* ((choices (unicode-inspector--blocks-alist))
         (name (completing-read prompt choices nil t)))
    (cdr (assoc name choices))))

(defun unicode-inspector--block-list-rows (start end query)
  "Return Codepoint/Char/Name rows for START..END filtered by QUERY."
  (let* ((needle (string-trim (or query "")))
         (needle (downcase needle)))
    (cl-loop for codepoint from start to end
             for name = (unicode-inspector--char-name codepoint)
             when (or (string-empty-p needle)
                      (string-match-p (regexp-quote needle) (downcase name)))
             collect (list (format "U+%04X" codepoint)
                           (unicode-inspector--char-cell codepoint)
                           name))))

(vui-defcomponent unicode-inspector--block-list (start end name initial-query)
  "Unicode block codepoint list."
  :state ((query (or initial-query "")))
  :render
  (vui-vstack
   :spacing 1
   (unicode-inspector--block-table-heading name start end)
   (vui-hstack
    :spacing 1
    (vui-text "Search (Name):")
    (vui-field :size 40
               :value query
               :on-change (lambda (value)
                            (vui-set-state :query value))))
   (vui-table
    :columns '((:header "Codepoint" :min-width 10)
               (:header "Char" :min-width 4)
               (:header "Name" :min-width 18))
    :rows (unicode-inspector--block-list-rows start end query))))

(defun unicode-inspector--open-block-list (start end name &optional initial-query)
  "Open a Unicode block codepoint list for START..END with NAME."
  (let* ((buf-name (unicode-inspector--block-list-buffer-name name))
         (buf (get-buffer buf-name)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (vui-mount
     (vui-component 'unicode-inspector--block-list
                    :start start
                    :end end
                    :name name
                    :initial-query initial-query)
     buf-name)
    (unicode-inspector--enable-mode (get-buffer buf-name))))

;;;###autoload
(defun unicode-inspector-block-table ()
  "Open a Unicode block table selected via completion."
  (interactive)
  (pcase-let ((`(,start ,end ,name) (unicode-inspector--read-block "Block table: ")))
    (unicode-inspector--open-block-table start end name)))

;;;###autoload
(defun unicode-inspector-block-list ()
  "Open a Unicode block codepoint list selected via completion."
  (interactive)
  (pcase-let ((`(,start ,end ,name) (unicode-inspector--read-block "Block list: ")))
    (unicode-inspector--open-block-list start end name)))

(defun unicode-inspector--enable-mode (buffer)
  "Enable `unicode-inspector-mode' in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unicode-inspector-mode 1))))

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
  (let ((buf (get-buffer unicode-inspector--buffer-name)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (vui-mount (vui-component 'unicode-inspector--app)
                 unicode-inspector--buffer-name)
      (unicode-inspector--enable-mode (get-buffer unicode-inspector--buffer-name)))))

(provide 'unicode-inspector)
;;; unicode-inspector.el ends here

;;; unicode-inspector-blocks.el --- Provide Unicode block ranges data  -*- lexical-binding: t; -*-

;; Copyright (C) ${year}  Unicode®, Inc.

;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; Keywords: text
;; License: Unicode-3.0

${license}

;;; Commentary:

;; Generated from ${url}.
;; Do not edit by hand.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defconst unicode-inspector-blocks
  '(${blocks})
  "Unicode block ranges from ${url}.")

(defconst unicode-inspector-blocks-url-format
  "https://www.unicode.org/charts/PDF/U%04X.pdf"
  "Format string for Unicode block chart URLs.")

(defun unicode-inspector-blocks-range-for (char)
  "Return the block range list for CHAR."
  (cl-loop for (start end name) in unicode-inspector-blocks
           when (<= start char end)
           return (list start end name)))

(defun unicode-inspector-blocks-url (start)
  "Return the Unicode chart PDF URL for block START."
  (format unicode-inspector-blocks-url-format start))

(provide 'unicode-inspector-blocks)
;;; unicode-inspector-blocks.el ends here

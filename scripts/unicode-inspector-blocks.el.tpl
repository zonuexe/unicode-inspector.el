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

(defconst unicode-inspector-ascii-control-names
  '((#x00 . "<control> NULL (NUL); null character")
    (#x01 . "<control> START OF HEADING (SOH)")
    (#x02 . "<control> START OF TEXT (STX)")
    (#x03 . "<control> END OF TEXT (ETX)")
    (#x04 . "<control> END OF TRANSMISSION (EOT)")
    (#x05 . "<control> ENQUIRY (ENQ)")
    (#x06 . "<control> ACKNOWLEDGE (ACK)")
    (#x07 . "<control> BELL (BEL)")
    (#x08 . "<control> BACKSPACE (BS)")
    (#x09 . "<control> CHARACTER TABULATION (HT)")
    (#x0A . "<control> LINE FEED (LF); new line (NL); end of line (EOL)")
    (#x0B . "<control> LINE TABULATION (VT)")
    (#x0C . "<control> FORM FEED (FF); new page (NP)")
    (#x0D . "<control> CARRIAGE RETURN (CR)")
    (#x0E . "<control> SHIFT OUT (SO)")
    (#x0F . "<control> SHIFT IN (SI)")
    (#x10 . "<control> DATA LINK ESCAPE (DLE)")
    (#x11 . "<control> DEVICE CONTROL ONE (DC1); XON")
    (#x12 . "<control> DEVICE CONTROL TWO (DC2)")
    (#x13 . "<control> DEVICE CONTROL THREE (DC3); XOFF")
    (#x14 . "<control> DEVICE CONTROL FOUR (DC4)")
    (#x15 . "<control> NEGATIVE ACKNOWLEDGE (NAK)")
    (#x16 . "<control> SYNCHRONOUS IDLE (SYN)")
    (#x17 . "<control> END OF TRANSMISSION BLOCK (ETB)")
    (#x18 . "<control> CANCEL (CAN)")
    (#x19 . "<control> END OF MEDIUM (EM)")
    (#x1A . "<control> SUBSTITUTE (SUB)")
    (#x1B . "<control> ESCAPE (ESC)")
    (#x1C . "<control> INFORMATION SEPARATOR FOUR; file separator (FS)")
    (#x1D . "<control> INFORMATION SEPARATOR THREE; group separator (GS)")
    (#x1E . "<control> INFORMATION SEPARATOR TWO; record separator (RS)")
    (#x1F . "<control> INFORMATION SEPARATOR ONE; unit separator (US)")
    (#x7F . "<control> DELETE (DEL)"))
  "ASCII control character names and aliases.")

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

;;; unicode-inspector-test.el --- Test for unicode-inspector  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Megurine Luka

;; Author: Megurine Luka <megurine@tadsan.local>
;; Keywords: maint

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

;; Test for unicode-inspector.el and unicode-inspector-blocks.el.

;;; Code:
(require 'ert)
(require 'unicode-inspector)
(require 'unicode-inspector-blocks)

(ert-deftest unicode-inspector-blocks ()
  (let ((cases '((?a . (0 127 "Basic Latin"))
                 (?あ . (12352 12447 "Hiragana"))
                 (?🖋 . (#x1F300 #x1F5FF "Miscellaneous Symbols and Pictographs")))))
    (cl-loop for (char . expected) in cases
             do (should (equal (cons char expected)
                               (cons char (unicode-inspector-blocks-range-for char)))))))

(provide 'unicode-inspector-test)
;;; unicode-inspector-test.el ends here

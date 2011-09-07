; -*- mode: lisp; package: xl-ppcre.test.util; encoding: shift_jis -*-

;;; xl-ppcre/test/test-utils.l
;;
;; Copyright (c) 2011 MIYAMUKO Katsuyuki.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require "xl-ppcre")
(require "lisp-unit")

(defpackage :xl-ppcre.test.util
  (:use :lisp :editor :lisp-unit :xl-ppcre))

(in-package :xl-ppcre.test.util)

(remove-all-tests :xl-ppcre.test.util)

(define-test utils/optimize-replacement
  (assert-equal
      "foobar"
      (ppcre::optimize-replacement "foobar" 0))
  (assert-equal
      "foobar\\1"
      (ppcre::optimize-replacement "foobar\\1" 1))
  (assert-equal
      "foobar\\1"
      (ppcre::optimize-replacement '("foobar" 0) 1))
  (assert-equal
      "foobar\\1\\2\\3\\4\\5\\6\\7\\8\\9"
      (ppcre::optimize-replacement '("foobar" 0 1 2 3 4 5 6 7 8) 9))
  (assert-equal
      "foo \\& bar \\1"
      (ppcre::optimize-replacement '("foo " :match " bar " 0) 1))
  (assert-equal
      "\\&\\1"
      (ppcre::optimize-replacement '(:match 0) 1))

  (assert-equal
      '("foo \\& before " :before-match)
      (ppcre::optimize-replacement '("foo " :match " before " :before-match) 0))
  (assert-equal
      '("foo \\& before " :after-match)
      (ppcre::optimize-replacement '("foo " :match " before " :after-match) 0))
  (assert-equal
      '("\\&\\\\" :before-match :after-match)
      (ppcre::optimize-replacement '(:match :backslash :before-match :after-match) 0))

  (assert-equal
      '("\\&\\\\" :before-match :after-match "foo\\1")
      (ppcre::optimize-replacement "\\&\\\\\\`\\'foo\\1" 1))
  )

;;; End

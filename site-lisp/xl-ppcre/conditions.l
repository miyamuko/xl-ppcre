; -*- mode: lisp; package: xl-ppcre; encoding: shift_jis -*-

;;; xl-ppcre/conditions.l
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "xl-ppcre/package")
  )

(in-package :xl-ppcre)

(export '(ppcre-error
          ppcre-error-format-arguments
          ppcre-error-format-string

          ppcre-invocation-error
          ppcre-invocation-error-format-arguments
          ppcre-invocation-error-format-string

          ppcre-syntax-error
          ppcre-syntax-error-format-arguments
          ppcre-syntax-error-format-string
          ppcre-syntax-error-pos
          ppcre-syntax-error-string
          ))

(define-condition ppcre-error (simple-error)
  )

(define-condition ppcre-invocation-error (ppcre-error)
  ()
  ;; 孫コンディションの場合、report を定義しないと
  ;; バイトコンパイルしたあとにロードできない
  ;; => ディスパッチングマクロ副文字ではありません: <
  (:report (lambda (c s)
             (apply 'format s
                     (ppcre-invocation-error-format-string c)
                     (ppcre-invocation-error-format-arguments c)))))

(define-condition ppcre-syntax-error (ppcre-error)
  (string pos)
  (:report (lambda (c s)
             (apply 'format s
                     (ppcre-syntax-error-format-string c)
                     (ppcre-syntax-error-format-arguments c)))))


(defun raise-invocation-error (format-string &rest format-args)
  (error 'ppcre-invocation-error
         :format-string format-string
         :format-arguments format-args
         ))

(defun raise-syntax-error (string pos format-string &rest format-args)
  (error 'ppcre-syntax-error
         :format-string format-string
         :format-arguments format-args
         :string string
         :pos pos
         ))

(defun raise-illegal-replacement (token)
  (raise-invocation-error "Illegal substring \"~A\" in replacement string." token))


(provide "xl-ppcre/conditions")

;;; End

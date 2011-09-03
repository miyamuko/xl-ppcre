; -*- mode: lisp; package: xl-ppcre; encoding: shift_jis -*-

;;; xl-ppcre/utils.l
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
  (require "xl-ppcre/psre-reader")
  )

(in-package :xl-ppcre)

(defun make-register-group-vars (var-list)
  (let ((n 0))
    (mapcan #'(lambda (var)
                (cond ((null var)
                       (incf n)
                       nil)
                      ((listp var)
                       (let ((fn (car var)))
                         (mapcar #'(lambda (var2)
                                     (let ((g (gensym)))
                                       `(,var2 (let ((,g (match-string ,(incf n))))
                                                 (when ,g
                                                   (funcall ,fn ,g))))))
                                 (cdr var))))
                      (t
                       (list `(,var (match-string ,(incf n)))))))
            var-list)))

(defun make-do-match-body (vars regex target-string result-form start end body)
  `(do ((#1=#:regex (create-scanner ,regex))
        (#2=#:target-string ,target-string)
        (#3=#:start ,start)
        (#4=#:end ,end))
       ((not (string-match #1# #2# #3# #4#))
        ,result-form)
     (let* (,@vars)
       (let ((#5=#:match-start (match-beginning 0))
             (#6=#:match-end (match-end 0)))
         (progn ,@body)
         (setf #3# (if (= #5# #6#)
                       (1+ #6#)
                     #6#))
         ))))

(defun count-register (regex)
  (ed::protect-match-data
    (labels ((count-regex (regex target-string)
               (let ((start 0)
                     (r 0))
                 (while (string-match regex target-string start)
                   (incf r)
                   (setf start (match-end 0)))
                 r)))
      (let ((regex-src (case (type-of regex)
                         (regexp (compiled-regexp-source regex))
                         (t regex))))
        (- (count-regex "\\\\(" regex-src)
           (count-regex "\\\\(\\?:" regex-src))))))

(defun make-register-list (n fn)
  (let (r)
    (dotimes (i n)
      (push (funcall fn (1+ i)) r))
    (nreverse r)))

(defun make-register-array (n fn)
  (make-array n :initial-contents (make-register-list n fn)))

(defun upcase-string-p (str)
  (every #'upper-case-p str))

(defun downcase-string-p (str)
  (every #'lower-case-p str))

(defun capitalcase-string-p (str)
  (and (upper-case-p (char str 0))
       (downcase-string-p (subseq str 1))))

(defun replace-match-string (str target-string match-var reg-vars match-start match-end)
  (let ((start 0)
        r)
    (while (string-match "\\\\\\([0-9&`']\\)" str start)
      (let ((ref (match-string 1))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (push (subseq str start beg) r)
        (push (cond ((string= "&" ref)
                     match-var)
                    ((string= "`" ref)
                     (subseq target-string 0 match-start))
                    ((string= "'" ref)
                     (subseq target-string match-end))
                    (t
                     (nth (- (parse-integer ref) 1) reg-vars)))
              r)
        (setf start end)))
    (if r
        (progn
          (push (subseq str start) r)
          (list->string (nreverse r)))
      str)))

(defun list->string (lst)
  (format nil "~{~A~}" (remove-if #'null lst)))


(provide "xl-ppcre/utils")

;;; End

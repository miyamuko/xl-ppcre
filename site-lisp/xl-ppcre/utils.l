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

(defun make-do-match-body (make-vars regex target-string result-form start end body)
  (let ((gregex (gensym))
        (gmatch-start (gensym))
        (gmatch-end (gensym)))
    `(let ((,gregex (create-scanner ,regex)))
       (do ((#1=#:target-string ,target-string)
            (#2=#:start ,start)
            (#3=#:end ,end))
           ((not (string-match ,gregex #1# #2# #3#))
            ,result-form)
         (let* ((,gmatch-start (match-beginning 0))
                (,gmatch-end (match-end 0))
                ,@(funcall make-vars gregex gmatch-start gmatch-end))
           (progn ,@body)
           (setf #2# (if (= ,gmatch-start ,gmatch-end)
                         (1+ ,gmatch-end)
                       ,gmatch-end))
           )))))

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

(defun convert-case (str match-var)
  (cond ((upcase-string-p match-var)
         (string-upcase str))
        ((downcase-string-p match-var)
         (string-downcase str))
        ((capitalcase-string-p match-var)
         (string-capitalize str))
        (t
         str)))

(defun optimize-replacement (replacement reg-bound)
  (when (atom replacement)
    (setf replacement (list replacement)))
  (let (r tmp)
    (flet ((flush-temp-string ()
             (when tmp
               (push (list->string (nreverse tmp)) r)
               (setf tmp nil))))
      (dolist (rep replacement)
        (cond ((integerp rep)
               ;; リスト形式では \n - 1 を指定する
               (incf rep)
               ;; 最大でも \1 ～ \9 までが有効な範囲
               (unless (<= 1 rep (min 9 reg-bound))
                 (raise-non-existent-register rep))
               (push (format nil "\\~A" rep) tmp))
              ((stringp rep)
               (when (string-match "\\0" rep)
                 (raise-illegal-replacement "\\0"))
               (when (and (< reg-bound 9)
                          (string-match (format nil "\\\\\\([~D-9]\\)" (1+ reg-bound))
                                        rep))
                 (raise-non-existent-register (match-string 1)))
               (let ((last-match 0))
                 ;; 置換文字列中の \` \' を :before-match, :after-match に展開する
                 (do-matches (ms me "\\\\['`]" rep)
                   (when (/= last-match ms)
                     (push (subseq rep last-match ms) tmp))
                   (flush-temp-string)
                   (push (if (char= (char (match-string 0) 1) #\`)
                             :before-match :after-match)
                         r)
                   (setf last-match me))
                 (push (subseq rep last-match) tmp)))
              ((symbolp rep)
               ;; :match, :backslash は \& \ として置換文字列に埋め込む
               ;; :before-match, :after-match はそのまま
               (let ((n (case rep
                          (:match "\\&")
                          (:backslash "\\\\")
                          (t rep))))
                 (if (stringp n)
                     (push n tmp)
                   (progn
                     (flush-temp-string)
                     (push n r)))))
              (t
               (flush-temp-string)
               (push rep r)
               )))
      (flush-temp-string)
      (if (and (stringp (car r))
               (not (cdr r)))
          (car r)
        (nreverse r)))))

(defun list->string (lst)
  (format nil "~{~A~}" (remove-if #'null lst)))


(provide "xl-ppcre/utils")

;;; End

; -*- mode: lisp; package: xl-ppcre; encoding: shift_jis -*-

;;; xl-ppcre/api.l
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
  (require "xl-ppcre/utils")
  )

(in-package :xl-ppcre)

(export '(scan
          scan-to-strings
          register-groups-bind
          do-scans
          do-matches
          do-matches-as-strings
          do-register-groups
          all-matches
          all-matches-as-strings
          split
          regex-replace
          regex-replace-all
          ))


(defun scan (regex target-string &key start end)
  (when (string-match regex target-string start end)
    (let ((n (count-register regex)))
      (values
       (match-beginning 0)
       (match-end 0)
       (make-register-array n #'match-beginning)
       (make-register-array n #'match-end)
       ))))

(defun scan-to-strings (regex target-string &key start end sharedp)
  (when (string-match regex target-string start end)
    (let ((n (count-register regex)))
      (values
       (match-string 0)
       (make-register-array n #'match-string)
       ))))

(defmacro register-groups-bind (var-list (regex target-string &key start end sharedp) &body body)
  `(when (string-match ,regex ,target-string ,start ,end)
     (let (,@(make-register-group-vars var-list))
       ,@body)))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex target-string
                                 &optional result-form
                                 &key start end)
                    &body body)
  (let ((gn (gensym)))
    (make-do-match-body `((,gn (count-register ,regex))
                          (,match-start (match-beginning 0))
                          (,match-end (match-end 0))
                          (,reg-starts (make-register-array ,gn #'match-beginning))
                          (,reg-ends (make-register-array ,gn #'match-end)))
                        regex target-string result-form start end body)))

(defmacro do-matches ((match-start match-end regex target-string
                                   &optional result-form
                                   &key start end)
                      &body body)
  (make-do-match-body `((,match-start (match-beginning 0))
                        (,match-end (match-end 0)))
                      regex target-string result-form start end body))

(defmacro do-matches-as-strings ((match-var regex target-string
                                            &optional result-form
                                            &key start end sharedp)
                                 &body body)
  (make-do-match-body `((,match-var (match-string 0)))
                      regex target-string result-form start end body))

(defmacro do-register-groups (var-list (regex target-string
                                              &optional result-form &key start end sharedp)
                                       &body body)
  (make-do-match-body (make-register-group-vars var-list)
                      regex target-string result-form start end body))

(defun all-matches (regex target-string &key start end)
  (let (r)
    (do-matches (match-start match-end regex target-string
                             (nreverse r)
                             :start start :end end)
      (push match-start r)
      (push match-end r)
      )))

(defun all-matches-as-strings (regex target-string &key start end sharedp)
  (let (r)
    (do-matches-as-strings (match-var regex target-string
                                      (nreverse r)
                                      :start start :end end :sharedp sharedp)
      (push match-var r)
      )))

(defun split (regex target-string &key start end limit with-registers-p omit-unmatched-p sharedp)
  (let ((last-match-start 0)
        (last-match-end 0)
        (n (count-register regex))
        (c 0)
        r)
    (when (and limit (zerop limit))
      (setf limit nil))
    (do-scans (match-start match-end reg-starts reg-ends
                           regex target-string
                           nil
                           :start start :end end)
      (unless (and (= match-start match-end)
                   (= match-start last-match-end))
        (when (and limit (<= limit c))
          (return))
        (incf c)
        (push (subseq target-string last-match-end match-start) r)
        (when with-registers-p
          (dolist (reg (make-register-list n #'match-string))
            (when (or reg (not omit-unmatched-p))
              (push reg r))))
        (setf last-match-start match-start
              last-match-end match-end)
        ))
    (when (or limit (/= last-match-start last-match-end))
      (push (subseq target-string last-match-end) r))
    (nreverse r)))

(defun regex-replace (regex target-string replacement
                            &key start end preserve-case simple-calls element-type)
  (regex-replace-all0 regex target-string replacement
                      :limit 1
                      :start start :end end
                      :preserve-case preserve-case
                      :simple-calls simple-calls
                      :element-type element-type))

(defun regex-replace-all (regex target-string replacement
                                &key start end preserve-case simple-calls element-type)
  (regex-replace-all0 regex target-string replacement
                      :start start :end end
                      :preserve-case preserve-case
                      :simple-calls simple-calls
                      :element-type element-type))

(defun regex-replace-all0 (regex target-string replacement
                                 &key limit start end preserve-case simple-calls element-type)
  (let ((last-match-end 0)
        (n (count-register regex))
        (c 0)
        r)
    (do-scans (match-start match-end reg-starts reg-ends
                           regex target-string nil
                           :start start :end end)
      (let ((match-var (match-string 0))
            (reg-vars (make-register-list n #'match-string)))
        (when (/= last-match-end match-start)
          (push (subseq target-string last-match-end match-start) r))
        (push (eval-replacement target-string replacement
                                match-var reg-vars
                                match-start match-end reg-starts reg-ends
                                :start start :end end
                                :simple-calls simple-calls
                                :preserve-case preserve-case)
              r)
        (setf last-match-end match-end)
        (incf c)
        (when (and limit (<= limit c))
          (return))
        ))
    (if (zerop c)
        (values target-string nil)
      (progn
        (push (subseq target-string last-match-end) r)
        (values (format nil "~{~A~}" (nreverse r)) t)))
    ))

(defun eval-replacement (target-string replacement
                                       match-var reg-vars
                                       match-start match-end reg-starts reg-ends
                                       &key start end simple-calls preserve-case)
  (let ((r (apply-replacement target-string replacement
                              match-var reg-vars
                              match-start match-end reg-starts reg-ends
                              :start start :end end
                              :simple-calls simple-calls)))
    (if (and r preserve-case)
        (cond ((upcase-string-p match-var)
               (string-upcase r))
              ((downcase-string-p match-var)
               (string-downcase r))
              ((capitalcase-string-p match-var)
               (string-capitalize r))
              (t
               r))
      r)))

(defun apply-replacement (target-string replacement
                                        match-var reg-vars
                                        match-start match-end reg-starts reg-ends
                                        &key start end simple-calls)
  (when (atom replacement)
    (setf replacement (list replacement)))
  (format nil "~{~A~}"
          (mapcar #'(lambda (rep)
                      (cond ((stringp rep)
                             rep)
                            ((or (functionp rep)
                                 (symbolp rep))
                             (if simple-calls
                                 (apply rep match-var reg-vars)
                               (apply rep target-string start end match-start match-end reg-starts reg-ends)))
                            ((integerp rep)
                             (nth rep reg-vars))
                            (t
                             (error "Invalid replacement: ~S" rep))))
                  replacement)))


#+xyzzy
(setf (get 'register-groups-bind 'ed:lisp-indent-hook) 1
      (get 'do-scans 'ed:lisp-indent-hook) 1
      (get 'do-matches 'ed:lisp-indent-hook) 1
      (get 'do-matches-as-strings 'ed:lisp-indent-hook) 1
      (get 'do-register-groups 'ed:lisp-indent-hook) 1
      )

(provide "xl-ppcre/api")

;;; End

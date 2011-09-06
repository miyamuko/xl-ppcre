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
  (require "xl-ppcre/conditions")
  )

(in-package :xl-ppcre)

(export '(create-scanner
          scan
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
          quote-meta-chars
          ))

;; cl-ppcre の create-scanner は closure を返すが、
;; xl-ppcre では Perl 風の正規表現を xyzzy 用の正規表現に変換しコンパイルして返す。
;; compile-regexp を与えられた場合は引数をそのまま返す。
;; 正規表現は文字列形式のみをサポートし parse tree 形式はサポートしていない。
(defun create-scanner (regex
                       &key case-insensitive-mode multi-line-mode single-line-mode
                       extended-mode destructive)
  (values
   (case (type-of regex)
     (regexp
      regex)
     ((simple-string string)
      ;; (?i) を特別に処理する
      (when (string-match "(\\?\\(-\\)?i)" regex)
        (unless (zerop (match-beginning 0))
          (raise-syntax-error regex nil
                              "~A は文字列の先頭にある場合のみサポートしてます。"
                              (match-string 0)))
        (setf regex (subseq regex (match-end 0)))
        (setf case-insensitive-mode (if (match-string 1) nil t)))
      ;; (compile-regexp "") => nil
      (handler-case
          (or (compile-regexp (convert-from-psre regex)
                              case-insensitive-mode)
              "")
        (error (c)
          (raise-syntax-error regex nil (format nil "~A" c)))))
     (t
      (error 'type-error
             :datum regex
             :expected-type '(or regexp string))))
   nil))

(defun scan (regex target-string &key start end)
  (setf regex (create-scanner regex))
  (when (string-match regex target-string start end)
    (let ((n (count-register regex)))
      (values
       (match-beginning 0)
       (match-end 0)
       (make-register-array n #'match-beginning)
       (make-register-array n #'match-end)
       ))))

(defun scan-to-strings (regex target-string &key start end sharedp)
  (setf regex (create-scanner regex))
  (when (string-match regex target-string start end)
    (let ((n (count-register regex)))
      (values
       (match-string 0)
       (make-register-array n #'match-string)
       ))))

(defmacro register-groups-bind (var-list (regex target-string &key start end sharedp) &body body)
  `(when (string-match (create-scanner ,regex) ,target-string ,start ,end)
     (let (,@(make-register-group-vars var-list))
       ,@body)))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex target-string
                                 &optional result-form
                                 &key start end)
                    &body body)
  (let ((gn (gensym)))
    (make-do-match-body #'(lambda (re ms me)
                            `((,gn (count-register ,re))
                              (,match-start ,ms)
                              (,match-end ,me)
                              (,reg-starts (make-register-array ,gn #'match-beginning))
                              (,reg-ends (make-register-array ,gn #'match-end))))
                        regex target-string result-form start end body)))

(defmacro do-matches ((match-start match-end regex target-string
                                   &optional result-form
                                   &key start end)
                      &body body)
  (make-do-match-body #'(lambda (re ms me)
                          `((,match-start ,ms)
                            (,match-end ,me)))
                      regex target-string result-form start end body))

(defmacro do-matches-as-strings ((match-var regex target-string
                                            &optional result-form
                                            &key start end sharedp)
                                 &body body)
  (make-do-match-body #'(lambda (re ms me)
                          `((,match-var (match-string 0))))
                      regex target-string result-form start end body))

(defmacro do-register-groups (var-list (regex target-string
                                              &optional result-form &key start end sharedp)
                                       &body body)
  (make-do-match-body #'(lambda (re ms me)
                          (make-register-group-vars var-list))
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
  ;; 正規表現が空の場合、マッチしないので特別に処理する
  ;; (string-match "" "hoge") ;=> nil
  (if (string= regex "")
      (split-by-empty target-string
                      :start start :end end :limit limit)
    (split-by-regex regex target-string
                    :start start :end end :limit limit
                    :with-registers-p with-registers-p
                    :omit-unmatched-p omit-unmatched-p)))

(defun split-by-empty (target-string &key start end limit)
  (when (and limit (zerop limit))
    (setf limit nil))
  (when (or start end)
    (setf target-string (subseq target-string (or start 0) end)))
  (flet ((split-each (str)
           (mapcar #'string (concatenate 'list str))))
    (case limit
      ((nil)
       (split-each target-string))
      (1
       ;; limit=1 は分割する必要なし
       ;; ここだけ sharedp が有効なのは気持ち悪いので常にコピーして返す
       (list (copy-seq target-string)))
      (t
       (let* ((maxlen (length target-string))
              (n (min maxlen (- limit 1))))
         (nconc
          (split-each (subseq target-string 0 n))
          (list (subseq target-string n)))))
      )))

(defun split-by-regex (regex target-string &key start end limit with-registers-p omit-unmatched-p)
  (setf regex (create-scanner regex))
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
      (unless (= match-start match-end last-match-end)
        (incf c)
        (when (and limit (<= limit c))
          (return))
        (push (subseq target-string last-match-end match-start) r)
        (when with-registers-p
          (dolist (reg (make-register-list n #'match-string))
            (when (or reg (not omit-unmatched-p))
              (push reg r))))
        (setf last-match-start match-start
              last-match-end match-end)
        ))
    (push (subseq target-string last-match-end) r)
    ;; limit が指定されていないときは最後の "" は削除する
    (unless limit
      (while (string= "" (car r))
        (pop r)))
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
  (setf regex (create-scanner regex))
  (setf replacement (optimize-replacement replacement))
  (if (and (stringp replacement)
           (not preserve-case))
      (regex-replace-all-simple regex target-string replacement
                                :start start :end end :limit limit)
    (regex-replace-all-complex regex target-string replacement
                               :start start :end end
                               :preserve-case preserve-case
                               :simple-calls simple-calls
                               :element-type element-type)))

(defun regex-replace-all-simple (regex target-string replacement
                                       &key limit start end)
  (multiple-value-bind (str n)
      (substitute-string target-string regex replacement :count limit :start start :end end)
    (values str (< 0 n))))

(defun regex-replace-all-complex (regex target-string replacement
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
        (values (list->string (nreverse r)) t)))
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
        (convert-case r match-var)
      r)))

(defun apply-replacement (target-string replacement
                                        match-var reg-vars
                                        match-start match-end reg-starts reg-ends
                                        &key start end simple-calls)
  (when (stringp replacement)
    (return-from apply-replacement (string-replace-match target-string replacement)))
  (flet ((call-replacement (replacement)
           (if simple-calls
               (apply replacement match-var reg-vars)
             (funcall replacement target-string start end match-start match-end reg-starts reg-ends))))
    (list->string
     (mapcar #'(lambda (rep)
                 (cond ((stringp rep)
                        (string-replace-match target-string rep))
                       ((functionp rep)
                        (call-replacement rep))
                       ((symbolp rep)
                        (case rep
                          (:before-match (subseq target-string 0 match-start))
                          (:after-match (subseq target-string match-end))
                          (t (call-replacement rep))))
                       ))
             replacement))))

(defun quote-meta-chars (string)
  (identity
   (substitute-string string "\\([^a-zA-Z0-9_]\\)" "\\\\\\1")))


#+xyzzy
(setf (get 'register-groups-bind 'ed:lisp-indent-hook) 1
      (get 'do-scans 'ed:lisp-indent-hook) 1
      (get 'do-matches 'ed:lisp-indent-hook) 1
      (get 'do-matches-as-strings 'ed:lisp-indent-hook) 1
      (get 'do-register-groups 'ed:lisp-indent-hook) 1
      )

(provide "xl-ppcre/api")

;;; End

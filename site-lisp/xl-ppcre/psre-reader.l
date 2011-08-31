; -*- mode: lisp; package: xl-ppcre; encoding: shift_jis -*-

;;; Perl Style Regexp Reader for xyzzy
;;
;; This file is part of http://github.com/youz/reut/
;;
;; Copyright (C) 2009-2010 Yousuke Ushiki
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;; 3. The name of the contributors may not be used to endorse or promote
;;    products derived from this software without specific prior
;;    written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE!
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "xl-ppcre/package")
  )

(in-package :xl-ppcre)

(defun psre-reader (stream subchar n)
  (with-output-to-string (os)
    (labels
        ((start ()
           (do ((c #1=(read-char stream nil subchar) #1#))
               ((eql c subchar))
             (case c
               ((#\( #\) #\{ #\} #\|) (format os "\\~A" c))
               (#\\ (escape #1#))
               (#\[ (char-class))
               (t (princ c os)))))
         (print-code (s &rest code)
           (let ((code (parse-integer (format nil "~{~A~}" code) :radix 16)))
             (princ (code-char code) s)))
         (escape (c)
           (unless c (error 'end-of-file))
           (case c
             ((#\( #\) #\{ #\} #\| #\/) (princ c os))
             (#\t (princ #\TAB os))
             (#\n (princ #\LFD os))
             (#\r (princ #\RET os))
             (#\f (princ #\C-l os))
             (#\d (princ "[0-9]" os))
             (#\D (princ "[^0-9]" os))
             (#\s (princ "[ \t\n\r\f]" os))
             (#\S (princ "[^ \t\n\r\f]" os))
             (#\x (print-code os #1# #1#))
             (#\X (print-code os #1# #1# #1# #1#))
             (t (format os "\\~A" c))))
         (char-class ()
           (princ #\[ os)
           (when (char= #\^ (peek-char nil stream))
             (princ #1# os))
           (format os "~A]"
                   (with-output-to-string (cs)
                     (do ((c #2=(read-char stream nil nil) #2#)
                          (hyphen ""))
                         ((eql c #\]) (princ hyphen cs))
                       (unless c
                         #3=(error "‘[’に対応する‘]’がありません" 'simple-error))
                       (case c
                         (#\\
                          (let ((e #1#))
                            (case e
                              (#\- (setq hyphen "-"))
                              (#\] (princ #\] os))
                              (#\w (princ "0-9A-Za-z_" cs))
                              (#\d (princ "0-9" cs))
                              (#\s (princ " \t\n\r\f" cs))
                              (#\t (princ #\TAB cs))
                              (#\n (princ #\LFD cs))
                              (#\r (princ #\RET cs))
                              (#\f (princ #\C-l cs))
                              (#\x (print-code cs #1# #1#))
                              (#\X (print-code cs #1# #1# #1# #1#))
                              (t (princ e cs)))))
                         (subchar #3#)
                         (t (princ c cs))))))))
      (start))))

(defun convert-from-psre (psre)
  (with-input-from-string (is psre)
    (psre-reader is nil nil)))


(provide "xl-ppcre/psre-reader")

;;; End

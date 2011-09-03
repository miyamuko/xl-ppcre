; -*- mode: lisp; package: xl-ppcre.test; encoding: shift_jis -*-

;;; xl-ppcre/test/test-api.l
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

(defpackage :xl-ppcre.test
  (:use :lisp :editor :lisp-unit :xl-ppcre))

(in-package :xl-ppcre.test)

(remove-all-tests :xl-ppcre.test)

(define-test api/create-scanner
  (assert-false
      (compiled-regexp-case-fold-p
       (create-scanner "foo")))
  (assert-false
      (compiled-regexp-case-fold-p
       (create-scanner "foo" :case-insensitive-mode nil)))
  (assert-true
      (compiled-regexp-case-fold-p
       (create-scanner "foo" :case-insensitive-mode t)))

  (assert-true
      (compiled-regexp-case-fold-p
       (create-scanner "(?i)foo")))
  (assert-true
      (compiled-regexp-case-fold-p
       (create-scanner "(?i)foo" :case-insensitive-mode nil)))
  (assert-true
      (compiled-regexp-case-fold-p
       (create-scanner "(?i)foo" :case-insensitive-mode t)))

  (assert-false
      (compiled-regexp-case-fold-p
       (create-scanner "(?i-)foo")))
  (assert-false
      (compiled-regexp-case-fold-p
       (create-scanner "(?i-)foo" :case-insensitive-mode nil)))
  (assert-false
      (compiled-regexp-case-fold-p
       (create-scanner "(?i-)foo" :case-insensitive-mode t)))

  (assert-error 'simple-error
      (create-scanner "bar(?i-)foo"))
  (assert-error 'simple-error
      (create-scanner "bar\n(?i-)foo"))
  )

(define-test api/scan
  (assert-error 'type-error
      (scan nil nil))

  (assert-error 'type-error
      (scan nil ""))

  (assert-error 'type-error
      (scan "" nil))

  (assert-equalp
      nil
      (scan "" ""))

  (assert-equalp
      nil
      (scan "^a+$" "a+"))

  (assert-equalp
      (values 1 5 #(3) #(4))
      (scan "(a)*b" "xaaabd"))

  (assert-equalp
      (values 1 5 #(3) #(4))
      (scan "(a)*b" "xaaabd" :start 1))

  (assert-equalp
      (values 2 5 #(3) #(4))
      (scan "(a)*b" "xaaabd" :start 2))

  (assert-equalp
      nil
      (scan "(a)*b" "xaaabd" :end 4))

  (assert-equalp
      (values 0 4 #(0 2) #(3 3))
      (let ((s (create-scanner "(([a-c])+)x")))
        (scan s "abcxy")))

  (assert-equalp
      (values 0 4 #(0 2) #(3 3))
      (let ((s (create-scanner "(([a-c])+)x" :case-insensitive-mode t)))
        (scan s "ABCXY")))
  )

(define-test api/scan-to-strings
  (assert-error 'type-error
      (scan-to-strings nil nil))

  (assert-error 'type-error
      (scan-to-strings nil ""))

  (assert-error 'type-error
      (scan-to-strings "" nil))

  (assert-equalp
      nil
      (scan-to-strings "" ""))

  (assert-equalp
      (values "aaab" #())
      (scan-to-strings "[^b]*b" "aaabd"))

  (assert-equalp
      (values "aaab" #("a"))
      (scan-to-strings "([^b])*b" "aaabd"))

  (assert-equalp
      (values "aaab" #("aaa" "a"))
      (scan-to-strings "(([^b])*)b" "aaabd"))

  (assert-equalp
      nil
      (scan-to-strings "(a)*b" "xaaabd" :end 4))

  (assert-equalp
      (values "ab" #("a"))
      (scan-to-strings "(a)*b" "xaaabd" :start 3))
  )

(define-test api/register-groups-bind
  (assert-equalp
      (list "c" "a" "b" "c")
      (register-groups-bind (first second third fourth)
        ("((a)|(b)|(c))+" "abababc" :sharedp t)
        (list first second third fourth)))

  (assert-equalp
      (list nil nil "c")
      (register-groups-bind (nil second third fourth)
        ("((a)|(b)|(c))()+" "abababc" :start 6)
        (list second third fourth)))

  (assert-equalp
      nil
      (register-groups-bind (first)
        ("(a|b)+" "accc" :start 1)
        first))

  (assert-equalp
      (list "Frank" "Zappa" 1292889600)
      (register-groups-bind (fname lname (#'parse-integer date month year))
        ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})" "Frank Zappa 21.12.1940")
        (list fname lname (encode-universal-time 0 0 0 date month year 0))))
  )

(define-test api/do-matches
  (flet ((foo (regex target-string &key (start 0) (end (length target-string)))
           (let ((sum 0))
             (do-matches (s e regex target-string nil :start start :end end)
               (incf sum (- e s)))
             (/ sum (- end start)))))
    (assert-eql 1/3 (foo "a" "abcabcabc"))
    (assert-eql 5/9 (foo "aa|b" "aacabcbbc"))
    ))

#+skip
(define-test api/do-matches-as-strings
  (labels ((crossfoot (target-string &key (start 0) (end (length target-string)))
             (let ((sum 0))
               (do-matches-as-strings (m :digit-class
                                         target-string nil
                                         :start start :end end)
                 (incf sum (parse-integer m)))
               (if (< sum 10)
                   sum
                 (crossfoot (format nil "~A" sum))))))
    (assert-eql 0 (crossfoot "bar"))
    (assert-eql 3 (crossfoot "a3x"))
    (assert-eql 6 (crossfoot "12345"))
    )
  )

(define-test api/do-register-groups
  (let (result)
    (do-register-groups (first second third fourth)
      ("((a)|(b)|(c))" "abababc" nil :start 2 :sharedp t)
      (push (list first second third fourth) result))
    (assert-equal
        (nreverse result)
        '(("a" "a" nil nil)
          ("b" nil "b" nil)
          ("a" "a" nil nil)
          ("b" nil "b" nil)
          ("c" nil nil "c"))))
  (let (result)
    (do-register-groups ((#'parse-integer n) (#'intern sign) whitespace)
      ("(\\d+)|(\\+|-|\\*|/)|(\\s+)" "12*15 - 42/3")
      (unless whitespace
        (push (or n sign) result)))
    (assert-equal
        (nreverse result)
        '(12 * 15 - 42 / 3)))
  )

(define-test api/all-matches
  (assert-equal
      (list 5 6 9 10)
      (all-matches "a" "foo bar baz"))
  (assert-equal
      (list 0 3 3 3 4 7 7 7 8 11 11 11)
      (all-matches "\\w*" "foo bar baz"))
  )

(define-test api/all-matches-as-strings
  (assert-equal
      (list "a" "a")
      (all-matches-as-strings "a" "foo bar baz"))
  (assert-equal (list "foo" "" "bar" "" "baz" "")
      (all-matches-as-strings "\\w*" "foo bar baz"))
  )

(define-test api/split
  (assert-equal
      '("foo" "bar" "baz" "frob")
      (split "\\s+" "foo   bar baz
frob"))

  (assert-equal
      '("f" "o" "o" "b" "a" "r" "b" "a" "z")
      (split "\\s*" "foo bar   baz"))

  (assert-equal
      '("foo" "bar" "baz")
      (split "(\\s+)" "foo bar   baz"))

  (assert-equal
      '("foo" " " "bar" "   " "baz")
      (split "(\\s+)" "foo bar   baz" :with-registers-p t))

  (assert-equal
      '("foo" " " "" "bar" " " "  " "baz")
      (split "(\\s)(\\s*)" "foo bar   baz" :with-registers-p t))

  (assert-equal
      '("foo" "," nil "bar" nil ";" "baz")
      (split "(,)|(;)" "foo,bar;baz" :with-registers-p t))

  (assert-equal
      '("foo" "," "bar" ";" "baz")
      (split "(,)|(;)" "foo,bar;baz" :with-registers-p t :omit-unmatched-p t))

  (assert-equal
      '("a" "b" "c" "d" "e" "f" "g")
      (split ":" "a:b:c:d:e:f:g::"))

  (assert-equal
      '("a:b:c:d:e:f:g::")
      (split ":" "a:b:c:d:e:f:g::" :limit 1))

  (assert-equal
      '("a" "b:c:d:e:f:g::")
      (split ":" "a:b:c:d:e:f:g::" :limit 2))

  (assert-equal
      '("a" "b" "c:d:e:f:g::")
      (split ":" "a:b:c:d:e:f:g::" :limit 3))

  (assert-equal
      '("a" "b" "c" "d" "e" "f" "g" "" "")
      (split ":" "a:b:c:d:e:f:g::" :limit 1000))

  (assert-equal
      '("" "a" "" "b" "" "" "c")
      (split "," ",a,,b,,,c,,,,"))

  (assert-equal
      '("" "a" "" "b" "" "" "c" "" "" "" "")
      (split "," ",a,,b,,,c,,,," :limit 1000))

  (assert-equal
      '(",a,,b,,,c,,,,")
      (split "," ",a,,b,,,c,,,," :limit -1))

  (assert-equal
      '("a" "b" "c")
      (split ".*?" "abc"))

  (assert-equal
      '("a" "b" "c" "")
      (split ".*?" "abc" :limit 1000))

  (assert-equal
      nil
      (split "." "abc"))
  )

(define-test api/split-with-start
  (assert-equal
      '("a" "b" "c")
      (split "" "abc" :start 0))

  (assert-equal
      '("b" "c")
      (split "" "abc" :start 1))

  (assert-equal
      '("c")
      (split "" "abc" :start 2))

  (assert-equal
      nil
      (split "" "abc" :start 3))
  )

(define-test api/split-with-end
  (assert-equal
      nil
      (split "" "abc" :end 0))

  (assert-equal
      '("a")
      (split "" "abc" :end 1))

  (assert-equal
      '("a" "b")
      (split "" "abc" :end 2))

  (assert-equal
      '("a" "b" "c")
      (split "" "abc" :end 3))

  (assert-error 'range-error
      (split "" "abc" :end 4))

  (assert-equal
      '("c" "d")
      (split "" "abcdefg" :start 2 :end 4))
  )

(define-test api/split-with-limit
  (assert-equal
      '("a" "b" "c")
      (split "" "abc" :limit nil))

  (assert-equal
      '("a" "b" "c")
      (split "" "abc" :limit 0))

  (assert-equal
      '("abc")
      (split "" "abc" :limit 1))

  (assert-equal
      '("a" "bc")
      (split "" "abc" :limit 2))

  (assert-equal
      '("a" "b" "c")
      (split "" "abc" :limit 3))

  (assert-equal
      '("a" "b" "c" "")
      (split "" "abc" :limit 4))

  (assert-equal
      '("a" "b" "c" "")
      (split "" "abc" :limit 5))

  (assert-equal
      '("b" "c" "def")
      (split "" "abcdefg" :start 1 :end 6 :limit 3))

  (assert-equal
      '("b" "c")
      (split "" "abc" :start 1 :limit 2))

  (assert-equal
      '("c" "")
      (split "" "abc" :start 2 :limit 2))

  (assert-equal
      '("c" "")
      (split "" "abc" :start 2 :limit 3))
  )

(define-test api/regex-replace
  (assert-equal
      (values "frob bar" t)
      (regex-replace "fo+" "foo bar" "frob"))

  (assert-equal
      (values "FOO bar" nil)
      (regex-replace "fo+" "FOO bar" "frob"))

  (assert-equal
      (values "frob bar" t)
      (regex-replace "(?i)fo+" "FOO bar" "frob"))

  (assert-equal
      (values "FROB bar" t)
      (regex-replace "(?i)fo+" "FOO bar" "frob" :preserve-case t))

  (assert-equal
      (values "Frob bar" t)
      (regex-replace "(?i)fo+" "Foo bar" "frob" :preserve-case t))

  (assert-equal
      (values "foo [frob (was 'bar' between 'foo ' and ' baz')] baz" t)
      (regex-replace "bar" "foo bar baz" "[frob (was '\\&' between '\\`' and '\\'')]"))

  #+skip
  (assert-equal
      (values "foo [frob (was 'bar' between 'foo ' and ' baz')] baz" t)
      (regex-replace "bar" "foo bar baz"
                     '("[frob (was '" :match "' between '" :before-match "' and '" :after-match "')]")))

  (assert-equal
      (values "benevolent [be.nev.o.lent]: adj. generous, kind" t)
      (regex-replace "(be)(nev)(o)(lent)"
                     "benevolent: adj. generous, kind"
                     #'(lambda (match &rest registers)
                         (format nil "~A [~{~A~^.~}]" match registers))
                     :simple-calls t))
  )

(define-test api/regex-replace-all
  (assert-equal
      (values "frob Frob FROB bar" t)
      (regex-replace-all "(?i)fo+" "foo Fooo FOOOO bar" "frob" :preserve-case t))

  (assert-equal
      "froob Frooob FROOOOB bar"
      (regex-replace-all "(?i)f(o+)" "foo Fooo FOOOO bar" "fr\\1b" :preserve-case t))

  (assert-equal
      "2011年09月03日 (2011-09-03)"
      (regex-replace-all "(\\d+)-(\\d+)-(\\d+)" "2011-09-03" "\\1年\\2月\\3日 (\\&)"))

  (assert-equal
      "--"
      (regex-replace-all "(\\d+)-(\\d+)-(\\d+)" "2011-09-03" "\\4-\\5-\\6"))

  (let ((qp-regex (create-scanner "[^\\x20-\\x7f]")))
    (flet ((encode-quoted-printable (string)
             "Converts 8-bit string to quoted-printable representation."
             ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
             (flet ((convert (target-string start end match-start match-end reg-starts reg-ends)
                      (declare (ignore start end match-end reg-starts reg-ends))
                      (format nil "=~2,'0x" (char-code (char target-string match-start)))))
               (regex-replace-all qp-regex string #'convert))))
      (assert-equal
          (values "=82a0=82a2=82a4abc=8341=8343=8345ABC" t)
          (encode-quoted-printable "あいうabcアイウABC"))))

  (let ((url-regex (create-scanner "[^a-zA-Z0-9_\\-.]")))
    (flet ((url-encode (string)
             "URL-encodes a string."
             ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
             (flet ((convert (target-string start end match-start match-end reg-starts reg-ends)
                      (declare (ignore start end match-end reg-starts reg-ends))
                      (format nil "%~2,'0x" (char-code (char target-string match-start)))))
               (regex-replace-all url-regex string #'convert))))
      (assert-equal
          (values "%82a0%82a2%82a4abc%8341%8343%8345ABC" t)
          (url-encode "あいうabcアイウABC"))))

  (flet ((how-many (target-string start end match-start match-end reg-starts reg-ends)
           (declare (ignore target-string start end match-start match-end))
           (format nil "~A" (- (svref reg-ends 0)
                               (svref reg-starts 0)))))
    (assert-equal
        "foo[3 dots]bar[5 dots][2 dots]baz[4 dots]frob"
        (regex-replace-all "{(.+?)}"
                           "foo{...}bar{.....}{..}baz{....}frob"
                           (list "[" #'how-many " dots]"))))

  (let ((qp-regex (create-scanner "[^\\x20-\\x7f]")))
    (flet ((encode-quoted-printable (string)
             "Converts 8-bit string to quoted-printable representation.
Version using SIMPLE-CALLS keyword argument."
             ;; ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
             (flet ((convert (match)
                      (format nil "=~2,'0x" (char-code (char match 0)))))
               (regex-replace-all qp-regex string #'convert
                                  :simple-calls t))))
      (assert-equal
          (values "=82a0=82a2=82a4abc=8341=8343=8345ABC" t)
          (encode-quoted-printable "あいうabcアイウABC"))))

  (flet ((how-many (match first-register)
           (declare (ignore match))
           (format nil "~A" (length first-register))))
    (assert-equal
        "foo[3 dots]bar[5 dots][2 dots]baz[4 dots]frob"
        (regex-replace-all "{(.+?)}"
                           "foo{...}bar{.....}{..}baz{....}frob"
                           (list "[" #'how-many " dots]")
                           :simple-calls t)))
  )
#|
(flet ((my-repetition (char min)
         `(:greedy-repetition ,min nil ,char)))
  (setf (parse-tree-synonym 'a*) (my-repetition #\a 0)
        (parse-tree-synonym 'b+) (my-repetition #\b 1))
  (unwind-protect
      (let ((scanner (create-scanner '(:sequence a* b+))))
        (equal (mapcar (lambda (target)
                         (scan scanner target))
                       '("ab" "b" "aab" "a" "x"))
               (list 0 0 0 nil nil)))
    (setf (parse-tree-synonym 'a*) nil
          (parse-tree-synonym 'b+) nil)))

(let ((*allow-quoting* t))
  ;;we use CREATE-SCANNER because of Lisps like SBCL that don't have an interpreter
  (equalp (multiple-value-list (scan (create-scanner "^\\Qa+\\E$") "a+"))
          (list 0 2 #() #())))

(string= (parse-string "\\k<reg>") "k<reg>")

(let ((*allow-named-registers* t))
  (equal (nth-value 1 (create-scanner "((?<small>[a-z]*)(?<big>[A-Z]*))"))
         (list nil "small" "big")))

(let ((*allow-named-registers* t))
  (equal (nth-value 1 (create-scanner '(:register
                                        (:sequence
                                         (:named-register "small"
                                          (:greedy-repetition 0 nil (:char-class (:range #\a #\z))))
                                         (:named-register "big"
                                          (:greedy-repetition 0 nil (:char-class (:range #\a #\z))))))))
         (list nil "small" "big")))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan (create-scanner "((?<small>[a-z]*)(?<big>[A-Z]*))") "aaaBBB"))
          (list 0 6 #(0 0 3) #(6 3 6))))

(let ((*allow-named-registers* t))
  ;; multiple-choice back-reference
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>[ab])(?<reg>[12])\\k<reg>\\k<reg>$") "a1aa"))
          (list 0 4 #(0 1) #(1 2))))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>[ab])(?<reg>[12])\\k<reg>\\k<reg>$") "a22a"))
          (list 0 4 #(0 1) #(1 2))))

(let ((*allow-named-registers* t))
  ;; demonstrating most-recently-seen-register-first property of back-reference;
  ;; "greedy" regex (analogous to "aa?")
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>)(?<reg>a)(\\k<reg>)") "a"))
          (list 0 1 #(0 0 1) #(0 1 1))))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>)(?<reg>a)(\\k<reg>)") "aa"))
          (list 0 2 #(0 0 1) #(0 1 2))))

(let ((*allow-named-registers* t))
  ;; switched groups
  ;; "lazy" regex (analogous to "aa??")
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>a)(?<reg>)(\\k<reg>)") "a"))
          (list 0 1 #(0 1 1) #(1 1 1))))

(let ((*allow-named-registers* t))
  ;; scanner ignores the second "a"
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>a)(?<reg>)(\\k<reg>)") "aa"))
          (list 0 1 #(0 1 1) #(1 1 1))))

(let ((*allow-named-registers* t))
  ;; "aa" will be matched only when forced by adding "$" at the end
  (equalp (multiple-value-list (scan (create-scanner "^(?<reg>a)(?<reg>)(\\k<reg>)$") "aa"))
          (list 0 2 #(0 1 1) #(1 1 2))))

(string= (quote-meta-chars "[a-z]*") "\\[a\\-z\\]\\*")

(string= (handler-case
             (create-scanner "foo**x")
           (ppcre-syntax-error (condition)
             (format nil "Houston, we've got a problem with the string ~S:  Looks like something went wrong at position ~A.  The last message we received was \"~?\"."
                     (ppcre-syntax-error-string condition)
                     (ppcre-syntax-error-pos condition)
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition))))
         "Houston, we've got a problem with the string \"foo**x\":  Looks like something went wrong at position 4.  The last message we received was \"Quantifier '*' not allowed.\".")

(flet ((my-weird-filter (pos)
         "Only match at this point if either pos is odd and the
character we're looking at is lowercase or if pos is even and the next
two characters we're looking at are uppercase. Consume these
characters if there's a match."
         (cond ((and (oddp pos)
                     (< pos cl-ppcre::*end-pos*)
                     (lower-case-p (char cl-ppcre::*string* pos)))
                (1+ pos))
               ((and (evenp pos)
                     (< (1+ pos) cl-ppcre::*end-pos*)
                     (upper-case-p (char cl-ppcre::*string* pos))
                     (upper-case-p (char cl-ppcre::*string* (1+ pos))))
                (+ pos 2))
               (t nil))))
  (let ((weird-regex `(:sequence "+" (:filter ,#'my-weird-filter) "+")))
    (equalp (multiple-value-list (scan weird-regex "+A++a+AA+"))
            (list 5 9 #() #()))))

(let ((a "\\E*"))
  (equalp (multiple-value-list (scan (concatenate 'string "(?:" (quote-meta-chars a) "){2}") "\\E*\\E*"))
          (list 0 6 #() #())))

(let ((a "\\E*"))
  (equalp (multiple-value-list (scan `(:greedy-repetition 2 2 ,a) "\\E*\\E*"))
          (list 0 6 #() #())))

(loop for *optimize-char-classes* in '(:hash-table :hash-table* :charset :charset* :charmap)
      for s = (create-scanner "(([a-c])+)x")
      always (equalp (multiple-value-list (scan s "abcxy"))
                     (list 0 4 #(0 2) #(3 3))))
|#

;;; End

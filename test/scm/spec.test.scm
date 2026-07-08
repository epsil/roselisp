;;; # Test specification
;;;
;;; Tests expressed as a Roselisp REPL session.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; ;; license
 ;; > (describe "license")
 ;; _
 ;; > license
 ;; 'MPL-2.0

 ;; `eq?`
 > (describe "eq?")
 _
 > (eq? #t #t)
 #t
 > (eq? #f #f)
 #t
 > (eq? #t #f)
 #f

 ;; `equal?`
 > (describe "equal?")
 _
 > (equal? #t #t)
 #t
 > (equal? #f #f)
 #t
 > (equal? #t #f)
 #f

 ;; `+`
 > (describe "+")
 _
 > (+)
 0
 > (+ 1)
 1
 > (+ 1 2)
 3
 > (+ 1 2 4)
 7

 ;; `-`
 > (describe "-")
 _
 > (-)
 0
 > (- 1)
 -1
 > (- 1 2)
 -1
 > (- 1 2 4)
 -5

 ;; `*`
 > (describe "*")
 _
 > (*)
 1
 > (* 1)
 1
 > (* 1 2)
 2
 > (* 1 2 4)
 8

 ;; `/`
 > (describe "/")
 _
 ;; > (/)
 ;; #u
 > (/ 1)
 1
 > (/ 1 2)
 0.5
 > (/ 1 2 4)
 0.125)

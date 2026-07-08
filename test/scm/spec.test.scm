;;; # Test specification
;;;
;;; Tests expressed as a Roselisp REPL session.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `license`
 > (describe "license")
 _
 > license
 'MPL-2.0

 ;; `#t`
 > (describe "#t")
 _
 > #t
 #t
 > '#t
 #t

 ;; `#f`
 > (describe "#f")
 _
 > #f
 #f
 > '#f
 #f

 ;; `#u`
 > (describe "#u")
 _
 > #u
 #u
 > '#u
 #u
 > undefined
 #u
 ;; > (undefined 0)
 ;; #u
 ;; > ((undefined 0) 0)
 ;; #u

 ;; `#n`
 > (describe "#n")
 _
 > #n
 #n
 > '#n
 #n
 > js-null
 #n
 > js/null
 #n

 ;; `true?`
 > (describe "true?")
 _
 > (true? #t)
 #t
 > (true? #f)
 #f
 > (true? #u)
 #f
 > (true? #n)
 #f
 > (true? '())
 #t

 ;; `false?`
 > (describe "false?")
 _
 > (false? #t)
 #f
 > (false? #f)
 #t
 > (false? #u)
 #t
 > (false? #n)
 #t
 > (false? '())
 #f

 ;; `nil`
 > (describe "nil")
 _
 > nil
 '()
 > (list? nil)
 #t
 > (length nil)
 0

 ;; `null`
 > (describe "null")
 _
 > null
 '()
 > (listp null)
 #t
 > (length null)
 0

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
 > (/)
 #u
 > (/ 1)
 1
 > (/ 1 2)
 0.5
 > (/ 1 2 4)
 0.125)

;;; # Test specification
;;;
;;; Tests expressed as a Roselisp REPL session.

(require (only-in "./test-util"
                  test-repl))

(define (print-sexp exp)
  (cond
   ((undefined? exp)
    "#u")
   ((js/null? exp)
    "#n")
   ((boolean? exp)
    (if exp "#t" "#f"))
   ((cons? exp)
    (string-append
     "("
     (string-join
      (map print-sexp exp)
      " ")
     ")"))
   ((string? exp)
    (string-append
     "\""
     (~> exp
         (regexp-replace (regexp "\\\\" "g") _ "\\\\")
         (regexp-replace (regexp "\"" "g") _ "\\\""))
     "\""))
   ((symbol? exp)
    (symbol->string exp))
   (else
    (string-append exp ""))))

(defmacro test-macro (&rest body)
  (define group '())
  (define groups '())
  (for ((i (range 0 (array-list-length body) 3)))
    (define prompt
      (aget body i))
    (define expression
      (aget body (+ i 1)))
    (define value
      (aget body (+ i 2)))
    (cond
     ((and (array-list? expression)
           (= (array-list-length expression)
              2)
           (eq? (array-list-first expression)
                'describe))
      (when (> (array-list-length group) 0)
        (push-right groups group)
        (set! group '()))
      (define description
        (array-list-second expression))
      (push-right! group description))
     (else
      (define it-description
        (print-sexp expression))
      (define it-expression
        `(it ,it-description
             (fn ()
               (test-repl
                '(roselisp
                  ,prompt
                  ,expression
                  ,value)))))
      (push-right! group it-expression))))
  (when (> (array-list-length group) 0)
    (push-right groups group))
  (define tests
    (map (lambda (group)
           `(describe ,(first group)
              (fn () ,@(rest group))))
         groups))
  `(begin ,@tests))

(test-macro
 ;; `eq?`
 > (describe "eq?")
 #u
 > (eq? #t #t)
 #t
 > (eq? #f #f)
 #t
 > (eq? #t #f)
 #f

 ;; `equal?`
 > (describe "equal?")
 #u
 > (equal? #t #t)
 #t
 > (equal? #f #f)
 #t
 > (equal? #t #f)
 #f)

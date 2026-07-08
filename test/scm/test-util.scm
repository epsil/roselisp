;;; # Test utilities
;;;
;;; Various helper functions for testing Lisp code.

(require chai "chai")
(require (only-in "../../src/ts/language"
                  LispEnvironment
                  compile
                  compile-lisp
                  extend-environment
                  interpret
                  print-sexp
                  write-to-string))

(define assert-equal
  (~> chai
      (get-field assert _)
      (get-field deepEqual _)))

(define assert-not-equal
  (~> chai
      (get-field assert _)
      (get-field notDeepEqual _)))

(define assert-throws
  (~> chai
      (get-field assert _)
      (get-field throws _)))

;;; Test helper function that verifies that interpretation and
;;; compilation amounts to the same:
;;;
;;;     evalLisp(x) = evalJavaScript(compile-lisp(x))
;;;
;;; Using `.` to denote right-to-left function composition, this can
;;; also be expressed as:
;;;
;;;     evalLisp = evalJavaScript . compile-lisp
;;;
;;; Alternatively, using `;` to denote left-to-right function
;;; composition (as is done in some texts):
;;;
;;;     evalLisp = compile-lisp ; evalJavaScript
;;;
;;; All of which is to say that the following diagram [commutes][1]:
;;;
;;;            compile-lisp
;;;     SExp --------------> string
;;;       \                     |
;;;         \                   |
;;;           \                 |
;;;             \               |
;;;               \             |
;;;       evalLisp  \           | evalJavaScript
;;;                   \         |
;;;                     \       |
;;;                       \     |
;;;                         \   |
;;;                           \ |
;;;                            VV
;;;                            any
;;;
;;; Here, `SExp` is an S-expression (atomic or non-atomic), `string` is
;;; a JavaScript code string, and `any` is any kind of value. Note that
;;; `evalJavaScript` is simply called [`eval`][2] in JavaScript, just
;;; like `evalLisp` is called [`eval`][3] in Lisp; they are here named
;;; `evalJavaScript` and `evalLisp` to avoid confusion.
;;;
;;; A prettier and more general version of this diagram can be found in
;;; the introductory chapter of the book [*Essentials of Compilation:
;;; An Incremental Approach in Racket*][4] by Jeremy G. Siek.
;;;
;;; [1]: http://en.wikipedia.org/wiki/Commutative_diagram
;;; [2]: http://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
;;; [3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm
;;; [4]: https://github.com/IUCompilerCourse/Essentials-of-Compilation
(define (test-lisp (exp #u)
                   (val #u)
                   (options (js-obj)))
  ;; FIXME: `exp` *might* be modified by side-effect. If so, the
  ;; compilation test will receive a different value. We should
  ;; clone the value to avoid this.
  (define-fields (compile
                  env
                  (interpret interpret-flag)
                  (interpretValue interpret-value-option)
                  verbose
                  wrap-parens)
    options)
  (set! env
        (or env (new LispEnvironment)))
  (define expected-value
    (if interpret-value-option
        (interpret val env)
        val))
  (define evaluation-options
    (js-obj-append options))
  (js/delete (get-field compile evaluation-options))
  (define compilation-options
    evaluation-options)
  (set! interpret-flag
        (if (eq? interpret-flag #u)
            #t
            compile))
  (set! compile
        (if (eq? compile #u)
            #t
            compile))
  (define interpreted-value
    #u)
  (define compiled-value
    #u)
  (when verbose
    (display "expression:" (write-to-string exp)))
  (when interpret-flag
    (define interpretation-env
      (extend-environment (new LispEnvironment)
                          env))
    (set! interpreted-value
          (interpret exp interpretation-env))
    (when verbose
      (display "interpreted value:" interpreted-value))
    (if (or (eq? interpreted-value #n)
            (eq? interpreted-value #u))
        (assert-equal
         (eq? interpreted-value expected-value)
         #t)
        (assert-equal
         interpreted-value
         expected-value)))
  (when compile
    (define compilation-env
      (extend-environment (new LispEnvironment)
                          env))
    (define compiled-js
      (compile-lisp exp compilation-env compilation-options))
    (when wrap-parens
      (set! compiled-js (string-append "(" compiled-js ")")))
    (when verbose
      (display "compiled expression:" compiled-js))
    (set! compiled-value
          (js/eval compiled-js))
    (when verbose
      (display "compiled value:" compiled-value))
    (if (or (eq? compiled-value #n)
            (eq? compiled-value #u))
        (assert-equal
         (eq? compiled-value expected-value)
         #t)
        (assert-equal
         compiled-value
         expected-value)))
  (if interpret-flag
      interpreted-value
      compiled-value))

;;; Test a REPL form.
(define (test-repl exp (options (js-obj)))
  (define-fields (env)
    options)
  (set! env (or env (new LispEnvironment)))
  (case (get-repl-form-type exp)
    ((javascript js node)
     (test-node-repl exp))
    (else
     (test-roselisp-repl exp options))))

;;; Test a Node REPL form.
(define (test-node-repl exp)
  (define clauses
    (parse-repl-form exp))
  (for ((clause clauses))
    (define actual
      (js/eval (first clause)))
    (unless (eq? (second clause) "_")
      (define expected
        (js/eval (second clause)))
      (assert-equal actual expected))))

;;; Test a Roselisp REPL form.
(define (test-roselisp-repl exp (options (js-obj)))
  (define-fields ((compile compile-option)
                  (verbose verbose-option)
                  env)
    (js-obj-append
     (js-obj "compile" #t)
     options))
  (when verbose-option
    (display "Roselisp REPL form: " exp))
  (define test-env
    (extend-environment (new LispEnvironment)
                        (or env
                            (new LispEnvironment))))
  (define clauses
    (parse-repl-form exp))
  (for ((clause clauses))
    (define actual
      (interpret (first clause) test-env))
    (unless (eq? (second clause) '_)
      (define expected
        (interpret (second clause) test-env))
      (assert-equal actual expected)))
  (when compile-option
    (define node-repl-form
      (compile-repl-form
       (simplify-repl-form exp)
       (js-obj "from" "roselisp"
               "to" "node")))
    (when verbose-option
      (display "Node REPL form: " node-repl-form))
    (test-repl node-repl-form test-env options)))

;;; Parse a REPL form, i.e., an expression on the form:
;;;
;;;     (repl
;;;      > (+ 1 1)
;;;      2
;;;      > (+ 2 2)
;;;      4)
;;;
;;; Returns a list of `(input output)` tuples.
(define (parse-repl-form exp)
  (define form
    (cond
     ((= (length exp) 0)
      exp)
     ((eq? (first exp) '>)
      exp)
     ((eq? (first exp) '$)
      (drop exp 2))
     (else
      (rest exp))))
  (define result '())
  (for ((i (range 0 (length form) 3)))
    (push-right! result
                 (list (nth (+ i 1) form)
                       (nth (+ i 2) form))))
  result)

(define (get-repl-form-type exp)
  (cond
   ((= (length exp) 0)
    'roselisp)
   ((eq? (first exp) '>)
    'roselisp)
   ((and (>= (length exp) 2)
         (eq? (first exp) '$))
    (second exp))
   (else
    (first exp))))

;;; Simplify a REPL form of multiple clauses
;;; to a single-clause form.
(define (simplify-repl-form exp)
  (define clauses
    (parse-repl-form exp))
  (cond
   ((<= (length clauses) 1)
    exp)
   (else
    `(,(get-repl-form-type exp)
      > (begin ,@(map first clauses))
      ,(second (last clauses))))))

;;; Compile a REPL form from one language
;;; to another.
(define (compile-repl-form exp (options (js-obj)))
  (define from-option
    (oget options "from"))
  (define to-option
    (oget options "to"))
  (cond
   ((eq? from-option "roselisp")
    (cond
     ((eq? to-option "node")
      (define clauses
        (parse-repl-form exp))
      (define result
        '(node))
      (for ((clause clauses))
        (push-right! result '>)
        (push-right! result
                     (compile (first clause)))
        (push-right! result
                     (if (eq? (second clause) '_)
                         "_"
                         (compile (second clause)))))
      result)
     (else
      exp)))
   (else
    exp)))

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

(provide
  (rename-out (test-repl test-repl-form))
  (rename-out (test-repl test-shell-form))
  assert-equal
  assert-not-equal
  assert-throws
  compile-repl-form
  parse-repl-form
  print-sexp
  simplify-repl-form
  test-lisp
  test-macro
  test-repl)

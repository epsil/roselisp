(require chai "chai")
(require (only-in "../../src/ts/parser"
                  LeadingCommentToken
                  NumberToken
                  StringToken
                  SymbolToken
                  TrailingCommentToken
                  parse-rose
                  read
                  read-rose
                  tokenize))
(require (only-in "../../src/ts/sexp"
                  s
                  sexp))
(require (only-in "./test-util"
                  assert-equal))

(describe "tokenize"
  (fn ()
    (it "''"
        (fn ()
          (assert-equal
           (tokenize "")
           '())))
    (it "1"
        (fn ()
          (assert-equal
           (tokenize "1")
           (list (new NumberToken 1)))))
    (it "foo"
        (fn ()
          (assert-equal
           (tokenize "foo")
           (list (new SymbolToken "foo")))))
    (it "\"foo\""
        (fn ()
          (assert-equal
           (tokenize "\"foo\"")
           (list (new StringToken "foo")))))
    (it "\"foo\\\"bar\""
        (fn ()
          (assert-equal
           (tokenize "\"foo\\\"bar\"")
           (list (new StringToken "foo\"bar")))))
    (it "'foo"
        (fn ()
          (assert-equal
           (tokenize "'foo")
           (list (new SymbolToken "'")
                 (new SymbolToken "foo")))))
    (it "()"
        (fn ()
          (assert-equal
           (tokenize "()")
           (list (new SymbolToken "(")
                 (new SymbolToken ")")))))
    (it "'(foo)"
        (fn ()
          (assert-equal
           (tokenize "'(foo)")
           (list (new SymbolToken "'")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken ")")))))
    (it "(foo \"bar\")"
        (fn ()
          (assert-equal
           (tokenize "(foo \"bar\")")
           (list (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new StringToken "bar")
                 (new SymbolToken ")")))))
    (it "(foo
\"bar\")"
        (fn ()
          (assert-equal
           (tokenize "(foo
\"bar\")")
           (list (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new StringToken "bar")
                 (new SymbolToken ")")))))
    (it "(foo) ; bar"
        (fn ()
          (assert-equal
           (tokenize "(foo) ; bar"
                     (js-obj "comments" #f))
           (list (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken ")")))))
    (it "'(foo) ; bar"
        (fn ()
          (assert-equal
           (tokenize "'(foo) ; bar"
                     (js-obj "comments" #f))
           (list (new SymbolToken "'")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken ")")))))
    (xit "(foo ; baz
bar)"
         (fn ()
           (assert-equal
            (tokenize "(foo ; baz
bar)")
            (list (new SymbolToken "(")
                  (new SymbolToken "foo")
                  (new SymbolToken "bar")
                  (new SymbolToken ")")))))
    (xit "(foo ; baz
bar)"
         (fn ()
           (assert-equal
            (tokenize "(foo ; baz
bar)"
                      (js-obj "comments" #t))
            (list (new SymbolToken "(")
                  (new SymbolToken "foo")
                  (new SymbolToken "bar")
                  (new SymbolToken ")")
                  (new TrailingCommentToken
                       "; baz")))))
    (it ";; baz
(foo bar)"
        (fn ()
          (assert-equal
           (tokenize ";; baz
(foo bar)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; baz\n")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")))))
    (it "  ;; baz
  (foo bar)"
        (fn ()
          (assert-equal
           (tokenize "  ;; baz
  (foo bar)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; baz\n")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")))))
    (it ";; baz
;; quux
(foo bar)"
        (fn ()
          (assert-equal
           (tokenize ";; baz
;; quux
(foo bar)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; baz
;; quux
")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")))))
    (it ";; baz
;;
;; quux
(foo bar)"
        (fn ()
          (assert-equal
           (tokenize ";; baz
;;
;; quux
(foo bar)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; baz
;;
;; quux
")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")))))
    (it ";; baz

;; quux
(foo bar)"
        (fn ()
          (assert-equal
           (tokenize ";; baz

;; quux
(foo bar)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; baz

;; quux
")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")))))
    (it ";; foo
`(foo)"
        (fn ()
          (assert-equal
           (tokenize ";; foo
`(foo)"
                     (js-obj "comments" #t))
           (list (new LeadingCommentToken
                      ";; foo\n")
                 (new SymbolToken "`")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken ")")))))
    (it "(foo '(bar))"
        (fn ()
          (assert-equal
           (tokenize "(foo '(bar))")
           (list (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken "'")
                 (new SymbolToken "(")
                 (new SymbolToken "bar")
                 (new SymbolToken ")")
                 (new SymbolToken ")")))))
    (it "((lambda (x) x) \"Lisp\")"
        (fn ()
          (assert-equal
           (tokenize "((lambda (x) x) \"Lisp\")")
           (list (new SymbolToken "(")
                 (new SymbolToken "(")
                 (new SymbolToken "lambda")
                 (new SymbolToken "(")
                 (new SymbolToken "x")
                 (new SymbolToken ")")
                 (new SymbolToken "x")
                 (new SymbolToken ")")
                 (new StringToken "Lisp")
                 (new SymbolToken ")")))))
    (it "(define (foo)
  ;; this
  this)"
        (fn ()
          (assert-equal
           (tokenize "(define (foo)
  ;; this
  this)"
                     (js-obj "comments" #t))
           (list (new SymbolToken "(")
                 (new SymbolToken "define")
                 (new SymbolToken "(")
                 (new SymbolToken "foo")
                 (new SymbolToken ")")
                 (new LeadingCommentToken ";; this\n")
                 (new SymbolToken "this")
                 (new SymbolToken ")")))))))

(describe "parse-rose"
  (fn ()
    (it "[s`exp`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "exp")))
               (send getValue))
           'exp)))
    (it "[s`(`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken ")")))
               (send getValue))
           '())))
    (it "[s`(`, s`(`, s`)`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "(")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list '()))))
    (it "[s`(`, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'foo))))
    (it "[s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "(")
                                 (new SymbolToken "lambda")
                                 (new SymbolToken "(")
                                 (new SymbolToken "x")
                                 (new SymbolToken ")")
                                 (new SymbolToken "x")
                                 (new SymbolToken ")")
                                 (new StringToken "Lisp")
                                 (new SymbolToken ")")))
               (send getValue))
           (list (list 'lambda
                       (list 'x)
                       'x)
                 "Lisp"))))
    (it "[s`'`, s`foo`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "'")
                                 (new SymbolToken "foo")))
               (send getValue))
           (list 'quote 'foo))))
    (it "[s`'`, s`(`, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "'")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'quote (list 'foo)))))
    (it "[s`'`, s`(`, s`(`, s`foo`, s`)`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "'")
                                 (new SymbolToken "(")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'quote (list (list 'foo))))))
    (it "[s`'`, s`(`, s`(`, s`foo`, s`)`, s`(`, s`bar`, s`)`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "'")
                                 (new SymbolToken "(")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")
                                 (new SymbolToken "(")
                                 (new SymbolToken "bar")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'quote
                 (list (list 'foo)
                       (list 'bar))))))
    (it "[s`(`, s`quote`, s`(`, s`(`, s`foo`, s`)`, s`(`, s`bar`, s`)`, s`)`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "quote")
                                 (new SymbolToken "(")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")
                                 (new SymbolToken "(")
                                 (new SymbolToken "bar")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'quote
                 (list (list 'foo)
                       (list 'bar))))))
    (it "[s`(`, s`truep`, s`'`, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "truep")
                                 (new SymbolToken "'")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'truep
                 (list 'quote 'foo)))))
    (it "[s`(`, s`truep`, s`'`, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "truep")
                                 (new SymbolToken "'")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'truep
                 (list 'quote
                       (list 'foo))))))
    (it "[s`(`, s`truep`, s`\\``, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "truep")
                                 (new SymbolToken "`")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'truep
                 (list 'quasiquote
                       'foo)))))
    (it "[s`(`, s`truep`, s`\\``, s`foo`, s`)`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "(")
                                 (new SymbolToken "truep")
                                 (new SymbolToken "`")
                                 (new SymbolToken "(")
                                 (new SymbolToken "foo")
                                 (new SymbolToken ")")
                                 (new SymbolToken ")")))
               (send getValue))
           (list 'truep
                 (list 'quasiquote
                       (list 'foo))))))
    (it "[s`\\``, s`foo`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken "`")
                                 (new SymbolToken "foo")))
               (send getValue))
           (list 'quasiquote 'foo))))
    (it "[s`,`, s`foo`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken ",")
                                 (new SymbolToken "foo")))
               (send getValue))
           (list 'unquote 'foo))))
    (it "[s`,@`, s`foo`]"
        (fn ()
          (assert-equal
           (~> (parse-rose (list (new SymbolToken ",@")
                                 (new SymbolToken "foo")))
               (send getValue))
           (list 'unquote-splicing 'foo))))))

(describe "read"
  (fn ()
    (it "(foo) ;comment"
        (fn ()
          (assert-equal
           (read "(foo) ;comment")
           (list 'foo))))
    (it "(foo) ;; this is a comment"
        (fn ()
          (assert-equal
           (read "(foo) ;; this is a comment")
           (list 'foo))))
    (it "(define (foo) (bar) (baz))"
        (fn ()
          (assert-equal
           (read "(define (foo)
  ;; this is a comment
  (bar) ; this is also a comment
  (baz))")
           (list 'define
                 (list 'foo)
                 (list 'bar)
                 (list 'baz)))))
    (it "string ;-D"
        (fn ()
          (assert-equal
           (read "\"string ;-D\"")
           "string ;-D")))
    (it "\"string\\\"test\""
        (fn ()
          (assert-equal
           (read "\"string\\\"test\"")
           "string\"test")))
    (it "\"string\\ntest\""
        (fn ()
          (assert-equal
           (read "\"string\\ntest\"")
           "string
test")))
    (it "\"string\\\\ntest\""
        (fn ()
          (assert-equal
           (read "\"string\\\\ntest\"")
           "string\\ntest")))
    (it "\"string\\ntest\""
        (fn ()
          (assert-equal
           (read "\"string
test\"")
           "string
test")))))

(describe "read-rose"
  (fn ()
    (it ";; comment
(foo)"
        (fn ()
          (assert-equal
           (~> (read-rose ";; comment
(foo)")
               (send getValue))
           (list 'foo))))
    (it ";; comment
(foo), comments"
        (fn ()
          (define actual
            (read-rose ";; comment
(foo)"
                       (js-obj "comments" #t)))
          (assert-equal
           (send actual getValue)
           (list 'foo))
          (assert-equal
           (send actual getProperty "comments")
           (list (new LeadingCommentToken
                      ";; comment\n")))))
    (it ";; comment
`(foo), comments"
        (fn ()
          (define actual
            (read-rose ";; comment
`(foo)"
                       (js-obj "comments" #t)))
          (assert-equal
           (send actual getValue)
           (list 'quasiquote
                 (list 'foo)))
          (assert-equal
           (send actual getProperty "comments")
           (list (new LeadingCommentToken
                      ";; comment\n")))))
    (it "(foo) ;comment"
        (fn ()
          (assert-equal
           (~> (read-rose "(foo) ;comment")
               (send getValue))
           (list 'foo))))))

(describe "sexp"
  (fn ()
    (it "sexp('')"
        (fn ()
          (assert-equal
           (js/tag sexp "")
           '())
          (assert-equal
           (sexp "")
           '())))
    (it "sexp('()')"
        (fn ()
          (assert-equal
           (js/tag sexp "()")
           '())
          (assert-equal
           (sexp "()")
           '())))
    (it "sexp('()')"
        (fn ()
          (assert-equal
           (js/tag sexp "'()")
           (list 'quote '()))
          (assert-equal
           (sexp "'()")
           (list 'quote '()))))
    (it "sexp(`(truep '())`)"
        (fn ()
          (assert-equal
           (js/tag sexp "(truep '())")
           (list 'truep
                 (list 'quote '())))
          (assert-equal
           (sexp "(truep '())")
           (list 'truep
                 (list 'quote '())))))
    (it "sexp('1')"
        (fn ()
          (assert-equal
           (js/tag sexp "1")
           1)
          (assert-equal
           (sexp "1")
           1)))
    (it "sexp('\"foo\"')"
        (fn ()
          (assert-equal
           (js/tag sexp "\"foo\"")
           "foo")
          (assert-equal
           (sexp "\"foo\"")
           "foo")))
    (it "sexp('\"foo;-D\"')"
        (fn ()
          (assert-equal
           (js/tag sexp "\"foo;-D\"")
           "foo;-D")
          (assert-equal
           (sexp "\"foo;-D\"")
           "foo;-D")))
    (it "sexp('a')"
        (fn ()
          (assert-equal
           (js/tag sexp "a")
           'a)
          (assert-equal
           (sexp "a")
           'a)))
    (it "sexp('(or 1 2)')"
        (fn ()
          (assert-equal
           (js/tag sexp "(or 1 2)")
           (list 'or 1 2))
          (assert-equal
           (sexp "(or 1 2)")
           (list 'or 1 2))))
    (xit "sexp('(or true false)')"
         (fn ()
           (assert-equal
            (js/tag sexp "(or true false)")
            (list 'or #t #f))
           (assert-equal
            (sexp "(or true false)")
            (list 'or #t #f))))
    (it "sexp`foo`"
        (fn ()
          (assert-equal
           (js/tag sexp "foo")
           'foo)
          (assert-equal
           (sexp "foo")
           'foo)))
    (it "sexp('(foo)')"
        (fn ()
          (assert-equal
           (js/tag sexp "(foo)")
           (list 'foo))
          (assert-equal
           (sexp "(foo)")
           (list 'foo))))
    (it "sexp('\\n(foo)\\n')"
        (fn ()
          (assert-equal
           (js/tag sexp "
      (foo)
  ")
           (list 'foo))
          (assert-equal
           (sexp "
      (foo)
  ")
           (list 'foo))))
    (it "sexp('\\n(foo\\n(bar))\\n')"
        (fn ()
          (assert-equal
           (js/tag sexp "
      (foo
        (bar))
  ")
           (list 'foo
                 (list 'bar)))
          (assert-equal
           (sexp "
      (foo
        (bar))
  ")
           (list 'foo
                 (list 'bar)))))
    (it "sexp('(foo \"bar\")')"
        (fn ()
          (assert-equal
           (js/tag sexp "(foo \"bar\")")
           (list 'foo "bar"))
          (assert-equal
           (sexp "(foo \"bar\")")
           (list 'foo "bar"))))
    (it "sexp('(+ 1 1')"
        (fn ()
          (assert-equal
           (js/tag sexp "(+ 1 1)")
           (list '+ 1 1))
          (assert-equal
           (sexp "(+ 1 1)")
           (list '+ 1 1))))
    (it "sexp(`'foo`)"
        (fn ()
          (assert-equal
           (js/tag sexp "'foo")
           (list 'quote 'foo))
          (assert-equal
           (sexp "'foo")
           (list 'quote 'foo))))
    (it "sexp('`foo')"
        (fn ()
          (assert-equal
           (js/tag sexp "`foo")
           (list 'quasiquote 'foo))
          (assert-equal
           (sexp "`foo")
           (list 'quasiquote 'foo))))
    (it "sexp`(1 . 2)`"
        (fn ()
          (assert-equal
           (dotted-list? (js/tag sexp "(1 . 2)"))
           #t)))
    (it "sexp`(1 '. 2)`"
        (fn ()
          (assert-equal
           (dotted-list? (js/tag sexp "(1 '. 2)"))
           #f)))
    (it "sexp`(+ 2 2)`"
        (fn ()
          (assert-equal
           (js/tag sexp "(+ 2 2)")
           (list '+ 2 2))
          (assert-equal
           (sexp "(+ 2 2)")
           (list '+ 2 2))))
    (xit "sexp`(+ ${2} ${2})`"
         (fn ()
           ;; (assert-equal
           ;;  (js/tag sexp "(+ ")
           ;;  (list (js/tag s "+") 2 2))
           ;; (assert-equal
           ;;  (sexp "(+ ")
           ;;  (list (js/tag s "+") 2 2))
           ))))

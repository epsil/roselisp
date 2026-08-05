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
(require (only-in "../../src/ts/rose"
                  rose->sexp))
(require (only-in "../../src/ts/sexp"
                  s
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare test-macro)

(test-macro
 ;; `tokenize`
 > (describe "tokenize")
 _
 > (tokenize "")
 '()
 > (tokenize "1")
 (list (new NumberToken 1))
 > (tokenize "foo")
 (list (new SymbolToken "foo"))
 > (tokenize "\\foo")
 (list (new SymbolToken "foo"))
 > (tokenize "f\\oo")
 (list (new SymbolToken "foo"))
 xit> (tokenize "|foo|")
 (list (new SymbolToken "foo"))
 > (tokenize "\"foo\"")
 (list (new StringToken "foo"))
 > (tokenize "\"foo\\\"bar\"")
 (list (new StringToken "foo\"bar"))
 > (tokenize "'foo")
 (list (new SymbolToken "'")
       (new SymbolToken "foo"))
 > (tokenize "()")
 (list (new SymbolToken "(")
       (new SymbolToken ")"))
 > (tokenize "'(foo)")
 (list (new SymbolToken "'")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken ")"))
 > (tokenize "(foo \"bar\")")
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new StringToken "bar")
       (new SymbolToken ")"))
 > (tokenize "(foo
\"bar\")")
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new StringToken "bar")
       (new SymbolToken ")"))
 > (tokenize "(foo) ; bar"
             (js/obj :comments #f))
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken ")"))
 xit> (tokenize "'(foo) ; bar"
                (js/obj :comments #f))
 (list (new SymbolToken "'")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken ")"))
 xit> (tokenize "(foo ; baz
bar)")
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 xit> (tokenize "(foo ; baz
bar)"
                (js/obj :comments #t))
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")")
       (new TrailingCommentToken
            "; baz"))
 > (tokenize ";; baz
(foo bar)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; baz\n")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 > (tokenize "  ;; baz
  (foo bar)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; baz\n")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 > (tokenize ";; baz
;; quux
(foo bar)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; baz
;; quux
")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 > (tokenize ";; baz
;;
;; quux
(foo bar)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; baz
;;
;; quux
")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 > (tokenize ";; baz

;; quux
(foo bar)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; baz

;; quux
")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "bar")
       (new SymbolToken ")"))
 > (tokenize ";; foo
`(foo)"
             (js/obj :comments #t))
 (list (new LeadingCommentToken
            ";; foo\n")
       (new SymbolToken "`")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken ")"))
 > (tokenize "(foo '(bar))")
 (list (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken "'")
       (new SymbolToken "(")
       (new SymbolToken "bar")
       (new SymbolToken ")")
       (new SymbolToken ")"))
 > (tokenize "((lambda (x) x) \"Lisp\")")
 (list (new SymbolToken "(")
       (new SymbolToken "(")
       (new SymbolToken "lambda")
       (new SymbolToken "(")
       (new SymbolToken "x")
       (new SymbolToken ")")
       (new SymbolToken "x")
       (new SymbolToken ")")
       (new StringToken "Lisp")
       (new SymbolToken ")"))
 > (tokenize "(define (foo)
  ;; this
  this)"
             (js/obj :comments #t))
 (list (new SymbolToken "(")
       (new SymbolToken "define")
       (new SymbolToken "(")
       (new SymbolToken "foo")
       (new SymbolToken ")")
       (new LeadingCommentToken ";; this\n")
       (new SymbolToken "this")
       (new SymbolToken ")"))

 ;; `parse-rose`
 > (describe "parse-rose")
 _
 > (rose->sexp
    (parse-rose (list (new SymbolToken "exp"))))
 'exp
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken ")"))))
 '()
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "(")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(())
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")"))))
 '(foo)
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "(")
                      (new SymbolToken "lambda")
                      (new SymbolToken "(")
                      (new SymbolToken "x")
                      (new SymbolToken ")")
                      (new SymbolToken "x")
                      (new SymbolToken ")")
                      (new StringToken "Lisp")
                      (new SymbolToken ")"))))
 '((lambda (x) x) "Lisp")
 > (rose->sexp
    (parse-rose (list (new SymbolToken "'")
                      (new SymbolToken "foo"))))
 '(quote foo)
 > (rose->sexp
    (parse-rose (list (new SymbolToken "'")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")"))))
 '(quote (foo))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "'")
                      (new SymbolToken "(")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(quote ((foo)))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "'")
                      (new SymbolToken "(")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")")
                      (new SymbolToken "(")
                      (new SymbolToken "bar")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(quote ((foo) (bar)))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "quote")
                      (new SymbolToken "(")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")")
                      (new SymbolToken "(")
                      (new SymbolToken "bar")
                      (new SymbolToken ")")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(quote ((foo) (bar)))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "truep")
                      (new SymbolToken "'")
                      (new SymbolToken "foo")
                      (new SymbolToken ")"))))
 '(truep (quote foo))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "truep")
                      (new SymbolToken "'")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(truep (quote (foo)))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "truep")
                      (new SymbolToken "`")
                      (new SymbolToken "foo")
                      (new SymbolToken ")"))))
 '(truep (quasiquote foo))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "(")
                      (new SymbolToken "truep")
                      (new SymbolToken "`")
                      (new SymbolToken "(")
                      (new SymbolToken "foo")
                      (new SymbolToken ")")
                      (new SymbolToken ")"))))
 '(truep (quasiquote (foo)))
 > (rose->sexp
    (parse-rose (list (new SymbolToken "`")
                      (new SymbolToken "foo"))))
 '(quasiquote foo)
 > (rose->sexp
    (parse-rose (list (new SymbolToken ",")
                      (new SymbolToken "foo"))))
 '(unquote foo)
 > (rose->sexp
    (parse-rose (list (new SymbolToken ",@")
                      (new SymbolToken "foo"))))
 '(unquote-splicing foo)

 ;; `read`
 > (describe "read")
 _
 > (read "(foo) ;comment")
 (list 'foo)
 > (read "(foo) ;; this is a comment")
 (list 'foo)
 > (read "(define (foo)
  ;; this is a comment
  (bar) ; this is also a comment
  (baz))")
 (list 'define
       (list 'foo)
       (list 'bar)
       (list 'baz))
 > (read "\"string ;-D\"")
 "string ;-D"
 > (read "\"string\\\"test\"")
 "string\"test"
 > (read "\"string\\ntest\"")
 "string
test"
 > (read "\"string\\\\ntest\"")
 "string\\ntest"
 > (read "\"string
test\"")
 "string
test"
 > (read "'()")
 '(quote ())
 > (read "`()")
 '(quasiquote ())
 > (read "`(,exp)")
 '(quasiquote ((unquote exp)))
 > (read "`((quote ,exp))")
 '(quasiquote ((quote (unquote exp))))
 > (read "`(',exp)")
 '(quasiquote ((quote (unquote exp))))
 > (read "`('',exp)")
 '(quasiquote ((quote (quote (unquote exp)))))
 > (read "`(''',exp)")
 '(quasiquote ((quote (quote (quote (unquote exp))))))
 > (read "(define foo `(,bar))")
 '(define foo (quasiquote ((unquote bar))))

 ;; `read-rose`
 > (describe "read-rose")
 _
 > (rose->sexp
    (read-rose ";; comment
(foo)"))
 (list 'foo)
 > (it ";; comment
(foo), comments"
       (define actual
         (read-rose ";; comment
(foo)"
                    (js/obj :comments #t)))
       (assert-equal
        (send actual get-value)
        (list 'foo))
       (assert-equal
        (send actual getProperty "comments")
        (list (new LeadingCommentToken
                   ";; comment\n"))))
 _
 > (it ";; comment
`(foo), comments"
       (define actual
         (read-rose ";; comment
`(foo)"
                    (js/obj :comments #t)))
       (assert-equal
        (send actual get-value)
        (list 'quasiquote
              (list 'foo)))
       (assert-equal
        (send actual getProperty "comments")
        (list (new LeadingCommentToken
                   ";; comment\n"))))
 _
 > (rose->sexp
    (read-rose "(foo) ;comment"))
 (list 'foo)

 ;; `sexp`
 > (describe "sexp")
 _
 > (js/tag sexp "")
 '()
 > (js/tag sexp "()")
 '()
 > (sexp "()")
 '()
 > (js/tag sexp "'()")
 (list 'quote '())
 > (sexp "'()")
 (list 'quote '())
 > (js/tag sexp "(truep '())")
 (list 'truep
       (list 'quote '()))
 > (sexp "(truep '())")
 (list 'truep
       (list 'quote '()))
 > (js/tag sexp "1")
 1
 > (sexp "1")
 1
 > (js/tag sexp "\"foo\"")
 "foo"
 > (sexp "\"foo\"")
 "foo"
 > (js/tag sexp "\"foo;-D\"")
 "foo;-D"
 > (sexp "\"foo;-D\"")
 "foo;-D"
 > (js/tag sexp "a")
 'a
 > (sexp "a")
 'a
 > (js/tag sexp "(or 1 2)")
 (list 'or 1 2)
 > (sexp "(or 1 2)")
 (list 'or 1 2)
 xit> (js/tag sexp "(or true false)")
 (list 'or #t #f)
 xit> (sexp "(or true false)")
 (list 'or #t #f)
 > (js/tag sexp "foo")
 'foo
 > (sexp "foo")
 'foo
 > (js/tag sexp "(foo)")
 (list 'foo)
 > (sexp "(foo)")
 (list 'foo)
 > (js/tag sexp "
      (foo)
  ")
 (list 'foo)
 > (sexp "
      (foo)
  ")
 (list 'foo)
 > (js/tag sexp "
      (foo
        (bar))
  ")
 (list 'foo
       (list 'bar))
 > (sexp "
      (foo
        (bar))
  ")
 (list 'foo
       (list 'bar))
 > (js/tag sexp "(foo \"bar\")")
 (list 'foo "bar")
 > (sexp "(foo \"bar\")")
 (list 'foo "bar")
 > (js/tag sexp "(+ 1 1)")
 (list '+ 1 1)
 > (sexp "(+ 1 1)")
 (list '+ 1 1)
 > (js/tag sexp "'foo")
 (list 'quote 'foo)
 > (sexp "'foo")
 (list 'quote 'foo)
 > (js/tag sexp "`foo")
 (list 'quasiquote 'foo)
 > (sexp "`foo")
 (list 'quasiquote 'foo)
 > (dotted-list? (js/tag sexp "(1 . 2)"))
 #t
 > (dotted-list? (js/tag sexp "(1 '. 2)"))
 #f
 > (js/tag sexp "(+ 2 2)")
 (list '+ 2 2)
 > (sexp "(+ 2 2)")
 (list '+ 2 2)
 xit> (js/tag sexp "(+ ")
 (list (js/tag s "+") 2 2)
 xit> (sexp "(+ ")
 (list (js/tag s "+") 2 2))

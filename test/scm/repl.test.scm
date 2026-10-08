;;; # REPL
;;;
;;; Various tests of the interactive interface.

(require (only-in "../../src/ts/repl"
                  history->string
                  rep
                  string->history))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "REPL"
 > (rep "#t")
 "#t"
 > (rep "\"string\"")
 "\"string\""
 > (rep "(+ 1 1)")
 "2"
 > (rep "(+ 1 1) (+ 1 1)")
 "2
2"

 :describe "string->history"
 > (string->history "")
 '()
 > (string->history "foo")
 '("foo")
 > (string->history "foo
bar")
 '("bar"
   "foo")
 > (string->history "foo

bar")
 '("bar"
   "foo")
 > (string->history "foo
bar
baz")
 '("baz"
   "bar"
   "foo")
 > (string->history "\"foo
bar\"")
 '("\"foo
bar\"")
 > (string->history "(foo))
(bar)")
 '("(bar)"
   "(foo))")
 > (string->history ")(
(foo)")
 '("(foo)"
   ")(")

 :describe "history->string"
 > (history->string '())
 ""
 > (history->string '("foo"))
 "foo"
 > (history->string '("foo"
                      "bar"))
 "bar
foo"
 > (history->string '("foo"
                      "bar"
                      "baz"))
 "baz
bar
foo"
 xit> (history->string '("foo
 bar"))
 "\"foo
bar\"")

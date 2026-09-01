(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/symbol"
                  symbolp_))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "s"
 > (s "foo")
 'foo
 > (js/tag s "foo")
 'foo
 > (js/tag s "foo${1}")
 'foo1
 > (js/tag s "${'foo'}")
 'foo

 :describe "symbolp"
 > (symbolp_ 'foo)
 #t)

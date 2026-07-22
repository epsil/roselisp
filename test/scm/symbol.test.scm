(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/symbol"
                  symbolp_))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `s`
 > (describe "s")
 _
 > (js/tag s "foo")
 'foo
 > (s "foo")
 'foo
 > (js/tag s "${'foo'}")
 'foo
 > (js/tag s "foo${2}")
 'foo2

 ;; `symbolp`
 > (describe "symbolp")
 _
 > (symbolp_ (js/tag s "foo"))
 #t)

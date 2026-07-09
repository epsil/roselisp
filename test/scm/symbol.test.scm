(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/symbol"
                  symbolp_))
(require (only-in "./test-util"
                  assert-equal))

(describe "s"
  (fn ()
    (it "s('foo')"
        (fn ()
          (assert-equal
           (js/tag s "foo")
           (send Symbol for "foo"))
          (assert-equal
           (s "foo")
           (send Symbol for "foo"))))
    (it "s`${'foo'}`"
        (fn ()
          (assert-equal
           (js/tag s "${'foo'}")
           (send Symbol for "foo"))))
    (it "s`foo${2}`"
        (fn ()
          (assert-equal
           (js/tag s "foo${2}")
           (send Symbol for "foo2"))))))

(describe "symbolp"
  (fn ()
    (it "s`foo`"
        (fn ()
          (assert-equal
           (symbolp_ (js/tag s "foo"))
           #t)))))

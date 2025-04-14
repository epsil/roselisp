(require chai "chai")
(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/symbol"
                  intern_
                  symbolp_
                  symbol-to-string_))
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

(describe "intern"
  (fn ()
    (it "foo"
        (fn ()
          (assert-equal
           (intern_ "foo")
           (js/tag s "foo"))))))

(describe "symbolp"
  (fn ()
    (it "s`foo`"
        (fn ()
          (assert-equal
           (symbolp_ (js/tag s "foo"))
           #t)))
    (it "1"
        (fn ()
          (assert-equal
           (symbolp_ 1)
           #f)))
    (it "'foo'"
        (fn ()
          (assert-equal
           (symbolp_ "foo")
           #f)))
    (it "'{}'"
        (fn ()
          (assert-equal
           (symbolp_ (js-obj))
           #f)))
    (it "'[]'"
        (fn ()
          (assert-equal
           (symbolp_ '())
           #f)))))

(describe "symbol-to-string"
  (fn ()
    (it "foo"
        (fn ()
          (assert-equal
           (symbol-to-string_ (js/tag s "foo"))
           "foo")))))

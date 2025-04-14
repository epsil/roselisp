(require chai "chai")
(require (only-in "../../src/ts/equal"
                  equal?_))
(require (only-in "./test-util"
                  assert-equal))

(describe "equal?"
  (fn ()
    (it "(equal? \"\" \"\")"
        (fn ()
          (assert-equal
           (equal?_ ""
                    "")
           #t)))
    (it "(equal? \"foo\" \"foo\")"
        (fn ()
          (assert-equal
           (equal?_ "foo"
                    "foo")
           #t)))
    (it "(equal? \"foo\" \"bar\")"
        (fn ()
          (assert-equal
           (equal?_ "foo"
                    "bar")
           #f)))
    (it "(equal? '() '())"
        (fn ()
          (assert-equal
           (equal?_ '()
                    '())
           #t)))
    (it "(equal? '(1 2 3) '(1 2 3))"
        (fn ()
          (assert-equal
           (equal?_ '(1 2 3)
                    '(1 2 3))
           #t)))
    (it "(equal? (make-hash) (make-hash))"
        (fn ()
          (assert-equal
           (equal?_ (make-hash)
                    (make-hash))
           #t)))
    (it "(equal? (make-hash '((\"foo\" . \"bar\"))) (make-hash '((\"foo\" . \"bar\"))))"
        (fn ()
          (assert-equal
           (equal?_ (make-hash '(("foo" . "bar")))
                    (make-hash '(("foo" . "bar"))))
           #t)))
    (it "(equal? (js-obj) (js-obj))"
        (fn ()
          (assert-equal
           (equal?_ (js-obj)
                    (js-obj))
           #t)))
    (it "(equal? (js-obj \"foo\" \"bar\") (js-obj \"foo\" \"bar\"))"
        (fn ()
          (assert-equal
           (equal?_ (js-obj "foo" "bar")
                    (js-obj "foo" "bar"))
           #t)))
    (it "(equal? (js-obj \"foo\" (js-obj \"bar\" \"baz\")) (js-obj \"foo\" (js-obj \"bar\" \"baz\")))"
        (fn ()
          (assert-equal
           (equal?_ (js-obj "foo"
                            (js-obj "bar" "baz"))
                    (js-obj "foo"
                            (js-obj "bar" "baz")))
           #t)))))

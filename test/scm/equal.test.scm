(require (only-in "../../src/ts/equal"
                  equal?_))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "equal?"
 > (equal?_ ""
            "")
 #t
 > (equal?_ "foo"
            "foo")
 #t
 > (equal?_ "foo"
            "bar")
 #f
 > (equal?_ '()
            '())
 #t
 > (equal?_ '(1 2 3)
            '(1 2 3))
 #t
 > (equal?_ (make-hash)
            (make-hash))
 #t
 > (equal?_ (make-hash '(("foo" . "bar")))
            (make-hash '(("foo" . "bar"))))
 #t
 > (equal?_ (object)
            (object))
 #t
 > (equal?_ (object "foo" "bar")
            (object "foo" "bar"))
 #t
 > (equal?_ (object "foo"
                    (object "bar" "baz"))
            (object "foo"
                    (object "bar" "baz")))
 #t)

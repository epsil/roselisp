(require (only-in "../../src/ts/equal"
                  equal?_))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `equal?`
 > (describe "equal?")
 _
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
 > (equal?_ (js/obj)
            (js/obj))
 #t
 > (equal?_ (js/obj "foo" "bar")
            (js/obj "foo" "bar"))
 #t
 > (equal?_ (js/obj "foo"
                    (js/obj "bar" "baz"))
            (js/obj "foo"
                    (js/obj "bar" "baz")))
 #t)

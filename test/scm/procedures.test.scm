(require chai "chai")
(require (only-in "../../src/ts/cons"
                  cons))
(require (only-in "../../src/ts/procedures"
                  compose
                  gt
                  lt
                  pipe))
(require (only-in "../../src/ts/list"
                  dotted-list-p
                  flatten))
(require (only-in "../../src/ts/sexp"
                  s
                  sexp))
(require (only-in "../../src/ts/string"
                  stringp))
(require (only-in "./test-util"
                  assert-equal))

(describe "stringp"
  (fn ()
    (it "'foo'"
        (fn ()
          (assert-equal
           (stringp "foo")
           #t)))
    (it "new String('foo')"
        (fn ()
          (assert-equal
           (stringp (new String "foo"))
           #t)))
    (it "s`foo`"
        (fn ()
          (assert-equal
           (stringp (js/tag s "foo"))
           #f)))
    (it "1"
        (fn ()
          (assert-equal
           (stringp 1)
           #f)))
    (it "{}"
        (fn ()
          (assert-equal
           (stringp (js-obj))
           #f)))
    (it "['foo']"
        (fn ()
          (assert-equal
           (stringp (list "foo"))
           #f)))
    (it "{ 'foo': '' }"
        (fn ()
          (assert-equal
           (stringp (js-obj "foo" ""))
           #f)))
    (it "{ 'foo': []"
        (fn ()
          (assert-equal
           (stringp (js-obj "foo" (quote ())))
           #f)))
    (it "{ 'foo': {}"
        (fn ()
          (assert-equal
           (stringp (js-obj "foo" (js-obj)))
           #f)))
    (it "{ 'foo': 'foo' }"
        (fn ()
          (assert-equal
           (stringp (js-obj "foo" "foo"))
           #f)))
    (it "[]"
        (fn ()
          (assert-equal
           (stringp (quote ()))
           #f)))))

(describe "flatten"
  (fn ()
    (it "(1 2 3 4)"
        (fn ()
          (assert-equal
           (flatten '(1 2 3 4))
           '(1 2 3 4))))
    (it "(1 . 2)"
        (fn ()
          (assert-equal
           (flatten '(1 . 2))
           '(1 2))))
    (it "((a) b (c (d) . e) ())"
        (fn ()
          (assert-equal
           (flatten '((a) b (c (d) . e) ()))
           '(a b c d e))))))

(describe "<"
  (fn ()
    (it "(< 1)"
        (fn ()
          (assert-equal
           (lt 1)
           #t)))
    (it "(< 1 2)"
        (fn ()
          (assert-equal
           (lt 1 2)
           #t)))
    (it "(< 1 2 3)"
        (fn ()
          (assert-equal
           (lt 1 2 3)
           #t)))
    (it "(< 1 2 0)"
        (fn ()
          (assert-equal
           (lt 1 2 0)
           #f)))))

(describe ">"
  (fn ()
    (it "(> 1)"
        (fn ()
          (assert-equal
           (gt 1)
           #t)))
    (it "(> 2 1)"
        (fn ()
          (assert-equal
           (gt 2 1)
           #t)))
    (it "(> 3 2 1)"
        (fn ()
          (assert-equal
           (gt 3 2 1)
           #t)))
    (it "(> 0 2 1)"
        (fn ()
          (assert-equal
           (gt 0 2 1)
           #f)))))

(describe "compose"
  (fn ()
    (it "f . g"
        (fn ()
          (define (f x)
            (+ x 1))
          (define (g x)
            (+ x 2))
          (assert-equal
           ((compose f g) 1)
           4)))
    (it "f . g . h"
        (fn ()
          (define (f x)
            (+ x 1))
          (define (g x)
            (+ x 2))
          (define (h x)
            (+ x 3))
          (assert-equal
           ((compose f g h) 1)
           7)))))

(describe "pipe"
  (fn ()
    (it "f | g"
        (fn ()
          (define (f x)
            (+ x 1))
          (define (g x)
            (+ x 2))
          (assert-equal
           ((pipe f g) 1)
           4)))
    (it "f | g | h"
        (fn ()
          (define (f x)
            (+ x 1))
          (define (g x)
            (+ x 2))
          (define (h x)
            (+ x 3))
          (assert-equal
           ((pipe f g h) 1)
           7)))))

(require (only-in "../../src/ts/procedures"
                  compose
                  pipe))
(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/string"
                  stringp))
(require (only-in "./test-util"
                  assert-equal))

(describe "stringp"
  (fn ()
    (it "new String('foo')"
        (fn ()
          (assert-equal
           (stringp (new String "foo"))
           #t)))
    (it "s`foo`"
        (fn ()
          (assert-equal
           (stringp (js/tag s "foo"))
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

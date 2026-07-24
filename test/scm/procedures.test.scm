(require (only-in "../../src/ts/procedures"
                  compose
                  pipe))
(require (only-in "../../src/ts/sexp"
                  s))
(require (only-in "../../src/ts/string"
                  stringp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `stringp`
 > (describe "stringp")
 _
 > (stringp "foo")
 #t
 > (stringp (new String "foo"))
 #t
 > (stringp 'foo)
 #f

 ;; `compose`
 > (describe "compose")
 _
 > (it "g . f"
       (let ((f (lambda (x)
                  (+ x 1)))
             (g (lambda (x)
                  (+ x 2))))
         ((compose g f) 1)))
 4
 > (it "h . g . f"
       (let ((f (lambda (x)
                  (+ x 1)))
             (g (lambda (x)
                  (+ x 2)))
             (h (lambda (x)
                  (+ x 3))))
         ((compose h g f) 1)))
 7

 ;; `pipe`
 > (describe "pipe")
 _
 > (it "f ; g"
       (let ((f (lambda (x)
                  (+ x 1)))
             (g (lambda (x)
                  (+ x 2))))
         ((pipe f g) 1)))
 4
 > (it "f ; g ; h"
       (let ((f (lambda (x)
                  (+ x 1)))
             (g (lambda (x)
                  (+ x 2)))
             (h (lambda (x)
                  (+ x 3))))
         ((pipe f g h) 1)))
 7)

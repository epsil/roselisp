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
 > (stringp (new String "foo"))
 #t
 > (stringp (js/tag s "foo"))
 #f

 ;; `compose`
 > (describe "compose")
 _
 > (let ((f (lambda (x)
              (+ x 1)))
         (g (lambda (x)
              (+ x 2))))
     ((compose g f) 1))
 4
 > (let ((f (lambda (x)
              (+ x 1)))
         (g (lambda (x)
              (+ x 2)))
         (h (lambda (x)
              (+ x 3))))
     ((compose h g f) 1))
 7

 ;; `pipe`
 > (describe "pipe")
 _
 > (let ((f (lambda (x)
              (+ x 1)))
         (g (lambda (x)
              (+ x 2))))
     ((pipe f g) 1))
 4
 > (let ((f (lambda (x)
              (+ x 1)))
         (g (lambda (x)
              (+ x 2)))
         (h (lambda (x)
              (+ x 3))))
     ((pipe f g h) 1))
 7)

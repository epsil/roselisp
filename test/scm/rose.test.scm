(require (only-in "../../src/ts/rose"
                  Rose
                  Forest
                  wrap-sexp-in-rose))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "Rose"
 > (it "insert"
       (let ((foo (new Rose "foo"))
             (bar (new Rose "bar")))
         (send foo insert bar)
         (assert-equal
          (send foo get-forest)
          (send (new Forest bar)
                set-parent
                foo))))

 :describe "wrap-sexp-in-rose"
 > (wrap-sexp-in-rose 1)
 (new Rose 1)
 > (wrap-sexp-in-rose "1")
 (new Rose "1")
 > (wrap-sexp-in-rose 'foo)
 (new Rose 'foo)
 > (wrap-sexp-in-rose '(foo bar))
 (new Rose
      '(foo bar)
      (new Forest
           (new Rose 'foo)
           (new Rose 'bar)))
 > (wrap-sexp-in-rose '(+ 1 1))
 (new Rose
      '(+ 1 1)
      (new Forest
           (new Rose '+)
           (new Rose 1)
           (new Rose 1)))
 > (wrap-sexp-in-rose '(+ 1 2))
 (new Rose
      '(+ 1 2)
      (new Forest
           (new Rose '+)
           (new Rose 1)
           (new Rose 2)))
 > (wrap-sexp-in-rose '(+ (+ 1)))
 (new Rose
      '(+ (+ 1))
      (new Forest
           (new Rose '+)
           (new Rose
                '(+ 1)
                (new Forest
                     (new Rose '+)
                     (new Rose 1)))))
 > (wrap-sexp-in-rose '(+ (+ 1 1)))
 (new Rose
      '(+ (+ 1 1))
      (new Forest
           (new Rose '+)
           (new Rose
                '(+ 1 1)
                (new Forest
                     (new Rose '+)
                     (new Rose 1)
                     (new Rose 1)))))
 > (wrap-sexp-in-rose '(+ (+ 1 1) 2))
 (new Rose
      '(+ (+ 1 1) 2)
      (new Forest
           (new Rose '+)
           (new Rose
                '(+ 1 1)
                (new Forest
                     (new Rose '+)
                     (new Rose 1)
                     (new Rose 1)))
           (new Rose 2))))

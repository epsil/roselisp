(require chai "chai")
(require (only-in "../../src/ts/language"
                  sexp))
(require (only-in "../../src/ts/rose"
                  Rose
                  Forest
                  wrap-sexp-in-rose))
(require (only-in "./test-util"
                  assert-equal))

(describe "Rose"
  (fn ()
    (describe "insert"
      (fn ()
        (it "foo/bar"
            (fn ()
              (define foo
                (new Rose "foo"))
              (define bar
                (new Rose "bar"))
              (send foo insert bar)
              (assert-equal
               (send foo get-forest)
               (send (new Forest bar)
                     set-parent
                     foo))))))))

(describe "wrap-sexp-in-rose"
  (fn ()
    (it "1"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose 1)
           (new Rose 1))))
    (it "\"1\""
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose "1")
           (new Rose "1"))))
    (it "foo"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose 'foo)
           (new Rose 'foo))))
    (it "(foo bar)"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(foo bar))
           (new Rose
                '(foo bar)
                (new Forest
                     (new Rose 'foo)
                     (new Rose 'bar))))))
    (it "(+ 1 1)"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(+ 1 1))
           (new Rose
                '(+ 1 1)
                (new Forest
                     (new Rose '+)
                     (new Rose 1)
                     (new Rose 1))))))
    (it "(+ 1 2)"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(+ 1 2))
           (new Rose
                '(+ 1 2)
                (new Forest
                     (new Rose '+)
                     (new Rose 1)
                     (new Rose 2))))))
    (it "(+ (+ 1))"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(+ (+ 1)))
           (new Rose
                '(+ (+ 1))
                (new Forest
                     (new Rose '+)
                     (new Rose
                          '(+ 1)
                          (new Forest
                               (new Rose '+)
                               (new Rose 1))))))))
    (it "(+ (+ 1 1))"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(+ (+ 1 1)))
           (new Rose
                '(+ (+ 1 1))
                (new Forest
                     (new Rose '+)
                     (new Rose
                          '(+ 1 1)
                          (new Forest
                               (new Rose '+)
                               (new Rose 1)
                               (new Rose 1))))))))
    (it "(+ (+ 1 1) 2)"
        (fn ()
          (assert-equal
           (wrap-sexp-in-rose '(+ (+ 1 1) 2))
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
                     (new Rose 2))))))))

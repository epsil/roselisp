(require chai "chai")
(require (only-in "../../src/ts/macros"
                  case_
                  case-eq_
                  thread-as_
                  thread-first_
                  thread-last_))
(require (only-in "../../src/ts"
                  LispEnvironment
                  macroexpand
                  macroexpand-1
                  macroexpand-all
                  make-lisp))
(require (only-in "./test-util"
                  assert-equal
                  test-repl))

(describe "macroexpand"
  (fn ()
    (it "(foo bar)"
        (fn ()
          (assert-equal
           (macroexpand
            '(foo bar)
            (new LispEnvironment
                 `((foo ,(fn (exp env) '(baz)) "macro"))))
           (values '(baz) #t))))
    (it "(+ 1 1)"
        (fn ()
          (assert-equal
           (macroexpand
            '(+ 1 1)
            (new LispEnvironment))
           (values '(+ 1 1) #f))))
    (it "(~> \"a b c d\" ...)"
        (fn ()
          (assert-equal
           (macroexpand-1
            '(~> "a b c d"
                 .toUpperCase
                 (.replace "A" "X")
                 (.split " ")
                 first)
            (make-lisp))
           (values
            '(as~> "a b c d" _
               (.toUpperCase _)
               (.replace _ "A" "X")
               (.split _ " ")
               (first _))
            #t))))
    (it "(~>> foo)"
        (fn ()
          (assert-equal
           (macroexpand-1
            '(~>> foo)
            (make-lisp))
           (values '(as~> foo _) #t))))
    (it "(~>> foo (bar))"
        (fn ()
          (assert-equal
           (macroexpand-1
            '(~>> foo (bar))
            (make-lisp))
           (values
            '(as~> foo _
               (bar _))
            #t))))
    (it "(~>> (range) ...)"
        (fn ()
          (assert-equal
           (macroexpand-1
            '(~>> (range)
                  (map (fn (x) (* x x)))
                  (filter even?)
                  (take 10)
                  (reduce +))
            (make-lisp))
           (values
            '(as~> (range) _
               (map (fn (x) (* x x)) _)
               (filter even? _)
               (take 10 _)
               (reduce + _))
            #t))))))

(describe "macroexpand-all"
  (fn ()
    (it "(~> ...)"
        (fn ()
          (assert-equal
           (macroexpand-all
            '(~> "a b c d"
                 .toUpperCase
                 (.replace "A" "X")
                 (.split " ")
                 first)
            (make-lisp))
           '(first
             (.split
              (.replace
               (.toUpperCase "a b c d")
               "A" "X")
              " ")))))
    (xit "(begin (~> ...))"
         (fn ()
           (assert-equal
            (macroexpand-all
             '(begin
                (~> "a b c d"
                    .toUpperCase
                    (.replace "A" "X")
                    (.split " ")
                    first))
             (make-lisp))
            '(begin
               (first
                (.split
                 (.replace
                  (.toUpperCase "a b c d")
                  "A" "X")
                 " "))))))
    (xit "(begin (~> ... (~> ...) ...))"
         (fn ()
           (assert-equal
            (macroexpand-all
             '(begin
                (~> "a b c d"
                    .toUpperCase
                    (.replace "A"
                              (~> "x"
                                  (.toUpperCase)))
                    (.split " ")
                    first))
             (make-lisp))
            '(begin
               (first
                (.split
                 (.replace
                  (.toUpperCase "a b c d")
                  "A"
                  (.toUpperCase "x"))
                 " "))))))))

(describe "as~>"
  (fn ()
    (it "(as~> x _)"
        (fn ()
          (assert-equal
           (thread-as_ '(as~> x _))
           'x)))
    (it "(as~> x _ (foo))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (foo)))
           '(begin
              x
              (foo)))))
    (it "(as~> x _ (foo) (bar))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (foo)
               (bar)))
           '(begin
              x
              (foo)
              (bar)))))
    (it "(as~> x _ (+ _ 1))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (+ _ 1)))
           '(+ x 1))))
    (it "(as~> x _ (+ _ _))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (+ _ _)))
           '(let ((_ x))
              (set! _ (+ _ _))
              _))))
    (it "(as~> x _ (+ _ _))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (+ _ 1)
               (+ _ 1)))
           '(+ (+ x 1) 1))))
    (it "(as~> x _ (+ _ _))"
        (fn ()
          (assert-equal
           (thread-as_
            '(as~> x _
               (+ _ 1)
               (+ _ _)))
           '(let ((_ (+ x 1)))
              (set! _ (+ _ _))
              _))))))

(describe "~>"
  (fn ()
    (it "(~> x foo)"
        (fn ()
          (assert-equal
           (thread-first_
            '(~> x
                 foo))
           '(as~> x _
              (foo _)))))
    (it "(~> x (foo))"
        (fn ()
          (assert-equal
           (thread-first_
            '(~> x
                 (foo)))
           '(as~> x _
              (foo _)))))
    (it "(~> x (foo _))"
        (fn ()
          (assert-equal
           (thread-first_
            '(~> x
                 (foo _)))
           '(as~> x _
              (foo _)))))
    (it "(~> x :hole-marker * (foo *))"
        (fn ()
          (assert-equal
           (thread-first_
            '(~> x
                 :hole-marker *
                 (foo *)))
           '(as~> x *
              (foo *)))))))

(describe "~>>"
  (fn ()
    (it "(~>> x foo)"
        (fn ()
          (assert-equal
           (thread-last_
            '(~>> x
                  foo))
           '(as~> x _
              (foo _)))))
    (it "(~>> x (foo))"
        (fn ()
          (assert-equal
           (thread-last_
            '(~>> x
                  (foo)))
           '(as~> x _
              (foo _)))))
    (it "(~>> x (foo _))"
        (fn ()
          (assert-equal
           (thread-last_
            '(~>> x
                  (foo _)))
           '(as~> x _
              (foo _)))))
    (it "(~>> x :hole-marker * (foo *))"
        (fn ()
          (assert-equal
           (thread-last_
            '(~>> x
                  :hole-marker *
                  (foo *)))
           '(as~> x *
              (foo *)))))))

(describe "case/eq"
  (fn ()
    (it "(case/eq x ((\"foo\") foo) (else bar))"
        (fn ()
          (assert-equal
           (case-eq_
            '(case/eq x
                      (("foo")
                       foo)
                      (else
                       bar)))
           '(js/switch x
                       (case (quote "foo")
                         foo
                         (break))
                       (default
                         bar)))))
    (it "(case/eq x ((\"foo\" \"bar\") foo) (else baz))"
        (fn ()
          (assert-equal
           (case-eq_
            '(case/eq x
                      (("foo" "bar")
                       foo)
                      (else
                       baz)))
           '(cond
             ((member? x '("foo" "bar"))
              foo)
             (else
              baz)))))
    (it "(case/eq x ((\"foo\" \"bar\") foo) (else baz))"
        (fn ()
          (define actual
            (case-eq_
             '(case/eq (get-field prop x)
                       (("foo" "bar")
                        foo)
                       (else
                        baz))))
          (define result-var
            (first (first (second actual))))
          (define expected
            `(let ((,result-var (get-field prop x)))
               (cond
                ((member? ,result-var '("foo" "bar"))
                 foo)
                (else
                 baz))))
          (assert-equal actual expected)))))

(describe "case"
  (fn ()
    (it "(case x ((\"foo\") foo) (else bar))"
        (fn ()
          (assert-equal
           (case_
            '(case x
               (("foo")
                foo)
               (else
                bar)))
           '(case/eq x
                     (("foo")
                      foo)
                     (else
                      bar)))))
    (it "(case x (((\"foo\")) foo) (else bar))"
        (fn ()
          (assert-equal
           (case_
            '(case x
               ((("foo"))
                foo)
               (else
                bar)))
           '(cond
             ((member? x '(("foo")) equal?)
              foo)
             (else
              bar)))))
    (it "> (case 'foo ((foo) 1))"
        (fn ()
          (test-repl
           '(roselisp
             > (case 'foo
                 ((foo)
                  1))
             1))))))

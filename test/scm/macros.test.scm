(require (only-in "../../src/ts/macros"
                  case_
                  case-eq_
                  thread-as_
                  thread-first_
                  thread-last_))
(require (only-in "../../src/ts"
                  LispEnvironment
                  macroexpand
                  macroexpand*
                  macroexpand-1
                  macroexpand-all
                  make-lisp))
(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `macroexpand`
 > (describe "macroexpand")
 _
 > (macroexpand
    '(foo bar)
    (new LispEnvironment
         `((foo ,(fn (exp env) '(baz)) "macro"))))
 '(baz)
 > (macroexpand
    '(+ 1 1)
    (new LispEnvironment))
 '(+ 1 1)

 ;; `macroexpand*`
 > (describe "macroexpand*")
 _
 > (macroexpand*
    '(foo bar)
    (new LispEnvironment
         `((foo ,(fn (exp env) '(baz)) "macro"))))
 (values '(baz) #t)
 > (macroexpand*
    '(+ 1 1)
    (new LispEnvironment))
 (values '(+ 1 1) #f)

 ;; `macroexpand-1`
 > (describe "macroexpand-1")
 _
 > (macroexpand-1
    '(~> "a b c d"
         .toUpperCase
         (.replace "A" "X")
         (.split " ")
         first)
    (make-lisp))
 '(as~> "a b c d" _
    (.toUpperCase _)
    (.replace _ "A" "X")
    (.split _ " ")
    (first _))
 > (macroexpand-1
    '(~>> foo)
    (make-lisp))
 '(as~> foo _)
 > (macroexpand-1
    '(~>> foo (bar))
    (make-lisp))
 '(as~> foo _
    (bar _))
 > (macroexpand-1
    '(~>> (range)
          (map (fn (x) (* x x)))
          (filter even?)
          (take 10)
          (reduce +))
    (make-lisp))
 '(as~> (range) _
    (map (fn (x) (* x x)) _)
    (filter even? _)
    (take 10 _)
    (reduce + _))

 ;; `macroexpand-all`
 > (describe "macroexpand-all")
 _
 > (macroexpand-all
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
    " "))
 xit> (macroexpand-all
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
      " ")))
 xit> (macroexpand-all
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
      " ")))

 ;; `as->`
 > (describe "as->")
 _
 > (thread-as_ '(as~> x _))
 'x
 > (thread-as_
    '(as~> x _
       (foo)))
 '(begin
    x
    (foo))
 > (thread-as_
    '(as~> x _
       (foo)
       (bar)))
 '(begin
    x
    (foo)
    (bar))
 > (thread-as_
    '(as~> x _
       (+ _ 1)))
 '(+ x 1)
 > (thread-as_
    '(as~> x _
       (+ _ _)))
 '(let ((_ x))
    (set! _ (+ _ _))
    _)
 > (thread-as_
    '(as~> x _
       (+ _ 1)
       (+ _ 1)))
 '(+ (+ x 1) 1)
 > (thread-as_
    '(as~> x _
       (+ _ 1)
       (+ _ _)))
 '(let ((_ (+ x 1)))
    (set! _ (+ _ _))
    _)

 ;; `~>`
 > (describe "~>")
 _
 > (thread-first_
    '(~> x
         foo))
 '(as~> x _
    (foo _))
 > (thread-first_
    '(~> x
         (foo)))
 '(as~> x _
    (foo _))
 > (thread-first_
    '(~> x
         (foo _)))
 '(as~> x _
    (foo _))
 > (thread-first_
    '(~> x
         :hole-marker *
         (foo *)))
 '(as~> x *
    (foo *))

 ;; `~>>`
 > (describe "~>>")
 _
 > (thread-last_
    '(~>> x
          foo))
 '(as~> x _
    (foo _))
 > (thread-last_
    '(~>> x
          (foo)))
 '(as~> x _
    (foo _))
 > (thread-last_
    '(~>> x
          (foo _)))
 '(as~> x _
    (foo _))
 > (thread-last_
    '(~>> x
          :hole-marker *
          (foo *)))
 '(as~> x *
    (foo *))

 ;; `case/eq`
 > (describe "case/eq")
 _
 > (case-eq_
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
               bar))
 > (case-eq_
    '(case/eq x
              (("foo" "bar")
               foo)
              (else
               baz)))
 '(cond
   ((member? x '("foo" "bar"))
    foo)
   (else
    baz))
 > (let* ((actual
           (case-eq_
            '(case/eq (get-field prop x)
                      (("foo" "bar")
                       foo)
                      (else
                       baz))))
          (result-var
           (first (first (second actual))))
          (expected
           `(let ((,result-var (get-field prop x)))
              (cond
               ((member? ,result-var '("foo" "bar"))
                foo)
               (else
                baz)))))
     (assert-equal actual expected))
 #u

 ;; `case`
 > (describe "case")
 _
 > (case_
    '(case x
       (("foo")
        foo)
       (else
        bar)))
 '(case/eq x
           (("foo")
            foo)
           (else
            bar))
 > (case_
    '(case x
       ((("foo"))
        foo)
       (else
        bar)))
 '(cond
   ((member? x '(("foo")) equal?)
    foo)
   (else
    bar)))

(test-macro
 :repl #t

 ;; `case`
 > (describe "case")
 _
 > (case 'foo
     ((foo)
      1))
 1)

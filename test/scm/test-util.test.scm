;;; # Test utilities tests
;;;
;;; Tests for the test utilities.

(require (only-in "./test-util"
                  assert-equal
                  compile-repl-form
                  simplify-repl-form
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `test-macro`
 > (describe "test-macro")
 _
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _)
            #u)
 '(begin
    (describe "foo"
      (fn ())))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              > (foo)
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (it "(foo)"
            (fn ()
              (assert-equal
               (foo)
               "foo"))))))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              > (define (foo x)
                  x)
              _
              > (foo "foo")
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (define (foo x)
          x)
        (it "(foo \"foo\")"
            (fn ()
              (assert-equal
               (foo "foo")
               "foo"))))))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              only> (foo)
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (send it only "(foo)"
              (fn ()
                (assert-equal
                 (foo)
                 "foo"))))))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              > (it "Call foo with no arguments"
                    (foo))
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (it "Call foo with no arguments"
            (fn ()
              (assert-equal
               (foo)
               "foo"))))))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              > (it "Call foo twice"
                    (assert-equal
                     (foo "foo")
                     "foo")
                    (assert-equal
                     (foo "bar")
                     "bar"))
              _)
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (it "Call foo twice"
            (fn ()
              (assert-equal
               (foo "foo")
               "foo")
              (assert-equal
               (foo "bar")
               "bar"))))))
 > (funcall test-macro
            '(test-macro
              > (describe "foo")
              _
              xit> (foo)
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (xit "(foo)"
             (fn ()
               (assert-equal
                (foo)
                "foo"))))))
 > (funcall test-macro
            '(test-macro
              :repl #t
              > (describe "foo")
              _
              > (foo)
              "foo")
            #u)
 '(begin
    (describe "foo"
      (fn ()
        (it "(foo)"
            (fn ()
              (test-repl
               '(roselisp
                 > (foo)
                 "foo")))))))

 ;; `test-repl`
 > (describe "test-repl")
 _
 > (it "(> ...)"
       (test-repl
        '(> (+ 1 1)
            2)))
 _
 > (it "(_ > ...)"
       (test-repl
        '(_
          > (+ 1 1)
          2)))
 _
 > (it "(repl > ...)"
       (test-repl
        '(repl
          > (+ 1 1)
          2)))
 _
 > (it "(shell > ...)"
       (test-repl
        '(shell
          > (+ 1 1)
          2)))
 _
 > (it "(roselisp > ...)"
       (test-repl
        '(roselisp
          > (+ 1 1)
          2)))
 _
 > (it "($ roselisp > ...)"
       (test-repl
        '($ roselisp
            ;; Roselisp REPL.
            > (+ 1 1)
            2)))
 _
 > (it "(+ 2 2)"
       (test-repl
        '(roselisp
          > (+ 2 2)
          4)))
 _
 > (it "(list 1 2 3 4)"
       (test-repl
        '(roselisp
          > (list 1 2 3 4)
          '(1 2 3 4))))
 _
 > (it "1 + 1"
       (test-repl
        '(node
          > "1 + 1"
          "2")))
 _
 xit> (it "const n = 1"
          (test-repl
           '(node
             > "const n = 1"
             "undefined"
             > "n + 1"
             "2")))
 _

 ;; `compile-repl-form`
 > (describe "compile-repl-form")
 _
 > (it "(roselisp > (+ 1 1) 2)"
       (compile-repl-form
        '(roselisp
          > (+ 1 1)
          2)
        (js/obj :from "roselisp"
                :to "node")))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "(function () {
  return 2;
})()")
 > (it "(roselisp > (+ 1 1) _)"
       (compile-repl-form
        '(roselisp
          > (+ 1 1)
          _)
        (js/obj :from "roselisp"
                :to "node")))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "_")
 xit> (it "(roselisp > (+ 1 1) 2), plist"
          (compile-repl-form
           '(roselisp
             > (+ 1 1)
             2)
           :from "roselisp"
           :to "node"))
 '(node
   > "(function () {
  return 1 + 1;
})()"
   "(function () {
  return 2;
})()")

 ;; `simplify-repl-form`
 > (describe "simplify-repl-form")
 _
 > (it "(roselisp > (+ 1 1) 2)"
       (simplify-repl-form
        '(roselisp
          > (+ 1 1)
          2
          > (+ 2 2)
          4)))
 '(roselisp
   > (begin
       (+ 1 1)
       (+ 2 2))
   4))

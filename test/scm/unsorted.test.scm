;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require (only-in "./test-util"
                  assert-equal
                  test-repl))

;;; Test inbox
(describe "Unsorted tests"
  (fn ()
    (describe "call/cc"
      (fn ()
        (it "(try ... (+ 5 (call/cc (lambda (x) (error \"error\")))) ...)"
            (fn ()
              (define result 0)
              (try
                (set! result
                      (+ 5 (call/cc
                            (lambda (x)
                              (error "error")))))
                (catch Object e))
              (assert-equal result 0)))))))

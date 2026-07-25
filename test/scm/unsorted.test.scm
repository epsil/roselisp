;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

;;; Test inbox
(test-macro
 :repl #t

 ;; `call/cc`
 > (describe "call/cc")
 _
 > (it "(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)"
       (let ((result 0))
         (try
           (set! result
                 (+ 5 (call/cc
                       (lambda (x)
                         (error "error")))))
           (catch Object e))
         result))
 0)

;;; # Property lists
;;;
;;; Tests of property list functions.

(require (only-in "../../src/ts/plist"
                  plist-set!_))
(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 > (describe "Property lists")
 _
 > (let ((plst '()))
     (plist-set! plst :foo 'bar)
     plst)
 '(:foo bar)
 > (let ((plst '(:foo bar)))
     (plist-set! plst :foo 'baz)
     plst)
 '(:foo baz))

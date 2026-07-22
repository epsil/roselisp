(require (only-in "../../src/ts/combinators"
                  I
                  K))
(require (only-in "../../src/ts/memo"
                  eof
                  memoize))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `memoize`
 > (describe "memoize")
 _
 > (it "cache"
       (let ((I-m (memoize I)))
         (instance-of? (get-field cache I-m)
                       Map)))
 #t
 > (it "(I)"
       (let ((I-m (memoize I)))
         (eq? (I-m) #u)))
 #t
 > (it "(I), (I)"
       (let ((I-m (memoize I)))
         (I-m)
         (eq? (I-m) #u)))
 #t
 > (it "(I), cache"
       (let ((I-m (memoize I)))
         (I-m)
         (get-field cache I-m)))
 (new Map `((,eof ,#u)))
 > (it "(I 1)"
       (let ((I-m (memoize I)))
         (I-m 1)))
 1
 > (it "(I 1), (I 1)"
       (let ((I-m (memoize I)))
         (I-m 1)
         (I-m 1)))
 1
 > (it "(I 1), cache"
       (let ((I-m (memoize I)))
         (I-m 1)
         (get-field cache I-m)))
 (new Map
      `((1 ,(new Map
                 `((,eof 1))))))
 > (it "(I 1), change cache"
       (let ((I-m (memoize I)))
         (I-m 1)
         (set-field! cache
                     I-m
                     (new Map
                          `((1 ,(new Map
                                     `((,eof 500)))))))
         (I-m 1)))
 500
 > (it "(K 1 2)"
       (let ((K-m (memoize K)))
         (K-m 1 2)))
 1
 > (it "(K 1 2), (K 1 2)"
       (let ((K-m (memoize K)))
         (K-m 1 2)
         (K-m 1 2)))
 1
 > (it "(K 1 2), cache"
       (let ((K-m (memoize K)))
         (K-m 1 2)
         (get-field cache K-m)))
 (new Map
      `((1 ,(new Map
                 `((2 ,(new Map
                            `((,eof 1))))))))))

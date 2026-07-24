(require (only-in "../../src/ts/curry"
                  __
                  curry))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `curry`
 > (describe "curry")
 _
 > (it "(a 1)"
       (let* ((a (lambda (x)
                   (list x)))
              (a-c (curry a)))
         (a-c 1)))
 '(1)
 > (it "((a) 1)"
       (let* ((a (lambda (x)
                   (list x)))
              (a-c (curry a)))
         ((a-c) 1)))
 '(1)
 > (it "(ab 1 2)"
       (let* ((ab (lambda (x y)
                    (list x y)))
              (ab-c (curry ab)))
         (ab-c 1 2)))
 '(1 2)
 > (it "((ab 1) 2)"
       (let* ((ab (lambda (x y)
                    (list x y)))
              (ab-c (curry ab)))
         ((ab-c 1) 2)))
 '(1 2)
 > (it "(((ab) 1) 2)"
       (let* ((ab (lambda (x y)
                    (list x y)))
              (ab-c (curry ab)))
         (((ab-c) 1) 2)))
 '(1 2)
 > (it "(abc 1 2 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc)))
         (abc-c 1 2 3)))
 '(1 2 3)
 > (it "((abc 1 2) 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc)))
         ((abc-c 1 2) 3)))
 '(1 2 3)
 > (it "(((abc 1) 2) 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc)))
         (((abc-c 1) 2) 3)))
 '(1 2 3)
 > (it "((((abc) 1) 2) 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc)))
         ((((abc-c) 1) 2) 3)))
 '(1 2 3)
 > (it "(abc 1)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc 1)))
         (abc-c 1)))
 '(1 #u #u)
 > (it "(abc 1 2)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc 1)))
         (abc-c 1 2)))
 '(1 2 #u)
 > (it "(abc 1 2 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc 1)))
         (abc-c 1 2 3)))
 '(1 2 3)
 > (it "((abc _ _ _) 1 2 3)"
       (let* ((abc (lambda (x y z)
                     (list x y z)))
              (abc-c (curry abc)))
         ((abc-c __ __ __) 1 2 3)))
 '(1 2 3)
 > (it "_ !== '_"
       (eq? __ '_))
 #f)

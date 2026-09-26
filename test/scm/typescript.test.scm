;;; # TypeScript
;;;
;;; Tests of TypeScript constructs.

(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t
 :describe "ts/as"
 > (ts/as #t Any)
 #t
 > (ts/as #f Any)
 #f
 > (ts/as #u Any)
 #u
 > (compile '(ts/as #t Any))
 "true;"
 > (compile '(ts/as #t Any) :to "typescript")
 "true as any;"
 > (compile '(ts/as 1 Number)
            :to "javascript")
 "1;"
 > (compile '(ts/as 1 Number)
            :to "typescript")
 "1 as number;"
 > (compile '(ts/as (list) Any)
            :to "typescript")
 "[] as any;"
 > (compile '(ts/as '() Any)
            :to "typescript")
 "[] as any;"
 > (compile '(ts/as x (List Any))
            :to "typescript")
 "x as [any];"
 > (compile '(ts/as x (List Number Any))
            :to "typescript")
 "x as [number, any];"
 > (compile '(ts/as x NN)
            :to "typescript")
 "x as NN;"
 > (compile '(ts/as x (NN Any))
            :to "typescript")
 "x as NN<any>;"
 > (compile '(ts/as x (NN Any Any))
            :to "typescript")
 "x as NN<any,any>;"
 > (compile '((ts/as (lambda (x) x) Any) 1)
            :to "typescript")
 "((x: any): any => x as any)(1);"
 > (compile '(lambda (x) (ts/as (send x foo) Any))
            :to "typescript")
 "(x: any): any => x.foo() as any;")

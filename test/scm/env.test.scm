(require (only-in "../../src/ts/env"
                  Environment
                  EnvironmentComposition
                  EnvironmentPipe
                  EnvironmentStack
                  JavaScriptEnvironment
                  LispEnvironment
                  ThunkedEnvironment
                  TypedEnvironment
                  extend-environment))
(require (only-in "../../src/ts/thunk"
                  thunk))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(test-macro
 ;; `Environment`
 > (describe "Environment")
 _
 > (it "find-frame"
       (define env
         (new Environment
              '((foo "bar"))))
       (assert-equal
        (send env find-frame 'foo)
        env))
 _
 > (it "find-frame, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env find-frame 'quux))
 #u
 > (it "find-frame, nonexistant binding, notFound option"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env find-frame 'quux (js/obj "notFound" #f)))
 #f
 > (it "find-frame, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env find-frame 'foo (js/obj "filter" filter)))
 #u
 > (it "find-frame, filter option, parent stack"
       (define env1
         (new LispEnvironment
              '((foo "baz" Any))))
       (define env2
         (new LispEnvironment
              '((bar "baz" Any))))
       (define env
         (new Environment
              '()
              (new EnvironmentStack
                   env1
                   env2)))
       (define (filter x)
         (not (eq? x env2)))
       (send env find-frame 'bar (js/obj "filter" filter)))
 #u
 > (it "find-frame, parent environment, filter option"
       (define env
         (new Environment
              '((foo "bar"))
              (new Environment
                   '((foo "baz")))))
       (define (filter x)
         (not (eq? x env)))
       (send env find-frame 'foo (js/obj "filter" filter)))
 #u
 > (it "get"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get 'foo))
 "bar"
 > (it "get, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get, parent environment, filter option"
       (define env
         (new Environment
              '((foo "bar"))
              (new Environment
                   '((foo "baz")))))
       (define (filter x)
         (not (eq? x env)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-value 'foo))
 "bar"
 > (it "get-local"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local 'foo))
 "bar"
 > (it "get-local, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local 'quux))
 #u
 > (it "get-local, nonexistant binding, notFound option"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-local, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env get-local 'foo (js/obj "filter" filter)))
 #u
 > (it "get-tuple"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-tuple 'foo))
 (values "bar" #t)
 > (it "get-tuple, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-tuple 'quux))
 (values #u #f)
 > (it "get-tuple, nonexistant binding, notFound option"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-tuple 'quux (js/obj "notFound" #f)))
 (values #f #f)
 > (it "get-tuple, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env get-tuple 'quux (js/obj "filter" filter)))
 (values #u #f)
 > (it "get-tuple, parent environment, filter option"
       (define env
         (new Environment
              '((foo "bar"))
              (new Environment
                   '((foo "baz")))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-tuple 'quux (js/obj "filter" filter)))
 (values #u #f)
 > (it "get-local-tuple"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local-tuple 'foo))
 (values "bar" #t)
 > (it "get-local-tuple, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local-tuple 'quux))
 (values #u #f)
 > (it "get-local-tuple, nonexistant binding, notFound option"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env get-local-tuple 'quux (js/obj "notFound" #f)))
 (values #f #f)
 > (it "get-local-tuple, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env get-local-tuple 'quux (js/obj "filter" filter)))
 (values #u #f)
 > (it "has?"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env has? 'foo))
 #t
 > (it "has?, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env has? 'quux))
 #f
 > (it "has?, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has?, parent environment, filter option"
       (define env
         (new Environment
              '((foo "bar"))
              (new Environment
                   '((foo "baz")))))
       (define (filter x)
         (not (eq? x env)))
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has-local?"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env has-local? 'foo))
 #t
 > (it "has-local?, parent environment"
       (define env
         (new Environment
              '((foo "foo"))
              (new Environment
                   '((bar "bar")))))
       (send env has-local? 'bar))
 #f
 > (it "has-local?, nonexistant binding"
       (define env
         (new Environment
              '((foo "bar"))))
       (send env has-local? 'quux))
 #f
 > (it "has-local?, filter option"
       (define env
         (new Environment
              '((foo "bar"))))
       (define (filter x)
         #f)
       (send env has-local? 'foo (js/obj "filter" filter)))
 #f
 > (it "set!"
       (define env
         (new Environment))
       (send env set! 'foo "bar")
       (send env get 'foo))
 "bar"
 > (it "set-entry!"
       (define env
         (new Environment))
       (send env set-entry! '(foo "bar"))
       (send env get 'foo))
 "bar"
 > (it "set-local!"
       (define env
         (new Environment))
       (send env set-local! 'foo "bar")
       (send env get-local 'foo))
 "bar"
 > (it "set!, mutate existing value in parent environment"
       (define parent
         (new Environment
              '((foo "bar"))))
       (define env
         (extend-environment
          (new Environment)
          parent))
       (send env set! 'foo "quux")
       (assert-equal
        (send parent get 'foo)
        "quux")
       (assert-equal
        (send env get-local 'foo)
        #u)
       (assert-equal
        (send env get 'foo)
        "quux"))
 _

 ;; `TypedEnvironment`
 > (describe "TypedEnvironment")
 _
 > (it "get"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get 'foo))
 "bar"
 > (it "get, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get, parent environment, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))
              (new TypedEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-value 'foo))
 "bar"
 > (it "get-value, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-value 'quux))
 #u
 > (it "get-value, nonexistant binding, notFound option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-value 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-value, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value, parent environment, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))
              (new TypedEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-typed-value"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'foo))
 '("bar" Any)
 > (it "get-typed-value, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'quux))
 '(#u Undefined)
 > (it "get-typed-value, nonexistant binding, notFound option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'quux (js/obj "notFound" '(#f Undefined))))
 '(#f Undefined)
 > (it "get-typed-value, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "get-typed-value, parent environment, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))
              (new TypedEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "get-local"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-local 'foo))
 "bar"
 > (it "get-local, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-local 'quux))
 #u
 > (it "get-local, nonexistant binding, notFound option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-local 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-local, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-local 'foo (js/obj "filter" filter)))
 #u
 > (it "get-type"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-type 'foo))
 'Any
 > (it "get-type, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-type 'quux))
 'Undefined
 > (it "get-type, nonexistant binding, notFound option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env get-type 'quux (js/obj "notFound" 'Any)))
 'Any
 > (it "get-type, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-type 'foo (js/obj "filter" filter)))
 'Undefined
 > (it "get-type, parent environment, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))
              (new TypedEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-type 'foo (js/obj "filter" filter)))
 'Undefined
 > (it "has?"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env has? 'foo))
 #t
 > (it "has?, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env has? 'quux))
 #f
 > (it "has?, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has?, parent environment, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))
              (new TypedEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has-local?"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env has-local? 'foo))
 #t
 > (it "has-local?, nonexistant binding"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (send env has-local? 'quux))
 #f
 > (it "has-local?, filter option"
       (define env
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env has-local? 'foo (js/obj "filter" filter)))
 #f
 > (it "set!"
       (define env
         (new TypedEnvironment))
       (send env set! 'foo "bar" 'Any)
       (send env get 'foo))
 "bar"
 > (it "set-entry!"
       (define env
         (new TypedEnvironment))
       (send env set-entry! '(foo ("bar" Any)))
       (send env get 'foo))
 "bar"
 xit> (it "set-local!"
          (define env
            (new TypedEnvironment))
          (send env set-local! 'foo "bar" 'Any)
          (send env get-local 'foo))
 "bar"
 > (it "set!, mutate existing value in parent environment"
       (define parent
         (new TypedEnvironment
              '((foo "bar" Any))))
       (define env
         (extend-environment
          (new TypedEnvironment)
          parent))
       (send env set! 'foo "quux" 'Any)
       (assert-equal
        (send parent get 'foo)
        "quux")
       (assert-equal
        (send env get-local 'foo)
        #u)
       (assert-equal
        (send env get 'foo)
        "quux"))
 _

 ;; `LispEnvironment`
 > (describe "LispEnvironment")
 _
 > (it "get"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get 'foo))
 "bar"
 > (it "get, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))
              (new LispEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-value 'foo))
 "bar"
 > (it "get-value, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-value 'quux))
 #u
 > (it "get-value, nonexistant binding, notFound option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-value 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-value, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value, parent environment, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))
              (new LispEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-typed-value"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'foo))
 '("bar" Any)
 > (it "get-typed-value, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'quux))
 '(#u Undefined)
 > (it "get-typed-value, nonexistant binding, notFound option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-typed-value 'quux (js/obj "notFound" '(#f Undefined))))
 '(#f Undefined)
 > (it "get-typed-value, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "get-typed-value, parent environment, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))
              (new LispEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "get-local"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-local 'foo))
 "bar"
 > (it "get-local, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-local 'quux))
 #u
 > (it "get-local, nonexistant binding, notFound option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-local 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-local, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-local 'foo (js/obj "filter" filter)))
 #u
 > (it "get-type"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-type 'foo))
 'Any
 > (it "get-type, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-type 'quux))
 'Undefined
 > (it "get-type, nonexistant binding, notFound option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env get-type 'quux (js/obj "notFound" 'Any)))
 'Any
 > (it "get-type, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env get-type 'foo (js/obj "filter" filter)))
 'Undefined
 > (it "get-type, parent environment, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))
              (new LispEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get-type 'foo (js/obj "filter" filter)))
 'Undefined
 > (it "has?"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env has? 'foo))
 #t
 > (it "has?, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env has? 'quux))
 #f
 > (it "has?, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has?, parent environment, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))
              (new LispEnvironment
                   '((foo "baz" Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env has? 'foo (js/obj "filter" filter)))
 #f
 > (it "has-local?"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env has-local? 'foo))
 #t
 > (it "has-local?, nonexistant binding"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (send env has-local? 'quux))
 #f
 > (it "has-local?, filter option"
       (define env
         (new LispEnvironment
              '((foo "bar" Any))))
       (define (filter x)
         #f)
       (send env has-local? 'foo (js/obj "filter" filter)))
 #f
 > (it "set!"
       (define env
         (new LispEnvironment))
       (send env set! 'foo "bar" 'Any)
       (send env get 'foo))
 "bar"
 > (it "set-entry!"
       (define env
         (new LispEnvironment))
       (send env set-entry! '(foo ("bar" Any)))
       (send env get 'foo))
 "bar"
 xit> (it "set-local!"
          (define env
            (new LispEnvironment))
          (send env set-local! 'foo "bar" 'Any)
          (send env get-local 'foo))
 "bar"
 > (it "set!, mutate existing value in parent environment"
       (define parent
         (new LispEnvironment
              '((foo "bar" Any))))
       (define env
         (extend-environment
          (new LispEnvironment)
          parent))
       (send env set! 'foo "quux" 'Any)
       (assert-equal
        (send parent get 'foo)
        "quux")
       (assert-equal
        (send env get-local 'foo)
        #u)
       (assert-equal
        (send env get 'foo)
        "quux"))
 _

 ;; `EnvironmentStack`
 > (describe "EnvironmentStack")
 _
 > (it "get"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get 'foo))
 "bar"
 > (it "get, nonexistant binding"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (define (filter x)
         #f)
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get, multiple environments, filter option"
       (define env1
         (new LispEnvironment
              '((foo "bar" Any))))
       (define env2
         (new LispEnvironment
              '((foo "baz" Any))))
       (define env
         (new EnvironmentStack
              env1
              env2))
       (define (filter x)
         (not (eq? x env1)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-value 'foo))
 "bar"
 > (it "get-value, nonexistant binding"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-value 'quux))
 #u
 > (it "get-value, nonexistant binding, notFound option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-value 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-value, filter option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (define (filter x)
         #f)
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value, multiple environments, filter option"
       (define env1
         (new LispEnvironment
              '((foo "bar" Any))))
       (define env2
         (new LispEnvironment
              '((foo "baz" Any))))
       (define env
         (new EnvironmentStack
              env1
              env2))
       (define (filter x)
         (not (eq? x env1)))
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-typed-value"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-typed-value 'foo))
 '("bar" Any)
 > (it "get-typed-value 2"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))
              (new EnvironmentStack
                   (new LispEnvironment
                        '((bar "bar" Any)))
                   (new JavaScriptEnvironment))))
       (send env get-typed-value 'foo))
 '("bar" Any)
 > (it "get-typed-value, nonexistant binding"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-typed-value 'quux))
 '(#u Undefined)
 > (it "get-typed-value, nonexistant binding, notFound option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (send env get-typed-value 'quux (js/obj "notFound" '(#f Undefined))))
 '(#f Undefined)
 > (it "get-typed-value, filter option"
       (define env
         (new EnvironmentStack
              (new LispEnvironment
                   '((foo "bar" Any)))))
       (define (filter x)
         #f)
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "get-typed-value, multiple environments, filter option"
       (define env1
         (new LispEnvironment
              '((foo "bar" Any))))
       (define env2
         (new LispEnvironment
              '((foo "baz" Any))))
       (define env
         (new EnvironmentStack
              env1
              env2))
       (define (filter x)
         (not (eq? x env1)))
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)
 > (it "set!, one environment"
       (define env1
         (new LispEnvironment))
       (define env
         (new EnvironmentStack
              env1))
       (send env set! 'foo "bar" 'Any)
       (assert-equal
        (send env get 'foo)
        "bar")
       (assert-equal
        (send env1 get 'foo)
        "bar"))
 _
 > (it "set!, two environments, previously defined in second"
       (define env1
         (new LispEnvironment))
       (define env2
         (new LispEnvironment
              '((foo "foo" Any))))
       (define env
         (new EnvironmentStack
              env1
              env2))
       (send env set! 'foo "bar" 'Any)
       (assert-equal
        (send env get 'foo)
        "bar")
       (assert-equal
        (send env1 get 'foo)
        #u)
       (assert-equal
        (send env2 get 'foo)
        "bar"))
 _
 > (it "set-entry!, two environments, previously defined in second"
       (define env1
         (new LispEnvironment))
       (define env2
         (new LispEnvironment
              '((foo "foo" Any))))
       (define env
         (new EnvironmentStack
              env1
              env2))
       (send env set-entry! '(foo ("bar" Any)))
       (assert-equal
        (send env get 'foo)
        "bar")
       (assert-equal
        (send env1 get 'foo)
        "bar")
       (assert-equal
        (send env2 get 'foo)
        "foo"))
 _

 ;; `EnvironmentPipe`
 > (describe "EnvironmentPipe")
 _
 > (it "get"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get 'foo))
 'baz
 > (it "get, nonexistant binding"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env1
         (new LispEnvironment
              '((foo bar Any))))
       (define env2
         (new LispEnvironment
              '((bar baz Any))))
       (define env
         (new EnvironmentPipe
              env1
              env2))
       (define (filter x)
         (not (eq? x env1)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-value 'foo))
 'baz
 > (it "get-value, notexistant binding"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-value 'quux))
 #u
 > (it "get-value, notexistant binding, notFound option"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-value 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-value, filter option"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (define (filter x)
         #f)
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-typed-value"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-typed-value 'foo))
 '(baz Any)
 > (it "get-typed-value, nonexistant binding"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-typed-value 'quux))
 '(#u Undefined)
 > (it "get-typed-value, nonexistant binding, notFound option"
       (define env
         (new EnvironmentPipe
              (new LispEnvironment
                   '((foo bar Any)))
              (new LispEnvironment
                   '((bar baz Any)))))
       (send env get-typed-value 'quux (js/obj "notFound" '(#f Undefined))))
 '(#f Undefined)
 > (it "get-typed-value, filter option"
       (define env1
         (new LispEnvironment
              '((foo bar Any))))
       (define env2
         (new LispEnvironment
              '((bar baz Any))))
       (define env
         (new EnvironmentPipe
              env1
              env2))
       (define (filter x)
         (not (eq? x env1)))
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)

 ;; `EnvironmentComposition`
 > (describe "EnvironmentComposition")
 _
 > (it "get"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get 'foo))
 'baz
 > (it "get, nonexistant binding"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env1
         (new LispEnvironment
              '((foo bar Any))))
       (define env2
         (new LispEnvironment
              '((bar baz Any))))
       (define env
         (new EnvironmentComposition
              env2
              env1))
       (define (filter x)
         (not (eq? x env1)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-value"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-value 'foo))
 'baz
 > (it "get-value, nonexistant binding"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-value 'quux))
 #u
 > (it "get-value, nonexistant binding, notFound option"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-value 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-value, filter option"
       (define env1
         (new LispEnvironment
              '((foo bar Any))))
       (define env2
         (new LispEnvironment
              '((bar baz Any))))
       (define env
         (new EnvironmentComposition
              env2
              env1))
       (define (filter x)
         (not (eq? x env1)))
       (send env get-value 'foo (js/obj "filter" filter)))
 #u
 > (it "get-typed-value"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-typed-value 'foo))
 '(baz Any)
 > (it "get-typed-value, nonexistant binding"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-typed-value 'quux))
 '(#u Undefined)
 > (it "get-typed-value, nonexistant binding, notFound option"
       (define env
         (new EnvironmentComposition
              (new LispEnvironment
                   '((bar baz Any)))
              (new LispEnvironment
                   '((foo bar Any)))))
       (send env get-typed-value 'quux (js/obj "notFound" '(#f Undefined))))
 '(#f Undefined)
 > (it "get-typed-value, filter option"
       (define env1
         (new LispEnvironment
              '((foo bar Any))))
       (define env2
         (new LispEnvironment
              '((bar baz Any))))
       (define env
         (new EnvironmentComposition
              env2
              env1))
       (define (filter x)
         (not (eq? x env1)))
       (send env get-typed-value 'foo (js/obj "filter" filter)))
 '(#u Undefined)

 ;; `ThunkedEnvironment`
 > (describe "ThunkedEnvironment")
 _
 > (it "get"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "bar"))
                 Any))))
       (send env get 'foo))
 "bar"
 > (it "get, nonexistant binding"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "bar"))
                 Any))))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "bar"))
                 Any))))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "bar"))
                 Any))))
       (define (filter x)
         #f)
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get, parent environment, filter option"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "bar"))
                 Any))
              (new ThunkedEnvironment
                   `((foo
                      ,(thunk (lambda () "baz"))
                      Any)))))
       (define (filter x)
         (not (eq? x env)))
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "has-thunk?, true"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "foo"))
                 Any))))
       (send env has-thunk? 'foo))
 #t
 > (it "has-thunk?, parent environment, true"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "foo"))
                 Any))
              (new ThunkedEnvironment
                   `((bar
                      ,(thunk (lambda () "bar"))
                      Any)))))
       (send env has-thunk? 'bar))
 #t
 > (it "has-thunk?, false"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "foo"))
                 Any)
                (bar "bar" Any))))
       (send env has-thunk? 'bar))
 #f
 > (it "has-local-thunk?, true"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "foo"))
                 Any))
              (new ThunkedEnvironment
                   `((bar
                      ,(thunk (lambda () "bar"))
                      Any)))))
       (send env has-local-thunk? 'foo))
 #t
 > (it "has-local-thunk?, false"
       (define env
         (new ThunkedEnvironment
              `((foo
                 ,(thunk (lambda () "foo"))
                 Any))
              (new ThunkedEnvironment
                   `((bar
                      ,(thunk (lambda () "bar"))
                      Any)))))
       (send env has-local-thunk? 'bar))
 #f

 ;; `JavaScriptEnvironment`
 > (describe "JavaScriptEnvironment")
 _
 > (it "get"
       (define env
         (new JavaScriptEnvironment))
       (send env get 'Map))
 Map
 > (it "get, nonexistant binding"
       (define env
         (new JavaScriptEnvironment))
       (send env get 'quux))
 #u
 > (it "get, nonexistant binding, notFound option"
       (define env
         (new JavaScriptEnvironment))
       (send env get 'quux (js/obj "notFound" #f)))
 #f
 > (it "get, filter option"
       (define env
         (new JavaScriptEnvironment))
       (define (filter x)
         #f)
       (send env get 'foo (js/obj "filter" filter)))
 #u
 > (it "get-local"
       (define env
         (new JavaScriptEnvironment))
       (send env get-local 'Map))
 Map
 > (it "get-local, nonexistant binding"
       (define env
         (new JavaScriptEnvironment))
       (send env get-local 'quux))
 #u
 > (it "get-local, nonexistant binding, notFound option"
       (define env
         (new JavaScriptEnvironment))
       (send env get-local 'quux (js/obj "notFound" #f)))
 #f
 > (it "get-local, filter option"
       (define env
         (new JavaScriptEnvironment))
       (define (filter x)
         #f)
       (send env get-local 'Map (js/obj "filter" filter)))
 #u
 > (it "has?"
       (define env
         (new JavaScriptEnvironment))
       (send env has? 'Map))
 #t
 > (it "has?, nonexistant binding"
       (define env
         (new JavaScriptEnvironment))
       (send env has? 'quux))
 #f
 > (it "has?, filter option"
       (define env
         (new JavaScriptEnvironment))
       (define (filter x)
         #f)
       (send env has? 'Map (js/obj "filter" filter)))
 #f
 > (it "has-local?"
       (define env
         (new JavaScriptEnvironment))
       (send env has-local? 'Map))
 #t
 > (it "has-local?, nonexistant binding"
       (define env
         (new JavaScriptEnvironment))
       (send env has-local? 'quux))
 #f
 > (it "has-local?, filter option"
       (define env
         (new JavaScriptEnvironment))
       (define (filter x)
         #f)
       (send env has-local? 'Map (js/obj "filter" filter)))
 #f)

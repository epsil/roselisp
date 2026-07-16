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
                  assert-equal))

(describe "Environment"
  (fn ()
    (it "find-frame"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env find-frame 'foo)
           env)))
    (it "find-frame, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env find-frame 'quux)
           #u)))
    (it "find-frame, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env find-frame 'quux (js-obj "notFound" #f))
           #f)))
    (it "find-frame, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env find-frame 'foo (js-obj "filter" filter))
           #u)))
    (it "find-frame, filter option, parent stack"
        (fn ()
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
          (assert-equal
           (send env find-frame 'bar (js-obj "filter" filter))
           #u)))
    (it "find-frame, parent environment, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))
                 (new Environment
                      '((foo "baz")))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env find-frame 'foo (js-obj "filter" filter))
           #u)))
    (it "get"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get, parent environment, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))
                 (new Environment
                      '((foo "baz")))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-local"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local 'foo)
           "bar")))
    (it "get-local, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local 'quux)
           #u)))
    (it "get-local, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-local, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-local 'foo (js-obj "filter" filter))
           #u)))
    (it "get-tuple"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-tuple 'foo)
           (values "bar" #t))))
    (it "get-tuple, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-tuple 'quux)
           (values #u #f))))
    (it "get-tuple, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-tuple 'quux (js-obj "notFound" #f))
           (values #f #f))))
    (it "get-tuple, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-tuple 'quux (js-obj "filter" filter))
           (values #u #f))))
    (it "get-tuple, parent environment, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))
                 (new Environment
                      '((foo "baz")))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-tuple 'quux (js-obj "filter" filter))
           (values #u #f))))
    (it "get-local-tuple"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local-tuple 'foo)
           (values "bar" #t))))
    (it "get-local-tuple, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local-tuple 'quux)
           (values #u #f))))
    (it "get-local-tuple, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get-local-tuple 'quux (js-obj "notFound" #f))
           (values #f #f))))
    (it "get-local-tuple, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-local-tuple 'quux (js-obj "filter" filter))
           (values #u #f))))
    (it "has"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (it "has, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has 'quux)
           #f)))
    (it "has, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has, parent environment, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))
                 (new Environment
                      '((foo "baz")))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has-local"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has-local 'foo)
           #t)))
    (it "has-local, nonexistant binding"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has-local 'quux)
           #f)))
    (it "has-local, filter option"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has-local 'foo (js-obj "filter" filter))
           #f)))
    (it "set"
        (fn ()
          (define env
            (new Environment))
          (send env set 'foo "bar")
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-entry"
        (fn ()
          (define env
            (new Environment))
          (send env set-entry '(foo "bar"))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-local"
        (fn ()
          (define env
            (new Environment))
          (send env set-local 'foo "bar")
          (assert-equal
           (send env get-local 'foo)
           "bar")))
    (it "set, mutate existing value in parent environment"
        (fn ()
          (define parent
            (new Environment
                 '((foo "bar"))))
          (define env
            (extend-environment
             (new Environment)
             parent))
          (send env set 'foo "quux")
          (assert-equal
           (send parent get 'foo)
           "quux")
          (assert-equal
           (send env get-local 'foo)
           #u)
          (assert-equal
           (send env get 'foo)
           "quux")))))

(describe "TypedEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get, parent environment, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))
                 (new TypedEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-value, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'quux)
           #u)))
    (it "get-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-value, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value, parent environment, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))
                 (new TypedEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" Any))))
    (it "get-typed-value, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'quux)
           '(#u Undefined))))
    (it "get-typed-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'quux (js-obj "notFound" '(#f Undefined)))
           '(#f Undefined))))
    (it "get-typed-value, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "get-typed-value, parent environment, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))
                 (new TypedEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "get-local"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'foo)
           "bar")))
    (it "get-local, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'quux)
           #u)))
    (it "get-local, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-local, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-local 'foo (js-obj "filter" filter))
           #u)))
    (it "get-type"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-type 'foo)
           'Any)))
    (it "get-type, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-type 'quux)
           'Undefined)))
    (it "get-type, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-type 'foo (js-obj "filter" filter))
           'Undefined)))
    (it "get-type, parent environment, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))
                 (new TypedEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-type 'foo (js-obj "filter" filter))
           'Undefined)))
    (it "has"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (it "has, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has 'quux)
           #f)))
    (it "has, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has, parent environment, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))
                 (new TypedEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has-local"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has-local 'foo)
           #t)))
    (it "has-local, nonexistant binding"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has-local 'quux)
           #f)))
    (it "has-local, filter option"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has-local 'foo (js-obj "filter" filter))
           #f)))
    (it "set"
        (fn ()
          (define env
            (new TypedEnvironment))
          (send env set 'foo "bar" 'Any)
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-entry"
        (fn ()
          (define env
            (new TypedEnvironment))
          (send env set-entry '(foo ("bar" Any)))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (xit "set-local"
         (fn ()
           (define env
             (new TypedEnvironment))
           (send env set-local 'foo "bar" 'Any)
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "set, mutate existing value in parent environment"
        (fn ()
          (define parent
            (new TypedEnvironment
                 '((foo "bar" Any))))
          (define env
            (extend-environment
             (new TypedEnvironment)
             parent))
          (send env set 'foo "quux" 'Any)
          (assert-equal
           (send parent get 'foo)
           "quux")
          (assert-equal
           (send env get-local 'foo)
           #u)
          (assert-equal
           (send env get 'foo)
           "quux")))))

(describe "LispEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))
                 (new LispEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-value, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'quux)
           #u)))
    (it "get-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-value 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-value, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value, parent environment, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))
                 (new LispEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" Any))))
    (it "get-typed-value, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'quux)
           '(#u Undefined))))
    (it "get-typed-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-typed-value 'quux (js-obj "notFound" '(#f Undefined)))
           '(#f Undefined))))
    (it "get-typed-value, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "get-typed-value, parent environment, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))
                 (new LispEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "get-local"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'foo)
           "bar")))
    (it "get-local, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'quux)
           #u)))
    (it "get-local, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-local 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-local, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-local 'foo (js-obj "filter" filter))
           #u)))
    (it "get-type"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-type 'foo)
           'Any)))
    (it "get-type, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env get-type 'quux)
           'Undefined)))
    (it "get-type, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-type 'foo (js-obj "filter" filter))
           'Undefined)))
    (it "get-type, parent environment, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))
                 (new LispEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env get-type 'foo (js-obj "filter" filter))
           'Undefined)))
    (it "has"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (it "has, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has 'quux)
           #f)))
    (it "has, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has, parent environment, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))
                 (new LispEnvironment
                      '((foo "baz" Any)))))
          (define (filter x)
            (not (eq? x env)))
          (assert-equal
           (send env has 'foo (js-obj "filter" filter))
           #f)))
    (it "has-local"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has-local 'foo)
           #t)))
    (it "has-local, nonexistant binding"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (assert-equal
           (send env has-local 'quux)
           #f)))
    (it "has-local, filter option"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env has-local 'foo (js-obj "filter" filter))
           #f)))
    (it "set"
        (fn ()
          (define env
            (new LispEnvironment))
          (send env set 'foo "bar" 'Any)
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-entry"
        (fn ()
          (define env
            (new LispEnvironment))
          (send env set-entry '(foo ("bar" Any)))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (xit "set-local"
         (fn ()
           (define env
             (new LispEnvironment))
           (send env set-local 'foo "bar" 'Any)
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "set, mutate existing value in parent environment"
        (fn ()
          (define parent
            (new LispEnvironment
                 '((foo "bar" Any))))
          (define env
            (extend-environment
             (new LispEnvironment)
             parent))
          (send env set 'foo "quux" 'Any)
          (assert-equal
           (send parent get 'foo)
           "quux")
          (assert-equal
           (send env get-local 'foo)
           #u)
          (assert-equal
           (send env get 'foo)
           "quux")))))

(describe "EnvironmentStack"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get, multiple environments, filter option"
        (fn ()
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
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-value, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-value 'quux)
           #u)))
    (it "get-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-value 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-value, filter option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value, multiple environments, filter option"
        (fn ()
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
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" Any))))
    (it "get-typed-value 2"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))
                 (new EnvironmentStack
                      (new LispEnvironment
                           '((bar "bar" Any)))
                      (new JavaScriptEnvironment))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" Any))))
    (it "get-typed-value, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-typed-value 'quux)
           '(#u Undefined))))
    (it "get-typed-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (assert-equal
           (send env get-typed-value 'quux (js-obj "notFound" '(#f Undefined)))
           '(#f Undefined))))
    (it "get-typed-value, filter option"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" Any)))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "get-typed-value, multiple environments, filter option"
        (fn ()
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
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))
    (it "set, one environment"
        (fn ()
          (define env1
            (new LispEnvironment))
          (define env
            (new EnvironmentStack
                 env1))
          (send env set 'foo "bar" 'Any)
          (assert-equal
           (send env get 'foo)
           "bar")
          (assert-equal
           (send env1 get 'foo)
           "bar")))
    (it "set, two environments, previously defined in second"
        (fn ()
          (define env1
            (new LispEnvironment))
          (define env2
            (new LispEnvironment
                 '((foo "foo" Any))))
          (define env
            (new EnvironmentStack
                 env1
                 env2))
          (send env set 'foo "bar" 'Any)
          (assert-equal
           (send env get 'foo)
           "bar")
          (assert-equal
           (send env1 get 'foo)
           #u)
          (assert-equal
           (send env2 get 'foo)
           "bar")))
    (it "set-entry, two environments, previously defined in second"
        (fn ()
          (define env1
            (new LispEnvironment))
          (define env2
            (new LispEnvironment
                 '((foo "foo" Any))))
          (define env
            (new EnvironmentStack
                 env1
                 env2))
          (send env set-entry '(foo ("bar" Any)))
          (assert-equal
           (send env get 'foo)
           "bar")
          (assert-equal
           (send env1 get 'foo)
           "bar")
          (assert-equal
           (send env2 get 'foo)
           "foo")))))

(describe "EnvironmentPipe"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get 'foo)
           'baz)))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
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
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-value 'foo)
           'baz)))
    (it "get-value, notexistant binding"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-value 'quux)
           #u)))
    (it "get-value, notexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-value 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-value, filter option"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-typed-value 'foo)
           '(baz Any))))
    (it "get-typed-value, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-typed-value 'quux)
           '(#u Undefined))))
    (it "get-typed-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar Any)))
                 (new LispEnvironment
                      '((bar baz Any)))))
          (assert-equal
           (send env get-typed-value 'quux (js-obj "notFound" '(#f Undefined)))
           '(#f Undefined))))
    (it "get-typed-value, filter option"
        (fn ()
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
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))))

(describe "EnvironmentComposition"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get 'foo)
           'baz)))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
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
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-value 'foo)
           'baz)))
    (it "get-value, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-value 'quux)
           #u)))
    (it "get-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-value 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-value, filter option"
        (fn ()
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
          (assert-equal
           (send env get-value 'foo (js-obj "filter" filter))
           #u)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-typed-value 'foo)
           '(baz Any))))
    (it "get-typed-value, nonexistant binding"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-typed-value 'quux)
           '(#u Undefined))))
    (it "get-typed-value, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz Any)))
                 (new LispEnvironment
                      '((foo bar Any)))))
          (assert-equal
           (send env get-typed-value 'quux (js-obj "notFound" '(#f Undefined)))
           '(#f Undefined))))
    (it "get-typed-value, filter option"
        (fn ()
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
          (assert-equal
           (send env get-typed-value 'foo (js-obj "filter" filter))
           '(#u Undefined))))))

(describe "ThunkedEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new ThunkedEnvironment
                 `((foo
                    ,(thunk (lambda () "bar"))
                    Any))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new ThunkedEnvironment
                 `((foo
                    ,(thunk (lambda () "bar"))
                    Any))))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new ThunkedEnvironment
                 `((foo
                    ,(thunk (lambda () "bar"))
                    Any))))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new ThunkedEnvironment
                 `((foo
                    ,(thunk (lambda () "bar"))
                    Any))))
          (define (filter x)
            #f)
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get, parent environment, filter option"
        (fn ()
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
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))))

(describe "JavaScriptEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get 'Map)
           Map)))
    (it "get, nonexistant binding"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get 'quux)
           #u)))
    (it "get, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get 'quux (js-obj "notFound" #f))
           #f)))
    (it "get, filter option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (define (filter x)
            #f)
          (assert-equal
           (send env get 'foo (js-obj "filter" filter))
           #u)))
    (it "get-local"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get-local 'Map)
           Map)))
    (it "get-local, nonexistant binding"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get-local 'quux)
           #u)))
    (it "get-local, nonexistant binding, notFound option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get-local 'quux (js-obj "notFound" #f))
           #f)))
    (it "get-local, filter option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (define (filter x)
            #f)
          (assert-equal
           (send env get-local 'Map (js-obj "filter" filter))
           #u)))
    (it "has"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env has 'Map)
           #t)))
    (it "has, nonexistant binding"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env has 'quux)
           #f)))
    (it "has, filter option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (define (filter x)
            #f)
          (assert-equal
           (send env has 'Map (js-obj "filter" filter))
           #f)))
    (it "has-local"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env has-local 'Map)
           #t)))
    (it "has-local, nonexistant binding"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env has-local 'quux)
           #f)))
    (it "has-local, filter option"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (define (filter x)
            #f)
          (assert-equal
           (send env has-local 'Map (js-obj "filter" filter))
           #f)))))

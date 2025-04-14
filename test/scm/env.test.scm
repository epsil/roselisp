(require chai "chai")
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
    (it "get"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env get 'foo)
           "bar")))
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
    (it "has"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (it "has-local"
        (fn ()
          (define env
            (new Environment
                 '((foo "bar"))))
          (assert-equal
           (send env has-local 'foo)
           #t)))
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
           undefined)
          (assert-equal
           (send env get 'foo)
           "quux")))))

(describe "TypedEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get-value"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-typed-value"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" "variable"))))
    (xit "get-local" ; ???
         (fn ()
           (define env
             (new TypedEnvironment
                  '((foo "bar" "variable"))))
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "get-type"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-type 'foo)
           "variable")))
    (it "has"
        (fn ()
          (define env
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (xit "has-local"
         (fn ()
           (define env
             (new TypedEnvironment
                  '((foo "bar" "variable"))))
           (assert-equal
            (send env has-local 'foo)
            #t)))
    (it "set"
        (fn ()
          (define env
            (new TypedEnvironment))
          (send env set 'foo "bar" "variable")
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-entry"
        (fn ()
          (define env
            (new TypedEnvironment))
          (send env set-entry '(foo ("bar" "variable")))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (xit "set-local"
         (fn ()
           (define env
             (new TypedEnvironment))
           (send env set-local 'foo "bar" "variable")
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "set, mutate existing value in parent environment"
        (fn ()
          (define parent
            (new TypedEnvironment
                 '((foo "bar" "variable"))))
          (define env
            (extend-environment
             (new TypedEnvironment)
             parent))
          (send env set 'foo "quux" "variable")
          (assert-equal
           (send parent get 'foo)
           "quux")
          (assert-equal
           (send env get-local 'foo)
           undefined)
          (assert-equal
           (send env get 'foo)
           "quux")))))

(describe "LispEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get-value"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-typed-value"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" "variable"))))
    (xit "get-local"
         (fn ()
           (define env
             (new LispEnvironment
                  '((foo "bar" "variable"))))
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "get-type"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env get-type 'foo)
           "variable")))
    (it "has"
        (fn ()
          (define env
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (assert-equal
           (send env has 'foo)
           #t)))
    (xit "has-local"
         (fn ()
           (define env
             (new LispEnvironment
                  '((foo "bar" "variable"))))
           (assert-equal
            (send env has-local 'foo)
            #t)))
    (it "set"
        (fn ()
          (define env
            (new LispEnvironment))
          (send env set 'foo "bar" "variable")
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "set-entry"
        (fn ()
          (define env
            (new LispEnvironment))
          (send env set-entry '(foo ("bar" "variable")))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (xit "set-local"
         (fn ()
           (define env
             (new LispEnvironment))
           (send env set-local 'foo "bar" "variable")
           (assert-equal
            (send env get-local 'foo)
            "bar")))
    (it "set, mutate existing value in parent environment"
        (fn ()
          (define parent
            (new LispEnvironment
                 '((foo "bar" "variable"))))
          (define env
            (extend-environment
             (new LispEnvironment)
             parent))
          (send env set 'foo "quux" "variable")
          (assert-equal
           (send parent get 'foo)
           "quux")
          (assert-equal
           (send env get-local 'foo)
           undefined)
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
                      '((foo "bar" "variable")))))
          (assert-equal
           (send env get 'foo)
           "bar")))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" "variable")))))
          (assert-equal
           (send env get-value 'foo)
           "bar")))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" "variable")))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" "variable"))))
    (it "get-typed-value 2"
        (fn ()
          (define env
            (new EnvironmentStack
                 (new LispEnvironment
                      '((foo "bar" "variable")))
                 (new EnvironmentStack
                      (new LispEnvironment
                           '((bar "bar" "variable")))
                      (new JavaScriptEnvironment))))
          (assert-equal
           (send env get-typed-value 'foo)
           '("bar" "variable"))))
    (it "set, one environment"
        (fn ()
          (define env1
            (new LispEnvironment))
          (define env
            (new EnvironmentStack
                 env1))
          (send env set 'foo "bar" "variable")
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
                 '((foo "foo" "variable"))))
          (define env
            (new EnvironmentStack
                 env1
                 env2))
          (send env set 'foo "bar" "variable")
          (assert-equal
           (send env get 'foo)
           "bar")
          (assert-equal
           (send env1 get 'foo)
           undefined)
          (assert-equal
           (send env2 get 'foo)
           "bar")))
    (it "set-entry, two environments, previously defined in second"
        (fn ()
          (define env1
            (new LispEnvironment))
          (define env2
            (new LispEnvironment
                 '((foo "foo" "variable"))))
          (define env
            (new EnvironmentStack
                 env1
                 env2))
          (send env set-entry '(foo ("bar" "variable")))
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
                      '((foo bar "variable")))
                 (new LispEnvironment
                      '((bar baz "variable")))))
          (assert-equal
           (send env get 'foo)
           'baz)))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar "variable")))
                 (new LispEnvironment
                      '((bar baz "variable")))))
          (assert-equal
           (send env get-value 'foo)
           'baz)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentPipe
                 (new LispEnvironment
                      '((foo bar "variable")))
                 (new LispEnvironment
                      '((bar baz "variable")))))
          (assert-equal
           (send env get-typed-value 'foo)
           '(baz "variable"))))))

(describe "EnvironmentComposition"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz "variable")))
                 (new LispEnvironment
                      '((foo bar "variable")))))
          (assert-equal
           (send env get 'foo)
           'baz)))
    (it "get-value"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz "variable")))
                 (new LispEnvironment
                      '((foo bar "variable")))))
          (assert-equal
           (send env get-value 'foo)
           'baz)))
    (it "get-typed-value"
        (fn ()
          (define env
            (new EnvironmentComposition
                 (new LispEnvironment
                      '((bar baz "variable")))
                 (new LispEnvironment
                      '((foo bar "variable")))))
          (assert-equal
           (send env get-typed-value 'foo)
           '(baz "variable"))))))

(describe "ThunkedEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new ThunkedEnvironment
                 `((foo
                    ,(thunk (lambda () "bar"))
                    "variable"))))
          (assert-equal
           (send env get 'foo)
           "bar")))))

(describe "JavaScriptEnvironment"
  (fn ()
    (it "get"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env get 'Map)
           Map)))
    (xit "get-local"
         (fn ()
           (define env
             (new JavaScriptEnvironment))
           (assert-equal
            (send env get-local 'Map)
            Map)))
    (it "has"
        (fn ()
          (define env
            (new JavaScriptEnvironment))
          (assert-equal
           (send env has 'Map)
           #t)))
    (xit "has-local"
         (fn ()
           (define env
             (new JavaScriptEnvironment))
           (assert-equal
            (send env has-local 'Map)
            #t)))))

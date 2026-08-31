;;; # Lisp tests
;;;
;;; Tests of some non-Scheme Lisp constructs.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 ;; `set`
 > (describe "set")
 _
 > (compile '(set 'x 1))
 "x = 1;"

 ;; `setq`
 > (describe "setq")
 _
 > (compile '(setq x 1))
 "x = 1;"
 > (compile '(setq x 1
                   y 2))
 "x = 1;

y = 2;"

 ;; `destructuring-bind`
 > (describe "destructuring-bind")
 _
 > (destructuring-bind (x)
                       '(1)
                       (list x))
 '(1)
 > (destructuring-bind (x y)
                       '(1 2)
                       (list x y))
 '(1 2)
 > (destructuring-bind (x y z)
                       '(1 2 3)
                       (list x y z))
 '(1 2 3)
 > (destructuring-bind ((x) y z)
                       '((1) 2 3)
                       (list x y z))
 '(1 2 3)
 > (destructuring-bind (x . y)
                       '(1 2)
                       (list x y))
 '(1 (2))
 > (compile '(destructuring-bind (x y)
                                 '(1 2)
                                 (list x y)))
 "let [x, y] = [1, 2];

[x, y];"
 > (compile '(destructuring-bind (x . y)
                                 '(1 2)
                                 (list x y)))
 "let [x, ...y] = [1, 2];

[x, y];"

 ;; `multiple-values-bind`
 > (describe "multiple-values-bind")
 _
 > (multiple-values-bind (x y)
                         (values 1 2)
                         (list x y))
 '(1 2)
 > (compile '(multiple-values-bind (x y)
                                   (values 1 2)
                                   (list x y)))
 "let [x, y] = [1, 2];

[x, y];"

 ;; `cl/listp`
 > (describe "cl/listp")
 _
 > (cl/listp #t)
 #f
 > (cl/listp '())
 #t
 > (cl/listp '(1 . 2))
 #t
 > (cl/listp '(1 2 3))
 #t

 ;; `el/listp`
 > (describe "el/listp")
 _
 > (el/listp #t)
 #f
 > (el/listp '())
 #t
 > (el/listp '(1 . 2))
 #t
 > (el/listp '(1 2 3))
 #t

 ;; `el/if`
 > (describe "el/if")
 _
 > (el/if #t 1 2)
 1
 > (el/if #f 1 2)
 2
 > (el/if #f 1 2 3)
 3
 > (el/if #f 1 2 3 4)
 4

 ;; `defun`
 > (describe "defun")
 _
 > ((lambda ()
      (defun my-add (x y)
        (+ x y))
      (my-add 2 3)))
 5
 > (let ((my-add (lambda (x y) (+ x y))))
     ((lambda ()
        (defun my-add-2 (x y)
          (my-add x y))
        (my-add-2 2 3))))
 5
 > (let ((my-add (lambda (x y z) (+ x y z))))
     ((lambda ()
        (defun my-add-2 (x y z)
          (my-add x y z))
        (my-add-2 1 2 3))))
 6

 ;; `defmacro`
 > (describe "defmacro")
 _
 > ((lambda ()
      (defmacro my-macro (x)
        x)
      (my-macro 1)))
 1
 > (compile
    '(module m scheme
       (defmacro foo ()
         '(begin))
       (foo)))
 "function foo(exp, env) {
  return [Symbol.for('begin')];
}

foo.ftype = 'macro';"
 > (compile
    '(module m scheme
       (defmacro foo (x)
         x)
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x)
         `(begin ,x))
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  return [Symbol.for('begin'), x];
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x . args)
         x)
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x . args)
         x)
       (define bar
         (foo 1))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

let bar = 1;"
 > (compile
    '(begin
       (defmacro foo (x . args)
         x)
       (define bar
         (foo 1))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

let bar = 1;"
 > (compile
    '(begin
       (define (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define (foo-bar x)
         x)
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define (foo-bar x)
         x)
       (defmacro bar (x)
         (foo-bar 'x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(Symbol.for('x'));
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(begin
       (define (foo-bar x)
         'x)
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return Symbol.for('x');
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(module m scheme
       (define (foo-bar x)
         (keyword? x))
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1)))
    :finline-functions #t)
 "let [keywordp] = (() => {
  function keywordp_(obj) {
    return (typeof obj === 'symbol') && (obj.description.match(new RegExp('^:')) ? true : false);
  }
  return [keywordp_];
})();

function fooBar(x) {
  return keywordp(x);
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = false;"
 > (compile
    '(begin
       (define foo
         (lambda (x)
           x))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let foo = function (x) {
  return x;
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-values (foo)
         (list
          (lambda (x)
            x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let [foo] = [function (x) {
  return x;
}];

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fields (foo)
         (js/obj "foo"
                 (lambda (x)
                   x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let {foo} = {
  foo: function (x) {
    return x;
  }
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fields ((foo foo1))
         (js/obj "foo"
                 (lambda (x)
                   x)))
       (defmacro bar (x)
         (foo1 x))
       (define baz
         (bar 1))))
 "let {foo: foo1} = {
  foo: function (x) {
    return x;
  }
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo1(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define/async (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "async function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define foo
         (async
          (lambda (x)
            x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "async function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fexpr (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "function foo(x) {
  return x;
}

foo.ftype = 'fexpr';

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(Symbol.for('x'));
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(begin
       (define-class Foo ()
         (define/public (foo)
           "foo"))
       (define bar
         (new Foo))
       (defmacro baz (x)
         (send bar foo))
       (define quux
         (baz 1))))
 "class Foo {
  foo() {
    return 'foo';
  }
}

let bar = new Foo();

function baz(exp, env) {
  let [x] = exp.slice(1);
  return bar.foo();
}

baz.ftype = 'macro';

let quux = 'foo';"

 ;; `unwind-protect`
 > (describe "unwind-protect")
 _
 > (unwind-protect 1 2 3)
 1
 > (compile '(unwind-protect (foo) (bar)))
 "try {
  foo();
} finally {
  bar();
}"

 ;; `clj/try`
 > (describe "clj/try")
 _
 > (clj/try
    (/ 1 2)
    (catch Exception e
      "there was an error")
    (finally
      (display "finally")))
 0.5
 > (clj/try
    (/ 1 3)
    (/ 1 2)
    (catch Exception e
      "there was an error")
    (finally
      (display "finally")))
 0.5
 > (compile '(clj/try))
 "try {
}"
 > (compile '(clj/try
              (set! x (/ 2 1))))
 "try {
  x = 2 / 1;
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}"
 > (compile '(clj/try
              (/ 1 2)
              (catch Object e
                (display "there was an error"))
              (finally
                (display "finally"))))
 "try {
  1 / 2;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('finally');
}"
 > (compile '(clj/try
              (/ 1 2)
              (catch Exception e
                (display "there was an error"))
              (finally
                (display "finally"))))
 "try {
  1 / 2;
} catch (e) {
  if (e instanceof Exception) {
    console.log('there was an error');
  } else {
    throw e;
  }
} finally {
  console.log('finally');
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch Object e
                (display "there was an error"))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch MyException e
                (display "there was an error")
                (return #f))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  if (e instanceof MyException) {
    console.log('there was an error');
    return false;
  } else {
    throw e;
  }
} finally {
  console.log('cleanup');
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch Object e
                (display "there was an error")
                (return #f))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
  return false;
} finally {
  console.log('cleanup');
}")

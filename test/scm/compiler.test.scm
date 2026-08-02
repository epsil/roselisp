(require (only-in "../../src/ts/combinators"
                  I))
(require (only-in "../../src/ts/language"
                  LispEnvironment
                  compilation-environment
                  compile
                  compile-modules
                  compile-with-environment
                  definition->macro
                  split-comments))
(require (only-in "../../src/ts/macros"
                  define-macro->lambda-form))
(require (only-in "../../src/ts/parser"
                  read-rose))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; Symbols
 > (describe "Symbols")
 _
 > (compile #t)
 "true;"
 > (compile #f)
 "false;"
 > (compile 'undefined)
 "undefined;"
 > (compile 'js/undefined)
 "undefined;"
 > (compile 'js-undefined)
 "undefined;"
 > (compile 'js/null)
 "null;"
 > (compile 'js-null)
 "null;"
 xit> (compile 'nil)
 "null;"
 xit> (compile 'null)
 "[];"
 > (compile 'foo-bar
            :case "none")
 "foo-bar;"
 > (compile 'foo-bar)
 "fooBar;"
 > (compile 'foo/bar)
 "fooBar;"
 > (compile 'foo!)
 "foox;"
 > (compile 'foo-bar!)
 "fooBarX;"
 > (compile 'foo?)
 "foop;"
 > (compile 'foo-bar?)
 "fooBarP;"
 > (compile '*foo-bar*)
 "starFooBarStar;"
 > (compile ''*foo-bar*)
 "Symbol.for('*foo-bar*');"
 > (compile 'A)
 "A;"
 > (compile '(module m scheme
               (define lst
                 (map symbol? '(a b c)))))
 "let lst = [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')].map(function (x) {
  return typeof x === 'symbol';
});"

 ;; `gensym`
 > (describe "gensym")
 _
 > (compile '(gensym "x"))
 "Symbol('x');"
 > (compile `(define ,(gensym "x") 1))
 "let x = 1;"
 > (compile `(let ((x 0))
               (define ,(gensym "x") 1)))
 "let x = 0;

let x1 = 1;"
 > (compile `(let ((x 0))
               (define ,(gensym "x") 1)
               (let ((x1 0)))))
 "let x = 0;

let x1 = 1;

{
  let x1 = 0;
}"
 xit> (compile (let ((gensym-x (gensym "x")))
                 `(let ((x 0))
                    (define ,gensym-x 1)
                    (let ((x1 0))
                      (define ,gensym-x 1)))))
 "let x = 0;

let x2 = 1;

{
  let x1 = 0;
  let x2 = 1;
}"
 > (compile `(begin
               (define foo
                 ,(gensym "test"))
               (define bar
                 ,(gensym "test"))))
 "let foo = test;

let bar = test1;"

 ;; Strings
 > (describe "Strings")
 _
 > (compile "")
 "'';"
 > (compile "foo")
 "'foo';"
 > (compile "don't")
 "'don\\'t';"
 > (compile "newline
test")
 "'newline\\n' +
  'test';"
 > (compile "newline\ntest")
 "'newline\\n' +
  'test';"
 > (compile "newline
test
three")
 "'newline\\n' +
  'test\\n' +
  'three';"
 xit> (compile "\\t")
 "	;"
 > (compile "\\s")
 "'\\\\s';"
 xit> (compile (js/tag sexp "\"\\\\s\""))
 "'\\\\s';"

 ;; `string-append`
 > (describe "string-append")
 _
 > (compile '(string-append "a"))
 "'a';"
 > (compile '(string-append "a" "b"))
 "'a' + 'b';"

 ;; `js/tag`
 > (describe "js/tag")
 _
 > (compile '(js/tag foo "bar"))
 "foo`bar`;"

 ;; `()`
 > (describe "()")
 _
 > (compile '())
 "[];"

 ;; `list`
 > (describe "list")
 _
 > (compile '(list))
 "[];"
 > (compile '(list 1))
 "[1];"
 > (compile '(list (list 1)))
 "[[1]];"

 ;; `append`
 > (describe "append")
 _
 > (compile '(append))
 "[];"
 > (compile '(append foo))
 "[...foo];"
 > (compile '(append foo bar))
 "[...foo, ...bar];"
 > (compile '(append (list)))
 "[];"
 > (compile '(append (list x)))
 "[x];"
 > (compile '(append '("foo") '("bar")))
 "['foo', 'bar'];"

 ;; `quote`
 > (describe "quote")
 _
 > (compile '(quote x))
 "Symbol.for('x');"
 > (compile '(quote ()))
 "[];"
 > (compile '(quote (1)))
 "[1];"
 > (compile '(quote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quote ((1))))
 "[[1]];"
 > (compile '(quote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
 > (compile '(quote (#t #f)))
 "[true, false];"

 ;; `quasiquote`
 > (describe "quasiquote")
 _
 > (compile '(quasiquote x))
 "Symbol.for('x');"
 > (compile '(quasiquote ()))
 "[];"
 > (compile '(quasiquote (1)))
 "[1];"
 > (compile '(quasiquote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quasiquote ((1 . 2))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote ((1 . (unquote 2)))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote ((1 . (unquote 2))
                          (3 . (unquote 4)))))
 "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]];"
 > (compile '(define test-map-1
               `(("foo" . ,test-fn)
                 ("bar" . ,test-fn))))
 "let testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];"
 > (compile '(quasiquote ((1))))
 "[[1]];"
 > (compile '(quasiquote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
 > (compile '(quasiquote (x y (unquote z))))
 "[Symbol.for('x'), Symbol.for('y'), z];"
 > (compile '(quasiquote (x y (unquote-splicing z))))
 "[Symbol.for('x'), Symbol.for('y'), ...z];"
 > (compile '(quasiquote (x y (quasiquote z))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]];"
 > (compile '(quasiquote (x y (quasiquote (z)))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]];"
 > (compile '(quasiquote (x y (quasiquote ((unquote z))))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]];"
 > (compile '(quasiquote (x y (quasiquote ((unquote-splicing z))))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]];"
 > (compile '(quasiquote ((unquote-splicing x))))
 "[...x];"
 > (compile '(quasiquote ((unquote-splicing x)
                          (unquote-splicing y))))
 "[...x, ...y];"
 > (compile '(set! let-exp
                   (quasiquote
                    (let (((unquote arg-list)
                           (quote (unquote args))))
                      (unquote-splicing body)))))
 "letExp = [Symbol.for('let'), [[argList, [Symbol.for('quote'), args]]], ...body];"

 ;; `begin`
 > (describe "begin")
 _
 > (compile '(begin x y z))
 "x;

y;

z;"
 > (compile '(begin x (begin y z)))
 "x;

y;

z;"
 > (compile '(begin x y z)
            :as 'expression)
 "(() => {
  x;
  y;
  return z;
})()"
 > (compile
    '(begin
       ;; Redefine core functions (nonsensically).
       (define (and x y)
         (or x y))
       (define (or x y) x)
       (and x (or y z))))
 "function and(x, y) {
  return or(x, y);
}

function or(x, y) {
  return x;
}

and(x, or(y, z));"

 ;; `+`
 > (describe "+")
 _
 > (compile '(+ x 1))
 "x + 1;"
 > (compile '(+ x 1 2))
 "x + 1 + 2;"

 ;; `-`
 > (describe "-")
 _
 > (compile '(- x))
 "-x;"
 xit> (compile '(- (- x)))
 "x;"
 > (compile '(- x 1))
 "x - 1;"
 > (compile '(- x 1 2))
 "x - 1 - 2;"

 ;; `mod`
 > (describe "mod")
 _
 > (compile '(mod x y))
 "x % y;"

 ;; `=`
 > (describe "=")
 _
 > (compile '(= 1 1))
 "1 === 1;"
 > (compile '(= x y))
 "x === y;"

 ;; `<`
 > (describe "<")
 _
 > (compile '(< 1))
 "true;"
 > (compile '(< 1 2))
 "1 < 2;"
 > (compile '(< 1 2 3))
 "(1 < 2) && (2 < 3);"

 ;; `>`
 > (describe ">")
 _
 > (compile '(> 1))
 "true;"
 > (compile '(> 2 1))
 "2 > 1;"
 > (compile '(> 3 2 1))
 "(3 > 2) && (2 > 1);"

 ;; `not`
 > (describe "not")
 _
 > (compile '(not (and x y)))
 "!(x && y);"
 > (compile '(not (= 1 2)))
 "1 !== 2;"
 > (compile '(not (> 1 2)))
 "!(1 > 2);"
 > (compile '(not (f x)))
 "!f(x);"
 xit> (compile '(and (not (f x)) (not (g y))))
 "!f(x) && !g(y);"

 ;; `and`
 > (describe "and")
 _
 > (compile '(and))
 "true;"
 > (compile '(and x))
 "x;"
 > (compile '(and x y))
 "x && y;"
 xit> (compile '(and x y z))
 "x && y && z;"
 xit> (compile '(and x y (w z)))
 "x && y && w(z);"
 xit> (compile '(and x y (or w z)))
 "x && y && (w || z);"

 ;; `or`
 > (describe "or")
 _
 > (compile '(or))
 "false;"
 > (compile '(or x))
 "x;"
 > (compile '(or x y))
 "x || y;"
 xit> (compile '(or x y z))
 "x || y || z;"

 ;; `if`
 > (describe "if")
 _
 > (compile '(if x
                 y
                 z))
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(if x
                 y)
            :as 'expression)
 "x ? y : undefined"
 > (compile '(if x y z)
            :as 'expression)
 "x ? y : z"
 > (compile '(if x
                 y
                 z)
            :as 'return)
 "if (x) {
  return y;
} else {
  return z;
}"
 > (compile '(if "foo"
                 "bar"
                 "baz")
            :as 'expression)
 "'foo' ? 'bar' : 'baz'"
 > (compile '(if x
                 (begin
                   y
                   z)
                 w)
            :as 'return)
 "if (x) {
  y;
  return z;
} else {
  return w;
}"
 > (compile '(if (set! x y)
                 z
                 w))
 "if ((x = y)) {
  z;
} else {
  w;
}"
 > (compile '(if (set! x y)
                 z
                 w)
            :as 'return)
 "if ((x = y)) {
  return z;
} else {
  return w;
}"
 > (compile '(if (set!-values (x) y)
                 z
                 w))
 "if (([x] = y)) {
  z;
} else {
  w;
}"

 > (compile '(if (set!-fields (x) y)
                 z
                 w))
 "if (({x} = y)) {
  z;
} else {
  w;
}"

 ;; `when`
 > (describe "when")
 _
 > (compile '(when x
               y
               z))
 "if (x) {
  y;
  z;
}"
 > (compile
    '(when (> (array-list-length args) 0)
       (set! args (.concat (.slice args 0 (- (array-list-length args) 1))
                           (aref args (- (array-list-length args) 1))))))
 "if (args.length > 0) {
  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);
}"

 ;; `unless`
 > (describe "unless")
 _
 > (compile '(unless x
               y z))
 "if (!x) {
  y;
  z;
}"

 ;; `cond`
 > (describe "cond")
 _
 > (compile '(cond
              (x
               y))
            :as 'return)
 "if (x) {
  return y;
}"
 > (compile '(cond
              (x
               y))
            :as 'expression)
 "x ? y : undefined"
 > (compile '(cond
              (x
               y)
              (else
               z))
            :as 'expression)
 "x ? y : z"
 > (compile '(cond
              (x
               y)
              (else
               w
               z))
            :as 'expression)
 "x ? y : (() => {
  w;
  return z;
})()"
 > (compile '(cond
              (x
               y)
              (else
               z))
            :as 'return)
 "if (x) {
  return y;
} else {
  return z;
}"
 xit> (compile '(cond
                 ((set! x y)
                  z)
                 (else
                  w))
               :as 'return)
 "if ((x = y)) {
  return z;
} else {
  return w;
}"

 ;; `let`
 > (describe "let")
 _
 > (compile '(let (x)))
 "let x;"
 > (compile '(let (x)
               x)
            :as 'return)
 "let x;

return x;"
 > (compile '(let (x)
               x)
            :as 'expression)
 "(() => {
  let x;
  return x;
})()"
 > (compile '(let (x)
               x)
            :as 'return
            :to 'typescript)
 "let x: any;

return x;"
 > (compile '(let ((x 1))
               x)
            :as 'return)
 "let x = 1;

return x;"
 > (compile '(let ((x 1))
               x)
            :as 'return
            :to 'typescript)
 "let x: any = 1;

return x;"
 xit> (compile '(let ((a 1))
                  (+ (let ((a 2)) a) a)))
 "let a = 1;

(() => {
  let a = 2;
  return a;
})() + a;"
 > (compile
    '(let ((compose (lambda (f g)
                      (lambda (x)
                        (f (g x)))))
           (square (lambda (x) (* x x)))
           (add1 (lambda (x) (+ x 1))))
       (display ((compose square add1) (add1 4)))))
 "let compose = function (f, g) {
  return function (x) {
    return f(g(x));
  };
};

let square = function (x) {
  return x * x;
};

let add1 = function (x) {
  return x + 1;
};

console.log(compose(square, add1)(add1(4)));"
 > (compile
    '(let ((and (lambda (x y)
                  (if x (if y #t #f) #f))))
       (and x y)))
 "let and = function (x, y) {
  if (x) {
    if (y) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
};

and(x, y);"
 ;; FIXME: This test is incorrect.
 ;; See the one below.
 xit> (compile '(begin
                  x
                  (let ((x 1))
                    x)))
 "x;

let x: any = 1;

return x;"
 ;; FIXME: Make this test pass.
 xit> (compile '(begin
                  x
                  (let ((x 1))
                    x)))
 "x;

{
  let x: any = 1;
  x;
}"
 > (compile '(begin
               (let ((x 1))
                 (display x))
               (let ((x 1))
                 (display x))))
 "let x = 1;

console.log(x);

{
  let x = 1;
  console.log(x);
}"
 > (compile '(cond
              (foo
               bar)
              (else
               x
               (let ((x 1))
                 x)))
            :as 'return
            :to 'typescript)
 "if (foo) {
  return bar;
} else {
  x;
  let x: any = 1;
  return x;
}"
 > (compile
    '(define make-compilation-evaluator
       (memoize
        (lambda (env (options (js/obj)))
          (let ((language (oget options "language")))
            (set! language (or language default-language))
            (let ((compilation-env (or (.get compilation-map
                                             language)
                                       javascript-env)))
              (new CompilationEvaluator
                   env
                   compilation-env
                   options))))))
    :to 'typescript)
 "let makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {
  let language: any = options['language'];
  language = language || defaultLanguage;
  let compilationEnv: any = compilationMap.get(language) || javascriptEnv;
  return new CompilationEvaluator(env, compilationEnv, options);
});"
 > (compile '(cond
              (foo
               (let ((x #t))
                 x))
              (else
               #f))
            :as 'return)
 "if (foo) {
  let x = true;
  return x;
} else {
  return false;
}"

 ;; `let-values`
 > (describe "let-values")
 _
 > (compile '(let-values ((value (foo bar baz)))
               value)
            :as 'return)
 "let value = foo(bar, baz);

return value;"
 > (compile '(let-values (((value) (foo bar baz)))
               value)
            :as 'return)
 "let [value] = foo(bar, baz);

return value;"
 > (compile '(let-values (((value) (foo bar baz)))
               value)
            :as 'return
            :to 'typescript)
 "let [value]: any[] = foo(bar, baz);

return value;"
 > (compile '(let-values (((x . fs) args))
               (.reduce fs (lambda (acc f) (f acc)) x)))
 "let [x, ...fs] = args;

fs.reduce(function (acc, f) {
  return f(acc);
}, x);"
 > (compile '(let-values (((x . fs) args))
               (.reduce fs (lambda (acc f) (f acc)) x))
            :to 'typescript)
 "let [x, ...fs]: any[] = args;

fs.reduce(function (acc: any, f: any): any {
  return f(acc);
}, x);"
 > (compile '(let-values (((value1) (foo bar))
                          ((value2) (bar baz)))
               (list value1 value2))
            :as 'return)
 "let [value1] = foo(bar);

let [value2] = bar(baz);

return [value1, value2];"
 > (compile '(begin
               value
               (let-values ((value (foo bar baz)))
                 value))
            :as 'return)
 "value;

let value = foo(bar, baz);

return value;"

 ;; `let-fields`
 > (describe "let-fields")
 _
 > (compile '(let-fields (((prop) obj))
                         prop))
 "let {prop} = obj;

prop;"

 ;; `lambda`
 > (describe "lambda")
 _
 > (compile '(lambda (x)
               x))
 "function (x) {
  return x;
};"
 > (compile '(lambda (x)
               x)
            :to 'typescript)
 "function (x: any): any {
  return x;
};"
 > (compile '(lambda args
               args))
 "function (...args) {
  return args;
};"
 > (compile '(lambda (x . args)
               args))
 "function (x, ...args) {
  return args;
};"
 > (compile '(lambda (x y . args)
               args))
 "function (x, y, ...args) {
  return args;
};"
 > (compile '(lambda (x)
               (let ((x 1))
                 x)))
 "function (x) {
  {
    let x = 1;
    return x;
  }
};"
 > (compile '(lambda (x)
               (let ((y 1))
                 y)))
 "function (x) {
  let y = 1;
  return y;
};"
 > (compile '(lambda (given (surname "Smith"))
               (string-append
                "Hello, "
                given
                " "
                surname)))
 "function (given, surname = 'Smith') {
  return 'Hello, ' + given + ' ' + surname;
};"
 > (compile '(lambda (given (surname "Smith"))
               (string-append
                "Hello, "
                given
                " "
                surname))
            :to 'typescript)
 "function (given: any, surname: any = 'Smith'): any {
  return 'Hello, ' + given + ' ' + surname;
};"
 > (compile '(lambda (arg (options (js/obj)))
               arg)
            :to 'typescript)
 "function (arg: any, options: any = {}): any {
  return arg;
};"
 xit> (compile '(lambda (this arg)
                  arg)
               :to 'typescript)
 "function (arg: any): any {
  return arg;
};"
 xit> (compile '(lambda (this . args)
                  args)
               :to 'typescript)
 "function (...args: any[]): any {
  return args;
};"
 xit> (compile '(lambda (this arg)
                  arg)
               :to 'typescript)
 "function (this: any, arg: any): any {
  return arg;
};"
 xit> (compile '(lambda (this . args)
                  args)
               :to 'typescript)
 "function (this: any, ...args: any[]): any {
  return args;
};"

 ;; `funcall`
 > (describe "funcall")
 _
 > (compile '(funcall f x))
 "f(x);"
 > (compile '(funcall f x y))
 "f(x, y);"

 ;; `apply`
 > (describe "apply")
 _
 > (compile '(apply f args))
 "f(...args);"
 > (compile '(apply f x args))
 "f(x, ...args);"
 > (compile '(apply new Foo args))
 "new Foo(...args);"
 > (compile '(apply new Foo x y args))
 "new Foo(x, y, ...args);"
 xit> (compile '(apply send obj method args))
 "obj.method(...args);"
 > (compile '(apply (get-field method obj) args))
 "obj.method(...args);"
 > (compile '(apply (.-method obj) args))
 "obj.method(...args);"

 ;; `define`
 > (describe "define")
 _
 > (compile '(define x))
 "let x;"
 > (compile '(define x)
            :to 'typescript)
 "let x: any;"
 > (compile '(define x 1))
 "let x = 1;"
 > (compile '(define x 1)
            :to 'typescript)
 "let x: any = 1;"
 xit> (compile '(define I
                  (lambda (x)
                    x)))
 "function I(x) {
  return x;
}"
 > (compile '(define I
               (memoize
                (lambda (x)
                  x))))
 "let I = memoize(function (x) {
  return x;
});"
 > (compile '(define (identity-function x)
               x))
 "function identityFunction(x) {
  return x;
}"
 > (compile '(define (I x)
               x))
 "function I(x) {
  return x;
}"
 > (compile '(define (K x y)
               x))
 "function K(x, y) {
  return x;
}"
 > (compile '(define (S f g x)
               (f x (g x))))
 "function S(f, g, x) {
  return f(x, g(x));
}"
 > (compile '(define (S f g x)
               ((f x) (g x))))
 "function S(f, g, x) {
  return f(x)(g(x));
}"
 > (compile '(define (C f x y)
               (f y x)))
 "function C(f, x, y) {
  return f(y, x);
}"
 > (compile '(define (U f)
               (f f)))
 "function U(f) {
  return f(f);
}"
 > (compile '(define (A f . args)
               (apply f args)))
 "function A(f, ...args) {
  return f(...args);
}"
 > (compile '(define (A f . args)
               (apply f args))
            :to 'typescript)
 "function A(f: any, ...args: any[]): any {
  return f(...args);
}"
 > (compile
    '(define (Q . args)
       (cond
        ((= (.-length args) 0)
         #u)
        ((= (.-length args) 1)
         (aref args 0))
        (else
         (let ((fs (.slice args 0 -1))
               (x (aref args (- (.-length args) 1))))
           (.reduce fs (lambda (acc f) (f acc)) x))))))
 "function Q(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    let fs = args.slice(0, -1);
    let x = args[args.length - 1];
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}"
 > (compile
    '(define (T . args)
       (cond
        ((= (.-length args) 0)
         #u)
        ((= (.-length args) 1)
         (aref args 0))
        (else
         (let-values (((x . fs) args))
           (.reduce fs (lambda (acc f) (f acc)) x))))))
 "function T(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    let [x, ...fs] = args;
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}"
 > (compile
    '(define (Y f)
       ((lambda (future)
          (f (lambda (arg)
               ((future future) arg))))
        (lambda (future)
          (f (lambda (arg)
               ((future future) arg)))))))
 "function Y(f) {
  return (function (future) {
    return f(function (arg) {
      return future(future)(arg);
    });
  })(function (future) {
    return f(function (arg) {
      return future(future)(arg);
    });
  });
}"
 > (compile '(define (compose f g)
               (lambda (x)
                 (f (g x)))))
 "function compose(f, g) {
  return function (x) {
    return f(g(x));
  };
}"
 > (compile '(define (foo)
               (set! x (+ x 1))
               (set! y (+ y 1))))
 "function foo() {
  x++;
  return ++y;
}"
 > (compile
    '(define (mapGet map path)
       (let-values (((value) (mapGet2 map path)))
         value)))
 "function mapGet(map, path) {
  let [value] = mapGet2(map, path);
  return value;
}"
 > (compile
    '(define _ (js/obj "dash" #t)))
 "let _ = {
  dash: true
};"
 > (compile
    '(define __ (js/obj "dash" #t)))
 "let __ = {
  dash: true
};"
 xit> (compile
       '(lambda (env (options (js/obj)))
          (let ((language (oget options "language")))
            (set! language (or language default-language))
            (let ((compilation-env (or (.get compilation-map
                                             language)
                                       javascript-env)))
              (new CompilationEvaluator
                   env
                   compilation-env
                   options)))))
 "function (env: any, options: any = {}): any {
  let language: any = options['language'];
  language = language || (default-language);
  {
    {
      let compilation-env: any = (compilation-map.get(language)) || (javascript-env);
      return new CompilationEvaluator(env, compilation-env, options);
    }
  }
}"
 > (compile
    '(define (add-matrix m1 m2)
       (let ((l1 (array-list-length m1))
             (l2 (array-list-length m2)))
         (let ((matrix (make-matrix l1 l2)))
           (for ((i (range 0 l1)))
             (for ((j (range 0 l2)))
               (set! (aget (aget matrix j) i)
                     (+ (aget (aget m1 j) i)
                        (aget (aget m2 j) i)))))
           matrix))))
 "function addMatrix(m1, m2) {
  let l1 = m1.length;
  let l2 = m2.length;
  let matrix = makeMatrix(l1, l2);
  for (let i = 0; i < l1; i++) {
    for (let j = 0; j < l2; j++) {
      matrix[j][i] = m1[j][i] + m2[j][i];
    }
  }
  return matrix;
}"

 ;; `define-values`
 > (describe "define-values")
 _
 > (compile
    '(define-values value
       (foo bar baz)))
 "let value = foo(bar, baz);"
 > (compile
    '(define-values (value)
       (foo bar baz)))
 "let [value] = foo(bar, baz);"
 > (compile
    '(define-values (value)
       (foo bar baz))
    :to 'typescript)
 "let [value]: any[] = foo(bar, baz);"
 > (compile
    '(define-values (_ _ value)
       (foo bar baz))
    :to 'typescript)
 "let [, , value]: any[] = foo(bar, baz);"
 > (compile
    '(define-values (_ __ value)
       :hole-marker __
       (foo bar baz))
    :to 'typescript)
 "let [_, , value]: any[] = foo(bar, baz);"
 > (compile
    '(module m scheme
       (define (foo)
         (define xs
           '(1 2 3 4))
         (define-values (x . rest)
           xs)
         (append rest '(5))))
    :to 'typescript)
 "function foo(): any {
  let xs: any = [1, 2, 3, 4];
  let [x, ...rest]: any[] = xs;
  return [...rest, 5];
}"

 ;; `define-fields`
 > (describe "define-fields")
 _
 > (compile '(define-fields (prop) obj))
 "let {prop} = obj;"
 > (compile '(define-fields (prop) obj))
 "let {prop} = obj;"
 > (compile '(define-fields ((x y) z) obj))
 "let {x: y, z} = obj;"
 > (compile '(define-fields ((x y) z) obj))
 "let {x: y, z} = obj;"
 > (compile
    '(module m scheme
       (define (foo)
         (define obj
           (js/obj))
         (define-fields (x rest)
           obj)
         (append rest '(5))))
    :to 'typescript)
 "function foo(): any {
  let obj: any = {};
  let {x, rest} = obj;
  return [...rest, 5];
}"
 > (compile
    '(module m scheme
       (define (foo)
         (define obj
           (js/obj))
         (define-fields ((rest r) x)
           obj)
         (list r x)))
    :to 'typescript)
 "function foo(): any {
  let obj: any = {};
  let {rest: r, x} = obj;
  return [r, x];
}"

 ;; `set!-fields'
 > (describe "set!-fields")
 _
 > (compile '(set!-fields (prop) obj))
 "({prop} = obj);"

 ;; `set!`
 > (describe "set!")
 _
 > (compile '(set! x 1))
 "x = 1;"
 > (compile '(set! x (add1 x))
            :as 'expression)
 "++x"
 > (compile '(set! x (sub1 x))
            :as 'expression)
 "--x"
 > (compile '(set! x (+ x 1))
            :as 'expression)
 "++x"
 > (compile '(set! x (+ x 1)))
 "x++;"
 > (compile '(set! x (+ x 1))
            :as 'return)
 "return ++x;"

 ;; `setq`
 > (describe "setq")
 _
 > (compile '(setq x 1))
 "x = 1;"

 ;; `set!-values`
 > (describe "set!-values")
 _
 > (compile
    '(set!-values (value) (foo bar baz)))
 "[value] = foo(bar, baz);"
 > (compile
    '(set!-values (_ value) (foo bar baz)))
 "[, value] = foo(bar, baz);"
 > (compile
    '(set!-values (_ __ value)
                  :hole-marker __
                  (foo bar baz)))
 "[_, , value] = foo(bar, baz);"

 ;; `aget`
 > (describe "aget")
 _
 > (compile '(aget args 0))
 "args[0];"
 > (compile '(aget args 0 1))
 "args[0][1];"

 ;; `aref`
 > (describe "aref")
 _
 > (compile '(aref args 0))
 "args[0];"
 > (compile '(aref args 0 1))
 "args[0][1];"

 ;; `aset`
 > (describe "aset")
 _
 > (compile '(aset args 0 1))
 "args[0] = 1;"

 ;; `set!...aref`
 > (describe "set!...aref!")
 _
 > (compile '(set! (aref args 0) 1))
 "args[0] = 1;"

 ;; `first`
 > (describe "first")
 _
 > (compile '(first x))
 "x[0];"

 ;; `last`
 > (describe "last")
 _
 xit> (compile '(last x))
 "x[x.length - 1];"

 ;; `nth`
 > (describe "nth")
 _
 xit> (compile '(nth 1 x))
 "x[1];"
 xit> (compile '(nth 2 (nth 1 x)))
 "x[1][2];"

 ;; `nthcdr`
 > (describe "nthcdr")
 _
 xit> (compile '(nthcdr 1 x))
 "x.slice(1);"

 ;; `drop`
 > (describe "drop")
 _
 > (compile '(drop x 1))
 "x.slice(1);"

 ;; `drop-right`
 > (describe "drop-right")
 _
 > (compile '(drop-right x 1))
 "x.slice(0, -1);"

 ;; `length`
 > (describe "length")
 _
 > (compile '(array-list-length x))
 "x.length;"

 ;; `get-field`
 > (describe "get-field")
 _
 > (compile '(get-field length arr))
 "arr.length;"
 > (compile '(get-field (- len 1) arr))
 "arr[len - 1];"

 ;; `set-field!`
 > (describe "set-field!")
 _
 > (compile '(set-field! prop obj val))
 "obj.prop = val;"
 > (compile
    '(set-field! def-method
                 generic-function
                 (lambda (arglist function-definition)
                   (let ((entry (list arglist function-definition)))
                     (push! (get-field methods generic-function) entry)
                     generic-function))))
 "genericFunction.defMethod = function (arglist, functionDefinition) {
  let entry = [arglist, functionDefinition];
  genericFunction.methods.unshift(entry);
  return genericFunction;
};"

 ;; `send`
 > (describe "send")
 _
 > (compile '(send map get "foo"))
 "map.get('foo');"

 ;; `send/apply`
 > (describe "send/apply")
 _
 > (compile '(send/apply map get foo))
 "map.get(...foo);"
 > (compile '(send/apply map get '("foo")))
 "map.get('foo');"

 ;; `.`
 > (describe ".")
 _
 > (compile '(. map get "foo"))
 "map.get('foo');"
 > (compile '(.get map "foo"))
 "map.get('foo');"
 > (compile '(.-length arr))
 "arr.length;"

 ;; `memq?`
 > (describe "memq?")
 _
 > (compile '(memq? 2 (list 1 2 3 4)))
 "[1, 2, 3, 4].includes(2);"
 > (compile '(memq? (+ 1 1) (list 1 2 3 4)))
 "[1, 2, 3, 4].includes(1 + 1);"

 ;; `member?`
 > (describe "member?")
 _
 > (compile '(member? 2 (list 1 2 3 4) f))
 "[1, 2, 3, 4].findIndex(function (x) {
  return f(2, x);
}) >= 0;"
 ;; TODO: Better compilation of this case:
 ;; `v` should be stored in a local variable.
 > (compile '(member? (+ 1 1) (list 1 2 3 4) f))
 "[1, 2, 3, 4].findIndex(function (x) {
  return f(1 + 1, x);
}) >= 0;"

 ;; `map`
 > (describe "map")
 _
 > (compile '(map f x))
 "x.map(function (x) {
  return f(x);
});"
 > (compile '(map (lambda (x) x) x))
 "x.map(function (x) {
  return x;
});"
 ;; We can't compile this to `x.map(g(y))` because
 ;; we need to ensure that the function is only
 ;; called with a single argument, and JavaScript's
 ;; `.map()` method passes multiple arguments.
 > (compile '(map (g y) x))
 "x.map((function (f) {
  return function (x) {
    return f(x);
  };
})(g(y)));"

 ;; `foldl`
 > (describe "foldl")
 _
 > (compile '(foldl f v l))
 "l.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldl (lambda (x acc)
                      (f x acc))
                    v
                    l))
 "l.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldl + 0 '(1 2 3 4)))
 "[1, 2, 3, 4].reduce(function (acc, x) {
  return x + acc;
}, 0);"

 ;; `foldr`
 > (describe "foldr")
 _
 xit> (compile '(foldr f v x))
 "x.reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(f), v);"
 xit> (compile '(foldr (f g) v x))
 "x.reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(f(g)), v);"
 xit> (compile '(foldr cons '() '(1 2 3 4)))
 "[1, 2, 3, 4].reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(cons), []);"

 ;; `for`
 > (describe "for")
 _
 > (compile '(for ((x '(1 2 3)))
               (display x)))
 "for (let x of [1, 2, 3]) {
  console.log(x);
}"
 > (compile '(for ((x '(1 2 3)))
               (break)))
 "for (let x of [1, 2, 3]) {
  break;
}"
 > (compile '(for ((x '(1 2 3)))
               (continue)))
 "for (let x of [1, 2, 3]) {
  continue;
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((x 1))
                 (display x))))
 "for (let x of [1, 2, 3]) {
  {
    let x = 1;
    console.log(x);
  }
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((y 1))
                 (display x y))))
 "for (let x of [1, 2, 3]) {
  let y = 1;
  console.log(x, y);
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((y 1))
                 (display y))
               (display x)))
 "for (let x of [1, 2, 3]) {
  let y = 1;
  console.log(y);
  console.log(x);
}"
 > (compile '(for ((i (range 0 10)))
               (display x)))
 "for (let i = 0; i < 10; i++) {
  console.log(x);
}"
 > (compile '(for ((i (range 0 10)))
               (display x))
            :to 'typescript)
 "for (let i: any = 0; i < 10; i++) {
  console.log(x);
}"
 > (compile '(for ((i (range 1 10 2)))
               (display x)))
 "for (let i = 1; i < 10; i = i + 2) {
  console.log(x);
}"
 > (compile '(for ((i (range 10 1 -1)))
               (display x)))
 "for (let i = 10; i > 1; i--) {
  console.log(x);
}"
 > (compile '(for ((i (range 10 1 -2)))
               (display x)))
 "for (let i = 10; i > 1; i = i - 2) {
  console.log(x);
}"
 > (compile '(for ((i (range 0 (+ 1 1))))
               (display i)))
 "let _end = 1 + 1;

for (let i = 0; i < _end; i++) {
  console.log(i);
}"
 > (compile '(begin
               (for ((i (range 0 (+ 1 1))))
                 (display i))
               (for ((j (range 0 (+ 2 2))))
                 (display j))))
 "let _end = 1 + 1;

for (let i = 0; i < _end; i++) {
  console.log(i);
}

let _end1 = 2 + 2;

for (let j = 0; j < _end1; j++) {
  console.log(j);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (display i)))
 "let _start = 1 + 1;

let _end = 2 + 2;

for (let i = _start; i < _end; i++) {
  console.log(i);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (display i))
            :to 'typescript)
 "let _start: any = 1 + 1;

let _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  console.log(i);
}"
 > (compile '(let ((_start 0)
                   (_end 0))
               (for ((i (range (+ 1 1) (+ 2 2))))
                 (display i)))
            :to 'typescript)
 "let _start: any = 0;

let _end: any = 0;

let _start1: any = 1 + 1;

let _end1: any = 2 + 2;

for (let i: any = _start1; i < _end1; i++) {
  console.log(i);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (for ((j (range (+ 3 3) (+ 4 4))))
                 (display j)))
            :to 'typescript)
 "let _start: any = 1 + 1;

let _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  let _start1: any = 3 + 3;
  let _end1: any = 4 + 4;
  for (let j: any = _start1; j < _end1; j++) {
    console.log(j);
  }
}"
 > (compile '(define (foo)
               (for ((x '(1 2 3)))
                 (display x))))
 "function foo() {
  for (let x of [1, 2, 3]) {
    console.log(x);
  }
}"

 ;; `do`
 > (describe "do")
 _
 > (compile '(do ()
                 ((not (< (array-list-length result) 3)))
               (display result)))
 "while (result.length < 3) {
  console.log(result);
}"
 xit> (compile '(do ((*do-result* (display result)))
                    ((not (< (array-list-length result) 3)))))
 "do {
  console.log(result);
} while (result.length < 3);"

 ;; `js/while`
 > (describe "js/while")
 _
 > (compile '(js/while (< (array-list-length result) 3)
               (display result)))
 "while (result.length < 3) {
  console.log(result);
}"
 > (compile '(js/while (begin
                         (set! x (- x 1))
                         (> x 0))
               (display x)))
 "while ((() => {
  x--;
  return x > 0;
})()) {
  console.log(x);
}"

 ;; `js/do-while`
 > (describe "js/do-while")
 _
 > (compile '(js/do-while ((display result))
                          (< (array-list-length result) 3)))
 "do {
  console.log(result);
} while (result.length < 3);"
 > (compile '(js/do-while ((foo)
                           (display result))
                          (< (array-list-length result) 3)))
 "do {
  foo();
  console.log(result);
} while (result.length < 3);"
 > (compile '(js/while (< (array-list-length result) 3)
               (display result)))
 "while (result.length < 3) {
  console.log(result);
}"

 ;; `js/obj`
 > (describe "js/obj")
 _
 > (compile '(js/obj))
 "({});"
 > (compile '(js/obj foo "bar"))
 "({
  [foo]: 'bar'
});"
 > (compile '(js/obj "foo" "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj "foo bar" "foo bar"))
 "({
  'foo bar': 'foo bar'
});"
 > (compile '(js/obj "foo" (js/obj "bar" "baz")))
 "({
  foo: {
    bar: 'baz'
  }
});"
 > (compile '(js/obj "foo" (js/obj "foo" "foo")
                     "bar" (js/obj "bar" "bar")))
 "({
  foo: {
    foo: 'foo'
  },
  bar: {
    bar: 'bar'
  }
});"
 > (compile '(js/obj "foo" (js/obj)
                     "bar" (js/obj "bar" "bar")
                     "baz" (js/obj "baz" "baz")))
 "({
  foo: {},
  bar: {
    bar: 'bar'
  },
  baz: {
    baz: 'baz'
  }
});"

 ;; `js/obj?`
 > (describe "js/obj?")
 _
 > (compile '(js/obj? x))
 "(x !== null) && (typeof x === 'object');"

 ;; `js/obj-append`
 > (describe "js/obj-append")
 _
 > (compile '(js/obj-append
              obj
              (js/obj "foo" "bar")))
 "({
  ...obj,
  foo: 'bar'
});"

 ;; `js/keys`
 > (describe "js/keys")
 _
 > (compile '(js/keys x))
 "Object.keys(x);"

 ;; `js/delete`
 > (describe "js/delete")
 _
 > (compile '(js/delete x))
 "delete x;"

 ;; `class`
 > (describe "class")
 _
 > (compile '(class ()
               (define/public (bar)
                 "bar")))
 "class {
  bar() {
    return 'bar';
  }
}"

 ;; `define-class`
 > (describe "define-class")
 _
 > (compile '(define-class Foo))
 "class Foo {
}"
 > (compile '(define-class Foo ()
               (define/public (bar)
                 "bar")))
 "class Foo {
  bar() {
    return 'bar';
  }
}"
 > (compile '(define-class Foo ()
               (define/public (bar)
                 "bar")
               (define/public (baz)
                 "baz")))
 "class Foo {
  bar() {
    return 'bar';
  }

  baz() {
    return 'baz';
  }
}"
 > (compile '(define-class Foo ()
               (define/public bar)
               (define/public baz "baz")
               (define/public (quux) "quux")))
 "class Foo {
  bar;

  baz = 'baz';

  quux() {
    return 'quux';
  }
}"
 > (compile '(define-class Foo ()
               (define x)
               (define/public (constructor x)
                 (super)
                 (set! (.-x this) x))
               (define/public (bar)
                 (.-x this))))
 "class Foo {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}"
 > (compile '(define-class Foo ()
               (define x)
               (define/public (constructor x)
                 (super)
                 (set! (.-x this) x))
               (define/public (bar)
                 (.-x this)))
            :to 'typescript)
 "class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}"
 > (compile '(define-class Foo ()
               (define/public x)
               (define/public (constructor . args)
                 (super)
                 (set! (.-stack this) args))
               (define/public (bar)
                 (.-x this)))
            :to 'typescript)
 "class Foo {
  x: any;

  constructor(...args: any[]) {
    super();
    this.stack = args;
  }

  bar(): any {
    return this.x;
  }
}"
 > (compile '(define-class Foo (Object)
               (define/public x)
               (define/public (constructor x)
                 (super)
                 (set! (.-x this) x))
               (define/public (bar)
                 (.-x this))))
 "class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}"
 > (compile '(define-class Foo (Object)
               (define/public x)
               (define/public (constructor x)
                 (super)
                 (set! (.-x this) x))
               (define/public (bar)
                 (.-x this))))
 "class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}"
 > (compile '(define-class Foo (Object)
               (define/private x)
               (define/public (constructor x)
                 (super)
                 (set! (.-x this) x))
               (define/private (bar)
                 (.-x this)))
            :to 'typescript)
 "class Foo extends Object {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}"
 > (compile '(define-class Foo ()
               (public x)
               (define x)
               (public constructor)
               (define (constructor x)
                 (super)
                 (set! (.-x this) x))
               (public bar)
               (define (bar)
                 (.-x this)))
            :to 'typescript)
 "class Foo {
  x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}"
 > (compile '(define-class Foo ()
               (private x)
               (define x)
               (define (constructor x)
                 (super)
                 (set! (.-x this) x))
               (private bar)
               (define (bar)
                 (.-x this)))
            :to 'typescript)
 "class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}"
 > (compile '(define-class Foo ()
               (define/public arr)
               (define/public (constructor arr)
                 (set-field! arr this arr))
               (define/public (nth i)
                 (aget (get-field arr this) i))))
 "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  nth(i) {
    return this.arr[i];
  }
}"
 > (compile '(define-class Foo ()
               (define/public arr)
               (define (constructor arr)
                 (set-field! arr this arr))
               (define/generator (generator)
                 (for ((x (get-field arr this)))
                   (yield x)))))
 "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *generator() {
    for (let x of this.arr) {
      yield x;
    }
  }
}"
 > (compile '(define-class Foo ()
               (define/public arr)
               (define (constructor arr)
                 (set-field! arr this arr))
               (define/generator ((get-field iterator Symbol))
                 (for ((x (get-field arr this)))
                   (yield x)))))
 "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *[Symbol.iterator]() {
    for (let x of this.arr) {
      yield x;
    }
  }
}"

 ;; `define`...`class`
 > (describe "define...class")
 _
 > (compile '(define Foo
               (class object%)))
 "class Foo {
}"
 > (compile '(define Foo
               (class Bar)))
 "class Foo extends Bar {
}"

 ;; `js`
 > (describe "js")
 _
 > (compile '(js "1"))
 "1"
 > (compile '(js "function I(x) { return x; }"))
 "function I(x) { return x; }"

 ;; `make-hash`
 > (describe "make-hash")
 _
 > (compile '(make-hash))
 "new Map();"
 > (compile '(make-hash
              '(("foo" . "bar")
                ("baz" . "quux"))))
 "new Map([['foo', 'bar'], ['baz', 'quux']]);"
 > (compile '(make-hash
              '(("foo" "bar")
                ("baz" "quux"))))
 "new Map([['foo', ['bar']], ['baz', ['quux']]]);"
 > (compile '(make-hash
              `(("foo" . "bar")
                ("baz" . "quux"))))
 "new Map([['foo', 'bar'], ['baz', 'quux']]);"
 > (compile '(make-hash
              `(("foo" . "bar")
                ("baz" . "quux")
                (unquote-splicing
                 (hash->list xyzzy)))))
 "new Map([['foo', 'bar'], ['baz', 'quux'], ...xyzzy.entries()]);"
 xit> (compile '(make-hash
                 (append
                  `(("foo" . "bar")
                    ("baz" . "quux"))
                  (hash->list xyzzy))))
 "new Map([...[['foo', 'bar'], ['baz', 'quux']], ...xyzzy.entries()]);"
 > (compile '(make-hash
              `(("foo" "bar")
                ("baz" "quux"))))
 "new Map([['foo', ['bar']], ['baz', ['quux']]]);"

 ;; `->`
 > (describe "->")
 _
 > (compile '(-> x
                 (.foo "bar")
                 (.baz)))
 "x.foo('bar').baz();"
 > (compile '(-> regular-args
                 (.map (lambda (arg)
                         (compile-expression
                          arg env inherited-options)))
                 (.join ", ")))
 "regularArgs.map(function (arg) {
  return compileExpression(arg, env, inheritedOptions);
}).join(', ');"

 ;; `js/switch`
 > (describe "js/switch")
 _
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar"))))
 "switch (x) {
  case 'foo': {
    console.log('foo');
    break;
  }
  default: {
    console.log('bar');
  }
}"
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar")))
    :as 'return)
 "switch (x) {
  case 'foo': {
    return console.log('foo');
    break;
  }
  default: {
    return console.log('bar');
  }
}"

 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar")))
    :as 'expression)
 "(() => {
  switch (x) {
    case 'foo': {
      return console.log('foo');
      break;
    }
    default: {
      return console.log('bar');
    }
  }
})()"

 ;; `js/try`
 > (describe "js/try")
 _
 > (compile '(js/try))
 "try {
}"
 > (compile '(js/try
              (set! x (/ 2 1))))
 "try {
  x = 2 / 1;
}"
 > (compile '(js/try
              (set! x (/ 2 1))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}"
 > (compile '(js/try
              (set! x (/ 2 1))
              (catch _
                  (display "there was an error"))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}"
 > (compile '(js/try
              (set! x (/ 2 1))
              (catch e
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
 > (compile
    '(js/try
      (set! x (/ 2 1))
      (catch e
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

 ;; `clj/try`
 > (describe "clj/try")
 _
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
}"

 ;; `throw`
 > (describe "throw")
 _
 > (compile '(throw (new Error "An error")))
 "throw new Error('An error');"

 ;; `return`
 > (describe "return")
 _
 > (compile '(return))
 "return;"
 > (compile '(return 0))
 "return 0;"

 ;; `yield`
 > (describe "yield")
 _
 > (compile '(yield))
 "yield;"
 > (compile '(yield 0))
 "yield 0;"

 ;; `await`
 > (describe "await")
 _
 > (compile '(await (foo)))
 "await foo();"

 ;; `async`
 > (describe "async")
 _
 > (compile '(async (lambda (x) x)))
 "async function (x) {
  return x;
};"
 > (compile '(define foo
               (async (lambda (x) x))))
 "async function foo(x) {
  return x;
}"
 > (compile '(define foo
               (async (lambda (x) x)))
            :to 'typescript)
 "async function foo(x: any): Promise<any> {
  return x;
}"
 > (compile '(define/async (foo x)
               x))
 "async function foo(x) {
  return x;
}"

 ;; `require`
 > (describe "require")
 _
 > (compile '(require "foo"))
 "import * as foo from 'foo';"
 it> (compile '(require "foo")
              :fes-module-interop #t)
 "import foo from 'foo';"
 > (compile '(require foo "bar"))
 "import * as foo from 'bar';"
 > (compile '(require foo "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require "foo" "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require (only-in foo
                               bar)))
 "import {
  bar
} from 'foo';"
 > (compile '(require (only-in foo
                               (bar baz))))
 "import {
  bar as baz
} from 'foo';"
 > (compile '(require (only-in "foo"
                               (bar baz))))
 "import {
  bar as baz
} from 'foo';"
 > (compile '(require (only-in foo bar bar)))
 "import {
  bar
} from 'foo';"
 > (compile '(require (only-in foo
                               bar
                               (baz bar))))
 "import {
  bar
} from 'foo';"
 xit> (compile '(require 'foo "bar"))
 "import foo from 'bar';"
 xit> (compile '(require foo :as bar))
 "import bar from 'foo';"
 xit> (compile '(require (foo :as bar)))
 "import bar from 'foo';"
 xit> (compile '(require ("foo" :as "bar")))
 "import bar from 'foo';"

 ;; `provide`
 > (describe "provide")
 _
 > (compile '(provide))
 ""
 > (compile '(provide x))
 "export {
  x
};"
 > (compile '(provide x y))
 "export {
  x,
  y
};"
 > (compile '(provide (rename-out (x y))))
 "export {
  x as y
};"
 > (compile '(provide (rename-out (x y) (w z))))
 "export {
  x as y,
  w as z
};"
 > (compile '(provide x (rename-out (y z))))
 "export {
  x,
  y as z
};"
 > (compile '(provide x x))
 "export {
  x
};"
 > (compile '(provide x (rename-out (y x))))
 "export {
  x
};"
 > (compile '(provide (rename-out (x js/undefined))))
 "export {
  x as jsUndefined
};"
 > (compile '(provide (all-from-out "foo")))
 "export * from 'foo';"
 > (compile '(provide
               (all-from-out "foo")
               bar))
 "export * from 'foo';

export {
  bar
};"

 ;; `module`
 > (describe "module")
 _
 > (compile '(module m scheme
               (define (I x)
                 x)
               (define (K x y)
                 x)))
 "function I(x) {
  return x;
}

function K(x, y) {
  return x;
}"
 > (compile '(module m scheme
               (define I
                 (lambda (x)
                   x))
               (define K
                 (lambda (x y)
                   x))))
 "let I = function (x) {
  return x;
};

let K = function (x, y) {
  return x;
};"
 > (compile '(module m scheme
               (define (foo length)
                 length))
            :to 'typescript)
 "function foo(length: any): any {
  return length;
}"
 > (compile '(module m scheme
               (define (foo (length : Number)) : Number
                 length))
            :to 'typescript)
 "function foo(length: number): number {
  return length;
}"
 xit> (compile
       '(module m scheme
          (define I
            (curry-n
             1
             (lambda (x)
               x)))
          (define K
            (curry-n
             2
             (lambda (x y)
               x)))))
 "let I = curryN(1, function (x) {
  return x;
});

let K = curryN(2, function (x, y) {
  return x;
});"
 > (compile '(module m scheme
               (define truish #t)
               (define falsy (not truish))))
 "let truish = true;

let falsy = !truish;"
 > (compile
    '(module m scheme
       (require (only-in "foo"
                         and
                         or))
       (and x (or y z))))
 "import {
  and,
  or
} from 'foo';

and(x, or(y, z));"
 > (compile '(module m lisp
               (define x 1)
               (define y 2)))
 "let x = 1;

let y = 2;"
 xit> (compile '(module m lisp
                  (define (I x) x)
                  (define x 1)
                  (define *lisp-map* #t)))
 "function I(x) {
  return x;
}

I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];

let x = 1;"
 xit> (compile '(module m lisp
                  (require (only-in "./combinators"
                                    I))
                  (define x 1)
                  (define *lisp-map* #t)))
 "import {
  I
} from './combinators';

let x = 1;"
 xit> (compile '(module m lisp
                  (require (only-in "./combinators"
                                    I))
                  (define x 1)
                  (define *lisp-map* #t)))
 "import {
  I
} from './combinators';

let x: any = 1;"
 > (compile '(module m lisp
               (define (js_ str)
                 (js/eval str))))
 "function js_(str) {
  return eval(str);
}"
 > (compile '(module m lisp
               (define (my-fn foldl f v l)
                 (foldl f v l))))
 "function myFn(foldl, f, v, l) {
  return foldl(f, v, l);
}"
 > (compile '(module m lisp
               (define (my-foldl-obj obj f v l)
                 (.foldl obj f v l))))
 "function myFoldlObj(obj, f, v, l) {
  return obj.foldl(f, v, l);
}"
 > (compile '(module m lisp
               (define-class Foo ()
                 (define/public (foldl f v l)
                   l))))
 "class Foo {
  foldl(f, v, l) {
    return l;
  }
}"
 > (compile '(module m lisp
               (define (my-pop lst x)
                 (pop! lst x))))
 "function myPop(lst, x) {
  return lst.shift();
}"
 > (compile '(module m lisp
               (define (my-pop-2 lst x)
                 (pop! (append lst) x))))
 "function myPop2(lst, x) {
  return [...lst].shift();
}"
 > (compile '(module m lisp
               (define (my-pop-right lst x)
                 (pop-right! lst x))))
 "function myPopRight(lst, x) {
  return lst.pop();
}"
 > (compile '(module m lisp
               (define (my-pop-right-2 lst x)
                 (pop-right! (append lst) x))))
 "function myPopRight2(lst, x) {
  return [...lst].pop();
}"
 > (compile '(module m lisp
               (define (my-push lst x)
                 (push! lst x))))
 "function myPush(lst, x) {
  lst.unshift(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-2 lst x)
                 (push! (append lst) x))))
 "function myPush2(lst, x) {
  return (function (lst, x) {
    lst.unshift(x);
    return lst;
  })([...lst], x);
}"
 > (compile '(module m lisp
               (define (my-push-3 lst x)
                 (push! lst x)
                 lst)))
 "function myPush3(lst, x) {
  lst.unshift(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-right lst x)
                 (push-right! lst x))))
 "function myPushRight(lst, x) {
  lst.push(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-right-2 lst x)
                 (push-right! (append lst) x))))
 "function myPushRight2(lst, x) {
  return (function (lst, x) {
    lst.push(x);
    return lst;
  })([...lst], x);
}"
 > (compile '(module m lisp
               (define (my-push-right-3 lst x)
                 (push-right! lst x)
                 lst)))
 "function myPushRight3(lst, x) {
  lst.push(x);
  return lst;
}"

 ;; `ann`
 > (describe "ann")
 _
 > (compile '(ann 1 Number)
            :to 'javascript)
 "1;"
 > (compile '(ann 1 Number)
            :to 'typescript)
 "1 as number;"
 > (compile '(ann (list) Any)
            :to 'typescript)
 "[] as any;"
 > (compile '(ann '() Any)
            :to 'typescript)
 "[] as any;"
 > (compile '(ann x (List Any))
            :to 'typescript)
 "x as [any];"
 > (compile '(ann x (List Number Any))
            :to 'typescript)
 "x as [number, any];"
 > (compile '(ann x NN)
            :to 'typescript)
 "x as NN;"
 > (compile '(ann x (NN Any))
            :to 'typescript)
 "x as NN<any>;"
 > (compile '(ann x (NN Any Any))
            :to 'typescript)
 "x as NN<any,any>;"
 > (compile '((ann (lambda (x) x) Any) 1)
            :to 'typescript)
 "(function (x: any): any {
  return x;
} as any)(1);"
 > (compile '(lambda (x) (ann (send x foo) Any))
            :to 'typescript)
 "function (x: any): any {
  return x.foo() as any;
};"

 ;; `:`
 > (describe ":")
 _
 > (compile '(begin
               (: x Any)
               (define x 1))
            :to 'javascript)
 "let x = 1;"
 > (compile '(begin
               (: x Any)
               (define x 1))
            :to 'typescript)
 "let x: any = 1;"
 > (compile '(begin
               (: x String)
               (define x "1"))
            :to 'typescript)
 "let x: string = '1';"
 > (compile '(begin
               (: x Number)
               (define x 1))
            :to 'typescript)
 "let x: number = 1;"
 > (compile '(begin
               (: x Integer)
               (define x 1))
            :to 'typescript)
 "let x: number = 1;"
 > (compile '(begin
               (: x Natural)
               (define x 1))
            :to 'typescript)
 "let x: number = 1;"
 > (compile '(begin
               (: x Real)
               (define x 1))
            :to 'typescript)
 "let x: number = 1;"
 > (compile '(begin
               (: x Symbol)
               (define x 'x))
            :to 'typescript)
 "let x: Symbol = Symbol.for('x');"
 > (compile '(begin
               (: x Boolean)
               (define x #t))
            :to 'typescript)
 "let x: boolean = true;"
 > (compile '(begin
               (: x True)
               (define x #t))
            :to 'typescript)
 "let x: true = true;"
 > (compile '(begin
               (: x False)
               (define x #f))
            :to 'typescript)
 "let x: false = false;"
 > (compile '(begin
               (: x (U Number String))
               (define x 1))
            :to 'typescript)
 "let x: number | string = 1;"
 > (compile '(begin
               (: x (U Number String Boolean))
               (define x 1))
            :to 'typescript)
 "let x: number | string | boolean = 1;"
 > (compile '(begin
               (: x (U Number (U String Boolean)))
               (define x 1))
            :to 'typescript)
 "let x: number | (string | boolean) = 1;"
 > (compile '(begin
               (: x (Listof Number))
               (define x (list 1)))
            :to 'typescript)
 "let x: number[] = [1];"
 > (compile '(begin
               (: x (Pairof Number))
               (define x '(1 . 2)))
            :to 'typescript)
 "let x: (number | Symbol)[] = [1, Symbol.for('.'), 2];"
 > (compile '(begin
               (: hello-world (-> Void))
               (define (hello-world)
                 (display "Hello world!")))
            :to 'javascript)
 "function helloWorld() {
  console.log('Hello world!');
}"
 > (compile '(begin
               (: hello-world (-> Void))
               (define (hello-world)
                 (display "Hello world!")))
            :to 'typescript)
 "function helloWorld(): void {
  console.log('Hello world!');
}"
 > (compile '(begin
               (: f (-> Number Number))
               (define (f x)
                 x))
            :to 'typescript)
 "function f(x: number): number {
  return x;
}"
 > (compile '(begin
               (: f (-> Number Number))
               (define f
                 (lambda (x)
                   x)))
            :to 'typescript)
 "let f: (a: number) => number = function (x: any): any {
  return x;
};"
 > (compile '(begin
               (: f (-> Number Number))
               (define f
                 (foo
                  (lambda (x)
                    x))))
            :to 'typescript)
 "let f: (a: number) => number = foo(function (x: any): any {
  return x;
});"
 > (compile '(begin
               (: f (-> Number Number Number))
               (define (f x (y 1))
                 x))
            :to 'typescript)
 "function f(x: number, y: number = 1): number {
  return x;
}"
 > (compile '(begin
               (: f (->* (Number) (Number) Number))
               (define f
                 (lambda (x (y 1))
                   x)))
            :to 'typescript)
 "let f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {
  return x;
};"
 > (compile '(begin
               (: f (-> Any * Any))
               (define f
                 (lambda x
                   x)))
            :to 'typescript)
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (-> :rest Any Any))
               (define f
                 (lambda x
                   x)))
            :to 'typescript)
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (->* :rest Any Any))
               (define f
                 (lambda x
                   x)))
            :to 'typescript)
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (->* :rest (Listof Any) Any))
               (define f
                 (lambda x
                   x)))
            :to 'typescript)
 "let f: (...a: any[]) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: x Foo)
               (define x
                 (new Foo)))
            :to 'typescript)
 "let x: Foo = new Foo();"

 ;; `define-type`
 > (describe "define-type")
 _
 > (compile '(define-type NN (-> Number Number))
            :to 'javascript)
 ""
 > (compile '(define-type NN (-> Number Number))
            :to 'typescript)
 "type NN = (a: number) => number;"
 > (compile '(begin
               (define-type NN (-> Number Number))
               (: f NN)
               (define f
                 (lambda (x)
                   x)))
            :to 'javascript)
 "let f = function (x) {
  return x;
};"
 > (compile '(begin
               (define-type NN (-> Number Number))
               (: f NN)
               (define f
                 (lambda (x)
                   x)))
            :to 'typescript)
 "type NN = (a: number) => number;

let f: NN = function (x: any): any {
  return x;
};"
 > (compile '(define f
               (lambda ((x : Number))
                 x))
            :to 'typescript)
 "let f: any = function (x: number): any {
  return x;
};"
 > (compile '(define f
               (js/arrow ((x : Number))
                 x))
            :to 'typescript)
 "let f: any = (x: number): any => {
  return x;
};"
 > (compile '(define (f (x : Number))
               x)
            :to 'typescript)
 "function f(x: number): any {
  return x;
}"
 > (compile '(define (f (x : Number) . args)
               x)
            :to 'typescript)
 "function f(x: number, ...args: any[]): any {
  return x;
}"
 > (compile '(define (id (x : Number)) : Number
               x)
            :to 'typescript)
 "function id(x: number): number {
  return x;
}"
 > (compile '(define (f (x : Number 1)) : Number
               x)
            :to 'typescript)
 "function f(x: number = 1): number {
  return x;
}"
 > (compile '(define (f (options : Any (js/obj))) : Any
               x)
            :to 'typescript)
 "function f(options: any = {}): any {
  return x;
}"
 > (compile '(define Foo
               (class object%
                 (define/public x)
                 (define (constructor (x : Number))
                   (set-field! x this x))))
            :to 'typescript)
 "class Foo {
  x: any;

  constructor(x: number) {
    this.x = x;
  }
}"
 > (compile '(define Foo
               (class object%
                 (define/public x)
                 (define (constructor (x : Number) . args)
                   (set-field! x this x))))
            :to 'typescript)
 "class Foo {
  x: any;

  constructor(x: number, ...args: any[]) {
    this.x = x;
  }
}"
 xit> (compile '(begin
                  (define-type NN (-> Number Number))
                  (: f NN)
                  (define (f x)
                    x))
               :to 'typescript)
 "type NN = (a: number) => number;

function f(x: number): number {
  return x;
};"

 ;; `field-bound?`
 > (describe "field-bound?")
 _
 > (compile '(begin
               (define foo
                 (js/obj))
               (define bar
                 (field-bound? baz foo))))
 "let foo = {};

let bar = foo && ('baz' in foo);"
 > (compile '(begin
               (define foo
                 (js/obj))
               (define bar
                 (field-bound? baz-baz foo))))
 "let foo = {};

let bar = foo && ('bazBaz' in foo);"

 ;; `js/?.`
 > (describe "js/?.")
 _
 > (compile '(define x
               (js/?. foo bar)))
 "let x = foo?.bar;"
 > (compile '(define x
               ((js/?. foo bar) baz)))
 "let x = foo?.bar(baz);"
 > (compile '(define x
               (js/?. foo (bar))))
 "let x = foo?.(bar);"

 ;; `assert`
 > (describe "assert")
 _
 > (compile '(assert #t))
 "console.assert(true);"
 > (compile '(assert #t "test"))
 "console.assert(true, 'test');"

 ;; `display`
 > (describe "display")
 _
 > (compile '(display #t))
 "console.log(true);"
 > (compile '(display #t "test"))
 "console.log(true, 'test');"

 ;; Macros
 > (describe "Macros")
 _
 xit> (compile
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

 ;; Fexprs
 > (describe "Fexprs")
 _
 > (compile
    '(begin
       (define-fexpr (foo x)
         x)
       (define x 1)
       (define bar
         (foo x))))
 "function foo(x) {
  return x;
}

foo.ftype = 'fexpr';

let x = 1;

let bar = foo(Symbol.for('x'));"

 ;; Global environment
 > (describe "Global environment")
 _
 > (compile '(module m scheme
               (define lst
                 `(,symbol? ,boolean?)))
            :finline-functions #t)
 "let [symbolp, booleanp] = (() => {
  function symbolp_(obj) {
    return typeof obj === 'symbol';
  }
  function booleanp_(obj) {
    return typeof obj === 'boolean';
  }
  return [symbolp_, booleanp_];
})();

let lst = [symbolp, booleanp];"
 xit> (compile
       '(define-values (_ regexp)
          (rl/sandbox
           ((js/arrow ()
              (define __
                (js/obj "@@functional/placeholder" #t))
              (define (js/regexp_ input (flags #u))
                (if (eq? (type-of input) "string")
                    (new RegExp input flags)
                    input))
              (values __ js/regexp_)))))
       :finline-functions #t)
 "let [, regexp] = (() => {
  let __ = {
    '@@functional/placeholder': true
  };
  function jsRegexp_(input, flags = undefined) {
    if (typeof input === 'string') {
      return new RegExp(input, flags);
    } else {
      return input;
    }
  }
  return [__, jsRegexp_];
})();"
 > (compile '(module m scheme
               (define one-plus-one
                 (apply + '(1 1))))
            :finline-functions #t)
 "let [_add] = (() => {
  function add_(...args) {
    let result = 0;
    for (let arg of args) {
      result = result + arg;
    }
    return result;
  }
  return [add_];
})();

let onePlusOne = _add(1, 1);"
 > (compile '(module m scheme
               (define one-minus-one
                 (apply - '(1 1))))
            :finline-functions #t)
 "let [_sub] = (() => {
  function sub_(...args) {
    let len = args.length;
    if (len === 0) {
      return 0;
    } else if (len === 1) {
      return -args[0];
    } else {
      let result = args[0];
      for (let i = 1; i < len; i++) {
        result = result - args[i];
      }
      return result;
    }
  }
  return [sub_];
})();

let oneMinusOne = _sub(1, 1);"
 > (compile '(module m scheme
               (define one-minus-one
                 (apply - '(1 1)))))
 "import {
  _sub
} from 'roselisp';

let oneMinusOne = _sub(1, 1);"
 > (compile '(module m scheme
               (define one-times-one
                 (apply * '(1 1))))
            :finline-functions #t)
 "let [_mul] = (() => {
  function mul_(...args) {
    let result = 1;
    for (let arg of args) {
      result = result * arg;
    }
    return result;
  }
  return [mul_];
})();

let oneTimesOne = _mul(1, 1);"
 > (compile '(module m scheme
               (define one-divided-by-one
                 (apply / '(1 1))))
            :finline-functions #t)
 "let [_div] = (() => {
  function div_(...args) {
    if (args.length === 1) {
      return 1 / args[0];
    } else {
      let result = args[0];
      let _end = args.length;
      for (let i = 1; i < _end; i++) {
        result = result / args[i];
      }
      return result;
    }
  }
  return [div_];
})();

let oneDividedByOne = _div(1, 1);"
 > (compile
    '(module m scheme
       (define foo-bar
         (apply string-append '("foo" "bar"))))
    :finline-functions #t)
 "let [stringAppend] = (() => {
  function stringAppend_(...args) {
    return args.reduce(function (acc, x) {
      return acc + x;
    }, '');
  }
  return [stringAppend_];
})();

let fooBar = stringAppend('foo', 'bar');"
 xit> (compile '(module m lisp
                  (define (my-foldl f v l)
                    (foldl f v l))
                  (define bar
                    (my-foldl + 0 '(1 2 3 4))))
               :finline-functions #t)
 "let [add] = (function () {
  function add(...args) {
    return args.reduce(function (y, x) {
      return y + x;
    }, 0);
  }
  return [add];
})();

function myFoldl(f, v, l) {
  return l.reduce(function (acc, x) {
    return f(x, acc);
  }, v);
}

let bar = myFoldl(add, 0, [1, 2, 3, 4]);"
 xit> (compile '(module m lisp
                  (define (my-foldl f v l)
                    (foldl f v l)))
               :finline-functions #t)
 "let [foldl] = (function () {
  function foldl(f, v, lst) {
    return lst.reduce(function (acc, x) {
      return f(x, acc);
    }, v);
  }
  return [foldl];
})();

function myFoldl(f, v, l) {
  return foldl(f, v, l);
}"
 > (compile '(module m lisp
               (define (my-map f x)
                 (map f x))
               (define bar
                 (my-map first '((1) (2) (3)))))
            :finline-functions #t)
 "let [first] = (() => {
  function first_(lst) {
    return lst[0];
  }
  return [first_];
})();

function myMap(f, x) {
  return x.map(function (x) {
    return f(x);
  });
}

let bar = myMap(first, [[1], [2], [3]]);"
 xit> (compile '(module m lisp
                  (define (foo f x y)
                    (f x y))
                  (define (my-push-4 lst x)
                    (foo push! lst x)))
               :finline-functions #t)
 "let [pushX] = (function () {
  function pushX(lst, x) {
    lst.unshift(x);
    return lst;
  }
  return [pushX];
})();

function foo(f, x, y) {
  return f(x, y);
}

function myPush4(lst, x) {
  return foo(pushX, lst, x);
}"
 xit> (compile '(module m lisp
                  (define (get-push-function)
                    push!)
                  (define (my-push-4 lst x)
                    ((get-push-function) lst x)))
               :finline-functions #t)
 "let [pushX] = (function () {
  function pushX(lst, x) {
    lst.unshift(x);
    return lst;
  }
  return [pushX];
})();

function getPushFunction() {
  return pushX;
}

function myPush4(lst, x) {
  return getPushFunction()(lst, x);
}"
 > (compile '(module m lisp
               (define (my-cdr x)
                 (cdr x)))
            :finline-functions #t)
 "let [cdr] = (() => {
  function cdr_(lst) {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  return [cdr_];
})();

function myCdr(x) {
  return cdr(x);
}"
 > (compile '(module m lisp
               (define (my-intersection x y)
                 (intersection x y)))
            :finline-functions #t)
 "let [intersection] = (() => {
  function intersection_(...args) {
    function intersection2(arr1, arr2) {
      let result = [];
      for (let element of arr1) {
        if (arr2.includes(element) && !result.includes(element)) {
          result.push(element);
        }
      }
      return result;
    }
    if (args.length === 0) {
      return [];
    } else if (args.length === 1) {
      return args[0];
    } else {
      return args.slice(1).reduce(function (acc, x) {
        return intersection2(acc, x);
      }, args[0]);
    }
  }
  return [intersection_];
})();

function myIntersection(x, y) {
  return intersection(x, y);
}"

 ;; `compile-modules`
 > (describe "compile-modules")
 _
 > (it "(module ... (define ...) ...)"
       (compile-modules
        (list '(module m scheme
                 (define (I x)
                   x)))
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 (list
  "function I(x) {
  return x;
}")
 > (it "import macro from another module"
       (compile-modules
        (list
         '(module a scheme
            (defmacro foo (x)
              x)
            (provide foo))
         '(module b scheme
            (require (only-in "./a"
                              foo))
            (declare-macro foo)
            (define (bar x)
              (foo x))))
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 (list
  "function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

export {
  foo
};"
  "import {
  foo
} from './a';

foo.ftype = 'macro';

function bar(x) {
  return x;
}")
 > (it "import macro from a module defined later"
       (compile-modules
        (list
         '(module a scheme
            (require (only-in "./b"
                              bar))
            (declare-macro bar)
            (define (foo x)
              (bar x)))
         '(module b scheme
            (defmacro bar (x)
              x)
            (provide bar)))
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 (list
  "import {
  bar
} from './b';

bar.ftype = 'macro';

function foo(x) {
  return x;
}"

  "function bar(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

bar.ftype = 'macro';

export {
  bar
};")
 > (it "import function for use in a macro"
       (compile-modules
        (list
         '(module a scheme
            (require (only-in "./b"
                              baz))
            (defmacro bar (x)
              (baz x))
            (define (foo x)
              (bar x)))
         '(module b scheme
            (define (baz x)
              x)
            (provide baz)))
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 (list
  "import {
  baz
} from './b';

function bar(exp, env) {
  const [x] = exp.slice(1);
  return baz(x);
}

bar.ftype = 'macro';

function foo(x) {
  return x;
}"

  "function baz(x) {
  return x;
}

export {
  baz
};")
 > (it "import renamed macro from another module"
       (compile-modules
        (list
         '(module a scheme
            (defmacro foo (x)
              x)
            (provide foo))
         '(module b scheme
            (require (only-in "./a"
                              (foo foo1)))
            (declare-macro foo1)
            (define (bar x)
              (foo1 x))))
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 (list
  "function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

export {
  foo
};" "import {
  foo as foo1
} from './a';

foo1.ftype = 'macro';

function bar(x) {
  return x;
}")

 ;; `--fsemicolon false`
 > (describe "--fsemicolon false")
 _
 > (compile '(begin x y z)
            :fsemicolon #f)
 "x

y

z"

 ;; `compile-with-environment`
 > (describe "compile-with-environment")
 _
 > (it "compiledEnvironment"
       (define options
         (js/obj))
       (compile-with-environment 'foo #u options)
       (define compiled-env
         (oget options "compiledEnvironment"))
       (instance-of? compiled-env LispEnvironment))
 #t
 xit> (it "has"
          (define options
            (js/obj))
          (compile-with-environment
           '(define foo 1)
           #u
           options)
          (define continuation-env
            (oget options "continuationEnv"))
          (send continuation-env has 'foo))
 #t
 xit> (it "EnvironmentStack"
          (define options
            (js/obj))
          (compile-with-environment
           'foo
           #u
           options)
          (define continuation-env
            (oget options "continuationEnv"))
          (instance-of? continuation-env EnvironmentStack))
 #t
 > (it ";; comment
(foo)"
       (compile-with-environment
        (read-rose
         ";; comment
(foo)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "// comment
foo();"
 > (it ";; multi-line
;; comment
(foo)"
       (compile-with-environment
        (read-rose
         ";; multi-line
;; comment
(foo)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "// multi-line
// comment
foo();"
 xit> (it ";; multi-line
;;
;; comment
(foo)"
          (compile-with-environment
           (read-rose
            ";; multi-line
;;
;; comment
(foo)")
           compilation-environment
           (js/obj "expressionType" "statement"
                   "language" "javascript"
                   "optimize" #t)))
 "// multi-line
//
// comment
foo();"
 > (it ";; multiple

;; comments
(foo)"
       (compile-with-environment
        (read-rose
         ";; multiple

;; comments
(foo)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "// multiple

// comments
foo();"
 > (it "(+
 ;; foo
 foo
 ;; bar
 bar)"
       (compile-with-environment
        (read-rose
         "(+
            ;; foo
            foo
            ;; bar
            bar)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "(
 // foo
 foo +
 // bar
 bar
);"
 > (it "(list foo
      ;; bar
      bar
      ;; baz
      baz)"
       (compile-with-environment
        (read-rose
         "(list foo
      ;; bar
      bar
      ;; baz
      baz)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "[
 foo,
 // bar
 bar,
 // baz
 baz
];"
 > (it "(+
 ;; foo
 foo
 ;; bar
 bar)"
       (compile-with-environment
        (read-rose
         "(+
            ;; foo
            foo
            ;; bar
            bar)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "(
 // foo
 foo +
 // bar
 bar
);"
 > (it ";; comment
(foo)"
       (compile-with-environment
        (read-rose
         ";; comment
(foo)")
        compilation-environment
        (js/obj "expressionType" "statement"
                "language" "javascript"
                "optimize" #t)))
 "// comment
foo();"
 > (it "I & K"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; I combinator.
  (define (I x)
   ;; Just return x.
   x)
  ;;; K combinator.
  (define (K x y)
    x))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * I combinator.
 */
function I(x) {
  // Just return x.
  return x;
}

/**
 * K combinator.
 */
function K(x, y) {
  return x;
}"
 > (it "A, JS"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * A combinator.
 */
function A(f, ...args) {
  // Apply f to args.
  return f(...args);
}"
 > (it "A, TS"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
        compilation-environment
        (js/obj "language" "typescript"
                "optimize" #t)))
 "/**
 * A combinator.
 */
function A(f: any, ...args: any[]): any {
  // Apply f to args.
  return f(...args);
}"
 > (it "B2, TS"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; B2 combinator.
  (define (B2 . args)
    (let ((fs (drop-right args 1))
          (x (array-list-last args)))
      (foldr A x fs))))")
        compilation-environment
        (js/obj "language" "typescript"
                "optimize" #t)))
 "/**
 * B2 combinator.
 */
function B2(...args: any[]): any {
  const fs: any = args.slice(0, -1);
  const x: any = args[args.length - 1];
  return fs.reduceRight(function (acc: any, x: any): any {
    return A(x, acc);
  }, x);
}"
 > (it "(define ... (let ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Foo.
  (define (foo x)
    ;; Bind y.
    (let ((y 1))
      ;; Return y.
      y)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Foo.
 */
function foo(x) {
  // Bind y.
  const y = 1;
  // Return y.
  return y;
}"
 > (it "(define ... (if ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Whether x is a truish value.
  (define (truish x)
    (if x
        ;; If x is truish, return true.
        #t
      ;; If x is falsey, return false.
      #f)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Whether x is a truish value.
 */
function truish(x) {
  if (x) {
    // If x is truish, return true.
    return true;
  } else {
    // If x is falsey, return false.
    return false;
  }
}"
 > (it "(define ... (cond ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Whether x is a truish value.
  (define (truish x)
    (cond
      ;; If x is truish, return true.
      (x
       #t)
      ;; If x is falsey, return false.
      (else
       #f))))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Whether x is a truish value.
 */
function truish(x) {
  if (x) {
    // If x is truish, return true.
    return true;
  } else {
    // If x is falsey, return false.
    return false;
  }
}"
 > (it "(define ... (let ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Wrap a value in a list.
  (define (wrap-in-list x)
    ;; Return x wrapped in a list.
    `(,x)))")
        compilation-environment
        (js/obj "case" "camelcase"
                "language" "javascript"
                "optimize" #t)))
 "/**
 * Wrap a value in a list.
 */
function wrapInList(x) {
  // Return x wrapped in a list.
  return [x];
}"
 > (it "while...if"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; test function.
  (define (test)
    ;; while loop.
    (while foo
      (cond
       ;; bar case.
       (bar
        ;; inner cond.
        (cond
         (baz
          \"baz\")))
       ;; else case.
       (else
        \"baz\")))))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * test function.
 */
function test() {
  // while loop.
  while (foo) {
    if (bar) {
      // bar case.
      // inner cond.
      if (baz) {
        return 'baz';
      }
    } else {
      // else case.
      return 'baz';
    }
  }
}"
 > (it "(define-class Foo ...)"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; bar property.
    (define/public bar 0)

    ;;; Foo constructor.
    (define/public (constructor n)
      ;; Set bar to n.
      (set! (.-this bar) n))))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Foo class.
 */
class Foo {
  /**
   * bar property.
   */
  bar = 0;

  /**
   * Foo constructor.
   */
  constructor(n) {
    // Set bar to n.
    bar.this = n;
  }
}"
 > (it "(define-class Foo ...)"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; foo method.
    (define/public (foo)
      ;; this
      this)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Foo class.
 */
class Foo {
  /**
   * foo method.
   */
  foo() {
    // this
    return this;
  }
}"
 > (it "(define Foo (class ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Foo class.
  (define Foo
    (class object%
      ;;; bar method.
      (define/public (bar)
        ;; this
        this))))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Foo class.
 */
class Foo {
  /**
   * bar method.
   */
  bar() {
    // this
    return this;
  }
}"
 > (it "(define Foo (class ...))"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Foo class.
  (define Foo
    (class object%
      ;;; foo method.
      (define/public (foo)
        0)

      ;;; bar generator method.
      (define/generator ((get-field iterator Symbol))
        (for ((x (list 1 2 3 4)))
          (yield x))))))")
        compilation-environment
        (js/obj "language" "typescript"
                "optimize" #t)))
 "/**
 * Foo class.
 */
class Foo {
  /**
   * foo method.
   */
  foo(): any {
    return 0;
  }

  /**
   * bar generator method.
   */
  *[Symbol.iterator](): any {
    for (let x of [1, 2, 3, 4]) {
      yield x;
    }
  }
}"
 xit> (it "(define (hello-world) ...)"
          (compile-with-environment
           (read-rose
            "(module m scheme
  ;;; Hello, world.
  (: hello-world (-> Void))
  (define (hello-world)
    (display \"hello, world\")))")
           compilation-environment
           (js/obj "case" "camelcase"
                   "language" "javascript"
                   "optimize" #t)))
 "/**
 * Hello, world.
 */
function helloWorld() {
  console.log('hello, world');
}"
 > (it ";;; Foo, blank line, (define (hello-world) ...)"
       (compile-with-environment
        (read-rose
         ";;; Foo

(require \"foo\")")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "/**
 * Foo
 */

import * as foo from 'foo';"
 > (it ";; Foo, blank line, ;;; Bar, (define (hello-world) ...)"
       (compile-with-environment
        (read-rose
         ";; Foo

;;; Bar
(require \"foo\")")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "// Foo

/**
 * Bar
 */
import * as foo from 'foo';"
 > (it "(define (hello-world) ...)"
       (compile-with-environment
        (read-rose
         ";; Foo
;;; Bar

(require \"foo\")")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "// Foo
/**
 * Bar
 */

import * as foo from 'foo';"
 > (it "(define foo\n  ;; bar\n  bar)"
       (compile-with-environment
        (read-rose
         "(define foo
  ;; bar
  bar)")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "const foo =
  // bar
  bar;"
 > (it "(set! foo\n  ;; bar\n  bar)"
       (compile-with-environment
        (read-rose
         "(set! foo
  ;; bar
  bar)")
        compilation-environment
        (js/obj "language" "javascript"
                "expressionType" "statement"
                "optimize" #t)))
 "foo =
  // bar
  bar;"
 xit> (it "x, camelCase"
          (compile-with-environment
           'x
           (new LispEnvironment
                (list
                 (list
                  "x"
                  1
                  "variable")))
           (js/obj "language" "javascript"
                   "optimize" #t)))
 "1"
 > (it "(module m scheme ... (apply + '(1 1)) ...), comment"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Module header.

  (define one-plus-one
    (apply + '(1 1))))")
        compilation-environment
        (js/obj "case" "camelcase"
                "finlineFunctions" #t
                "language" "javascript"
                "optimize" #t)))
 "/**
 * Module header.
 */

const [_add] = (() => {
  function add_(...args) {
    let result = 0;
    for (let arg of args) {
      result = result + arg;
    }
    return result;
  }
  return [add_];
})();

const onePlusOne = _add(1, 1);"
 > (it "(module m scheme ... (apply + '(1 1)) ...), comments"
       (compile-with-environment
        (read-rose
         "(module m scheme
  ;;; Module header.

  ;;; Custom addition function.
  (define one-plus-one
    (apply + '(1 1))))")
        compilation-environment
        (js/obj "case" "camelcase"
                "finlineFunctions" #t
                "language" "javascript"
                "optimize" #t)))
 "/**
 * Module header.
 */

const [_add] = (() => {
  function add_(...args) {
    let result = 0;
    for (let arg of args) {
      result = result + arg;
    }
    return result;
  }
  return [add_];
})();

/**
 * Custom addition function.
 */
const onePlusOne = _add(1, 1);"
 xit> (it "(I x), JS function"
          (compile-with-environment
           '(I x)
           (new LispEnvironment
                (list
                 (list
                  "I"
                  (lambda (x)
                    x)
                  "function")))
           (js/obj "language" "javascript"
                   "optimize" #t)))
 "(function {
   let I = function(x) {
     return x;
   }
   return I;
})()(x)"
 > (it "(truep x)"
       (compile-with-environment
        '(truep x)
        compilation-environment
        (js/obj "case" "camelcase"
                "language" "javascript"
                "optimize" #t)))
 "x ? true : false"
 > (it "(falsep x)"
       (compile-with-environment
        '(falsep x)
        compilation-environment
        (js/obj "case" "camelcase"
                "language" "javascript"
                "optimize" #t)))
 "x ? false : true"
 > (it "read-rose"
       (compile-with-environment
        (read-rose
         "(module m scheme
  (define foo
    `(foo)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "const foo = [Symbol.for('foo')];"
 > (it "read-rose, quasiquote"
       (compile-with-environment
        (read-rose
         "(module m scheme
  (define foo 1)
  (define bar
    `(,foo)))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "const foo = 1;

const bar = [foo];"
 > (it "read-rose, quasiquoted list of pairs"
       (compile-with-environment
        (read-rose
         "(module m scheme
  (define foo 1)
  (define bar 2)
  (define quux
    `((\"foo\" . ,foo)
       (\"bar\" . ,bar))))")
        compilation-environment
        (js/obj "language" "javascript"
                "optimize" #t)))
 "const foo = 1;

const bar = 2;

const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];"
 xit> (it "(module m lisp ... (define *lisp-map* '()))"
          (compile-with-environment
           (read-rose
            "(module m lisp
  ;; inline-lisp-sources: true

  (define (I x) x))")
           compilation-environment
           (js/obj "case" "camelcase"
                   "language" "javascript"
                   "optimize" #t)))
 "// inline-lisp-sources: true

function I(x) {
  return x;
}

I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];"
 > (it "(: f (-> Number Number)), lambda, comments, TS"
       (compile-with-environment
        (read-rose
         "(begin
  ;; NN type alias.
  (define-type NN (-> Number Number))
  (: f NN)
  (define f
    (lambda (x)
      x)))")
        compilation-environment
        (js/obj "language" "typescript"
                "expressionType" "statement"
                "optimize" #t)))
 "// NN type alias.
type NN = (a: number) => number;

const f: NN = function (x: any): any {
  return x;
};"
 > (compile-with-environment
    '(module m scheme
       (define (foo x)
         x))
    compilation-environment
    (js/obj "language" "javascript"
            "inlineLispSources" #t
            "optimize" #t))
 "function foo(x) {
  return x;
}

foo.fsource = [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"
 xit> (compile-with-environment
       '(module m scheme
          (define foo
            (lambda (x)
              x)))
       compilation-environment
       (js/obj "language" "javascript"
               "inlineLispSources" #t
               "optimize" #t))
 "const foo = function (x) {
  return x;
};

foo.fsource = [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')];"
 > (compile-with-environment
    '(module m scheme
       (define foo
         (async
          (lambda (x)
            x))))
    compilation-environment
    (js/obj "language" "javascript"
            "inlineLispSources" #t
            "optimize" #t))
 "async function foo(x) {
  return x;
}

foo.fsource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"

 ;; `definition->macro`
 > (describe "definition->macro")
 _
 > (definition->macro
     '(define (inc x)
        (+ x 1))
     '(1))
 '(+ 1 1)
 > (definition->macro
     '(define (logical-or x)
        (or x x))
     '(#t))
 '(or #t #t)
 > (definition->macro
     '(define (repeat x)
        (string-append x x))
     '("1"))
 '(string-append "1" "1")
 > (definition->macro
     '(define (square x)
        (* x x))
     '(1))
 '(* 1 1)
 > (definition->macro
     '(define (square x)
        (* x x))
     '(x))
 '(* x x)
 xit> (definition->macro
        '(define (square x)
           (* x x))
        '((+ 1 1)))
 '((lambda (x)
     (* x x))
   (+ 1 1))

 ;; `define-macro->lambda-form`
 > (describe "define-macro->lambda-form")
 _
 > (define-macro->lambda-form
     '(define-macro (foo x)
        x))
 '(lambda (exp env)
    (define-values (x)
      (rest exp))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo &whole expression x)
        x))
 '(lambda (expression env)
    (define-values (x)
      (rest expression))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo &whole exp &environment env)
        exp))
 '(lambda (exp env)
    exp)
 > (define-macro->lambda-form
     '(define-macro (foo &whole exp &environment env x)
        x))
 '(lambda (exp env)
    (define-values (x)
      (rest exp))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo &rest x)
        x))
 '(lambda (exp env)
    (define-values x
      (rest exp))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo x &rest y)
        x))
 '(lambda (exp env)
    (define-values (x . y)
      (rest exp))
    x)

 ;; `split-comments`
 > (describe "split-comments")
 _
 xit> (split-comments ";;; Foo")
 '(";;; Foo")
 > (split-comments ";;; Foo\n")
 '(";;; Foo\n")
 xit> (split-comments ";; Foo\n;;; Bar")
 '(";; Foo\n" ";;; Bar")
 xit> (split-comments ";; Foo\n;;; Bar\n")
 '(";; Foo\n" ";;; Bar\n"))

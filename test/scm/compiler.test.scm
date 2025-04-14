(require chai "chai")
(require (only-in "../../src/ts/combinators"
                  I))
(require (only-in "../../src/ts/language"
                  LispEnvironment
                  compilation-environment
                  compile
                  compile-modules
                  definition-to-macro
                  define-macro-to-lambda-form
                  split-comments))
(require (only-in "../../src/ts/parser"
                  read-rose))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal))

(describe "compile-modules"
  (fn ()
    (describe "single module"
      (fn ()
        (it "(module ... (define ...) ...)"
            (fn ()
              (assert-equal
               (compile-modules
                (list '(module m scheme
                         (define (I x)
                           x)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               (list
                "function I(x) {
  return x;
}"))))))
    (describe "multiple modules"
      (fn ()
        (it "import macro from another module"
            (fn ()
              (assert-equal
               (compile-modules
                (list
                 '(module a scheme
                    (defmacro foo (x)
                      x)
                    (provide foo))
                 '(module b scheme
                    (require (only-in "./a"
                                      foo))
                    (define (bar x)
                      (foo x))))
                compilation-environment
                (js-obj "language" "JavaScript"))
               (list
                "function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

export {
  foo
};" "import {
  foo
} from './a';

function bar(x) {
  return x;
}"))))
        (it "import renamed macro from another module"
            (fn ()
              (assert-equal
               (compile-modules
                (list
                 '(module a scheme
                    (defmacro foo (x)
                      x)
                    (provide foo))
                 '(module b scheme
                    (require (only-in "./a"
                                      (foo foo1)))
                    (define (bar x)
                      (foo1 x))))
                compilation-environment
                (js-obj "language" "JavaScript"))
               (list
                "function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

export {
  foo
};" "import {
  foo as foo1
} from './a';

function bar(x) {
  return x;
}"))))))))

(describe "compile"
  (fn ()
    (describe "macros"
      (fn ()
        ;; FIXME: Failing test
        (xit "(module ... (defmacro foo ...) ...)"
             (fn ()
               (assert-equal
                (compile
                 '(module m scheme
                    (defmacro foo ()
                      '(begin))
                    (foo))
                 compilation-environment
                 (js-obj "language" "JavaScript"))
                "function foo(exp, env) {
  return [Symbol.for('begin')];
}

foo.lispMacro = true;")))
        (it "(module ... (defmacro foo ...) ...)"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (defmacro foo (x)
                     x)
                   (define (bar x)
                     (foo x)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

function bar(x) {
  return x;
}")))
        (it "(module ... (defmacro foo (x . args) ...) ...)"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (defmacro foo (x . args)
                     x)
                   (define (bar x)
                     (foo x)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function foo(exp, env) {
  const [x, ...args] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

function bar(x) {
  return x;
}")))
        (it "(module ... (defmacro foo (x . args) ...) ...)"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (defmacro foo (x . args)
                     x)
                   (define bar
                     (foo 1 2 3)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function foo(exp, env) {
  const [x, ...args] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

const bar = 1;")))
        (xit "(begin (defmacro foo (x . args) ...) ...)"
             (fn ()
               (assert-equal
                (compile
                 '(begin
                    (defmacro foo (x . args)
                      x)
                    (define bar
                      (foo 1 2 3)))
                 compilation-environment
                 (js-obj "expressionType" "statement"
                         "language" "JavaScript"))
                "function foo(exp, env) {
  const [x, ...args] = exp.slice(1);
  return x;
}

foo.lispMacro = true;

const bar = 1;")))))
    (describe "comments"
      (fn ()
        (it ";; comment
(foo)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; comment
(foo)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "// comment
foo();")))
        (it ";; multi-line
;; comment
(foo)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; multi-line
;; comment
(foo)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "// multi-line
// comment
foo();")))
        (xit ";; multi-line
;;
;; comment
(foo)"
             (fn ()
               (assert-equal
                (compile
                 (read-rose
                  ";; multi-line
;;
;; comment
(foo)")
                 compilation-environment
                 (js-obj "expressionType" "statement"
                         "language" "JavaScript"))
                "// multi-line
//
// comment
foo();")))
        (it ";; multiple

;; comments
(foo)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; multiple

;; comments
(foo)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "// multiple

// comments
foo();")))
        (it "(+
 ;; foo
 foo
 ;; bar
 bar)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(+
            ;; foo
            foo
            ;; bar
            bar)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "(
 // foo
 foo +
 // bar
 bar
);")))
        (it "(list foo
      ;; bar
      bar
      ;; baz
      baz)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(list foo
      ;; bar
      bar
      ;; baz
      baz)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "[
 foo,
 // bar
 bar,
 // baz
 baz
];")))
        (it "(+
 ;; foo
 foo
 ;; bar
 bar)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(+
            ;; foo
            foo
            ;; bar
            bar)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "(
 // foo
 foo +
 // bar
 bar
);")))
        (it ";; comment
(foo)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; comment
(foo)")
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "// comment
foo();")))
        (it "I & K"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (it "A, JS"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "/**
 * A combinator.
 */
function A(f, ...args) {
  // Apply f to args.
  return f(...args);
}")))
        (it "A, TS"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
                compilation-environment
                (js-obj "language" "TypeScript"))
               "/**
 * A combinator.
 */
function A(f: any, ...args: any[]): any {
  // Apply f to args.
  return f(...args);
}")))
        (it "B2, TS"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; B2 combinator.
  (define (B2 . args)
    (let ((fs (drop-right args 1))
          (x (array-list-last args)))
      (foldr A x fs))))")
                compilation-environment
                (js-obj "language" "TypeScript"))
               "/**
 * B2 combinator.
 */
function B2(...args: any[]): any {
  const fs: any = args.slice(0, -1);
  const x: any = args[args.length - 1];
  return fs.reduceRight(function (acc: any, x: any): any {
    return A(x, acc);
  }, x);
}")))
        (it "(define ... (let ...))"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; Foo.
  (define (foo x)
    ;; Bind y.
    (let ((y 1))
      ;; Return y.
      y)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "/**
 * Foo.
 */
function foo(x) {
  // Bind y.
  const y = 1;
  // Return y.
  return y;
}")))
        (it "(define ... (if ...))"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (it "(define ... (cond ...))"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (it "(define ... (let ...))"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; Wrap a value in a list.
  (define (wrap-in-list x)
    ;; Return x wrapped in a list.
    `(,x)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "/**
 * Wrap a value in a list.
 */
function wrapInList(x) {
  // Return x wrapped in a list.
  return [x];
}")))
        (it "while...if"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (it "(define-class Foo ...)"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (it "(define-class Foo ...)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; bar property.
    (define/public (foo)
      ;; this
      this)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "/**
 * Foo class.
 */
class Foo {
  /**
   * bar property.
   */
  foo() {
    // this
    return this;
  }
}")))
        (it "(define Foo (class ...))"
            (fn ()
              (assert-equal
               (compile
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
                (js-obj "language" "JavaScript"))
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
}")))
        (xit "(define (hello-world) ...)"
             (fn ()
               (assert-equal
                (compile
                 (read-rose
                  "(module m scheme
             ;; Hello, world.
             (: hello-world (-> Void))
             (define (hello-world)
               (display \"hello, world\")))")
                 compilation-environment
                 (js-obj "language" "JavaScript"))
                "/**
 * Hello, world.
 */
function helloWorld() {
  console.log('hello, world');
}")))
        (it ";;; Foo, blank line, (define (hello-world) ...)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";;; Foo

(require \"foo\")")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "/**
 * Foo
 */

import * as foo from 'foo';")))
        (it ";; Foo, blank line, ;;; Bar, (define (hello-world) ...)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; Foo

;;; Bar
(require \"foo\")")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "// Foo

/**
 * Bar
 */
import * as foo from 'foo';")))
        (it "(define (hello-world) ...)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 ";; Foo
;;; Bar

(require \"foo\")")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "// Foo
/**
 * Bar
 */

import * as foo from 'foo';")))
        (it "(define foo\n  ;; bar\n  bar)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(define foo
  ;; bar
  bar)")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const foo =
  // bar
  bar;")))
        (it "(set! foo\n  ;; bar\n  bar)"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(set! foo
  ;; bar
  bar)")
                compilation-environment
                (js-obj "language" "JavaScript"
                        "expressionType" "statement"))
               "foo =
  // bar
  bar;")))))
    (describe "symbols"
      (fn ()
        (it "#t"
            (fn ()
              (assert-equal
               (compile
                #t
                compilation-environment
                (js-obj "language" "JavaScript"))
               "true")))
        (it "#f"
            (fn ()
              (assert-equal
               (compile
                #f
                compilation-environment
                (js-obj "language" "JavaScript"))
               "false")))
        ;;; fails
        (it "undefined"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "undefined")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "undefined")))
        (it "js/undefined"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "js/undefined")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "undefined")))
        (it "js-undefined"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "js-undefined")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "undefined")))
        (it "js/null"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "js/null")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "null")))
        (it "js-null"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "js-null")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "null")))
        (xit "nil" (fn ()
                     (assert-equal
                      (compile (js/tag sexp "nil")
                               compilation-environment
                               (js-obj "language" "JavaScript"))
                      "null")))
        (xit "null"
             (fn ()
               (assert-equal
                (compile (js/tag sexp "null")
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "[]")))
        (it "foo-bar"
            (fn ()
              (assert-equal
               (compile 'foo-bar
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "fooBar")))
        (it "foo-bar, camelCase"
            (fn ()
              (assert-equal
               (compile 'foo-bar
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "fooBar")))
        (it "foo/bar, camelCase"
            (fn ()
              (assert-equal
               (compile 'foo/bar
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "fooBar")))
        (it "foo-bar!, camelCase"
            (fn ()
              (assert-equal
               (compile 'foo-bar!
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "fooBarX")))
        (it "foo-bar?, camelCase"
            (fn ()
              (assert-equal
               (compile 'foo-bar?
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "fooBarP")))
        (it "*foo-bar*, camelCase"
            (fn ()
              (assert-equal
               (compile '*foo-bar*
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "starFooBarStar")))
        (it "'*foo-bar*, camelCase"
            (fn ()
              (assert-equal
               (compile '(quote *foo-bar*)
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "Symbol.for('*foo-bar*')")))
        (it "A, camelCase"
            (fn ()
              (assert-equal
               (compile 'A
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "A")))
        (xit "x, camelCase"
             (fn ()
               (assert-equal
                (compile 'x
                         (new LispEnvironment
                              (list
                               (list
                                "x"
                                1
                                "variable")))
                         (js-obj "language" "JavaScript"))
                "1")))
        (it "(map symbol? '(a b c))"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define lst
                             (map symbol? '(a b c))))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "const lst = [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')].map(function (x) {
  return typeof x === 'symbol';
});")))))
    (describe "gensym"
      (fn ()
        (it "(gensym \"x\")"
            (fn ()
              (assert-equal
               (compile '(gensym "x")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "Symbol('x')")))
        (it "`(define ,(gensym \"x\") 1)"
            (fn ()
              (assert-equal
               (compile `(define ,(gensym "x") 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "const x = 1;")))
        (it "`(let ((x 0)) (define ,(gensym \"x\") 1))"
            (fn ()
              (assert-equal
               (compile `(let ((x 0))
                           (define ,(gensym "x") 1))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = 0;

const x1 = 1;")))
        (it "`(let ((x 0)) (define ,(gensym \"x\") 1) (let ((x1 0))))"
            (fn ()
              (assert-equal
               (compile `(let ((x 0))
                           (define ,(gensym "x") 1)
                           (let ((x1 0))))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = 0;

const x1 = 1;

{
  const x1 = 0;
}")))
        (xit "`(let ((x 0)) ... (let ((x1 0)) ...))"
             (fn ()
               (define gensym-x
                 (gensym "x"))
               (assert-equal
                (compile `(let ((x 0))
                            (define ,gensym-x 1)
                            (let ((x1 0))
                              (define ,gensym-x 1)))
                         compilation-environment
                         (js-obj "language" "JavaScript"
                                 "expressionType" "statement"))
                "const x = 0;

const x2 = 1;

{
  const x1 = 0;
  const x2 = 1;
}")))))
    (describe "global environment"
      (fn ()
        (it "(define lst `(,symbol? ,boolean?))"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define lst
                             `(,symbol? ,boolean?)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [symbolp, booleanp] = (() => {
  function symbolp_(obj) {
    return typeof obj === 'symbol';
  }
  function booleanp_(obj) {
    return typeof obj === 'boolean';
  }
  return [symbolp_, booleanp_];
})();

const lst = [symbolp, booleanp];")))
        (xit "(define __ (js-obj \"dash\" #t)), JS"
             (fn ()
               (assert-equal
                (compile
                 '(define-values (_ regexp)
                    (rl/sandbox
                     ((js/arrow ()
                        (define __
                          (js-obj "@@functional/placeholder" #t))
                        (define (js-regexp_ input (flags undefined))
                          (if (eq? (type-of input) "string")
                              (new RegExp input flags)
                              input))
                        (values __ js-regexp_)))))
                 compilation-environment
                 (js-obj "language" "JavaScript"
                         "inlineFunctions" #t
                         "expressionType" "statement"))
                "const [, regexp] = (() => {
  const __ = {
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
})();")))
        (it "(module m scheme ... (apply + '(1 1)) ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define one-plus-one
                             (apply + '(1 1))))
                        compilation-environment
                        (js-obj "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [_add] = (() => {
  function add_(...args) {
    let result = 0;
    for (let arg of args) {
      result = result + arg;
    }
    return result;
  }
  return [add_];
})();

const onePlusOne = _add(...[1, 1]);")))
        (it "(module m scheme ... (apply - '(1 1)) ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define one-minus-one
                             (apply - '(1 1))))
                        compilation-environment
                        (js-obj "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [_sub] = (() => {
  function sub_(...args) {
    const len = args.length;
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

const oneMinusOne = _sub(...[1, 1]);")))
        (it "(module m scheme ... (apply - '(1 1)) ...), inlineFunctions false"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define one-minus-one
                             (apply - '(1 1))))
                        compilation-environment
                        (js-obj "inlineFunctions" #f
                                "language" "JavaScript"))
               "import {
  _sub
} from 'roselisp';

const oneMinusOne = _sub(...[1, 1]);")))
        (it "(module m scheme ... (apply * '(1 1)) ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define one-times-one
                             (apply * '(1 1))))
                        compilation-environment
                        (js-obj "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [_mul] = (() => {
  function mul_(...args) {
    let result = 1;
    for (let arg of args) {
      result = result * arg;
    }
    return result;
  }
  return [mul_];
})();

const oneTimesOne = _mul(...[1, 1]);")))
        (it "(module m scheme ... (apply / '(1 1)) ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define one-divided-by-one
                             (apply / '(1 1))))
                        compilation-environment
                        (js-obj "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [_div] = (() => {
  function div_(...args) {
    if (args.length === 1) {
      return 1 / args[0];
    } else {
      let result = args[0];
      const _end = args.length;
      for (let i = 1; i < _end; i++) {
        result = result / args[i];
      }
      return result;
    }
  }
  return [div_];
})();

const oneDividedByOne = _div(...[1, 1]);")))
        (it "(module m scheme ... (apply string-append '(\"foo\" \"bar\")) ...)"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (define foo-bar
                     (apply string-append '("foo" "bar"))))
                compilation-environment
                (js-obj "inlineFunctions" #t
                        "language" "JavaScript"))
               "const [stringAppend] = (() => {
  function stringAppend_(...args) {
    return args.reduce(function (acc, x) {
      return acc + x;
    }, '');
  }
  return [stringAppend_];
})();

const fooBar = stringAppend(...['foo', 'bar']);")))
        (xit "(module m lisp ... (my-foldl + 0 '(1 2 3 4)) ...)"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (define (my-foldl f v l)
                              (foldl f v l))
                            (define bar
                              (my-foldl + 0 '(1 2 3 4))))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "inlineFunctions" #t
                                 "language" "JavaScript"))
                "const [add] = (function () {
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

const bar = myFoldl(add, 0, [1, 2, 3, 4]);")))
        (xit "(module m lisp (define (my-foldl ...) ...))"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (define (my-foldl f v l)
                              (foldl f v l)))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "inlineFunctions" #t
                                 "language" "JavaScript"))
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
}")))
        (it "(module m lisp ... (my-map first '((1) (2) (3))) ...)"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-map f x)
                             (map f x))
                           (define bar
                             (my-map first '((1) (2) (3)))))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [first] = (() => {
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

const bar = myMap(first, [[1], [2], [3]]);")))
        (xit "(module m lisp (define (my-push-4 ...) ...))"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (define (foo f x y)
                              (f x y))
                            (define (my-push-4 lst x)
                              (foo push! lst x)))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "inlineFunctions" #t
                                 "language" "JavaScript"))
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
}")))
        (xit "(module m lisp (define (my-push-5 ...) ...))"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (define (get-push-function)
                              push!)
                            (define (my-push-4 lst x)
                              ((get-push-function) lst x)))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "inlineFunctions" #t
                                 "language" "JavaScript"))
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
}")))
        (it "(module m lisp (define (my-cdr ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-cdr x)
                             (cdr x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [cdr] = (() => {
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
}")))
        (it "(module m lisp (define (my-intersection ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-intersection x y)
                             (intersection x y)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "inlineFunctions" #t
                                "language" "JavaScript"))
               "const [intersection] = (() => {
  function intersection_(...args) {
    function intersection2(arr1, arr2) {
      const result = [];
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
}")))
        (it "(module m scheme ... (apply + '(1 1)) ...), comment"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; Module header.

  (define one-plus-one
    (apply + '(1 1))))")
                compilation-environment
                (js-obj "inlineFunctions" #t
                        "language" "JavaScript"))
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

const onePlusOne = _add(...[1, 1]);")))
        (it "(module m scheme ... (apply + '(1 1)) ...), comments"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  ;;; Module header.

  ;;; Custom addition function.
  (define one-plus-one
    (apply + '(1 1))))")
                compilation-environment
                (js-obj "inlineFunctions" #t
                        "language" "JavaScript"))
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
const onePlusOne = _add(...[1, 1]);")))))
    (describe "strings"
      (fn ()
        (it "\"\""
            (fn ()
              (assert-equal
               (compile ""
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "''")))
        (it "\"foo\""
            (fn ()
              (assert-equal
               (compile "foo"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'foo'")))
        (it "\"don't\""
            (fn ()
              (assert-equal
               (compile "don't"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'don\\'t'")))
        (it "\"newline\\ntest\" 1"
            (fn ()
              (assert-equal
               (compile "newline
test"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'newline\\n' +
  'test'")))
        (it "\"newline\\ntest\" 2"
            (fn ()
              (assert-equal
               (compile "newline\ntest"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'newline\\n' +
  'test'")))
        (it "\"newline\\ntest\" 3"
            (fn ()
              (assert-equal
               (compile "newline
test
three"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'newline\\n' +
  'test\\n' +
  'three'")))
        (xit "tab"
             (fn ()
               (assert-equal
                (compile "\\t"
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "	")))
        (it "\"\\s\" 1"
            (fn ()
              (assert-equal
               (compile "\\s"
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'\\\\s'")))
        (it "\"\\s\" 2"
            (fn ()
              (assert-equal
               (compile (js/tag sexp "\"\\\\s\"")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'\\\\s'")))))
    (describe "function calls"
      (fn ()
        (xit "(I x), JS function"
             (fn ()
               (assert-equal
                (compile '(I x)
                         (new LispEnvironment
                              (list
                               (list
                                "I"
                                (lambda (x)
                                  x)
                                "function")))
                         (js-obj "language" "JavaScript"))
                "(function {
   let I = function(x) {
     return x;
   }
   return I;
})()(x)")))
        (it "(truep x)"
            (fn ()
              (assert-equal
               (compile '(truep x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "(() => {
  function truep(x) {
    return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;
  }
  return truep;
})()(x)")))
        (it "(falsep x)"
            (fn ()
              (assert-equal
               (compile '(falsep x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "(() => {
  function falsep(x) {
    return !truep(x);
  }
  function truep(x) {
    return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;
  }
  return falsep;
})()(x)")))))
    (describe "()"
      (fn ()
        (it "()"
            (fn ()
              (assert-equal
               (compile '()
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))))
    (describe "list"
      (fn ()
        (it "(list)"
            (fn ()
              (assert-equal
               (compile '(list)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))
        (it "(list 1)"
            (fn ()
              (assert-equal
               (compile '(list 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1]")))
        (it "(list (list 1))"
            (fn ()
              (assert-equal
               (compile '(list (list 1))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1]]")))))
    (describe "member?"
      (fn ()
        (it "(member? 2 (list 1 2 3 4) f)"
            (fn ()
              (assert-equal
               (compile '(member? 2 (list 1 2 3 4) f)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, 2, 3, 4].findIndex(function (x) {
  return f(2, x);
}) >= 0")))
        (it "(member? (+ 1 1) (list 1 2 3 4) f)"
            ;; TODO: Better compilation of this case:
            ;; `v` should be stored in a local variable.
            (fn ()
              (assert-equal
               (compile '(member? (+ 1 1) (list 1 2 3 4) f)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, 2, 3, 4].findIndex(function (x) {
  return f(1 + 1, x);
}) >= 0")))))
    (describe "memq?"
      (fn ()
        (it "(memq? 2 (list 1 2 3 4))"
            (fn ()
              (assert-equal
               (compile '(memq? 2 (list 1 2 3 4))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, 2, 3, 4].includes(2)")))
        (it "(memq? (+ 1 1) (list 1 2 3 4))"
            (fn ()
              (assert-equal
               (compile '(memq? (+ 1 1) (list 1 2 3 4))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, 2, 3, 4].includes(1 + 1)")))

        ))
    (describe "append"
      (fn ()
        (it "(append)"
            (fn ()
              (assert-equal
               (compile '(append)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))
        (it "(append foo)"
            (fn ()
              (assert-equal
               (compile '(append foo)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[...foo]")))
        (it "(append foo bar)"
            (fn ()
              (assert-equal
               (compile '(append foo bar)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[...foo, ...bar]")))
        (it "(append (list))"
            (fn ()
              (assert-equal
               (compile '(append (list))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))
        (it "(append (list x))"
            (fn ()
              (assert-equal
               (compile '(append (list x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[x]")))
        (it "(append '(\"foo\") '(\"bar\"))"
            (fn ()
              (assert-equal
               (compile '(append '("foo") '("bar"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "['foo', 'bar']")))))
    (describe "quote"
      (fn ()
        (it "'x"
            (fn ()
              (assert-equal
               (compile '(quote x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "Symbol.for('x')")))
        (it "'()"
            (fn ()
              (assert-equal
               (compile '(quote ())
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))
        (it "'(1)"
            (fn ()
              (assert-equal
               (compile '(quote (1))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1]")))
        (it "'(1 . 2)"
            (fn ()
              (assert-equal
               (compile '(quote (1 . 2))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, Symbol.for('.'), 2]")))
        (it "'((1))"
            (fn ()
              (assert-equal
               (compile '(quote ((1)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1]]")))
        (it "'(x y z)"
            (fn ()
              (assert-equal
               (compile '(quote (x y z))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]")))))
    (describe "quasiquote"
      (fn ()
        (it "`x"
            (fn ()
              (assert-equal
               (compile '(quasiquote x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "Symbol.for('x')")))
        (it "`()"
            (fn ()
              (assert-equal
               (compile '(quasiquote ())
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[]")))
        (it "`(1)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (1))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1]")))
        (it "`(1 . 2)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (1 . 2))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, Symbol.for('.'), 2]")))
        (it "`((1 . 2))"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((1 . 2)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1, Symbol.for('.'), 2]]")))
        (it "`((1 . ,2))"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((1 . (unquote 2))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1, Symbol.for('.'), 2]]")))
        (it "`((1 . ,2) (3 . ,4))"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((1 . (unquote 2))
                                      (3 . (unquote 4))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]]")))
        (it "(define test-map-1 ...)"
            (fn ()
              (assert-equal
               (compile '(define test-map-1
                           `(("foo" . ,test-fn)
                             ("bar" . ,test-fn)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "const testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];")))
        (it "`((1))"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((1)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[[1]]")))
        (it "`(x y z)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y z))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]")))
        (it "`(x y ,z)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (unquote z)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), z]")))
        (it "`(x y ,@z)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (unquote-splicing z)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), ...z]")))
        (it "`(x y `z)"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (quasiquote z)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]]")))
        (it "`(x y `(z))"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (quasiquote (z))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]]")))
        (it "`(x y `(,z))"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (quasiquote ((unquote z)))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]]")))
        (it "`(x y `(,@z))"
            (fn ()
              (assert-equal
               (compile '(quasiquote (x y (quasiquote ((unquote-splicing z)))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]]")))
        (it "`(,@x)"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((unquote-splicing x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[...x]")))
        (it "`(,@x ,@y)"
            (fn ()
              (assert-equal
               (compile '(quasiquote ((unquote-splicing x)
                                      (unquote-splicing y)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[...x, ...y]")))
        (it "(set! let-exp `(let ((,arg-list (quote ,args))) ,@body))"
            (fn ()
              (assert-equal
               (compile '(set! let-exp
                               (quasiquote
                                (let (((unquote arg-list)
                                       (quote (unquote args))))
                                  (unquote-splicing body))))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "letExp = [Symbol.for('let'), [[argList, [Symbol.for('quote'), args]]], ...body];")))))
    (describe "apply"
      (fn ()
        (it "(apply f args)"
            (fn ()
              (assert-equal
               (compile '(apply f args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "f(...args)")))
        (it "(apply f x args)"
            (fn ()
              (assert-equal
               (compile '(apply f x args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "f(x, ...args)")))
        (it "(apply new Foo args)"
            (fn ()
              (assert-equal
               (compile '(apply new Foo args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Foo(...args)")))
        (it "(apply new Foo x y args)"
            (fn ()
              (assert-equal
               (compile '(apply new Foo x y args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Foo(x, y, ...args)")))
        (xit "(apply send obj method args)"
             (fn ()
               (assert-equal
                (compile '(apply send obj method args)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "obj.method(...args)")))
        (it "(apply (get-field method obj) args)"
            (fn ()
              (assert-equal
               (compile '(apply (get-field method obj) args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "obj.method(...args)")))
        (it "(apply (.-method obj) args)"
            (fn ()
              (assert-equal
               (compile '(apply (.-method obj) args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "obj.method(...args)")))))
    (describe "define"
      (fn ()
        (it "(define x), JS"
            (fn ()
              (assert-equal
               (compile '(define x)
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "let x;")))
        (it "(define x), TS"
            (fn ()
              (assert-equal
               (compile '(define x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "let x: any;")))
        (it "(define x 1), JS"
            (fn ()
              (assert-equal
               (compile '(define x 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "const x = 1;")))
        (it "(define x 1), TS"
            (fn ()
              (assert-equal
               (compile '(define x 1)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "const x: any = 1;")))
        (xit "(define I (lambda (x) x))"
             (fn ()
               (assert-equal
                (compile '(define I
                            (lambda (x)
                              x))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "function I(x) {
  return x;
}")))
        (it "(define I (memoize (lambda (x) x)))"
            (fn ()
              (assert-equal
               (compile '(define I
                           (memoize
                            (lambda (x)
                              x)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const I = memoize(function (x) {
  return x;
});")))
        (it "(define (identity-function x) x), camelCase"
            (fn ()
              (assert-equal
               (compile '(define (identity-function x)
                           x)
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function identityFunction(x) {
  return x;
}")))
        (it "(define (I x) x)"
            (fn ()
              (assert-equal
               (compile '(define (I x)
                           x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function I(x) {
  return x;
}")))
        (it "(define (K x y) x)"
            (fn ()
              (assert-equal
               (compile '(define (K x y)
                           x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function K(x, y) {
  return x;
}")))
        (it "(define (S f g x) (f x (g x)))"
            (fn ()
              (assert-equal
               (compile '(define (S f g x)
                           (f x (g x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function S(f, g, x) {
  return f(x, g(x));
}")))
        (it "(define (S f g x) ((f x) (g x)))"
            (fn ()
              (assert-equal
               (compile '(define (S f g x)
                           ((f x) (g x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function S(f, g, x) {
  return f(x)(g(x));
}")))
        (it "(define (C f x y) (f y x))"
            (fn ()
              (assert-equal
               (compile '(define (C f x y)
                           (f y x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function C(f, x, y) {
  return f(y, x);
}")))
        (it "(define (U f) (f f))"
            (fn ()
              (assert-equal
               (compile '(define (U f)
                           (f f))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function U(f) {
  return f(f);
}")))
        (it "(define (A f . args) (apply f args)), JS"
            (fn ()
              (assert-equal
               (compile '(define (A f . args)
                           (apply f args))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function A(f, ...args) {
  return f(...args);
}")))
        (it "(define (A f . args) (apply f args)), TS"
            (fn ()
              (assert-equal
               (compile '(define (A f . args)
                           (apply f args))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function A(f: any, ...args: any[]): any {
  return f(...args);
}")))
        (it "(define (Q . args) ...)"
            (fn ()
              (assert-equal
               (compile
                '(define (Q . args)
                   (cond
                    ((= (.-length args) 0)
                     undefined)
                    ((= (.-length args) 1)
                     (aref args 0))
                    (else
                     (let ((fs (.slice args 0 -1))
                           (x (aref args (- (.-length args) 1))))
                       (.reduce fs (lambda (acc f) (f acc)) x)))))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function Q(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    const fs = args.slice(0, -1);
    const x = args[args.length - 1];
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}")))
        (it "(define (T . args) ...)"
            (fn ()
              (assert-equal
               (compile
                '(define (T . args)
                   (cond
                    ((= (.-length args) 0)
                     undefined)
                    ((= (.-length args) 1)
                     (aref args 0))
                    (else
                     (let-values (((x . fs) args))
                       (.reduce fs (lambda (acc f) (f acc)) x)))))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function T(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    const [x, ...fs] = args;
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}")))
        (it "(define (Y f) ...)"
            (fn ()
              (assert-equal
               (compile
                '(define (Y f)
                   ((lambda (future)
                      (f (lambda (arg)
                           ((future future) arg))))
                    (lambda (future)
                      (f (lambda (arg)
                           ((future future) arg))))))
                compilation-environment
                (js-obj "language" "JavaScript"))
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
}")))
        (it "(define (compose f g) (lambda (x) (f (g x))))"
            (fn ()
              (assert-equal
               (compile '(define (compose f g)
                           (lambda (x)
                             (f (g x))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function compose(f, g) {
  return function (x) {
    return f(g(x));
  };
}")))
        (it "(define (foo) (set! x (+ x 1)) ...)"
            (fn ()
              (assert-equal
               (compile '(define (foo)
                           (set! x (+ x 1))
                           (set! y (+ y 1)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function foo() {
  x++;
  return ++y;
}")))
        (it "(define (mapGet map path) ...), JS"
            (fn ()
              (assert-equal
               (compile
                '(define (mapGet map path)
                   (let-values (((value) (mapGet2 map path)))
                     value))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function mapGet(map, path) {
  const [value] = mapGet2(map, path);
  return value;
}")))
        (it "(define _ (js-obj \"dash\" #t)), JS"
            (fn ()
              (assert-equal
               (compile
                '(define _ (js-obj "dash" #t))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const _ = {
  dash: true
};")))
        (it "(define __ (js-obj \"dash\" #t)), JS"
            (fn ()
              (assert-equal
               (compile
                '(define __ (js-obj "dash" #t))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const __ = {
  dash: true
};")))
        (xit "(lambda (env (options (js-obj))) ...), TS"
             (fn ()
               (assert-equal
                (compile
                 '(lambda (env (options (js-obj)))
                    (let ((language (oget options "language")))
                      (set! language (or language default-language))
                      (let ((compilation-env (or (.get compilation-map
                                                       language)
                                                 javascript-env)))
                        (new CompilationEvaluator
                             env
                             compilation-env
                             options))))
                 compilation-environment
                 (js-obj "language" "TypeScript"))
                "function (env: any, options: any = {}): any {
  let language: any = options['language'];
  language = language || (default-language);
  {
    {
      let compilation-env: any = (compilation-map.get(language)) || (javascript-env);
      return new CompilationEvaluator(env, compilation-env, options);
    }
  }
}")))
        (it "(define (add-matrix ...) ...), JS"
            (fn ()
              (assert-equal
               (compile
                '(define (add-matrix m1 m2)
                   (let ((l1 (array-list-length m1))
                         (l2 (array-list-length m2)))
                     (let ((matrix (make-matrix l1 l2)))
                       (for ((i (range 0 l1)))
                         (for ((j (range 0 l2)))
                           (set! (aget (aget matrix j) i)
                                 (+ (aget (aget m1 j) i)
                                    (aget (aget m2 j) i)))))
                       matrix)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "function addMatrix(m1, m2) {
  const l1 = m1.length;
  const l2 = m2.length;
  const matrix = makeMatrix(l1, l2);
  for (let i = 0; i < l1; i++) {
    for (let j = 0; j < l2; j++) {
      matrix[j][i] = m1[j][i] + m2[j][i];
    }
  }
  return matrix;
}")))))
    (describe "funcall"
      (fn ()
        (it "(funcall f x)"
            (fn ()
              (assert-equal
               (compile '(funcall f x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "f(x)")))
        (it "(funcall f x y)"
            (fn ()
              (assert-equal
               (compile '(funcall f x y)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "f(x, y)")))))
    (describe "lambda"
      (fn ()
        (it "(lambda (x) x), JS"
            (fn ()
              (assert-equal
               (compile '(lambda (x)
                           x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (x) {
  return x;
}")))
        (it "(lambda (x) x), TS"
            (fn ()
              (assert-equal
               (compile '(lambda (x)
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function (x: any): any {
  return x;
}")))
        (it "(lambda args args)"
            (fn ()
              (assert-equal
               (compile '(lambda args
                           args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (...args) {
  return args;
}")))
        (it "(lambda (x . args) args)"
            (fn ()
              (assert-equal
               (compile '(lambda (x . args)
                           args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (x, ...args) {
  return args;
}")))
        (it "(lambda (x y . args) args)"
            (fn ()
              (assert-equal
               (compile '(lambda (x y . args)
                           args)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (x, y, ...args) {
  return args;
}")))
        (it "(lambda (x) (let ((x 1)) x))"
            (fn ()
              (assert-equal
               (compile '(lambda (x)
                           (let ((x 1))
                             x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (x) {
  {
    const x = 1;
    return x;
  }
}")))
        (it "(lambda (x) (let ((y 1)) y))"
            (fn ()
              (assert-equal
               (compile '(lambda (x)
                           (let ((y 1))
                             y))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (x) {
  const y = 1;
  return y;
}")))
        (it "(lambda (given (surname \"Smith\")) ...), JS"
            (fn ()
              (assert-equal
               (compile '(lambda (given (surname "Smith"))
                           (string-append
                            "Hello, "
                            given
                            " "
                            surname))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function (given, surname = 'Smith') {
  return 'Hello, ' + given + ' ' + surname;
}")))
        (it "(lambda (given (surname \"Smith\")) ...), TS"
            (fn ()
              (assert-equal
               (compile '(lambda (given (surname "Smith"))
                           (string-append
                            "Hello, "
                            given
                            " "
                            surname))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function (given: any, surname: any = 'Smith'): any {
  return 'Hello, ' + given + ' ' + surname;
}")))
        (it "(lambda (arg (options (js-obj))) ...), TS"
            (fn ()
              (assert-equal
               (compile '(lambda (arg (options (js-obj)))
                           arg)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function (arg: any, options: any = {}): any {
  return arg;
}")))
        (xit "(lambda (this arg) args), JS"
             (fn ()
               (assert-equal
                (compile '(lambda (this arg)
                            arg)
                         compilation-environment
                         (js-obj "language" "TypeScript"))
                "function (arg: any): any {
  return arg;
}")))
        (xit "(lambda (this . args) args), JS"
             (fn ()
               (assert-equal
                (compile '(lambda (this . args)
                            args)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "function (...args: any[]): any {
  return args;
}")))
        (xit "(lambda (this arg) args), TS"
             (fn ()
               (assert-equal
                (compile '(lambda (this arg)
                            arg)
                         compilation-environment
                         (js-obj "language" "TypeScript"))
                "function (this: any, arg: any): any {
  return arg;
}")))
        (xit "(lambda (this . args) args), TS"
             (fn ()
               (assert-equal
                (compile '(lambda (this . args)
                            args)
                         compilation-environment
                         (js-obj "language" "TypeScript"))
                "function (this: any, ...args: any[]): any {
  return args;
}")))))
    (describe "let"
      (fn ()
        (it "(let (x)), statement, JS"
            (fn ()
              (assert-equal
               (compile '(let (x))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "let x;")))
        (it "(let (x) x), statement, JS"
            (fn ()
              (assert-equal
               (compile '(let (x)
                           x)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "let x;

return x;")))
        (it "(let (x) x), expression, JS"
            (fn ()
              (assert-equal
               (compile '(let (x)
                           x)
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "(() => {
  let x;
  return x;
})()")))
        (it "(let (x) x), statement, TS"
            (fn ()
              (assert-equal
               (compile '(let (x)
                           x)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "TypeScript"))
               "let x: any;

return x;")))
        (it "(let ((x 1)) x), JS"
            (fn ()
              (assert-equal
               (compile '(let ((x 1))
                           x)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "const x = 1;

return x;")))
        (it "(let ((x 1)) x), TS"
            (fn ()
              (assert-equal
               (compile '(let ((x 1))
                           x)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "TypeScript"))
               "const x: any = 1;

return x;")))
        (xit "(let ((a 1)) (+ (let ((a 2)) a) a))"
             (fn ()
               (assert-equal
                (compile '(let ((a 1))
                            (+ (let ((a 2)) a) a))
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "JavaScript"))
                "const a = 1;

(() => {
  const a = 2;
  return a;
})() + a;")))
        (it "(let ((compose ...) ...) ...)"
            (fn ()
              (assert-equal
               (compile
                '(let ((compose (lambda (f g)
                                  (lambda (x)
                                    (f (g x)))))
                       (square (lambda (x) (* x x)))
                       (add1 (lambda (x) (+ x 1))))
                   (display ((compose square add1) (add1 4))))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "const compose = function (f, g) {
  return function (x) {
    return f(g(x));
  };
};

const square = function (x) {
  return x * x;
};

const add1 = function (x) {
  return x + 1;
};

console.log(compose(square, add1)(add1(4)));")))
        (it "(let ((and ...)) (and x y))"
            (fn ()
              (assert-equal
               (compile
                '(let ((and (lambda (x y)
                              (if x (if y #t #f) #f))))
                   (and x y))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "const and = function (x, y) {
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

and(x, y);")))
        ;; FIXME: This test is incorrect.
        ;; See the one below.
        (xit "(begin x (let ((x 1)) x)), TS"
             (fn ()
               (assert-equal
                (compile '(begin
                            x
                            (let ((x 1))
                              x))
                         compilation-environment
                         (js-obj "expressionType" "return"
                                 "language" "TypeScript"))
                "x;

const x: any = 1;

return x;")))
        ;; FIXME: Make this test pass.
        (xit "(begin x (let ((x 1)) x)), TS"
             (fn ()
               (assert-equal
                (compile '(begin
                            x
                            (let ((x 1))
                              x))
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "TypeScript"))
                "x;

{
  const x: any = 1;
  x;
}")))
        (it "(cond ... (else x (let ...))), TS"
            (fn ()
              (assert-equal
               (compile '(cond
                          (foo
                           bar)
                          (else
                           x
                           (let ((x 1))
                             x)))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "TypeScript"))
               "if (foo) {
  return bar;
} else {
  x;
  const x: any = 1;
  return x;
}")))
        (it "(define make-compilation-evaluator ...), TS"
            (fn ()
              (assert-equal
               (compile
                '(define make-compilation-evaluator
                   (memoize
                    (lambda (env (options (js-obj)))
                      (let ((language (oget options "language")))
                        (set! language (or language default-language))
                        (let ((compilation-env (or (.get compilation-map
                                                         language)
                                                   javascript-env)))
                          (new CompilationEvaluator
                               env
                               compilation-env
                               options))))))
                compilation-environment
                (js-obj "language" "TypeScript"))
               "const makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {
  let language: any = options['language'];
  language = language || defaultLanguage;
  const compilationEnv: any = compilationMap.get(language) || javascriptEnv;
  return new CompilationEvaluator(env, compilationEnv, options);
});")))
        (it "(define make-compilation-evaluator ...), JS"
            (fn ()
              (assert-equal
               (compile '(cond
                          (foo
                           (let ((x #t))
                             x))
                          (else
                           #f))
                        compilation-environment
                        (js-obj "expressionType" "return"))
               "if (foo) {
  const x = true;
  return x;
} else {
  return false;
}")))))
    (describe "let-values"
      (fn ()
        (it "(let-values ((value (foo bar baz))) value), JS"
            (fn ()
              (assert-equal
               (compile '(let-values ((value (foo bar baz)))
                           value)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "const value = foo(bar, baz);

return value;")))
        (it "(let-values (((value) (foo bar baz))) value), JS"
            (fn ()
              (assert-equal
               (compile '(let-values (((value) (foo bar baz)))
                           value)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "const [value] = foo(bar, baz);

return value;")))
        (it "(let-values (((value) (foo bar baz))) value), TS"
            (fn ()
              (assert-equal
               (compile '(let-values (((value) (foo bar baz)))
                           value)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "TypeScript"))
               "const [value]: any[] = foo(bar, baz);

return value;")))
        (it "(let-values (((x . fs) args)) ...), JS"
            (fn ()
              (assert-equal
               (compile '(let-values (((x . fs) args))
                           (.reduce fs (lambda (acc f) (f acc)) x))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const [x, ...fs] = args;

fs.reduce(function (acc, f) {
  return f(acc);
}, x);")))
        (it "(let-values (((x . fs) args)) ...), TS"
            (fn ()
              (assert-equal
               (compile '(let-values (((x . fs) args))
                           (.reduce fs (lambda (acc f) (f acc)) x))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "TypeScript"))
               "const [x, ...fs]: any[] = args;

fs.reduce(function (acc: any, f: any): any {
  return f(acc);
}, x);")))
        (it "(let-values (((value1) ...) ((value2) ...)) ...)"
            (fn ()
              (assert-equal
               (compile '(let-values (((value1) (foo bar))
                                      ((value2) (bar baz)))
                           (list value1 value2))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "const [value1] = foo(bar);

const [value2] = bar(baz);

return [value1, value2];")))
        (it "(begin ... (let-values ...)), JS"
            (fn ()
              (assert-equal
               (compile '(begin
                           value
                           (let-values ((value (foo bar baz)))
                             value))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "value;

const value = foo(bar, baz);

return value;")))))
    (describe "define-values"
      (fn ()
        (it "(define-values value (foo bar baz)), JS"
            (fn ()
              (assert-equal
               (compile
                '(define-values value
                   (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "const value = foo(bar, baz);")))
        (it "(define-values (value) (foo bar baz)), JS"
            (fn ()
              (assert-equal
               (compile
                '(define-values (value)
                   (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "const [value] = foo(bar, baz);")))
        (it "(define-values (value) (foo bar baz)), TS"
            (fn ()
              (assert-equal
               (compile
                '(define-values (value)
                   (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "TypeScript"))
               "const [value]: any[] = foo(bar, baz);")))
        (it "(define-values (_ _ value) (foo bar baz)), TS"
            (fn ()
              (assert-equal
               (compile
                '(define-values (_ _ value)
                   (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "TypeScript"))
               "const [, , value]: any[] = foo(bar, baz);")))
        (it "(define-values (_ __ value) :hole-marker __ (foo bar baz)), TS"
            (fn ()
              (assert-equal
               (compile
                '(define-values (_ __ value)
                   :hole-marker __
                   (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "TypeScript"))
               "const [_, , value]: any[] = foo(bar, baz);")))))
    (describe "set!-values"
      (fn ()
        (it "(set!-values (value) (foo bar baz)), JS"
            (fn ()
              (assert-equal
               (compile
                '(set!-values (value) (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "[value] = foo(bar, baz);")))
        (it "(set!-values (_ value) (foo bar baz)), TS"
            (fn ()
              (assert-equal
               (compile
                '(set!-values (_ value) (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "[, value] = foo(bar, baz);")))
        (it "(set!-values (_ __ value) :hole-marker __ (foo bar baz)), JS"
            (fn ()
              (assert-equal
               (compile
                '(set!-values (_ __ value)
                              :hole-marker __
                              (foo bar baz))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "[_, , value] = foo(bar, baz);")))))
    (describe "let-js-obj"
      (fn ()
        (it "(let-js-obj (((prop) obj)) prop), JS"
            (fn ()
              (assert-equal
               (compile '(let-js-obj (((prop) obj))
                                     prop)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const {prop} = obj;

prop;")))))
    (describe "define-js-obj"
      (fn ()
        (it "(define-js-obj (prop) obj), JS"
            (fn ()
              (assert-equal
               (compile '(define-js-obj (prop) obj)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const {prop} = obj;")))
        (it "(define-js-obj (prop) obj), TS"
            (fn ()
              (assert-equal
               (compile '(define-js-obj (prop) obj)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "TypeScript"))
               "const {prop} = obj;")))
        (it "(define-js-obj ((x y) z) obj), JS"
            (fn ()
              (assert-equal
               (compile '(define-js-obj ((x y) z) obj)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const {x: y, z} = obj;")))
        (it "(define-js-obj ((x y) z) obj), TS"
            (fn ()
              (assert-equal
               (compile '(define-js-obj ((x y) z) obj)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "TypeScript"))
               "const {x: y, z} = obj;")))))
    (describe "set!-js-obj"
      (fn ()
        (it "(set!-js-obj (prop) obj), JS"
            (fn ()
              (assert-equal
               (compile '(set!-js-obj (prop) obj)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "({prop} = obj);")))))
    (describe "set!"
      (fn ()
        (it "(set! x 1)"
            (fn ()
              (assert-equal
               (compile '(set! x 1)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "x = 1;")))
        (it "(set! x (add1 x)), expression"
            (fn ()
              (assert-equal
               (compile '(set! x (add1 x))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "++x")))
        (it "(set! x (sub1 x)), expression"
            (fn ()
              (assert-equal
               (compile '(set! x (sub1 x))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "--x")))
        (it "(set! x (+ x 1)), expression"
            (fn ()
              (assert-equal
               (compile '(set! x (+ x 1))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "++x")))
        (it "(set! x (+ x 1)), statement"
            (fn ()
              (assert-equal
               (compile '(set! x (+ x 1))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "x++;")))
        (it "(set! x (+ x 1)), return statement"
            (fn ()
              (assert-equal
               (compile '(set! x (+ x 1))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "return ++x;")))))
    (describe "setq"
      (fn ()
        (it "(setq x 1)"
            (fn ()
              (assert-equal
               (compile '(setq x 1)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "x = 1;")))))
    (describe "+"
      (fn ()
        (it "(+ x 1)"
            (fn ()
              (assert-equal
               (compile '(+ x 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x + 1")))
        (it "(+ x 1 2)"
            (fn ()
              (assert-equal
               (compile '(+ x 1 2)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x + 1 + 2")))))
    (describe "-"
      (fn ()
        (it "(- x)"
            (fn ()
              (assert-equal
               (compile '(- x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "-x")))
        (xit "(- (- x))"
             (fn ()
               (assert-equal
                (compile '(- (- x))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x")))
        (it "(- x 1)"
            (fn ()
              (assert-equal
               (compile '(- x 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x - 1")))
        (it "(- x 1 2)"
            (fn ()
              (assert-equal
               (compile '(- x 1 2)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x - 1 - 2")))))
    (describe "mod"
      (fn ()
        (it "(mod x y)"
            (fn ()
              (assert-equal
               (compile '(mod x y)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x % y")))))
    (describe "begin"
      (fn ()
        (it "(begin x y z), statement"
            (fn ()
              (assert-equal
               (compile '(begin x y z)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "x;

y;

z;")))
        (it "(begin x (begin y z)), statement"
            (fn ()
              (assert-equal
               (compile '(begin x (begin y z))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "x;

y;

z;")))
        (it "(begin x y z), expression"
            (fn ()
              (assert-equal
               (compile '(begin x y z)
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "(() => {
  x;
  y;
  return z;
})()")))
        (it "redefine core functions"
            (fn ()
              (assert-equal
               (compile
                '(begin
                   ;; Redefine core functions (nonsensically).
                   (define (and x y)
                     (or x y))
                   (define (or x y) x)
                   (and x (or y z)))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "function and(x, y) {
  return or(x, y);
}

function or(x, y) {
  return x;
}

and(x, or(y, z));")))
        (it "import core functions"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (require (only-in "foo"
                                     and
                                     or))
                   (and x (or y z)))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "import {
  and,
  or
} from 'foo';

and(x, or(y, z));")))))
    (describe "provide"
      (fn ()
        (it "(provide)"
            (fn ()
              (assert-equal
               (compile '(provide)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "")))
        (it "(provide x)"
            (fn ()
              (assert-equal
               (compile '(provide x)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x
};")))
        (it "(provide x y)"
            (fn ()
              (assert-equal
               (compile '(provide x y)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x,
  y
};")))
        (it "(provide (rename-out (x y)))"
            (fn ()
              (assert-equal
               (compile '(provide (rename-out (x y)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x as y
};")))
        (it "(provide (rename-out (x y) (w z)))"
            (fn ()
              (assert-equal
               (compile '(provide (rename-out (x y) (w z)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x as y,
  w as z
};")))
        (it "(provide x (rename-out (y z)))"
            (fn ()
              (assert-equal
               (compile '(provide x (rename-out (y z)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x,
  y as z
};")))
        (it "(provide x x)"
            (fn ()
              (assert-equal
               (compile '(provide x x)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x
};")))
        (it "(provide x (rename-out (y x)))"
            (fn ()
              (assert-equal
               (compile '(provide x (rename-out (y x)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x
};")))
        (it "(provide (rename-out (y x)))"
            (fn ()
              (assert-equal
               (compile '(provide (rename-out (x js/undefined)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export {
  x as jsUndefined
};")))
        (it "(provide (all-from-out \"foo\"))"
            (fn ()
              (assert-equal
               (compile '(provide (all-from-out "foo"))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export * from 'foo';")))
        (it "(provide (all-from-out \"foo\") bar)"
            (fn ()
              (assert-equal
               (compile '(provide
                           (all-from-out "foo")
                           bar)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "export * from 'foo';

export {
  bar
};")))))
    (describe "require"
      (fn ()
        (it "(require \"foo\")"
            (fn ()
              (assert-equal
               (compile '(require "foo")
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"
                                "esModuleInterop" #f))
               "import * as foo from 'foo';")))
        (it "(require \"foo\"), esModuleInterop"
            (fn ()
              (assert-equal
               (compile '(require "foo")
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"
                                "esModuleInterop" #t))
               "import foo from 'foo';")))
        (it "(require foo \"bar\")"
            (fn ()
              (assert-equal
               (compile '(require foo "bar")
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"
                                "esModuleInterop" #f))
               "import * as foo from 'bar';")))
        (it "(require foo \"bar\"), esModuleInterop"
            (fn ()
              (assert-equal
               (compile '(require foo "bar")
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"
                                "esModuleInterop" #t))
               "import foo from 'bar';")))
        (it "(require \"foo\" \"bar\"), esModuleInterop"
            (fn ()
              (assert-equal
               (compile '(require "foo" "bar")
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"
                                "esModuleInterop" #t))
               "import foo from 'bar';")))
        (it "(require (only-in foo bar))"
            (fn ()
              (assert-equal
               (compile '(require (only-in foo
                                           bar))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "import {
  bar
} from 'foo';")))
        (it "(require (only-in foo (bar baz)))"
            (fn ()
              (assert-equal
               (compile '(require (only-in foo
                                           (bar baz)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "import {
  bar as baz
} from 'foo';")))
        (it "(require (only-in \"foo\" (bar baz)))"
            (fn ()
              (assert-equal
               (compile '(require (only-in "foo"
                                           (bar baz)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "import {
  bar as baz
} from 'foo';")))
        (it "(require (only-in foo bar bar))"
            (fn ()
              (assert-equal
               (compile '(require (only-in foo bar bar))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "import {
  bar
} from 'foo';")))
        (it "(require (only-in foo bar (baz bar)))"
            (fn ()
              (assert-equal
               (compile '(require (only-in foo
                                           bar
                                           (baz bar)))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "import {
  bar
} from 'foo';")))
        (xit "(require 'foo \"bar\")"
             (fn ()
               (assert-equal
                (compile '(require 'foo "bar")
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "JavaScript"))
                "import foo from 'bar';")))
        (xit "(require foo :as bar)"
             (fn ()
               (assert-equal
                (compile '(require foo :as bar)
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "JavaScript"))
                "import bar from 'foo';")))
        (xit "(require (foo :as bar))"
             (fn ()
               (assert-equal
                (compile '(require (foo :as bar))
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "JavaScript"))
                "import bar from 'foo';")))
        (xit "(require (\"foo\" :as \"bar\"))"
             (fn ()
               (assert-equal
                (compile '(require ("foo" :as "bar"))
                         compilation-environment
                         (js-obj "expressionType" "statement"
                                 "language" "JavaScript"))
                "import bar from 'foo';")))))
    (describe "module"
      (fn ()
        (it "(module m scheme ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define (I x)
                             x)
                           (define (K x y)
                             x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function I(x) {
  return x;
}

function K(x, y) {
  return x;
}")))
        (it "(module m scheme (define I ...) ...)"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define I (lambda (x)
                                       x))
                           (define K (lambda (x y)
                                       x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "const I = function (x) {
  return x;
};

const K = function (x, y) {
  return x;
};")))
        (it "(module m scheme (define (foo length) length))"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define (foo length)
                             length))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function foo(length: any): any {
  return length;
}")))
        (it "(module m scheme (define (foo (length : Number)) : Number length))"
            (fn ()
              (assert-equal
               (compile '(module m scheme
                           (define (foo (length : Number)) : Number
                             length))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function foo(length: number): number {
  return length;
}")))
        (xit "(module m scheme (define I (curry-n ...)) ...)"
             (fn ()
               (assert-equal
                (compile
                 '(module m scheme
                    (define I (curry-n 1 (lambda (x)
                                           x)))
                    (define K (curry-n 2 (lambda (x y)
                                           x))))
                 compilation-environment
                 (js-obj "language" "JavaScript"))
                "const I = curryN(1, function (x) {
  return x;
});

const K = curryN(2, function (x, y) {
  return x;
});")))
        (xit "(module m scheme (define I (curry-n ...)) ...)"
             (fn ()
               (assert-equal
                (compile '(module m scheme
                            (define truish #t)
                            (define falsy (not truish)))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "const truish = true;

const falsy = !truish;")))
        (it "read-rose"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  (define foo
    `(foo)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const foo = [Symbol.for('foo')];")))
        (it "read-rose, quasiquote"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  (define foo 1)
  (define bar
    `(,foo)))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const foo = 1;

const bar = [foo];")))
        (it "read-rose, quasiquoted list of pairs"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(module m scheme
  (define foo 1)
  (define bar 2)
  (define quux
    `((\"foo\" . ,foo)
       (\"bar\" . ,bar))))")
                compilation-environment
                (js-obj "language" "JavaScript"))
               "const foo = 1;

const bar = 2;

const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];")))))
    (describe "cond"
      (fn ()
        (it "(cond (x y)), statement"
            (fn ()
              (assert-equal
               (compile '(cond
                          (x
                           y))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "if (x) {
  return y;
}")))
        (it "(cond (x y)), expression"
            (fn ()
              (assert-equal
               (compile '(cond
                          (x
                           y))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "x ? y : undefined")))
        (it "(cond (x y) (else z)), expression"
            (fn ()
              (assert-equal
               (compile '(cond
                          (x
                           y)
                          (else
                           z))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "x ? y : z")))
        (it "(cond (x y) (else w z)), expression"
            (fn ()
              (assert-equal
               (compile '(cond
                          (x
                           y)
                          (else
                           w
                           z))
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "x ? y : (() => {
  w;
  return z;
})()")))
        (it "(cond (x y) (else z)), statement"
            (fn ()
              (assert-equal
               (compile '(cond
                          (x
                           y)
                          (else
                           z))
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "if (x) {
  return y;
} else {
  return z;
}")))
        (xit "(cond ((set! x y) z) (else w)), statement"
             (fn ()
               (assert-equal
                (compile '(cond
                           ((set! x y)
                            z)
                           (else
                            w))
                         compilation-environment
                         (js-obj "expressionType" "return"
                                 "language" "JavaScript"))
                "if ((x = y)) {
  return z;
} else {
  return w;
}")))))
    (describe "if"
      (fn ()
        (it "(if x y z), statement"
            (fn ()
              (assert-equal
               (compile '(if x
                             y
                             z)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "if (x) {
  y;
} else {
  z;
}")))
        (it "(if x y), expression"
            (fn ()
              (assert-equal
               (compile '(if x
                             y)
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "x ? y : undefined")))
        (it "(if x y z), expression"
            (fn ()
              (assert-equal
               (compile '(if x y z)
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "x ? y : z")))
        (it "(if x y z), statement"
            (fn ()
              (assert-equal
               (compile '(if x
                             y
                             z)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "if (x) {
  return y;
} else {
  return z;
}")))
        (it "(if \"foo\" \"bar\" \"baz\"), expression"
            (fn ()
              (assert-equal
               (compile '(if "foo"
                             "bar"
                             "baz")
                        compilation-environment
                        (js-obj "expressionType" "expression"
                                "language" "JavaScript"))
               "'foo' ? 'bar' : 'baz'")))
        (it "(if x (begin y z) w), statement"
            (fn ()
              (assert-equal
               (compile '(if x
                             (begin
                               y
                               z)
                             w)
                        compilation-environment
                        (js-obj "expressionType" "return"
                                "language" "JavaScript"))
               "if (x) {
  y;
  return z;
} else {
  return w;
}")))
        (xit "(if (set! x y) z w), statement"
             (fn ()
               (assert-equal
                (compile '(if (set! x y)
                              z
                              w)
                         compilation-environment
                         (js-obj "expressionType" "return"
                                 "language" "JavaScript"))
                "if ((x = y)) {
  return z;
} else {
  return w;
}")))))
    (describe "when"
      (fn ()
        (it "(when x y z), statement"
            (fn ()
              (assert-equal
               (compile '(when x
                           y z)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "if (x) {
  y;
  z;
}")))
        (it "(when (> (array-list-length args) 0) ...), statement"
            (fn ()
              (assert-equal
               (compile
                '(when (> (array-list-length args) 0)
                   (set! args (.concat (.slice args 0 (- (array-list-length args) 1))
                                       (aref args (- (array-list-length args) 1)))))
                compilation-environment
                (js-obj "expressionType" "statement"
                        "language" "JavaScript"))
               "if (args.length > 0) {
  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);
}")))))
    (describe "unless"
      (fn ()
        (it "(unless x y z), statement"
            (fn ()
              (assert-equal
               (compile '(unless x
                           y z)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "if (!x) {
  y;
  z;
}")))))
    (describe "aget"
      (fn ()
        (it "(aget args 0)"
            (fn ()
              (assert-equal
               (compile '(aget args 0)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "args[0]")))
        (it "(aget args 0 1)"
            (fn ()
              (assert-equal
               (compile '(aget args 0 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "args[0][1]")))))
    (describe "aref"
      (fn ()
        (it "(aref args 0)"
            (fn ()
              (assert-equal
               (compile '(aref args 0)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "args[0]")))
        (it "(aref args 0 1)"
            (fn ()
              (assert-equal
               (compile '(aref args 0 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "args[0][1]")))))
    (describe "aset"
      (fn ()
        (it "(aset args 0 1)"
            (fn ()
              (assert-equal
               (compile '(aset args 0 1)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "args[0] = 1;")))))
    (describe "set!...aref"
      (fn ()
        (it "(set! (aref args 0) 1)"
            (fn ()
              (assert-equal
               (compile '(set! (aref args 0) 1)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "args[0] = 1;")))))
    (describe "="
      (fn ()
        (it "(= 1 1)"
            (fn ()
              (assert-equal
               (compile '(= 1 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "1 === 1")))
        (it "(= x y)"
            (fn ()
              (assert-equal
               (compile '(= x y)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x === y")))))
    (describe "<"
      (fn ()
        (it "(< 1)"
            (fn ()
              (assert-equal
               (compile '(< 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "true")))
        (it "(< 1 2)"
            (fn ()
              (assert-equal
               (compile '(< 1 2)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "1 < 2")))
        (it "(< 1 2 3)"
            (fn ()
              (assert-equal
               (compile '(< 1 2 3)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "(1 < 2) && (2 < 3)")))))
    (describe ">"
      (fn ()
        (it "(> 1)"
            (fn ()
              (assert-equal
               (compile '(> 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "true")))
        (it "(> 2 1)"
            (fn ()
              (assert-equal
               (compile '(> 2 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "2 > 1")))
        (it "(> 3 2 1)"
            (fn ()
              (assert-equal
               (compile '(> 3 2 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "(3 > 2) && (2 > 1)")))))
    (describe "not"
      (fn ()
        (it "(not (and x y))"
            (fn ()
              (assert-equal
               (compile '(not (and x y))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "!(x && y)")))
        (it "(not (= 1 2))"
            (fn ()
              (assert-equal
               (compile '(not (= 1 2))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "1 !== 2")))
        (it "(not (> 1 2))"
            (fn ()
              (assert-equal
               (compile '(not (> 1 2))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "!(1 > 2)")))
        (it "(not (f x))"
            (fn ()
              (assert-equal
               (compile '(not (f x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "!f(x)")))
        (xit "(and (not (f x)) (not (g y)))"
             (fn ()
               (assert-equal
                (compile '(and (not (f x)) (not (g y)))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "!f(x) && !g(y)")))))
    (describe "and"
      (fn ()
        (it "(and)"
            (fn ()
              (assert-equal
               (compile '(and)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "true")))
        (it "(and x)"
            (fn ()
              (assert-equal
               (compile '(and x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x")))
        (it "(and x y)"
            (fn ()
              (assert-equal
               (compile '(and x y)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x && y")))
        (xit "(and x y z)"
             (fn ()
               (assert-equal
                (compile '(and x y z)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x && y && z")))
        (xit "(and x y (w z))"
             (fn ()
               (assert-equal
                (compile '(and x y (w z))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x && y && w(z)")))
        (xit "(and x y (or w z))"
             (fn ()
               (assert-equal
                (compile '(and x y (or w z))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x && y && (w || z)")))))
    (describe "or"
      (fn ()
        (it "(or)"
            (fn ()
              (assert-equal
               (compile '(or)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "false")))
        (it "(or x)"
            (fn ()
              (assert-equal
               (compile '(or x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x")))
        (it "(or x y)"
            (fn ()
              (assert-equal
               (compile '(or x y)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x || y")))
        (xit "(or x y z)"
             (fn ()
               (assert-equal
                (compile '(or x y z)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x || y || z")))))
    (describe "."
      (fn ()
        (it "(. map get \"foo\")"
            (fn ()
              (assert-equal
               (compile '(. map get "foo")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "map.get('foo')")))
        (it "(.get map \"foo\")"
            (fn ()
              (assert-equal
               (compile '(.get map "foo")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "map.get('foo')")))
        (it "(.-length arr)"
            (fn ()
              (assert-equal
               (compile '(.-length arr)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "arr.length")))))
    (describe "send"
      (fn ()
        (it "(send map get \"foo\")"
            (fn ()
              (assert-equal
               (compile '(send map get "foo")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "map.get('foo')")))))
    (describe "get-field"
      (fn ()
        (it "(get-field length arr)"
            (fn ()
              (assert-equal
               (compile '(get-field length arr)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "arr.length")))
        (it "(get-field (- len 1) arr)"
            (fn ()
              (assert-equal
               (compile '(get-field (- len 1) arr)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "arr[len - 1]")))))
    (describe "set-field!"
      (fn ()
        (it "(set-field! prop obj val)"
            (fn ()
              (assert-equal
               (compile '(set-field! prop obj val)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "obj.prop = val")))
        (it "(set-field! def-method ...)"
            (fn ()
              (assert-equal
               (compile
                '(set-field! def-method
                             generic-function
                             (lambda (arglist function-definition)
                               (let ((entry (list arglist function-definition)))
                                 (push! (get-field methods generic-function) entry)
                                 generic-function)))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "genericFunction.defMethod = function (arglist, functionDefinition) {
  const entry = [arglist, functionDefinition];
  genericFunction.methods.unshift(entry);
  return genericFunction;
}")))))
    (describe "length"
      (fn ()
        (it "(array-list-length x)"
            (fn ()
              (assert-equal
               (compile '(array-list-length x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.length")))))
    (describe "foldl"
      (fn ()
        (it "(foldl f v l)"
            (fn ()
              (assert-equal
               (compile '(foldl f v l)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "l.reduce(function (acc, x) {
  return f(x, acc);
}, v)")))
        (it "(foldl (lambda ...) v l)"
            (fn ()
              (assert-equal
               (compile '(foldl (lambda (x acc)
                                  (f x acc))
                                v
                                l)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "l.reduce(function (acc, x) {
  return f(x, acc);
}, v)")))
        (it "(foldl + 0 '(1 2 3 4))"
            (fn ()
              (assert-equal
               (compile '(foldl + 0 '(1 2 3 4))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "[1, 2, 3, 4].reduce(function (acc, x) {
  return x + acc;
}, 0)")))))
    (describe "foldr"
      (fn ()
        (xit "(foldr f v x)"
             (fn ()
               (assert-equal
                (compile '(foldr f v x)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x.reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(f), v)")))
        (xit "(foldr (f g) v x)"
             (fn ()
               (assert-equal
                (compile '(foldr (f g) v x)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x.reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(f(g)), v)")))
        (xit "(foldr cons '() '(1 2 3 4))"
             (fn ()
               (assert-equal
                (compile '(foldr cons '() '(1 2 3 4))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "[1, 2, 3, 4].reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(cons), [])")))))
    (describe "nth"
      (fn ()
        (xit "(nth 1 x)"
             (fn ()
               (assert-equal
                (compile '(nth 1 x)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x[1]")))
        (xit "(nth 2 (nth 1 x))"
             (fn ()
               (assert-equal
                (compile '(nth 2 (nth 1 x))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x[1][2]")))))
    (describe "nthcdr"
      (fn ()
        (xit "(nthcdr 1 x)"
             (fn ()
               (assert-equal
                (compile '(nthcdr 1 x)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x.slice(1)")))))
    (describe "drop"
      (fn ()
        (it "(drop x 1)"
            (fn ()
              (assert-equal
               (compile '(drop x 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.slice(1)")))))
    (describe "drop-right"
      (fn ()
        (it "(drop-right x 1)"
            (fn ()
              (assert-equal
               (compile '(drop-right x 1)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.slice(0, -1)")))))
    (describe "for"
      (fn ()
        (it "(for ((x '(1 2 3))) (display x))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  console.log(x);
}")))
        (it "(for ((x '(1 2 3))) (break))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (break))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  break;
}")))
        (it "(for ((x '(1 2 3))) (continue))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (continue))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  continue;
}")))
        (it "(for ((x ...)) (let ((x ...)) ...))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (let ((x 1))
                             (display x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  {
    const x = 1;
    console.log(x);
  }
}")))
        (it "(for ((x ...)) (let ((y ...)) ...))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (let ((y 1))
                             (display x y)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  const y = 1;
  console.log(x, y);
}")))
        (it "(for ((x ...)) (let ((y ...)) ...))"
            (fn ()
              (assert-equal
               (compile '(for ((x '(1 2 3)))
                           (let ((y 1))
                             (display y))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let x of [1, 2, 3]) {
  const y = 1;
  console.log(y);
  console.log(x);
}")))
        (it "(for ((i (range 0 10))) (display x)), JS"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 0 10)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let i = 0; i < 10; i++) {
  console.log(x);
}")))
        (it "(for ((i (range 0 10))) (display x)), TS"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 0 10)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "for (let i: any = 0; i < 10; i++) {
  console.log(x);
}")))
        (it "(for ((i (range 1 10 2))) (display x))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 1 10 2)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let i = 1; i < 10; i = i + 2) {
  console.log(x);
}")))
        (it "(for ((i (range 10 1 -1))) (display x))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 10 1 -1)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let i = 10; i > 1; i--) {
  console.log(x);
}")))
        (it "(for ((i (range 10 1 -2))) (display x))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 10 1 -2)))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "for (let i = 10; i > 1; i = i - 2) {
  console.log(x);
}")))
        (it "(for ((i (range 0 (+ 1 1)))) (display i))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range 0 (+ 1 1))))
                           (display i))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const _end = 1 + 1;

for (let i = 0; i < _end; i++) {
  console.log(i);
}")))
        (it "(for ((i (range (+ 1 1) (+ 2 2)))) (display i))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range (+ 1 1) (+ 2 2))))
                           (display i))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const _start = 1 + 1;

const _end = 2 + 2;

for (let i = _start; i < _end; i++) {
  console.log(i);
}")))
        (it "(for ((i (range (+ 1 1) (+ 2 2)))) (display i))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range (+ 1 1) (+ 2 2))))
                           (display i))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const _start: any = 1 + 1;

const _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  console.log(i);
}")))
        (it "(let ((_start 0) (_end 0)) (for ((i (range (+ 1 1) (+ 2 2)))) (display i)))"
            (fn ()
              (assert-equal
               (compile '(let ((_start 0)
                               (_end 0))
                           (for ((i (range (+ 1 1) (+ 2 2))))
                             (display i)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const _start: any = 0;

const _end: any = 0;

const _start1: any = 1 + 1;

const _end1: any = 2 + 2;

for (let i: any = _start1; i < _end1; i++) {
  console.log(i);
}")))
        (it "(for ((i (range (+ 1 1) (+ 2 2)))) (for ((j (range (+ 3 3) (+ 4 4)))) (display j)))"
            (fn ()
              (assert-equal
               (compile '(for ((i (range (+ 1 1) (+ 2 2))))
                           (for ((j (range (+ 3 3) (+ 4 4))))
                             (display j)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const _start: any = 1 + 1;

const _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  const _start1: any = 3 + 3;
  const _end1: any = 4 + 4;
  for (let j: any = _start1; j < _end1; j++) {
    console.log(j);
  }
}")))
        (it "(define (foo) (for ...))"
            (fn ()
              (assert-equal
               (compile '(define (foo)
                           (for ((x '(1 2 3)))
                             (display x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function foo() {
  for (let x of [1, 2, 3]) {
    console.log(x);
  }
}")))))
    (describe "do"
      (fn ()
        (it "(do () ((not (< (array-list-length result) 3))) (display result))"
            (fn ()
              (assert-equal
               (compile '(do ()
                             ((not (< (array-list-length result) 3)))
                           (display result))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "while (result.length < 3) {
  console.log(result);
}")))
        (xit "(do ((*do-result* (display result))) ((not (< (array-list-length result) 3))))"
             (fn ()
               (assert-equal
                (compile '(do ((*do-result* (display result)))
                              ((not (< (array-list-length result) 3))))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "do {
  console.log(result);
} while (result.length < 3);")))))
    (describe "js/while"
      (fn ()
        (it "(js/while (< (array-list-length result) 3) (display result))"
            (fn ()
              (assert-equal
               (compile '(js/while (< (array-list-length result) 3)
                           (display result))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "while (result.length < 3) {
  console.log(result);
}")))
        (it "(js/while (begin ...) ...)"
            (fn ()
              (assert-equal
               (compile '(js/while (begin
                                     (set! x (- x 1))
                                     (> x 0))
                           (display x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "while ((() => {
  x--;
  return x > 0;
})()) {
  console.log(x);
}")))))
    (describe "js/do-while"
      (fn ()
        (it "(js/do-while (display result) (< (array-list-length result) 3))"
            (fn ()
              (assert-equal
               (compile '(js/do-while (display result)
                                      (< (array-list-length result) 3))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "do {
  console.log(result);
} while (result.length < 3);")))
        (it "(js/do-while (begin (foo) (display result)) (< (array-list-length result) 3))"
            (fn ()
              (assert-equal
               (compile '(js/do-while (begin
                                        (foo)
                                        (display result))
                                      (< (array-list-length result) 3))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "do {
  foo();
  console.log(result);
} while (result.length < 3);")))))
    (describe "first"
      (fn ()
        (it "(first x)"
            (fn ()
              (assert-equal
               (compile '(first x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x[0]")))))
    (describe "last"
      (fn ()
        (xit "(last x)"
             (fn ()
               (assert-equal
                (compile '(last x)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "x[x.length - 1]")))))
    (describe "class"
      (fn ()
        (it "(class () (define (bar) \"bar\"))"
            (fn ()
              (assert-equal
               (compile '(class ()
                           (define/public (bar)
                             "bar"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class {
  bar() {
    return 'bar';
  }
}")))))
    (describe "define-class"
      (fn ()
        (it "(define-class Foo)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
}")))
        (it "(define-class Foo () (define (bar) \"bar\"))"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public (bar)
                             "bar"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
  bar() {
    return 'bar';
  }
}")))
        (it "(define-class Foo () (define (bar) \"bar\") (define (baz) \"baz\"))"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public (bar)
                             "bar")
                           (define/public (baz)
                             "baz"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
  bar() {
    return 'bar';
  }

  baz() {
    return 'baz';
  }
}")))
        (it "(define-class Foo () (define bar) (define baz \"baz\") (define (quux) \"quux\"))"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public bar)
                           (define/public baz "baz")
                           (define/public (quux) "quux"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
  bar;

  baz = 'baz';

  quux() {
    return 'quux';
  }
}")))
        (it "(define-class Foo () (define x) (define (constructor x) ...) ...), JS"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define x)
                           (define/public (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (define/public (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}")))
        (it "(define-class Foo () (define x) (define (constructor x) ...) ...), TS"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define x)
                           (define/public (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (define/public (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}")))
        (it "(define-class Foo () (define x) (define (constructor . args) ...) ...), TS"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public x)
                           (define/public (constructor . args)
                             (super)
                             (set! (.-stack this) args))
                           (define/public (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "class Foo {
  x: any;

  constructor(...args: any[]) {
    super();
    this.stack = args;
  }

  bar(): any {
    return this.x;
  }
}")))
        (it "(define-class Foo (Object) (define x) (define (constructor x) ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo (Object)
                           (define/public x)
                           (define/public (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (define/public (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}")))
        (it "(define-class Foo (Object) (define/public ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo (Object)
                           (define/public x)
                           (define/public (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (define/public (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}")))
        (it "(define-class Foo (Object) (define/private ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo (Object)
                           (define/private x)
                           (define/public (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (define/private (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "class Foo extends Object {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}")))
        (it "(define-class Foo () (public ...) (define ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (public x)
                           (define x)
                           (public constructor)
                           (define (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (public bar)
                           (define (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "class Foo {
  x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}")))
        (it "(define-class Foo () (private ...) (define ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (private x)
                           (define x)
                           (define (constructor x)
                             (super)
                             (set! (.-x this) x))
                           (private bar)
                           (define (bar)
                             (.-x this)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}")))
        (it "(define-class Foo () ... (define (aget ...) ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public arr)
                           (define/public (constructor arr)
                             (set-field! arr this arr))
                           (define/public (nth i)
                             (aget (get-field arr this) i)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  nth(i) {
    return this.arr[i];
  }
}")))
        (it "(define-class Foo () ... (define/generator (generator) ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public arr)
                           (define (constructor arr)
                             (set-field! arr this arr))
                           (define/generator (generator)
                             (for ((x (get-field arr this)))
                               (yield x))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
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
}")))
        (it "(define-class Foo () ... (define/generator ((get-field iterator Symbol)) ...) ...)"
            (fn ()
              (assert-equal
               (compile '(define-class Foo ()
                           (define/public arr)
                           (define (constructor arr)
                             (set-field! arr this arr))
                           (define/generator ((get-field iterator Symbol))
                             (for ((x (get-field arr this)))
                               (yield x))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
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
}")))))
    (describe "define...class"
      (fn ()
        (it "(define Foo (class object%))"
            (fn ()
              (assert-equal
               (compile '(define Foo
                           (class object%))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo {
}")))
        (it "(define Foo (class Bar))"
            (fn ()
              (assert-equal
               (compile '(define Foo
                           (class Bar))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "class Foo extends Bar {
}")))))
    (describe "js"
      (fn ()
        (it "(js \"1\")"
            (fn ()
              (assert-equal
               (compile '(js "1")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "1")))
        (it "(js \"function I(x) { return x; }\")"
            (fn ()
              (assert-equal
               (compile '(js "function I(x) { return x; }")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "function I(x) { return x; }")))))
    (describe "make-hash"
      (fn ()
        (it "(make-hash)"
            (fn ()
              (assert-equal
               (compile '(make-hash)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map()")))
        (it "(make-hash (quote ...)), list of pairs"
            (fn ()
              (assert-equal
               (compile '(make-hash
                          '(("foo" . "bar")
                            ("baz" . "quux")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map([['foo', 'bar'], ['baz', 'quux']])")))
        (it "(make-hash (quote ...)), list of lists"
            (fn ()
              (assert-equal
               (compile '(make-hash
                          '(("foo" "bar")
                            ("baz" "quux")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map([['foo', ['bar']], ['baz', ['quux']]])")))
        (it "(make-hash (quasiquote ...)), list of pairs"
            (fn ()
              (assert-equal
               (compile '(make-hash
                          `(("foo" . "bar")
                            ("baz" . "quux")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map([['foo', 'bar'], ['baz', 'quux']])")))
        (it "(make-hash (quasiquote ...)), list of pairs, hash>list"
            (fn ()
              (assert-equal
               (compile '(make-hash
                          `(("foo" . "bar")
                            ("baz" . "quux")
                            (unquote-splicing
                             (hash->list xyzzy))))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map([['foo', 'bar'], ['baz', 'quux'], ...xyzzy.entries()])")))
        (xit "(make-hash (quasiquote ...)), list of pairs"
             (fn ()
               (assert-equal
                (compile '(make-hash
                           (append
                            `(("foo" . "bar")
                              ("baz" . "quux"))
                            (hash->list xyzzy)))
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "new Map([...[['foo', 'bar'], ['baz', 'quux']], ...xyzzy.entries()])")))
        (it "(make-hash (quasiquote ...)), list of lists"
            (fn ()
              (assert-equal
               (compile '(make-hash
                          `(("foo" "bar")
                            ("baz" "quux")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "new Map([['foo', ['bar']], ['baz', ['quux']]])")))))
    (describe "JavaScript objects"
      (fn ()
        (xit "{}"
             (fn ()
               (assert-equal
                (compile (js-obj)
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "{}")))
        (xit "{ foo: 'bar' }"
             (fn ()
               (assert-equal
                (compile (js-obj "foo" "bar")
                         compilation-environment
                         (js-obj "language" "JavaScript"))
                "{ foo: 'bar' }")))))
    (describe "js-obj"
      (fn ()
        (it "(js-obj)"
            (fn ()
              (assert-equal
               (compile '(js-obj)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{}")))
        (it "(js-obj foo \"bar\")"
            (fn ()
              (assert-equal
               (compile '(js-obj foo "bar")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  [foo]: 'bar'
}")))
        (it "(js-obj \"foo\" \"bar\")"
            (fn ()
              (assert-equal
               (compile '(js-obj "foo" "bar")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  foo: 'bar'
}")))
        (it "(js-obj \"foo bar\" \"foo bar\")"
            (fn ()
              (assert-equal
               (compile '(js-obj "foo bar" "foo bar")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  'foo bar': 'foo bar'
}")))
        (it "(js-obj \"foo\" (js-obj \"bar\" \"baz\"))"
            (fn ()
              (assert-equal
               (compile '(js-obj "foo" (js-obj "bar" "baz"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  foo: {
    bar: 'baz'
  }
}")))
        (it "(js-obj \"foo\" (js-obj \"foo\" \"foo\") \"bar\" (js-obj \"bar\" \"bar\"))"
            (fn ()
              (assert-equal
               (compile '(js-obj "foo" (js-obj "foo" "foo")
                                 "bar" (js-obj "bar" "bar"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  foo: {
    foo: 'foo'
  },
  bar: {
    bar: 'bar'
  }
}")))
        (it "(js-obj \"foo\" (js-obj) \"bar\" (js-obj \"bar\" \"bar\") \"baz\" (js-obj \"baz\" \"baz\"))"
            (fn ()
              (assert-equal
               (compile '(js-obj "foo" (js-obj)
                                 "bar" (js-obj "bar" "bar")
                                 "baz" (js-obj "baz" "baz"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  foo: {},
  bar: {
    bar: 'bar'
  },
  baz: {
    baz: 'baz'
  }
}")))))
    (describe "js-obj?"
      (fn ()
        (it "(js-obj? x)"
            (fn ()
              (assert-equal
               (compile '(js-obj? x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "(x !== null) && (typeof x === 'object')")))))
    (describe "js-obj-append"
      (fn ()
        (it "(js-obj-append obj (js-obj \"foo\" \"bar\"))"
            (fn ()
              (assert-equal
               (compile '(js-obj-append
                          obj
                          (js-obj "foo" "bar"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "{
  ...obj,
  foo: 'bar'
}")))))
    (describe "js-keys"
      (fn ()
        (it "(js-keys x)"
            (fn ()
              (assert-equal
               (compile '(js-keys x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "Object.keys(x)")))))
    (describe "js/tag"
      (fn ()
        (it "(js/tag foo \"bar\")"
            (fn ()
              (assert-equal
               (compile '(js/tag foo "bar")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "foo`bar`")))))
    (describe "->"
      (fn ()
        (it "(-> x (.foo \"bar\") (.baz))"
            (fn ()
              (assert-equal
               (compile '(-> x
                             (.foo "bar")
                             (.baz))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.foo('bar').baz()")))
        (it "(-> regular-args (.map ...) (.join ...))"
            (fn ()
              (assert-equal
               (compile '(-> regular-args
                             (.map (lambda (arg)
                                     (compile-expression
                                      arg env inherited-options)))
                             (.join ", "))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "regularArgs.map(function (arg) {
  return compileExpression(arg, env, inheritedOptions);
}).join(', ')")))))
    (describe "js/try"
      (fn ()
        (it "(js/try)"
            (fn ()
              (assert-equal
               (compile '(js/try)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
}")))
        (it "(js/try ...)"
            (fn ()
              (assert-equal
               (compile '(js/try
                          (set! x (/ 2 1)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
}")))
        (it "(js/try ... (finally ...))"
            (fn ()
              (assert-equal
               (compile '(js/try
                          (set! x (/ 2 1))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}")))
        (it "(js/try ... (catch _ ...) (finally ...))"
            (fn ()
              (assert-equal
               (compile '(js/try
                          (set! x (/ 2 1))
                          (catch _
                              (display "there was an error"))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} catch {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}")))
        (it "(js/try ... (catch e ...) (finally ...))"
            (fn ()
              (assert-equal
               (compile '(js/try
                          (set! x (/ 2 1))
                          (catch e
                              (display "there was an error"))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}")))))
    (describe "clj/try"
      (fn ()
        (it "(clj/try)"
            (fn ()
              (assert-equal
               (compile '(clj/try)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
}")))
        (it "(clj/try ...)"
            (fn ()
              (assert-equal
               (compile '(clj/try
                          (set! x (/ 2 1)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
}")))
        (it "(clj/try ... (finally ...))"
            (fn ()
              (assert-equal
               (compile '(clj/try
                          (set! x (/ 2 1))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}")))
        (it "(clj/try ... (catch ...) (finally ...))"
            (fn ()
              (assert-equal
               (compile '(clj/try
                          (set! x (/ 2 1))
                          (catch Object e
                            (display "there was an error"))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}")))
        (it "(clj/try ... (catch ...) (finally ...))"
            (fn ()
              (assert-equal
               (compile '(clj/try
                          (set! x (/ 2 1))
                          (catch MyException e
                            (display "there was an error"))
                          (finally
                            (display "cleanup")))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} catch (e) {
  if (e instanceof MyException) {
    console.log('there was an error');
  } else {
    throw e;
  }
} finally {
  console.log('cleanup');
}")))))
    (describe "map"
      (fn ()
        (it "(map f x)"
            (fn ()
              (assert-equal
               (compile '(map f x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.map(function (x) {
  return f(x);
})")))
        (it "(map (lambda (x) x) x)"
            (fn ()
              (assert-equal
               (compile '(map (lambda (x) x) x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.map(function (x) {
  return x;
})")))
        (it "(map (g y) x)"
            (fn ()
              (assert-equal
               (compile '(map (g y) x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "x.map((function (f) {
  return function (x) {
    return f(x);
  };
})(g(y)))")))))
    (describe "throw"
      (fn ()
        (it "(throw (new Error \"An error\"))"
            (fn ()
              (assert-equal
               (compile '(throw (new Error "An error"))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "throw new Error('An error');")))))
    (describe "js/delete"
      (fn ()
        (it "(js/delete x)"
            (fn ()
              (assert-equal
               (compile '(js/delete x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "delete x")))))
    (describe "return"
      (fn ()
        (it "(return)"
            (fn ()
              (assert-equal
               (compile '(return)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "return;")))
        (it "(return 0)"
            (fn ()
              (assert-equal
               (compile '(return 0)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "return 0;")))))
    (describe "yield"
      (fn ()
        (it "(yield)"
            (fn ()
              (assert-equal
               (compile '(yield)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "yield;")))
        (it "(yield 0)"
            (fn ()
              (assert-equal
               (compile '(yield 0)
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "yield 0;")))))
    (describe "await"
      (fn ()
        (it "(await (foo))"
            (fn ()
              (assert-equal
               (compile '(await (foo))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "await foo();")))))
    (describe "async"
      (fn ()
        (it "(async (lambda (x) x))"
            (fn ()
              (assert-equal
               (compile '(async (lambda (x) x))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "async function (x) {
  return x;
}")))
        (it "(define foo (async (lambda (x) x))), JS"
            (fn ()
              (assert-equal
               (compile '(define foo
                           (async (lambda (x) x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "async function foo(x) {
  return x;
}")))
        (it "(define foo (async (lambda (x) x))), TS"
            (fn ()
              (assert-equal
               (compile '(define foo
                           (async (lambda (x) x)))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "async function foo(x: any): Promise<any> {
  return x;
}")))
        (it "(define/async (foo x) x)"
            (fn ()
              (assert-equal
               (compile '(define/async (foo x)
                           x)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "async function foo(x) {
  return x;
}")))))
    (describe "module"
      (fn ()
        (it "(module m lisp (define x 1) (define y 2))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define x 1)
                           (define y 2))
                        compilation-environment
                        (js-obj "expressionType" "statement"
                                "language" "JavaScript"))
               "const x = 1;

const y = 2;")))
        (xit "(module m lisp ... (define *lisp-map* '()))"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (define (I x) x)
                            (define x 1)
                            (define *lisp-map* #t))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "language" "JavaScript"))
                "function I(x) {
  return x;
}

I.lispSource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];

const x = 1;")))
        (xit "(module m lisp ... (define *lisp-map* '()))"
             (fn ()
               (assert-equal
                (compile
                 (read-rose
                  "(module m lisp
  ;; inline-lisp-sources: true

  (define (I x) x))")
                 compilation-environment
                 (js-obj "camelCase" #t
                         "language" "JavaScript"))
                "// inline-lisp-sources: true

function I(x) {
  return x;
}

I.lispSource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];")))
        (xit "(module m lisp (require ...) ... (define *lisp-map* '())), JS"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (require (only-in "./combinators"
                                              I))
                            (define x 1)
                            (define *lisp-map* #t))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "language" "JavaScript"))
                "import {
  I
} from './combinators';

const x = 1;")))
        (xit "(module m lisp (require ...) ... (define *lisp-map* '())), TS"
             (fn ()
               (assert-equal
                (compile '(module m lisp
                            (require (only-in "./combinators"
                                              I))
                            (define x 1)
                            (define *lisp-map* #t))
                         compilation-environment
                         (js-obj "camelCase" #t
                                 "language" "TypeScript"))
                "import {
  I
} from './combinators';

const x: any = 1;")))
        (it "(module m lisp (define (js_ str) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (js_ str)
                             (js/eval str)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function js_(str) {
  return eval(str);
}")))
        (it "(module m lisp (define (my-foldl ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-fn foldl f v l)
                             (foldl f v l)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myFn(foldl, f, v, l) {
  return foldl(f, v, l);
}")))
        (it "(module m lisp (define (my-foldl-obj ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-foldl-obj obj f v l)
                             (.foldl obj f v l)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myFoldlObj(obj, f, v, l) {
  return obj.foldl(f, v, l);
}")))
        (it "(module ... (define-class ... (define ...)))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define-class Foo ()
                             (define/public (foldl f v l)
                               l)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "class Foo {
  foldl(f, v, l) {
    return l;
  }
}")))
        (it "(module m lisp (define (my-pop ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-pop lst x)
                             (pop! lst x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPop(lst, x) {
  return lst.shift();
}")))
        (it "(module m lisp (define (my-pop-2 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-pop-2 lst x)
                             (pop! (append lst) x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPop2(lst, x) {
  return [...lst].shift();
}")))
        (it "(module m lisp (define (my-pop-right ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-pop-right lst x)
                             (pop-right! lst x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPopRight(lst, x) {
  return lst.pop();
}")))
        (it "(module m lisp (define (my-pop-right-2 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-pop-right-2 lst x)
                             (pop-right! (append lst) x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPopRight2(lst, x) {
  return [...lst].pop();
}")))
        (it "(module m lisp (define (my-push ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push lst x)
                             (push! lst x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPush(lst, x) {
  lst.unshift(x);
  return lst;
}")))
        (it "(module m lisp (define (my-push-2 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push-2 lst x)
                             (push! (append lst) x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPush2(lst, x) {
  return (function (lst, x) {
    lst.unshift(x);
    return lst;
  })([...lst], x);
}")))
        (it "(module m lisp (define (my-push-3 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push-3 lst x)
                             (push! lst x)
                             lst))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPush3(lst, x) {
  lst.unshift(x);
  return lst;
}")))
        (it "(module m lisp (define (my-push-right ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push-right lst x)
                             (push-right! lst x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPushRight(lst, x) {
  lst.push(x);
  return lst;
}")))
        (it "(module m lisp (define (my-push-right-2 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push-right-2 lst x)
                             (push-right! (append lst) x)))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPushRight2(lst, x) {
  return (function (lst, x) {
    lst.push(x);
    return lst;
  })([...lst], x);
}")))
        (it "(module m lisp (define (my-push-right-3 ...) ...))"
            (fn ()
              (assert-equal
               (compile '(module m lisp
                           (define (my-push-right-3 lst x)
                             (push-right! lst x)
                             lst))
                        compilation-environment
                        (js-obj "camelCase" #t
                                "language" "JavaScript"))
               "function myPushRight3(lst, x) {
  lst.push(x);
  return lst;
}")))))
    (describe "string-append"
      (fn ()
        (it "(string-append \"a\")"
            (fn ()
              (assert-equal
               (compile '(string-append "a")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'a'")))
        (it "(string-append \"a\" \"b\")"
            (fn ()
              (assert-equal
               (compile '(string-append "a" "b")
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "'a' + 'b'")))))
    (describe "ann"
      (fn ()
        (it "(ann 1 Number), JS"
            (fn ()
              (assert-equal
               (compile '(ann 1 Number)
                        compilation-environment
                        (js-obj "language" "JavaScript"))
               "1")))
        (it "(ann 1 Number), TS"
            (fn ()
              (assert-equal
               (compile '(ann 1 Number)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "1 as number")))
        (it "(ann (list) Any), TS"
            (fn ()
              (assert-equal
               (compile '(ann (list) Any)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "[] as any")))
        (it "(ann '() Any), TS"
            (fn ()
              (assert-equal
               (compile '(ann '() Any)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "[] as any")))
        (it "(ann x (List Any)), TS"
            (fn ()
              (assert-equal
               (compile '(ann x (List Any))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "x as [any]")))
        (it "(ann x (List Number Any)), TS"
            (fn ()
              (assert-equal
               (compile '(ann x (List Number Any))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "x as [number, any]")))
        (it "(ann x NN), TS"
            (fn ()
              (assert-equal
               (compile '(ann x NN)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "x as NN")))
        (it "(ann x (NN Any)), TS"
            (fn ()
              (assert-equal
               (compile '(ann x (NN Any))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "x as NN<any>")))
        (it "(ann x (NN Any Any)), TS"
            (fn ()
              (assert-equal
               (compile '(ann x (NN Any Any))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "x as NN<any,any>")))
        (it "((ann (lambda (x) x) Any) 1), TS"
            (fn ()
              (assert-equal
               (compile '((ann (lambda (x) x) Any) 1)
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "(function (x: any): any {
  return x;
} as any)(1)")))
        (it "(lambda (x) (ann (send x foo) Any)), TS"
            (fn ()
              (assert-equal
               (compile '(lambda (x) (ann (send x foo) Any))
                        compilation-environment
                        (js-obj "language" "TypeScript"))
               "function (x: any): any {
  return x.foo() as any;
}")))))
    (describe ":"
      (fn ()
        (it "(: x Any), JS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Any)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = 1;")))
        (it "(: x Any), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Any)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: any = 1;")))
        (it "(: x String), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x String)
                           (define x "1"))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: string = '1';")))
        (it "(: x Number), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Number)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number = 1;")))
        (it "(: x Integer), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Integer)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number = 1;")))
        (it "(: x Natural), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Natural)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number = 1;")))
        (it "(: x Real), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Real)
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number = 1;")))
        (it "(: x Symbol), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Symbol)
                           (define x 'x))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: Symbol = Symbol.for('x');")))
        (it "(: x Boolean), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Boolean)
                           (define x #t))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: boolean = true;")))
        (it "(: x True), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x True)
                           (define x #t))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: true = true;")))
        (it "(: x False), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x False)
                           (define x #f))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: false = false;")))
        (it "(: x (U Number String)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x (U Number String))
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number | string = 1;")))
        (it "(: x (U Number String Boolean)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x (U Number String Boolean))
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number | string | boolean = 1;")))
        (it "(: x (U Number (U String Boolean))), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x (U Number (U String Boolean)))
                           (define x 1))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number | (string | boolean) = 1;")))
        (it "(: x (Listof Number)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x (Listof Number))
                           (define x (list 1)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: number[] = [1];")))
        (it "(: x (Pairof Number)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x (Pairof Number))
                           (define x '(1 . 2)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: (number | Symbol)[] = [1, Symbol.for('.'), 2];")))
        (it "(: hello-world (-> Void)), JS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: hello-world (-> Void))
                           (define (hello-world)
                             (display "Hello world!")))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "function helloWorld() {
  console.log('Hello world!');
}")))
        (it "(: hello-world (-> Void)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: hello-world (-> Void))
                           (define (hello-world)
                             (display "Hello world!")))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function helloWorld(): void {
  console.log('Hello world!');
}")))
        (it "(: f (-> Number Number)), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: f (-> Number Number))
                           (define (f x)
                             x))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(x: number): number {
  return x;
}")))
        (it "(: f (-> Number Number)), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: f (-> Number Number))
                           (define f
                             (lambda (x)
                               x)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const f: (a: number) => number = function (x: any): any {
  return x;
};")))
        (it "(: f (-> Number Number)), foo'd lambda, TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: f (-> Number Number))
                           (define f
                             (foo
                              (lambda (x)
                                x))))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const f: (a: number) => number = foo(function (x: any): any {
  return x;
});")))
        (it "(: f (-> Number Number Number)), function with optional argument, TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: f (-> Number Number Number))
                           (define (f x (y 1))
                             x))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(x: number, y: number = 1): number {
  return x;
}")))
        (it "(: f (->* (Number) (Number) Number)), lambda with optional argument, TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: f (->* (Number) (Number) Number))
                           (define f
                             (lambda (x (y 1))
                               x)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {
  return x;
};")))
        (it "(: x Foo), TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (: x Foo)
                           (define x (new Foo)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const x: Foo = new Foo();")))))
    (describe "define-type"
      (fn ()
        (it "(define-type NN (-> Number Number)), JS"
            (fn ()
              (assert-equal
               (compile '(define-type NN (-> Number Number))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "")))
        (it "(define-type NN (-> Number Number)), TS"
            (fn ()
              (assert-equal
               (compile '(define-type NN (-> Number Number))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "type NN = (a: number) => number;")))
        (it "(: f (-> Number Number)), lambda, JS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (define-type NN (-> Number Number))
                           (: f NN)
                           (define f
                             (lambda (x)
                               x)))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const f = function (x) {
  return x;
};")))
        (it "(: f (-> Number Number)), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(begin
                           (define-type NN (-> Number Number))
                           (: f NN)
                           (define f
                             (lambda (x)
                               x)))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "type NN = (a: number) => number;

const f: NN = function (x: any): any {
  return x;
};")))
        (it "(lambda ((x : Number)) x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define f
                           (lambda ((x : Number))
                             x))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const f: any = function (x: number): any {
  return x;
};")))
        (it "(js/arrow ((x : Number)) x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define f
                           (js/arrow ((x : Number))
                             x))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "const f: any = (x: number): any => {
  return x;
};")))
        (it "(define (f (x : Number)) x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define (f (x : Number))
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(x: number): any {
  return x;
}")))
        (it "(define (f (x : Number) . args) x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define (f (x : Number) . args)
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(x: number, ...args: any[]): any {
  return x;
}")))
        (it "(define (id (x : Number)) : Number x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define (id (x : Number)) : Number
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function id(x: number): number {
  return x;
}")))
        (it "(define (f (x : Number 1)) : Number x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define (f (x : Number 1)) : Number
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(x: number = 1): number {
  return x;
}")))
        (it "(define (f (options : Any (js-obj))) : Any x), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define (f (options : Any (js-obj))) : Any
                           x)
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "function f(options: any = {}): any {
  return x;
}")))
        (it "(define Foo (class object% ... (define (constructor (x : Number)) ...))), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define Foo
                           (class object%
                             (define/public x)
                             (define (constructor (x : Number))
                               (set-field! x this x))))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "class Foo {
  x: any;

  constructor(x: number) {
    this.x = x;
  }
}")))
        (it "(define Foo (class object% ... (define (constructor (x : Number) . args) ...))), lambda, TS"
            (fn ()
              (assert-equal
               (compile '(define Foo
                           (class object%
                             (define/public x)
                             (define (constructor (x : Number) . args)
                               (set-field! x this x))))
                        compilation-environment
                        (js-obj "language" "TypeScript"
                                "expressionType" "statement"))
               "class Foo {
  x: any;

  constructor(x: number, ...args: any[]) {
    this.x = x;
  }
}")))
        (xit "(: f NN), TS"
             (fn ()
               (assert-equal
                (compile '(begin
                            (define-type NN (-> Number Number))
                            (: f NN)
                            (define (f x)
                              x))
                         compilation-environment
                         (js-obj "language" "TypeScript"
                                 "expressionType" "statement"))
                "type NN = (a: number) => number;

function f(x: number): number {
  return x;
};")))
        (it "(: f (-> Number Number)), lambda, comments, TS"
            (fn ()
              (assert-equal
               (compile
                (read-rose
                 "(begin
  ;; NN type alias.
  (define-type NN (-> Number Number))
  (: f NN)
  (define f
    (lambda (x)
      x)))")
                compilation-environment
                (js-obj "language" "TypeScript"
                        "expressionType" "statement"))
               "// NN type alias.
type NN = (a: number) => number;

const f: NN = function (x: any): any {
  return x;
};")))))))

(describe "definition-to-macro"
  (fn ()
    (it "(define (inc x) (+ x 1)), 1"
        (fn ()
          (assert-equal
           (definition-to-macro
             '(define (inc x)
                (+ x 1))
             '(1))
           '(+ 1 1))))
    (it "(define (logical-or x) (or x x)), #t"
        (fn ()
          (assert-equal
           (definition-to-macro
             '(define (logical-or x)
                (or x x))
             '(#t))
           '(or #t #t))))
    (it "(define (repeat x) (string-append x x)), \"1\""
        (fn ()
          (assert-equal
           (definition-to-macro
             '(define (repeat x)
                (string-append x x))
             '("1"))
           '(string-append "1" "1"))))
    (it "(define (square x) (* x x)), 1"
        (fn ()
          (assert-equal
           (definition-to-macro
             '(define (square x)
                (* x x))
             '(1))
           '(* 1 1))))
    (it "(define (square x) (* x x)), x"
        (fn ()
          (assert-equal
           (definition-to-macro
             '(define (square x)
                (* x x))
             '(x))
           '(* x x))))
    (xit "(define (square x) (* x x)), (+ 1 1)"
         (fn ()
           (assert-equal
            (definition-to-macro
              '(define (square x)
                 (* x x))
              '((+ 1 1)))
            '((lambda (x)
                (* x x))
              (+ 1 1)))))))

;;; Unsorted tests, to be sorted later.
(describe "unsorted"
  (fn ()
    (describe "?."
      (fn ()
        (it "const x = foo?.bar;"
            (fn ()
              (assert-equal
               (compile '(define x
                           (js/?. foo bar))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = foo?.bar;")))
        (it "const x = foo?.bar(baz);"
            (fn ()
              (assert-equal
               (compile '(define x
                           ((js/?. foo bar) baz))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = foo?.bar(baz);")))
        (it "const x = foo?.bar(baz);"
            (fn ()
              (assert-equal
               (compile '(define x
                           (js/?. foo (bar)))
                        compilation-environment
                        (js-obj "language" "JavaScript"
                                "expressionType" "statement"))
               "const x = foo?.(bar);")))))
    (describe "define-macro-to-lambda-form"
      (fn ()
        (it "(define-macro (foo x) x)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo x)
                    x))
               '(lambda (exp env)
                  (define-values (x)
                    (rest exp))
                  x))))
        (it "(define-macro (foo &whole expression x) x)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo &whole expression x)
                    x))
               '(lambda (expression env)
                  (define-values (x)
                    (rest expression))
                  x))))
        (it "(define-macro (foo &whole exp &environment env) exp)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo &whole exp &environment env)
                    exp))
               '(lambda (exp env)
                  exp))))
        (it "(define-macro (foo &whole exp &environment env x) x)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo &whole exp &environment env x)
                    x))
               '(lambda (exp env)
                  (define-values (x)
                    (rest exp))
                  x))))
        (it "(define-macro (foo &rest ...) ...)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo &rest x)
                    x))
               '(lambda (exp env)
                  (define-values x
                    (rest exp))
                  x))))
        (it "(define-macro (foo &rest ...) ...)"
            (fn ()
              (assert-equal
               (define-macro-to-lambda-form
                 '(define-macro (foo x &rest y)
                    x))
               '(lambda (exp env)
                  (define-values (x . y)
                    (rest exp))
                  x))))))
    ;; (describe "defmacro-to-lambda-form"
    ;;   (fn ()
    ;;     (it "(defmacro foo (x) x)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (x)
    ;;                 x))
    ;;            '(lambda (exp env)
    ;;               (define-values (x)
    ;;                 (rest exp))
    ;;               x))))
    ;;     (it "(defmacro foo (&whole expression x) x)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (&whole expression x)
    ;;                 x))
    ;;            '(lambda (expression env)
    ;;               (define-values (x)
    ;;                 (rest expression))
    ;;               x))))
    ;;     (it "(defmacro foo (&whole exp &environment env) exp)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (&whole exp &environment env)
    ;;                 exp))
    ;;            '(lambda (exp env)
    ;;               exp))))
    ;;     (it "(defmacro foo (&whole exp &environment env x) x)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (&whole exp &environment env x)
    ;;                 x))
    ;;            '(lambda (exp env)
    ;;               (define-values (x)
    ;;                 (rest exp))
    ;;               x))))
    ;;     (it "(defmacro foo (&rest ...) ...)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (&rest x)
    ;;                 x))
    ;;            '(lambda (exp env)
    ;;               (define-values x
    ;;                 (rest exp))
    ;;               x))))
    ;;     (it "(defmacro foo (&rest ...) ...)"
    ;;         (fn ()
    ;;           (assert-equal
    ;;            (defmacro-to-lambda-form
    ;;              '(defmacro foo (x &rest y)
    ;;                 x))
    ;;            '(lambda (exp env)
    ;;               (define-values (x . y)
    ;;                 (rest exp))
    ;;               x))))))
    (describe "js/switch"
      (fn ()
        (it "(js/switch x (case \"foo\" ...) (default ...))"
            (fn ()
              (assert-equal
               (compile
                '(js/switch x
                            (case "foo"
                              (display "foo")
                              (break))
                            (default
                              (display "bar")))
                compilation-environment
                (js-obj "language" "JavaScript"
                        "expressionType" "statement"))
               "switch (x) {
  case 'foo': {
    console.log('foo');
    break;
  }
  default: {
    console.log('bar');
  }
}")))
        (it "(js/switch x (case \"foo\" ...) (default ...)), return"
            (fn ()
              (assert-equal
               (compile
                '(js/switch x
                            (case "foo"
                              (display "foo")
                              (break))
                            (default
                              (display "bar")))
                compilation-environment
                (js-obj "language" "JavaScript"
                        "expressionType" "return"))
               "switch (x) {
  case 'foo': {
    return console.log('foo');
    break;
  }
  default: {
    return console.log('bar');
  }
}")))
        (it "(js/switch x (case \"foo\" ...) (default ...)), return"
            (fn ()
              (assert-equal
               (compile
                '(js/switch x
                            (case "foo"
                              (display "foo")
                              (break))
                            (default
                              (display "bar")))
                compilation-environment
                (js-obj "language" "JavaScript"
                        "expressionType" "expression"))
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
})()")))))))

(describe "compilation options"
  (fn ()
    (describe "inlineLispSources"
      (fn ()
        (it "(define (foo x) x)"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (define (foo x)
                     x))
                compilation-environment
                (js-obj "language" "JavaScript"
                        "inlineLispSources" #t))
               "function foo(x) {
  return x;
}

foo.lispSource = [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];")))
        (xit "(define foo (lambda (foo x) x))"
             (fn ()
               (assert-equal
                (compile
                 '(module m scheme
                    (define foo
                      (lambda (x)
                        x)))
                 compilation-environment
                 (js-obj "language" "JavaScript"
                         "inlineLispSources" #t))
                "const foo = function (x) {
  return x;
};

foo.lispSource = [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')];")))
        (it "(define foo (lambda (foo x) x))"
            (fn ()
              (assert-equal
               (compile
                '(module m scheme
                   (define foo
                     (async
                      (lambda (x)
                        x))))
                compilation-environment
                (js-obj "language" "JavaScript"
                        "inlineLispSources" #t))
               "async function foo(x) {
  return x;
}

foo.lispSource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];")))))))

(describe "assert"
  (fn ()
    (it "(assert #t)"
        (fn ()
          (assert-equal
           (compile '(assert #t)
                    compilation-environment
                    (js-obj "language" "JavaScript"))
           "console.assert(true)")))
    (it "(assert #t \"test\")"
        (fn ()
          (assert-equal
           (compile '(assert #t "test")
                    compilation-environment
                    (js-obj "language" "JavaScript"))
           "console.assert(true, 'test')")))))

(describe "display"
  (fn ()
    (it "(display #t)"
        (fn ()
          (assert-equal
           (compile '(display #t)
                    compilation-environment
                    (js-obj "language" "JavaScript"))
           "console.log(true)")))
    (it "(display #t \"test\")"
        (fn ()
          (assert-equal
           (compile '(display #t "test")
                    compilation-environment
                    (js-obj "language" "JavaScript"))
           "console.log(true, 'test')")))))

(describe "split-comments"
  (fn ()
    (xit "1"
         (fn ()
           (assert-equal
            (split-comments ";;; Foo")
            '(";;; Foo"))))
    (it "2"
        (fn ()
          (assert-equal
           (split-comments ";;; Foo\n")
           '(";;; Foo\n"))))
    (xit "3"
         (fn ()
           (assert-equal
            (split-comments ";; Foo\n;;; Bar")
            '(";; Foo\n" ";;; Bar"))))
    (it "4"
        (fn ()
          (assert-equal
           (split-comments ";; Foo\n;;; Bar\n")
           '(";; Foo\n" ";;; Bar\n"))))))

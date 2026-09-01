;;; # Compiler tests
;;;
;;; Various compiler tests.

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
                  read-syntax))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "Global environment"
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
    let result = '';
    for (let x of args) {
      result = result + x;
    }
    return result;
  }
  return [stringAppend_];
})();

let fooBar = stringAppend('foo', 'bar');"
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
 > (compile '(module m lisp
               (define (my-cdr x)
                 (cdr x)))
            :finline-functions #t)
 "function myCdr(x) {
  return ((x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
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

 :describe "compile-modules"
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

 :describe "--fsemicolon false"
 > (compile '(begin x y z)
            :fsemicolon #f)
 "x

y

z"

 :describe "compile-with-environment"
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
        (read-syntax
         ";; comment
(foo)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
 "// comment
foo();"
 > (it ";; multi-line
;; comment
(foo)"
       (compile-with-environment
        (read-syntax
         ";; multi-line
;; comment
(foo)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
 "// multi-line
// comment
foo();"
 xit> (it ";; multi-line
;;
;; comment
(foo)"
          (compile-with-environment
           (read-syntax
            ";; multi-line
;;
;; comment
(foo)")
           compilation-environment
           (js/obj :expression-type "statement"
                   :language "javascript"
                   :optimize #t)))
 "// multi-line
//
// comment
foo();"
 > (it ";; multiple

;; comments
(foo)"
       (compile-with-environment
        (read-syntax
         ";; multiple

;; comments
(foo)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
 "// multiple

// comments
foo();"
 > (it "(+
 ;; foo
 foo
 ;; bar
 bar)"
       (compile-with-environment
        (read-syntax
         "(+
            ;; foo
            foo
            ;; bar
            bar)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(list foo
      ;; bar
      bar
      ;; baz
      baz)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(+
            ;; foo
            foo
            ;; bar
            bar)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
 "(
 // foo
 foo +
 // bar
 bar
);"
 > (it ";; comment
(foo)"
       (compile-with-environment
        (read-syntax
         ";; comment
(foo)")
        compilation-environment
        (js/obj :expression-type "statement"
                :language "javascript"
                :optimize #t)))
 "// comment
foo();"
 > (it "I & K"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  ;;; I combinator.
  (define (I x)
   ;; Just return x.
   x)
  ;;; K combinator.
  (define (K x y)
    x))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "/**
 * A combinator.
 */
function A(f, ...args) {
  // Apply f to args.
  return f(...args);
}"
 > (it "A, TS"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))")
        compilation-environment
        (js/obj :language "typescript"
                :optimize #t)))
 "/**
 * A combinator.
 */
function A(f: any, ...args: any[]): any {
  // Apply f to args.
  return f(...args);
}"
 xit> (it "B2, TS"
          (compile-with-environment
           (read-syntax
            "(module m scheme
  ;;; B2 combinator.
  (define (B2 . args)
    (let ((fs (drop-right args 1))
          (x (array-list-last args)))
      ;; Right-to-left function composition
      ;; corresponds to a right fold.
      (foldr A x fs))))")
           compilation-environment
           (js/obj :language "typescript"
                   :optimize #t)))
 "/**
 * B2 combinator.
 */
function B2(...args: any[]): any {
  const fs: any = args.slice(0, -1);
  const x: any = args[args.length - 1];
  // Right-to-left function composition
  // corresponds to a right fold.
  return fs.reduceRight(function (acc: any, x: any): any {
    return A(x, acc);
  }, x);
}"
 > (it "(define ... (let ...))"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  ;;; Foo.
  (define (foo x)
    ;; Bind y.
    (let ((y 1))
      ;; Return y.
      y)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; Whether x is a truish value.
  (define (truish x)
    (if x
        ;; If x is truish, return true.
        #t
      ;; If x is falsey, return false.
      #f)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
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
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; Wrap a value in a list.
  (define (wrap-in-list x)
    ;; Return x wrapped in a list.
    `(,x)))")
        compilation-environment
        (js/obj :case "camelcase"
                :language "javascript"
                :optimize #t)))
 "/**
 * Wrap a value in a list.
 */
function wrapInList(x) {
  // Return x wrapped in a list.
  return [x];
}"
 > (it "while...if"
       (compile-with-environment
        (read-syntax
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
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
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
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; foo method.
    (define/public (foo)
      ;; this
      this)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; Foo class.
  (define Foo
    (class object%
      ;;; bar method.
      (define/public (bar)
        ;; this
        this))))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
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
        (read-syntax
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
        (js/obj :language "typescript"
                :optimize #t)))
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
           (read-syntax
            "(module m scheme
  ;;; Hello, world.
  (: hello-world (-> Void))
  (define (hello-world)
    (display \"hello, world\")))")
           compilation-environment
           (js/obj :case "camelcase"
                   :language "javascript"
                   :optimize #t)))
 "/**
 * Hello, world.
 */
function helloWorld() {
  console.log('hello, world');
}"
 > (it ";;; Foo, blank line, (define (hello-world) ...)"
       (compile-with-environment
        (read-syntax
         ";;; Foo

(require \"foo\")")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "/**
 * Foo
 */

import * as foo from 'foo';"
 > (it ";; Foo, blank line, ;;; Bar, (define (hello-world) ...)"
       (compile-with-environment
        (read-syntax
         ";; Foo

;;; Bar
(require \"foo\")")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "// Foo

/**
 * Bar
 */
import * as foo from 'foo';"
 > (it "(define (hello-world) ...)"
       (compile-with-environment
        (read-syntax
         ";; Foo
;;; Bar

(require \"foo\")")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "// Foo
/**
 * Bar
 */

import * as foo from 'foo';"
 > (it "(define foo\n  ;; bar\n  bar)"
       (compile-with-environment
        (read-syntax
         "(define foo
  ;; bar
  bar)")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "const foo =
  // bar
  bar;"
 > (it "(set! foo\n  ;; bar\n  bar)"
       (compile-with-environment
        (read-syntax
         "(set! foo
  ;; bar
  bar)")
        compilation-environment
        (js/obj :language "javascript"
                :expression-type "statement"
                :optimize #t)))
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
           (js/obj :language "javascript"
                   :optimize #t)))
 "1"
 > (it "(module m scheme ... (apply + '(1 1)) ...), comment"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  ;;; Module header.

  (define one-plus-one
    (apply + '(1 1))))")
        compilation-environment
        (js/obj :case "camelcase"
                :finline-functions #t
                :language "javascript"
                :optimize #t)))
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
        (read-syntax
         "(module m scheme
  ;;; Module header.

  ;;; Custom addition function.
  (define one-plus-one
    (apply + '(1 1))))")
        compilation-environment
        (js/obj :case "camelcase"
                :finline-functions #t
                :language "javascript"
                :optimize #t)))
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
 > (it "(module m scheme ... (apply + '(1 1)) ...), comments"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  ;;; Module header.

  ;;; Custom macro.
  (define-macro (foo &rest body)
    `(begin ,@body))

  (foo
   (cond
    ;; False clause.
    (#f
     1)
    ;; True clause.
    (else
     2))))")
        compilation-environment
        (js/obj :case "camelcase"
                :finline-functions #t
                :language "javascript"
                :optimize #t)))
 "/**
 * Module header.
 */

/**
 * Custom macro.
 */
function foo(exp, env) {
  const body = exp.slice(1);
  return [Symbol.for('begin'), ...body];
}

foo.ftype = 'macro';

if (false) {
  // False clause.
  1;
} else {
  // True clause.
  2;
}"
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
           (js/obj :language "javascript"
                   :optimize #t)))
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
        (js/obj :case "camelcase"
                :language "javascript"
                :optimize #t)))
 "x ? true : false"
 > (it "(falsep x)"
       (compile-with-environment
        '(falsep x)
        compilation-environment
        (js/obj :case "camelcase"
                :language "javascript"
                :optimize #t)))
 "x ? false : true"
 > (it "read-syntax"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  (define foo
    `(foo)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "const foo = [Symbol.for('foo')];"
 > (it "read-syntax, quasiquote"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  (define foo 1)
  (define bar
    `(,foo)))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "const foo = 1;

const bar = [foo];"
 > (it "read-syntax, quasiquoted list of pairs"
       (compile-with-environment
        (read-syntax
         "(module m scheme
  (define foo 1)
  (define bar 2)
  (define quux
    `((\"foo\" . ,foo)
       (\"bar\" . ,bar))))")
        compilation-environment
        (js/obj :language "javascript"
                :optimize #t)))
 "const foo = 1;

const bar = 2;

const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];"
 xit> (it "(module m lisp ... (define *lisp-map* '()))"
          (compile-with-environment
           (read-syntax
            "(module m lisp
  ;; inline-lisp-sources: true

  (define (I x) x))")
           compilation-environment
           (js/obj :case "camelcase"
                   :language "javascript"
                   :optimize #t)))
 "// inline-lisp-sources: true

function I(x) {
  return x;
}

I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];"
 > (it "(: f (-> Number Number)), lambda, comments, TS"
       (compile-with-environment
        (read-syntax
         "(begin
  ;; NN type alias.
  (define-type NN (-> Number Number))
  (: f NN)
  (define f
    (lambda (x)
      x)))")
        compilation-environment
        (js/obj :language "typescript"
                :expression-type "statement"
                :optimize #t)))
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
    (js/obj :language "javascript"
            :inline-lisp-sources #t
            :optimize #t))
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
       (js/obj :language "javascript"
               :inline-lisp-sources #t
               :optimize #t))
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
    (js/obj :language "javascript"
            :inline-lisp-sources #t
            :optimize #t))
 "async function foo(x) {
  return x;
}

foo.fsource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"

 :describe "definition->macro"
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

 :describe "define-macro->lambda-form"
 > (define-macro->lambda-form
     '(define-macro (foo x)
        x)
     (js/obj :exp 'exp :env 'env))
 '(lambda (exp env)
    (define-values (x)
      (rest exp))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo &whole expression x)
        x)
     (js/obj :env 'env))
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
        x)
     (js/obj :exp 'exp :env 'env))
 '(lambda (exp env)
    (define-values x
      (rest exp))
    x)
 > (define-macro->lambda-form
     '(define-macro (foo x &rest y)
        x)
     (js/obj :exp 'exp :env 'env))
 '(lambda (exp env)
    (define-values (x . y)
      (rest exp))
    x)

 :describe "split-comments"
 > (split-comments ";;; Foo\n")
 '(";;; Foo\n")
 xit> (split-comments ";;; Foo")
 '(";;; Foo")
 xit> (split-comments ";; Foo\n;;; Bar")
 '(";; Foo\n" ";;; Bar")
 xit> (split-comments ";; Foo\n;;; Bar\n")
 '(";; Foo\n" ";;; Bar\n"))

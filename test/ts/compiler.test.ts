/**
 * # Compiler tests
 *
 * Various compiler tests.
 */

import {
  LispEnvironment,
  compilationEnvironment,
  compile,
  compileModules,
  compileWithEnvironment,
  splitComments
} from '../../src/ts/language';

import {
  defineMacroToLambdaForm
} from '../../src/ts/macros';

import {
  readSyntax
} from '../../src/ts/parser';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('Global environment', (): any => {
  it('(compile \'(module m scheme (define lst `(,symbol? ,boolean?))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('lst'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('symbol?')], [Symbol.for('unquote'), Symbol.for('boolean?')]]]]], Symbol.for(':finline-functions'), true), `let [symbolp, booleanp] = (() => {
  function symbolp_(obj) {
    return typeof obj === 'symbol';
  }
  function booleanp_(obj) {
    return typeof obj === 'boolean';
  }
  return [symbolp_, booleanp_];
})();

let lst = [symbolp, booleanp];`));
  it('(compile \'(module m scheme (define one-plus-one (apply + \'(1 1)))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('one-plus-one'), [Symbol.for('apply'), Symbol.for('+'), [Symbol.for('quote'), [1, 1]]]]], Symbol.for(':finline-functions'), true), `let [_add] = (() => {
  function add_(...args) {
    let result = 0;
    for (let arg of args) {
      result = result + arg;
    }
    return result;
  }
  return [add_];
})();

let onePlusOne = _add(1, 1);`));
  it('(compile \'(module m scheme (define one-minus-one (apply - \'(1 1)))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('one-minus-one'), [Symbol.for('apply'), Symbol.for('-'), [Symbol.for('quote'), [1, 1]]]]], Symbol.for(':finline-functions'), true), `let [_sub] = (() => {
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

let oneMinusOne = _sub(1, 1);`));
  it('(compile \'(module m scheme (define one-minus-one (apply - \'(1 1)))))', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('one-minus-one'), [Symbol.for('apply'), Symbol.for('-'), [Symbol.for('quote'), [1, 1]]]]]), `import {
  _sub
} from 'roselisp';

let oneMinusOne = _sub(1, 1);`));
  it('(compile \'(module m scheme (define one-times-one (apply * \'(1 1)))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('one-times-one'), [Symbol.for('apply'), Symbol.for('*'), [Symbol.for('quote'), [1, 1]]]]], Symbol.for(':finline-functions'), true), `let [_mul] = (() => {
  function mul_(...args) {
    let result = 1;
    for (let arg of args) {
      result = result * arg;
    }
    return result;
  }
  return [mul_];
})();

let oneTimesOne = _mul(1, 1);`));
  it('(compile \'(module m scheme (define one-divided-by-one (apply / \'(1 1)))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('one-divided-by-one'), [Symbol.for('apply'), Symbol.for('/'), [Symbol.for('quote'), [1, 1]]]]], Symbol.for(':finline-functions'), true), `let [_div] = (() => {
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

let oneDividedByOne = _div(1, 1);`));
  it('(compile \'(module m scheme (define foo-bar (apply string-append \'("foo" "bar")))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('foo-bar'), [Symbol.for('apply'), Symbol.for('string-append'), [Symbol.for('quote'), ['foo', 'bar']]]]], Symbol.for(':finline-functions'), true), `let [stringAppend] = (() => {
  function stringAppend_(...args) {
    let result = '';
    for (let x of args) {
      result = result + x;
    }
    return result;
  }
  return [stringAppend_];
})();

let fooBar = stringAppend('foo', 'bar');`));
  it('(compile \'(module m lisp (define (my-map f x) (map f x)) (define bar (my-map first \'((1) (2) (3))))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-map'), Symbol.for('f'), Symbol.for('x')], [Symbol.for('map'), Symbol.for('f'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('my-map'), Symbol.for('first'), [Symbol.for('quote'), [[1], [2], [3]]]]]], Symbol.for(':finline-functions'), true), `let [first] = (() => {
  function first_(lst) {
    return lst[0];
  }
  return [first_];
})();

function myMap(f, x) {
  return x.map((f.length === 1) ? f : x => f(x));
}

let bar = myMap(first, [[1], [2], [3]]);`));
  it('(compile \'(module m lisp (define (my-cdr x) (cdr x))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-cdr'), Symbol.for('x')], [Symbol.for('cdr'), Symbol.for('x')]]], Symbol.for(':finline-functions'), true), `function myCdr(x) {
  return ((x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
}`));
  return it('(compile \'(module m lisp (define (my-intersection x y) (intersection x y))) :finline-functions #t)', (): any => assertEqual(compile([Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-intersection'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('intersection'), Symbol.for('x'), Symbol.for('y')]]], Symbol.for(':finline-functions'), true), `let [intersection] = (() => {
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
      return args.slice(1).reduce((acc, x) => intersection2(acc, x), args[0]);
    }
  }
  return [intersection_];
})();

function myIntersection(x, y) {
  return intersection(x, y);
}`));
});

describe('compile-modules', (): any => {
  it('(module ... (define ...) ...)', (): any => assertEqual(compileModules([[Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')]]], compilationEnvironment, {
    language: 'javascript',
    optimize: true
  }), [`function I(x) {
  return x;
}`]));
  it('import macro from another module', (): any => assertEqual(compileModules([[Symbol.for('module'), Symbol.for('a'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('provide'), Symbol.for('foo')]], [Symbol.for('module'), Symbol.for('b'), Symbol.for('scheme'), [Symbol.for('require'), [Symbol.for('only-in'), './a', Symbol.for('foo')]], [Symbol.for('declare-macro'), Symbol.for('foo')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]]]], compilationEnvironment, {
    language: 'javascript',
    optimize: true
  }), [`function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

export {
  foo
};`, `import {
  foo
} from './a';

foo.ftype = 'macro';

function bar(x) {
  return x;
}`]));
  it('import macro from a module defined later', (): any => assertEqual(compileModules([[Symbol.for('module'), Symbol.for('a'), Symbol.for('scheme'), [Symbol.for('require'), [Symbol.for('only-in'), './b', Symbol.for('bar')]], [Symbol.for('declare-macro'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('bar'), Symbol.for('x')]]], [Symbol.for('module'), Symbol.for('b'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('provide'), Symbol.for('bar')]]], compilationEnvironment, {
    language: 'javascript',
    optimize: true
  }), [`import {
  bar
} from './b';

bar.ftype = 'macro';

function foo(x) {
  return x;
}`, `function bar(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

bar.ftype = 'macro';

export {
  bar
};`]));
  it('import function for use in a macro', (): any => assertEqual(compileModules([[Symbol.for('module'), Symbol.for('a'), Symbol.for('scheme'), [Symbol.for('require'), [Symbol.for('only-in'), './b', Symbol.for('baz')]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('baz'), Symbol.for('x')]], [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('bar'), Symbol.for('x')]]], [Symbol.for('module'), Symbol.for('b'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('baz'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('provide'), Symbol.for('baz')]]], compilationEnvironment, {
    language: 'javascript',
    optimize: true
  }), [`import {
  baz
} from './b';

function bar(exp, env) {
  const [x] = exp.slice(1);
  return baz(x);
}

bar.ftype = 'macro';

function foo(x) {
  return x;
}`, `function baz(x) {
  return x;
}

export {
  baz
};`]));
  return it('import renamed macro from another module', (): any => assertEqual(compileModules([[Symbol.for('module'), Symbol.for('a'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('provide'), Symbol.for('foo')]], [Symbol.for('module'), Symbol.for('b'), Symbol.for('scheme'), [Symbol.for('require'), [Symbol.for('only-in'), './a', [Symbol.for('foo'), Symbol.for('foo1')]]], [Symbol.for('declare-macro'), Symbol.for('foo1')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('x')], [Symbol.for('foo1'), Symbol.for('x')]]]], compilationEnvironment, {
    language: 'javascript',
    optimize: true
  }), [`function foo(exp, env) {
  const [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

export {
  foo
};`, `import {
  foo as foo1
} from './a';

foo1.ftype = 'macro';

function bar(x) {
  return x;
}`]));
});

describe('--fsemicolon false', (): any => it('(compile \'(begin x y z) :fsemicolon #f)', (): any => assertEqual(compile([Symbol.for('begin'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], Symbol.for(':fsemicolon'), false), `x

y

z`)));

describe('compile-with-environment', (): any => {
  xit('has', (): any => assertEqual(((): any => {
    const options: any = {};
    compileWithEnvironment([Symbol.for('define'), Symbol.for('foo'), 1], undefined, options);
    const continuationEnv: any = options['continuationEnv'];
    return continuationEnv.has(Symbol.for('foo'));
  })(), true));
  xit('EnvironmentStack', (): any => assertEqual(((): any => {
    const options: any = {};
    compileWithEnvironment(Symbol.for('foo'), undefined, options);
    const continuationEnv: any = options['continuationEnv'];
    return continuationEnv instanceof EnvironmentStack;
  })(), true));
  it(`;; comment
(foo)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`;; comment
(foo)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `// comment
foo();`));
  it(`;; multi-line
;; comment
(foo)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`;; multi-line
;; comment
(foo)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `// multi-line
// comment
foo();`));
  xit(`;; multi-line
;;
;; comment
(foo)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`;; multi-line
;;
;; comment
(foo)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `// multi-line
//
// comment
foo();`));
  it(`;; multiple

;; comments
(foo)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`;; multiple

;; comments
(foo)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `// multiple

// comments
foo();`));
  it(`(+
 ;; foo
 foo
 ;; bar
 bar)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`(+
            ;; foo
            foo
            ;; bar
            bar)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `(
 // foo
 foo +
 // bar
 bar
);`));
  it(`(list foo
      ;; bar
      bar
      ;; baz
      baz)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`(list foo
      ;; bar
      bar
      ;; baz
      baz)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `[
 foo,
 // bar
 bar,
 // baz
 baz
];`));
  it(`(+
 ;; foo
 foo
 ;; bar
 bar)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`(+
            ;; foo
            foo
            ;; bar
            bar)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `(
 // foo
 foo +
 // bar
 bar
);`));
  it(`;; comment
(foo)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`;; comment
(foo)`), compilationEnvironment, {
    expressionType: 'statement',
    to: 'javascript',
    optimize: true
  }), `// comment
foo();`));
  it('I & K', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; I combinator.
  (define (I x)
   ;; Just return x.
   x)
  ;;; K combinator.
  (define (K x y)
    x))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('A, JS', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
 * A combinator.
 */
function A(f, ...args) {
  // Apply f to args.
  return f(...args);
}`));
  it('A, TS', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; A combinator.
  (define (A f . args)
    ;; Apply f to args.
    (apply f args)))`), compilationEnvironment, {
    to: 'typescript',
    optimize: true
  }), `/**
 * A combinator.
 */
function A(f: any, ...args: any[]): any {
  // Apply f to args.
  return f(...args);
}`));
  xit('B2, TS', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; B2 combinator.
  (define (B2 . args)
    (let ((fs (drop-right args 1))
          (x (array-list-last args)))
      ;; Right-to-left function composition
      ;; corresponds to a right fold.
      (foldr A x fs))))`), compilationEnvironment, {
    to: 'typescript',
    optimize: true
  }), `/**
 * B2 combinator.
 */
function B2(...args: any[]): any {
  const fs: any = args.slice(0, -1);
  const x: any = args[args.length - 1];
  // Right-to-left function composition
  // corresponds to a right fold.
  return fs.reduceRight((acc: any, x: any): any => A(x, acc), x);
}`));
  it('(define ... (let ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Foo.
  (define (foo x)
    ;; Bind y.
    (let ((y 1))
      ;; Return y.
      y)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
 * Foo.
 */
function foo(x) {
  // Bind y.
  const y = 1;
  // Return y.
  return y;
}`));
  it('(define ... (if ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Whether x is a truish value.
  (define (truish x)
    (if x
        ;; If x is truish, return true.
        #t
      ;; If x is falsey, return false.
      #f)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define ... (cond ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Whether x is a truish value.
  (define (truish x)
    (cond
      ;; If x is truish, return true.
      (x
       #t)
      ;; If x is falsey, return false.
      (else
       #f))))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define ... (let ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Wrap a value in a list.
  (define (wrap-in-list x)
    ;; Return x wrapped in a list.
    \`(,x)))`), compilationEnvironment, {
    case: 'camelcase',
    to: 'javascript',
    optimize: true
  }), `/**
 * Wrap a value in a list.
 */
function wrapInList(x) {
  // Return x wrapped in a list.
  return [x];
}`));
  it('while...if', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
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
          "baz")))
       ;; else case.
       (else
        "baz")))))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define-class Foo ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; bar property.
    (define/public bar 0)

    ;;; Foo constructor.
    (define/public (constructor n)
      ;; Set bar to n.
      (set! (.-this bar) n))))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define-class Foo ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Foo class.
  (define-class Foo ()
    ;;; foo method.
    (define/public (foo)
      ;; this
      this)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define Foo (class ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Foo class.
  (define Foo
    (class object%
      ;;; bar method.
      (define/public (bar)
        ;; this
        this))))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  it('(define Foo (class ...))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Foo class.
  (define Foo
    (class object%
      ;;; foo method.
      (define/public (foo)
        0)

      ;;; bar generator method.
      (define/generator ((get-field iterator Symbol))
        (for ((x (list 1 2 3 4)))
          (yield x))))))`), compilationEnvironment, {
    to: 'typescript',
    optimize: true
  }), `/**
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
}`));
  xit('(define (hello-world) ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Hello, world.
  (: hello-world (-> Void))
  (define (hello-world)
    (display "hello, world")))`), compilationEnvironment, {
    case: 'camelcase',
    to: 'javascript',
    optimize: true
  }), `/**
 * Hello, world.
 */
function helloWorld() {
  console.log('hello, world');
}`));
  it(';;; Foo, blank line, (define (hello-world) ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`;;; Foo

(require "foo")`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `/**
 * Foo
 */

import * as foo from 'foo';`));
  it(';; Foo, blank line, ;;; Bar, (define (hello-world) ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`;; Foo

;;; Bar
(require "foo")`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `// Foo

/**
 * Bar
 */
import * as foo from 'foo';`));
  it('(define (hello-world) ...)', (): any => assertEqual(compileWithEnvironment(readSyntax(`;; Foo
;;; Bar

(require "foo")`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `// Foo
/**
 * Bar
 */

import * as foo from 'foo';`));
  it(`(define foo
  ;; bar
  bar)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`(define foo
  ;; bar
  bar)`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `const foo =
  // bar
  bar;`));
  it(`(set! foo
  ;; bar
  bar)`, (): any => assertEqual(compileWithEnvironment(readSyntax(`(set! foo
  ;; bar
  bar)`), compilationEnvironment, {
    to: 'javascript',
    expressionType: 'statement',
    optimize: true
  }), `foo =
  // bar
  bar;`));
  xit('x, camelCase', (): any => assertEqual(compileWithEnvironment(Symbol.for('x'), new LispEnvironment([['x', 1, 'variable']]), {
    to: 'javascript',
    optimize: true
  }), '1'));
  it('(module m scheme ... (apply + \'(1 1)) ...), comment', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Module header.

  (define one-plus-one
    (apply + '(1 1))))`), compilationEnvironment, {
    case: 'camelcase',
    finlineFunctions: true,
    to: 'javascript',
    optimize: true
  }), `/**
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

const onePlusOne = _add(1, 1);`));
  it('(module m scheme ... (apply + \'(1 1)) ...), comments', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Module header.

  ;;; Custom addition function.
  (define one-plus-one
    (apply + '(1 1))))`), compilationEnvironment, {
    case: 'camelcase',
    finlineFunctions: true,
    to: 'javascript',
    optimize: true
  }), `/**
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
const onePlusOne = _add(1, 1);`));
  it('(module m scheme ... (apply + \'(1 1)) ...), comments', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  ;;; Module header.

  ;;; Custom macro.
  (define-macro (foo &rest body)
    \`(begin ,@body))

  (foo
   (cond
    ;; False clause.
    (#f
     1)
    ;; True clause.
    (else
     2))))`), compilationEnvironment, {
    case: 'camelcase',
    finlineFunctions: true,
    to: 'javascript',
    optimize: true
  }), `/**
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
}`));
  xit('(I x), JS function', (): any => assertEqual(compileWithEnvironment([Symbol.for('I'), Symbol.for('x')], new LispEnvironment([['I', (x: any): any => x, 'function']]), {
    to: 'javascript',
    optimize: true
  }), `(function {
   let I = function(x) {
     return x;
   }
   return I;
})()(x)`));
  it('(truep x)', (): any => assertEqual(compileWithEnvironment([Symbol.for('truep'), Symbol.for('x')], compilationEnvironment, {
    case: 'camelcase',
    to: 'javascript',
    optimize: true
  }), 'x ? true : false'));
  it('(falsep x)', (): any => assertEqual(compileWithEnvironment([Symbol.for('falsep'), Symbol.for('x')], compilationEnvironment, {
    case: 'camelcase',
    to: 'javascript',
    optimize: true
  }), 'x ? false : true'));
  it('read-syntax', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  (define foo
    \`(foo)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), 'const foo = [Symbol.for(\'foo\')];'));
  it('read-syntax, quasiquote', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  (define foo 1)
  (define bar
    \`(,foo)))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `const foo = 1;

const bar = [foo];`));
  it('read-syntax, quasiquoted list of pairs', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m scheme
  (define foo 1)
  (define bar 2)
  (define quux
    \`(("foo" . ,foo)
       ("bar" . ,bar))))`), compilationEnvironment, {
    to: 'javascript',
    optimize: true
  }), `const foo = 1;

const bar = 2;

const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];`));
  xit('(module m lisp ... (define *lisp-map* \'()))', (): any => assertEqual(compileWithEnvironment(readSyntax(`(module m lisp
  ;; inline-lisp-sources: true

  (define (I x) x))`), compilationEnvironment, {
    case: 'camelcase',
    to: 'javascript',
    optimize: true
  }), `// inline-lisp-sources: true

function I(x) {
  return x;
}

I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];`));
  it('(: f (-> Number Number)), lambda, comments, TS', (): any => assertEqual(compileWithEnvironment(readSyntax(`(begin
  ;; NN type alias.
  (define-type NN (-> Number Number))
  (: f NN)
  (define f
    (lambda (x)
      x)))`), compilationEnvironment, {
    to: 'typescript',
    expressionType: 'statement',
    optimize: true
  }), `// NN type alias.
type NN = (a: number) => number;

const f: NN = (x: any): any => x;`));
  it('(compile-with-environment \'(module m scheme (define (foo x) x)) compilation-environment (js/obj :to "javascript" :inline-lisp-sources #t :optimize #t))', (): any => assertEqual(compileWithEnvironment([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')]], compilationEnvironment, {
    to: 'javascript',
    inlineLispSources: true,
    optimize: true
  }), `function foo(x) {
  return x;
}

foo.fsource = [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];`));
  xit('(compile-with-environment \'(module m scheme (define foo (lambda (x) x))) compilation-environment (js/obj :to "javascript" :inline-lisp-sources #t :optimize #t))', (): any => assertEqual(compileWithEnvironment([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], compilationEnvironment, {
    to: 'javascript',
    inlineLispSources: true,
    optimize: true
  }), `const foo = (x) => x;

foo.fsource = [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')];`));
  return it('(compile-with-environment \'(module m scheme (define foo (async (lambda (x) x)))) compilation-environment (js/obj :to "javascript" :inline-lisp-sources #t :optimize #t))', (): any => assertEqual(compileWithEnvironment([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], compilationEnvironment, {
    to: 'javascript',
    inlineLispSources: true,
    optimize: true
  }), `async function foo(x) {
  return x;
}

foo.fsource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];`));
});

describe('define-macro->lambda-form', (): any => {
  it('(define-macro->lambda-form \'(define-macro (foo x) x) (js/obj :exp \'exp :env \'env))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], {
    exp: Symbol.for('exp'),
    env: Symbol.for('env')
  }), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], Symbol.for('x')]));
  it('(define-macro->lambda-form \'(define-macro (foo &whole expression x) x) (js/obj :env \'env))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&whole'), Symbol.for('expression'), Symbol.for('x')], Symbol.for('x')], {
    env: Symbol.for('env')
  }), [Symbol.for('lambda'), [Symbol.for('expression'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('expression')]], Symbol.for('x')]));
  it('(define-macro->lambda-form \'(define-macro (foo &whole exp &environment env) exp))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&whole'), Symbol.for('exp'), Symbol.for('&environment'), Symbol.for('env')], Symbol.for('exp')]), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], Symbol.for('exp')]));
  it('(define-macro->lambda-form \'(define-macro (foo &whole exp &environment env x) x))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&whole'), Symbol.for('exp'), Symbol.for('&environment'), Symbol.for('env'), Symbol.for('x')], Symbol.for('x')]), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], Symbol.for('x')]));
  it('(define-macro->lambda-form \'(define-macro (foo &rest x) x) (js/obj :exp \'exp :env \'env))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&rest'), Symbol.for('x')], Symbol.for('x')], {
    exp: Symbol.for('exp'),
    env: Symbol.for('env')
  }), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), Symbol.for('x'), [Symbol.for('rest'), Symbol.for('exp')]], Symbol.for('x')]));
  it('(define-macro->lambda-form \'(define-macro (foo x &rest y) x) (js/obj :exp \'exp :env \'env))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('x'), Symbol.for('&rest'), Symbol.for('y')], Symbol.for('x')], {
    exp: Symbol.for('exp'),
    env: Symbol.for('env')
  }), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('rest'), Symbol.for('exp')]], Symbol.for('x')]));
  return it('(define-macro->lambda-form \'(define-macro (foo (x 1)) x) (js/obj :exp \'exp :env \'env))', (): any => assertEqual(defineMacroToLambdaForm([Symbol.for('define-macro'), [Symbol.for('foo'), [Symbol.for('x'), 1]], Symbol.for('x')], {
    exp: Symbol.for('exp'),
    env: Symbol.for('env')
  }), [Symbol.for('lambda'), [Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('undefined?'), Symbol.for('x')], [Symbol.for('set!'), Symbol.for('x'), 1]], Symbol.for('x')]));
});

describe('split-comments', (): any => {
  it(`(split-comments ";;; Foo
")`, (): any => assertEqual(splitComments(';;; Foo\n'), [';;; Foo\n']));
  xit('(split-comments ";;; Foo")', (): any => assertEqual(splitComments(';;; Foo'), [';;; Foo']));
  xit(`(split-comments ";; Foo
;;; Bar")`, (): any => assertEqual(splitComments(`;; Foo
;;; Bar`), [';; Foo\n', ';;; Bar']));
  return xit(`(split-comments ";; Foo
;;; Bar
")`, (): any => assertEqual(splitComments(`;; Foo
;;; Bar
`), [';; Foo\n', ';;; Bar\n']));
});
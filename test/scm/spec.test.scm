;;; # Test specification
;;;
;;; Various language tests, expressed as a REPL session.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 ;; `#t`
 > (describe "#t")
 _
 > #t
 #t
 > '#t
 #t
 > (compile #t)
 "true;"

 ;; `#f`
 > (describe "#f")
 _
 > #f
 #f
 > '#f
 #f
 > (compile #f)
 "false;"

 ;; `#u`
 > (describe "#u")
 _
 > #u
 #u
 > '#u
 #u
 > undefined
 #u
 > (compile #u)
 "undefined;"

 ;; `#n`
 > (describe "#n")
 _
 > #n
 #n
 > '#n
 #n
 > js-null
 #n
 > js/null
 #n
 > (compile #n)
 "null;"

 ;; `true?`
 > (describe "true?")
 _
 > (true? #t)
 #t
 > (true? #f)
 #f
 > (true? #u)
 #f
 > (true? #n)
 #f
 > (true? '())
 #t

 ;; `false?`
 > (describe "false?")
 _
 > (false? #t)
 #f
 > (false? #f)
 #t
 > (false? #u)
 #t
 > (false? #n)
 #t
 > (false? '())
 #f

 ;; `nil`
 > (describe "nil")
 _
 > nil
 '()
 > (list? nil)
 #t
 > (length nil)
 0

 ;; `null`
 > (describe "null")
 _
 > null
 '()
 > (listp null)
 #t
 > (length null)
 0

 ;; Numbers
 > (describe "Numbers")
 _
 > 0
 0
 > 1
 1
 > 2
 2
 > (compile 0)
 "0;"
 > (compile 1)
 "1;"
 > (compile 2)
 "2;"

 ;; Strings
 > (describe "Strings")
 _
 > ""
 ""
 > "foo"
 "foo"
 > "\"foo\""
 "\"foo\""
 > (eq? "\t" "	")
 #t
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

 ;; Symbols
 > (describe "Symbols")
 _
 > 'foo
 'foo
 > (compile 'foo)
 "foo;"
 > (compile 'foo-bar)
 "fooBar;"
 > (compile ''foo)
 "Symbol.for('foo');"
 > (compile ''foo-bar)
 "Symbol.for('foo-bar');"
 xit> (compile 'undefined)
 "undefined;"
 > (compile 'js/undefined)
 "undefined;"
 xit> (compile 'js-undefined)
 "undefined;"
 > (compile 'js/null)
 "null;"
 xit> (compile 'js-null)
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

 ;; `symbol?`
 > (describe "symbol?")
 _
 > (symbol? 'foo)
 #t
 > (symbol? 1)
 #f
 > (symbol? "foo")
 #f
 > (symbol? (js/obj))
 #f
 > (symbol? '())
 #f

 ;; `symbol->string`
 > (describe "symbol->string")
 _
 > (symbol->string 'foo)
 "foo"

 ;; `intern`
 > (describe "intern")
 _
 > (intern "foo")
 'foo

 ;; `gensym`
 > (describe "gensym")
 _
 > (symbol? (gensym "foo"))
 #t
 > (eq? (gensym "foo") 'foo)
 #f
 > (eq? (gensym "foo") (gensym "foo"))
 #f
 > (compile '(gensym "foo"))
 "Symbol('foo');"
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

let x2 = 1;

let x1 = 0;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)))
 "let x = 1;

let x1 = 2;"
 > (compile `(begin
               (define x 1)
               (define x1 2)
               (define ,(gensym "x") 3)))
 "let x = 1;

let x1 = 2;

let x2 = 3;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)
               (define x1 3)))
 "let x = 1;

let x2 = 2;

let x1 = 3;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)
               (define-values (x1)
                 (list 3))))
 "let x = 1;

let x2 = 2;

let [x1] = [3];"
 > (compile `(begin
               (define x 1)
               (define (,(gensym "x"))
                 2)
               (define-values (x1)
                 (list 3))))
 "let x = 1;

function x2() {
  return 2;
}

let [x1] = [3];"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)
               (define ,(gensym "x") 3)
               (define x1 4)
               (define x2 5)))
 "let x = 1;

let x3 = 2;

let x4 = 3;

let x1 = 4;

let x2 = 5;"
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

 ;; Keywords
 > (describe "Keywords")
 _
 > :foo
 ':foo
 > ':foo
 ':foo
 > (keyword? ':foo)
 #t
 > (keyword? 'foo)
 #f
 > (compile ':foo)
 "Symbol.for(':foo');"

 ;; Cons cells
 > (describe "Cons cells")
 _
 > (cons 1 2)
 '(1 . 2)
 > (cons 1 (cons 2 3))
 '(1 2 . 3)
 > (cons 1 '())
 '(1)
 > (cons 1 '(2))
 '(1 2)
 > (car '(1 . 2))
 1
 > (cdr '(1 . 2))
 2
 > (car (cons 1 2))
 1
 > (cdr (cons 1 2))
 2
 > (compile ''(1 . ()))
 "[1, Symbol.for('.'), []];"
 > (compile ''(1 . 2))
 "[1, Symbol.for('.'), 2];"
 > (compile ''(1 2 . 3))
 "[1, 2, Symbol.for('.'), 3];"

 ;; Lists
 > (describe "Lists")
 _
 > (list 1 2)
 '(1 2)
 > (aget '(1 2) 0)
 1
 > (aget '((1 2) (3 4)) 0 1)
 2
 > (aref '(1 2) 0)
 1
 > (aset! '(1 2) 0 3)
 3
 > (let ((lst '(1 2)))
     (aset! lst 0 3)
     lst)
 '(3 2)
 > (let ((lst '(1 2))
         (i 0))
     (aget lst i))
 1
 > (let ((lst '(1 2))
         (i 0))
     (aget lst (+ i 1)))
 2
 > (let ((lst '(1 2)))
     (set! (aref lst 0) 3)
     lst)
 '(3 2)
 > (compile '())
 "[];"
 > (compile ''())
 "[];"
 > (compile ''(1))
 "[1];"
 > (compile ''(1 2))
 "[1, 2];"
 > (compile '(aget x 0))
 "x[0];"
 > (compile '(let ((length 0))
               (aget x length)))
 "let length = 0;

x[length];"
 > (compile '(aget x 'length))
 "x['length'];"
 > (compile '(aget x :length))
 "x['length'];"
 > (compile '(aget (js/?. x) 0))
 "x?.[0];"

 ;; `list`
 > (describe "list")
 _
 > (compile '(list))
 "[];"
 > (compile '(list 1))
 "[1];"
 > (compile '(list 1 2))
 "[1, 2];"
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
 > (quote foo)
 'foo
 > (quote (1))
 '(1)
 > (quote (1 2))
 '(1 2)
 > (quote ((1 2) (3 4)))
 '((1 2) (3 4))
 > (compile '(quote foo))
 "Symbol.for('foo');"
 > (compile '(quote ()))
 "[];"
 > (compile '(quote (1)))
 "[1];"
 > (compile '(quote (1 2)))
 "[1, 2];"
 > (compile '(quote ((1))))
 "[[1]];"
 > (compile '(quote ((1 2) (3 4))))
 "[[1, 2], [3, 4]];"
 > (compile '(quote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quote (#t #f)))
 "[true, false];"
 > (compile '(quote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"

 ;; `quasiquote`
 > (describe "quasiquote")
 _
 > `foo
 'foo
 > (quasiquote foo)
 'foo
 > (quasiquote (,1))
 '(1)
 > (quasiquote ((,1)))
 '((1))
 > (quasiquote (,@(list 1 2 3)))
 '(1 2 3)
 > (compile '(quasiquote foo))
 "Symbol.for('foo');"
 > (compile '(quasiquote ()))
 "[];"
 > (compile '(quasiquote (1)))
 "[1];"
 > (compile '(quasiquote ((1))))
 "[[1]];"
 > (compile '(quasiquote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quasiquote ((1 . 2))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
 > (compile '(quasiquote (,1)))
 "[1];"
 > (compile '(quasiquote ((,1))))
 "[[1]];"
 > (compile '(quasiquote (,@(list 1 2 3))))
 "[...[1, 2, 3]];"
 > (compile '(quasiquote ((1 . (unquote 2)))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote ((1 . (unquote 2))
                          (3 . (unquote 4)))))
 "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]];"
 > (compile '(define test-map-1
               `(("foo" . ,test-fn)
                 ("bar" . ,test-fn))))
 "let testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];"
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

 ;; Variables
 > (describe "Variables")
 _
 > (let ((x 2))
     x)
 2
 > (let ((x 2)
         y)
     y)
 #u
 > (let (x)
     (set! x 2)
     x)
 2
 > ((lambda ()
      (define x 2)
      x))
 2
 > ((lambda ()
      (define x)
      (set! x 2)
      x))
 2
 > (let (x y)
     (set! x 2)
     (set! y 3)
     (+ x y))
 5

 ;; Function calls
 > (describe "Function calls")
 _
 > (let ((identity (lambda (x) x)))
     (identity "foo"))
 "foo"
 > (let ((my-add (lambda (x y) (+ x y))))
     (my-add 1 2))
 3
 > (let ((my-add (lambda (x y z) (+ x y z))))
     (my-add 1 2 3))
 6

 ;; `define`
 > (describe "define")
 _
 > ((lambda ()
      (define x 1)
      1))
 1
 > ((lambda ()
      (define (foo . args)
        args)
      (foo)))
 '()
 > ((lambda ()
      (define (my-add x y)
        (+ x y))
      (my-add 2 3)))
 5
 > (let ((my-add (lambda (x y) (+ x y))))
     ((lambda ()
        (define (my-add-2 x y)
          (my-add x y))
        (my-add-2 2 3))))
 5
 > (let ((my-add (lambda (x y z) (+ x y z))))
     ((lambda ()
        (define (my-add-2 x y z)
          (my-add x y z))
        (my-add-2 1 2 3))))
 6
 > (compile '(define x))
 "let x;"
 > (compile '(define x 1))
 "let x = 1;"
 > (compile '(define (foo x)
               x))
 "function foo(x) {
  return x;
}"
 > (compile '(define foo
               (lambda (x)
                 x)))
 "let foo = function (x) {
  return x;
};"
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
 > (compile '(define Foo
               (class object%)))
 "class Foo {
}"
 > (compile '(define Foo
               (class Bar)))
 "class Foo extends Bar {
}"

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

 ;; `define-syntax`
 > (describe "define-syntax")
 _
 > (compile '(module m scheme
               (define-syntax foo
                 (lambda (x)
                   (syntax test)))
               (foo 1)))
 "import {
  datumToSyntax
} from 'roselisp';

let foo = function (x) {
  return datumToSyntax(false, Symbol.for('test'));
};

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

test;"
 > (compile '(module m scheme
               (define-syntax (foo x)
                 (syntax test))
               (foo 1)))
 "import {
  datumToSyntax
} from 'roselisp';

function foo(x) {
  return datumToSyntax(false, Symbol.for('test'));
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

test;"

 > (compile '(module m scheme
               (define-syntax (foo x)
                 (js/second (syntax-e x)))
               (foo 1)))
 "import {
  syntaxE
} from 'roselisp';

function foo(x) {
  return syntaxE(x)[1];
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

1;"

 ;; `syntax->list`
 > (describe "syntax->list")
 _
 > (syntax->list (syntax ()))
 '()
 > (syntax->list (syntax (1 . 2)))
 #f

 ;; `syntax-e`
 > (describe "syntax-e")
 _
 > (syntax-e (syntax ()))
 '()
 > (dotted-list? (syntax-e (syntax (1 . 2))))
 #t
 > (dotted-list? (cdr (syntax-e (syntax (1 . (2 . 3))))))
 #t

 ;; `define-macro`
 > (describe "define-macro")
 _
 > ((lambda ()
      (define-macro (my-macro x)
        x)
      (my-macro 1)))
 1

 ;; `defmacro`
 > (describe "defmacro")
 _
 > ((lambda ()
      (defmacro my-macro (x)
        x)
      (my-macro 1)))
 1
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
 > (describe "define-fexpr")
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

 ;; `let`
 > (describe "let")
 _
 > (let ((x 0))
     x)
 0
 > (let ((x 1))
     (let ((y 2))
       x))
 1
 > (let ((x '((1 2) (3 4))))
     x)
 '((1 2) (3 4))
 > (let (x)
     (set! x 1)
     x)
 1
 > (let (x)
     (set! x 1)
     (set! x 2)
     x)
 2
 > (let (x))
 #u
 > (let ((a 1))
     (+ (let ((a 2))
          a)
        a))
 3
 > (let ((compose (lambda (f g)
                    (lambda (x)
                      (f (g x)))))
         (square (lambda (x) (* x x)))
         (add1 (lambda (x) (+ x 1))))
     ((compose square add1) (add1 4)))
 36
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

 ;; `let*`
 > (describe "let*")
 _
 > (let* ((x 1))
     x)
 1

 ;; `let-values`
 > (describe "let-values")
 _
 > (let-values (((x y) (values 1 2)))
     (list x y))
 '(1 2)
 > (let-values (((x . y) (values 1 2)))
     (list x y))
 '(1 (2))
 > (compile '(let-values (((x y) (values 1 2)))
               (define z
                 (+ x y))))
 "let [x, y] = [1, 2];

let z = x + y;"
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

 ;; `let*-values`
 > (describe "let*-values")
 _
 > (let*-values (((x y) (values 1 2))
                 ((w z) (values 3 4)))
     (list x y w z))
 '(1 2 3 4)
 > (compile '(let*-values (((x y) (values 1 2))
                           ((w z) (values 3 4)))
               (define z
                 (+ x y w z))))
 "let [x, y] = [1, 2];

let [w, z] = [3, 4];

let z = x + y + w + z;"

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
 > ((lambda (x) x) 1)
 1
 > ((lambda (x)
      x)
    "Lisp")
 "Lisp"
 > ((lambda x
      x)
    "Lisp")
 '("Lisp")
 > ((fn (x)
      x)
    1)
 1
 > ((λ (x)
      x)
    1)
 1
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

 ;; `js/function`
 > (describe "js/function")
 _
 > (compile '(js/function ()
               0))
 "function () {
  return 0;
};"
 > (compile '(js/function () : Number
                          0)
            :to 'typescript)
 "function (): number {
  return 0;
};"
 > (compile '(js/function ()
               :name foo
               0))
 "function foo() {
  return 0;
}"
 > (compile '(js/function () : Number
                          :name foo
                          0)
            :to 'typescript)
 "function foo(): number {
  return 0;
}"

 ;; `js/arrow`
 > (describe "js/arrow")
 _
 > (compile '(js/arrow ()
               0))
 "() => {
  return 0;
};"
 > (compile '(js/arrow () : Number
                       0)
            :to 'typescript)
 "(): number => {
  return 0;
};"
 > (compile '(js/arrow ()
               :name foo
               0))
 "let foo = () => {
  return 0;
};"
 > (compile '(js/arrow () : Number
                       :name foo
                       0)
            :to 'typescript)
 "let foo: any = (): number => {
  return 0;
};"

 ;; `js/=>`
 > (describe "js/=>")
 _
 > (compile '(js/=> () 0))
 "() => {
  return 0;
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
 > (compile '(apply (get-field method obj) args))
 "obj.method(...args);"
 > (compile '(apply (.-method obj) args))
 "obj.method(...args);"
 xit> (compile '(apply send obj method args))
 "obj.method(...args);"

 ;; `js/=`
 > (describe "js/=")
 _
 > (compile '(js/= x y))
 "x = y;"
 > (compile '(js/= (aget x i) y))
 "x[i] = y;"
 > (compile '(js/= (list x y) z))
 "[x, y] = z;"
 > (compile '(js/= (list #f y) z))
 "[, y] = z;"
 > (compile '(js/= (list* x) y))
 "x = y;"
 > (compile '(js/= (list* x y) z))
 "[x, ...y] = z;"
 > (compile '(js/= (list* #f x y) z))
 "[, x, ...y] = z;"
 > (compile '(js/= (values x y) z))
 "[x, y] = z;"
 > (compile '(js/= (js/obj x x) y))
 "({x} = y);"
 > (compile '(js/= (js/obj x y) z))
 "({x: y} = z);"
 > (compile '(js/= (set! x) y))
 "x = y;"
 > (compile '(js/= (aset! x i) y))
 "x[i] = y;"
 > (compile '(js/= (oset! x "y") z))
 "x['y'] = z;"
 > (compile '(js/= (set!-values (x)) y))
 "[x] = y;"
 > (compile '(js/= (set!-fields (x)) y))
 "({x} = y);"
 > (compile '(js/= (define x) y))
 "let x = y;"
 > (compile '(js/= (define-values (x)) y))
 "let [x] = y;"
 > (compile '(js/= (define-values (_ x)) y))
 "let [, x] = y;"
 > (compile '(js/= (define-values (_ __ x)
                     :hole-marker __)
                   y))
 "let [_, , x] = y;"
 > (compile '(js/= (define-fields (x)) y))
 "let {x} = y;"

 ;; Lexical scope
 > (describe "lexical scope")
 _
 > ((lambda ()
      (define (K x)
        (lambda () x))
      ((K 42))))
 42
 > ((lambda ()
      (define incrementer #u)
      (let ((x 1))
        (set! incrementer
              (lambda ()
                (set! x (+ x 1))
                x)))
      (incrementer)))
 2
 > (let ((x 100)
         incrementer)
     (let ((x 1))
       (set! incrementer
             (lambda ()
               (set! x (+ x 1))
               x)))
     (incrementer)
     x)
 100

 ;; `begin`
 > (describe "begin")
 _
 > (begin)
 #u
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

 ;; `begin0`
 > (describe "begin0")
 _
 > (begin0 1
     2)
 1

 ;; `if`
 > (describe "if")
 _
 > (if #t 1 2)
 1
 > (if #f 1 2)
 2
 > (if (< 1 2) 1 2)
 1
 > (if (> 2 1) 1 2)
 1
 > (compile '(if x
                 y
                 z))
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(if #t (foo) (bar)))
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as 'statement)
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as 'return)
 "if (true) {
  return foo();
} else {
  return bar();
}"
 > (compile '(if #t (foo) (bar)) :as 'expression)
 "true ? foo() : bar()"
 > (compile '(if #t (foo) (bar) (baz)))
 "if (true) {
  foo();
} else {
  bar();
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
 > (when (< 1 2)
     1 2)
 2
 > (when (> 1 2)
     1 2)
 #u
 > (compile '(when x
               y
               z))
 "if (x) {
  y;
  z;
}"
 > (compile '(when (< 1 2)
               (foo)
               (bar)))
 "if (1 < 2) {
  foo();
  bar();
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
 > (unless (< 1 2)
     1 2)
 #u
 > (unless (> 1 2)
     1 2)
 2
 > (compile '(unless x
               y z))
 "if (!x) {
  y;
  z;
}"
 > (compile '(unless (> 1 2)
               (foo)
               (bar)))
 "if (!(1 > 2)) {
  foo();
  bar();
}"

 ;; `cond`
 > (describe "cond")
 _
 > (cond
    (#f
     1)
    (else
     2))
 2
 > (cond
    (#t
     1)
    (#f
     2))
 1
 > (cond
    (#f
     1)
    (#t
     2))
 2
 > (cond
    (#f
     1)
    (#t
     2))
 2
 > (macroexpand-1 '(cond
                    (#f
                     (foo))
                    (else
                     (bar))))
 '(if #f
      (foo)
      (bar))
 > (macroexpand-1 '(cond
                    (#f
                     (foo)
                     (bar))
                    (else
                     (baz))))
 '(if #f
      (begin
        (foo)
        (bar))
      (baz))
 > (macroexpand-1 '(cond
                    (#f
                     (foo)
                     (bar))
                    (else
                     (baz)
                     (quux))))
 '(if #f
      (begin
        (foo)
        (bar))
      (begin
        (baz)
        (quux)))
 > (macroexpand-1 '(cond
                    (x
                     (foo))
                    (y
                     (bar))
                    (else
                     (baz))))
 '(if x
      (foo)
      (if y
          (bar)
          (baz)))
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar))))
 "if (false) {
  foo();
} else {
  bar();
}"
 > (compile '(cond
              (x
               (foo))
              (y
               (bar))
              (else
               (baz))))
 "if (x) {
  foo();
} else if (y) {
  bar();
} else {
  baz();
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as 'statement)
 "if (false) {
  foo();
} else {
  bar();
}"
 > (compile '(cond
              (x
               y))
            :as 'return)
 "if (x) {
  return y;
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as 'return)
 "if (false) {
  return foo();
} else {
  return bar();
}"
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
              (#f
               (foo))
              (else
               (bar)))
            :as 'expression)
 "false ? foo() : bar()"
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

 ;; `js/?`
 > (describe "js/?")
 _
 > (compile '(js/? x y))
 "x ? y : undefined;"
 > (compile '(js/? x y z))
 "x ? y : z;"
 > (compile '(js/? x y (js/? z w)))
 "x ? y : (z ? w : undefined);"
 > (compile '(js/? x y z)
            :as 'statement)
 "x ? y : z;"
 > (compile '(js/? x y z)
            :as 'return)
 "return x ? y : z;"
 > (compile '(js/? x y z)
            :as 'expression)
 "x ? y : z"

 ;; `js/if`
 > (describe "js/if")
 _
 > (compile '(js/if x y))
 "if (x) {
  y;
}"
 > (compile '(js/if x y z))
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(js/if x y (js/if z w)))
 "if (x) {
  y;
} else if (z) {
  w;
}"
 > (compile '(js/if x y z)
            :as 'statement)
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(js/if x y z)
            :as 'return)
 "if (x) {
  return y;
} else {
  return z;
}"
 it> (compile '(js/if x y z)
              :as 'expression)
 "(() => {
  if (x) {
    return y;
  } else {
    return z;
  }
})()"

 ;; `js/switch`
 > (describe "js/switch")
 _
 > (let* ((x "foo")
          (y "bar"))
     (js/switch x
                (case "foo"
                  (set! y "baz")
                  (break))
                (default
                  (set! y "quux")))
     y)
 "baz"
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
                  (display "foo"))
                (default
                  (display "bar")))
    :as 'return)
 "switch (x) {
  case 'foo': {
    console.log('foo');
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

 ;; `=`
 > (describe "=")
 _
 > (compile '(= 1 1))
 "1 === 1;"
 > (compile '(= x y))
 "x === y;"

 ;; `eq?`
 > (describe "eq?")
 _
 > (eq? #t #t)
 #t
 > (eq? #f #f)
 #t
 > (eq? #t #f)
 #f
 > (eq '_ '_)
 #t
 > (eq _ '_)
 #f
 > (compile '(eq? #t #t))
 "true === true;"

 ;; `equal?`
 > (describe "equal?")
 _
 > (equal? #t #t)
 #t
 > (equal? #f #f)
 #t
 > (equal? #t #f)
 #f
 > (equal? _ '_)
 #f
 > (equal? 1 1)
 #t
 > (equal? '() '())
 #t

 ;; `not`
 > (describe "not")
 _
 > (not #f)
 #t
 > (not #t)
 #f
 > (compile '(not x))
 "!x;"
 > (compile '(not (f x)))
 "!f(x);"
 > (compile '(not (= 1 2)))
 "1 !== 2;"
 > (compile '(not (> 1 2)))
 "!(1 > 2);"
 > (compile '(not (and x y)))
 "!(x && y);"
 xit> (compile '(and (not (f x)) (not (g y))))
 "!f(x) && !g(y);"

 ;; `js/!`
 > (describe "js/!")
 _
 > (js/! #f)
 #t
 > (js/! #t)
 #f
 > (compile '(js/! x))
 "!x;"

 ;; `and`
 > (describe "and")
 _
 > (and)
 #t
 > (and #t)
 #t
 > (and #t #t)
 #t
 > (and #f #f)
 #f
 > (and #f #t)
 #f
 > (and #t #f)
 #f
 > (compile '(and))
 "true;"
 > (compile '(and x))
 "x;"
 > (compile '(and #t #t))
 "true && true;"
 > (compile '(and x y))
 "x && y;"
 xit> (compile '(and x y z))
 "x && y && z;"
 xit> (compile '(and x y (w z)))
 "x && y && w(z);"
 xit> (compile '(and x y (or w z)))
 "x && y && (w || z);"

 ;; `js/&&`
 > (describe "js/&&")
 _
 > (js/&&)
 #t
 > (js/&& #t)
 #t
 > (js/&& #t #t)
 #t
 > (js/&& #t #f)
 #f
 > (funcall js/&&)
 #t
 > (funcall js/&& #t)
 #t
 > (funcall js/&& #t #t)
 #t
 > (funcall js/&& #t #f)
 #f
 > (compile '(js/&& x y))
 "x && y;"
 > (compile '(js/&& x y z))
 "x && y && z;"

 ;; `or`
 > (describe "or")
 _
 > (or)
 #f
 > (or #t)
 #t
 > (or #t #t)
 #t
 > (or #f #f)
 #f
 > (or #f #t)
 #t
 > (or #t #f)
 #t
 > (or 1 2)
 1
 > (or #u 2)
 2
 > (compile '(or))
 "false;"
 > (compile '(or x))
 "x;"
 > (compile '(or #t #t))
 "true || true;"
 > (compile '(or x y))
 "x || y;"
 xit> (compile '(or x y z))
 "x || y || z;"

 ;; `js/\|\|`
 > (describe "js/\|\|")
 _
 > (js/\|\|)
 #f
 > (js/\|\| #t)
 #t
 > (js/\|\| #t #t)
 #t
 > (js/\|\| #t #f)
 #t
 > (funcall js/\|\|)
 #f
 > (funcall js/\|\| #t)
 #t
 > (funcall js/\|\| #t #t)
 #t
 > (funcall js/\|\| #t #f)
 #t
 > (compile '(js/\|\| x y))
 "x || y;"
 > (compile '(js/\|\| x y z))
 "x || y || z;"

 ;; `bitwise-and`
 > (describe "bitwise-and")
 _
 > (compile '(bitwise-and x y))
 "x & y;"
 > (compile '(bit-and x y))
 "x & y;"
 > (compile '(js/& x y))
 "x & y;"
 > (compile '(js/& x y z))
 "x & y & z;"

 ;; `bitwise-or`
 > (describe "bitwise-or")
 _
 > (compile '(bitwise-or x y))
 "x | y;"
 > (compile '(bit-or x y))
 "x | y;"
 > (compile '(js/\| x y))
 "x | y;"
 > (compile '(js/\| x y z))
 "x | y | z;"

 ;; `bitwise-xor`
 > (describe "bitwise-xor")
 _
 > (compile '(bitwise-xor x y))
 "x ^ y;"
 > (compile '(bit-xor x y))
 "x ^ y;"
 > (compile '(js/^ x y))
 "x ^ y;"

 ;; `bitwise-not`
 > (describe "bitwise-not")
 _
 > (compile '(bitwise-negation x))
 "~x;"
 > (compile '(bitwise-not x))
 "~x;"
 > (compile '(bit-not x))
 "~x;"
 > (compile '(js/~ x))
 "~x;"

 ;; `bitwise-shift-left`
 > (describe "bitwise-shift-left")
 _
 > (compile '(bitwise-shift-left x y))
 "x << y;"
 > (compile '(bit-shift-left x y))
 "x << y;"
 > (compile '(js/<< x y))
 "x << y;"

 ;; `bitwise-shift-right`
 > (describe "bitwise-shift-right")
 _
 > (compile '(bitwise-shift-right x y))
 "x >> y;"
 > (compile '(bit-shift-right x y))
 "x >> y;"
 > (compile '(js/>> x y))
 "x >> y;"

 ;; `unsigned-bitwise-shift-right`
 > (describe "unsigned-bitwise-shift-right")
 _
 > (compile '(unsigned-bitwise-shift-right x y))
 "x >>> y;"
 > (compile '(unsigned-bit-shift-right x y))
 "x >>> y;"
 > (compile '(js/>>> x y))
 "x >>> y;"

 ;; `js/op`
 > (describe "js/op")
 _
 > (js/op ! #t)
 #f
 > (js/op && #t #f)
 #f
 > (js/op \|\| #t #f)
 #t
 > (compile '(js/op ! x))
 "!x;"
 > (compile '(js/op ~ x))
 "~x;"
 > (compile '(js/op & x y))
 "x & y;"
 > (compile '(js/op && x y))
 "x && y;"
 > (compile '(js/op \|\| x y))
 "x || y;"

 ;; `while`
 > (describe "while")
 _
 > (let ((result '()))
     (while (< (length result) 3)
       (set! result (cons 1 result)))
     result)
 '(1 1 1)
 > (compile '(while (> x 0)
               (set! x (- x 1))))
 "while (x > 0) {
  x--;
}"

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

 ;; `for`
 > (describe "for")
 _
 > (let (result)
     (js/for (() () ())
             (set! result 1)
             (break))
     result)
 1
 > (let (result)
     (js/for (#u #u #u)
             (set! result 1)
             (break))
     result)
 1
 > (let ((result '()))
     (for ((x '(1 2 3)))
       (set! result (cons x result)))
     result)
 '(3 2 1)
 > ((lambda ()
      (define foo
        '(1 2 3 4))
      (define len
        (length foo))
      (for ((i (range 0 len)))
        (pop-right! foo))
      foo))
 '()
 > ((lambda ()
      (define foo
        '(1 2 3 4))
      (for ((i (range 0 (length foo))))
        (pop-right! foo))
      foo))
 '()
 > (compile '(js/for (() () ())
                     (break)))
 "for (;;) {
  break;
}"
 > (compile '(js/for (#f #f #f)
                     (break)))
 "for (;;) {
  break;
}"
 > (compile '(js/for (#u #u #u)
                     (break)))
 "for (;;) {
  break;
}"
 > (compile '(for ((i (range 0 10)))
               (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(for ((i (range 0 10 2)))
               (foo)))
 "for (let i = 0; i < 10; i = i + 2) {
  foo();
}"
 > (compile '(for ((x lst))
               (foo)))
 "for (let x of lst) {
  foo();
}"
 > (compile '(for ((x '(1 2 3)))
               (foo)))
 "for (let x of [1, 2, 3]) {
  foo();
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
 > (compile '(for ((i (range 0 len)))
               (foo)))
 "for (let i = 0; i < len; i++) {
  foo();
}"
 > (compile '(for ((i (range 0 (js/length foo))))
               (foo)))
 "let _end = foo.length;

for (let i = 0; i < _end; i++) {
  foo();
}"
 xit> (compile '(for ((i (range 0 10))
                      (j (range 0 10)))
                  (foo)))
 "for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"
 > (compile '(for ((x '(1 2 3)))
               (display x)))
 "for (let x of [1, 2, 3]) {
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

 ;; `js/for`
 > (describe "js/for")
 _
 > (let ((result 0))
     (js/for ((i 0) (< i 10) (+ i 1))
             (set! result (+ result 2)))
     result)
 20
 > (compile '(js/for ((i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((set! i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((define i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((begin (set! i 0) (set! j 0))
                      (and (< i 10) (< j 10))
                      (begin (set! i (+ i 1)) (set! j (+ j 1))))
                     (foo)))
 "for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"

 ;; `js/for-in`
 > (describe "js/for-in")
 _
 > (compile '(js/for-in ((i obj))
                        (foo)))
 "for (let i in obj) {
  foo();
}"

 ;; `js/for-of`
 > (describe "js/for-of")
 _
 > (compile '(js/for-of ((i lst))
                        (foo)))
 "for (let i of lst) {
  foo();
}"

 ;; `break`
 > (describe "break")
 _
 > ((lambda ()
      (while #t
        (break))
      1))
 1
 > (let ((result (list)))
     (for ((i (range 0 10)))
       (break)
       (push-right! result i))
     result)
 '()
 > (compile '(while #t
               (break)))
 "while (true) {
  break;
}"

 ;; `continue`
 > (describe "continue")
 _
 > (let ((result (list))
         (i 0))
     (while (< i 10)
       (set! i (+ i 1))
       (when (< i 5)
         (continue))
       (push-right! result i))
     result)
 '(5 6 7 8 9 10)
 > (let ((result (list)))
     (for ((i (range 0 11)))
       (when (< i 5)
         (continue))
       (push-right! result i))
     result)
 '(5 6 7 8 9 10)
 > (compile '(while #f
               (continue)))
 "while (false) {
  continue;
}"

 ;; `return`
 > (describe "return")
 _
 > ((lambda ()
      (return 1)
      2))
 1
 > ((js/function ()
      (return 1)
      2))
 1
 > ((js/arrow ()
      (return 1)
      2))
 1
 > (compile '(return))
 "return;"
 > (compile '(return 0))
 "return 0;"
 > (compile '(while #t
               (return 0)))
 "while (true) {
  return 0;
}"

 ;; `yield`
 > (describe "yield")
 _
 > (compile '(yield))
 "yield;"
 > (compile '(yield 0))
 "yield 0;"

 ;; `throw`
 > (describe "throw")
 _
 > (compile '(throw (new Error "An error")))
 "throw new Error('An error');"

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

 ;; `js/.`
 > (describe "js/.")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/. obj foo))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/. obj "foo"))
 "bar"
 > (let ((obj (js/obj "foo" (js/obj "bar" "baz"))))
     (js/. obj foo bar))
 "baz"
 > (let ((obj (js/obj "foo" (js/obj "bar" "baz"))))
     (js/. (js/. obj foo) bar))
 "baz"
 > (compile '(js/. obj prop))
 "obj.prop;"
 > (compile '(js/. obj "prop"))
 "obj['prop'];"
 > (compile '(js/. obj :foo))
 "obj.foo;"
 > (compile '(js/. obj :foo-bar))
 "obj.fooBar;"
 > (compile '(js/. obj 'foo))
 "obj.foo;"
 > (compile '(js/. obj 'foo-bar))
 "obj.fooBar;"
 > (compile '(js/. obj prop1 prop2))
 "obj.prop1.prop2;"
 > (compile '(js/. (js/. obj prop1) prop2))
 "obj.prop1.prop2;"

 ;; `js/?.`
 > (describe "js/?.")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj foo))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj "foo"))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj quux))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     ((js/?. obj quux)))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj quux wobble))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. (js/?. obj quux) wobble))
 #u
 > (compile '(js/?. obj prop))
 "obj?.prop;"
 > (compile '(js/?. obj prop1 prop2))
 "obj?.prop1?.prop2;"
 > (compile '(js/?. (js/?. obj prop1) prop2))
 "obj?.prop1?.prop2;"
 > (compile '(js/?. (js/?. (js/?. obj) prop1) prop2))
 "obj?.prop1?.prop2;"
 > (compile '(define x
               (js/?. foo bar)))
 "let x = foo?.bar;"
 > (compile '(define x
               ((js/?. foo bar) baz)))
 "let x = foo?.bar(baz);"
 > (compile '(define x
               (js/?. foo (bar))))
 "let x = foo?.(bar);"

 ;; `get-field`
 > (describe "get-field")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (get-field foo obj))
 "bar"
 > (compile '(get-field foo obj))
 "obj.foo;"
 > (compile '(get-field foo-bar obj))
 "obj.fooBar;"
 > (compile '(get-field "foo" obj))
 "obj['foo'];"
 > (compile '(get-field "foo-bar" obj))
 "obj['foo-bar'];"
 > (compile '(get-field (foo-bar) obj))
 "obj[fooBar()];"
 > (compile '(get-field length arr))
 "arr.length;"
 xit> (compile '(get-field (- len 1) arr))
 "arr[len - 1];"

 ;; `set-field!`
 > (describe "set-field!")
 _
 > (let ((obj (js/obj)))
     (set-field! foo-bar obj "baz")
     (get-field foo-bar obj))
 "baz"
 > (compile '(set-field! foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! 'foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! :foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! "foo-bar" obj "baz"))
 "obj['foo-bar'] = 'baz';"
 > (compile '(set-field! (foo-bar) obj "baz"))
 "obj[fooBar()] = 'baz';"
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

 ;; `field-bound?`
 > (describe "field-bound?")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (field-bound? foo obj))
 #t
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

 ;; `oget`
 > (describe "oget")
 _
 > (let ((obj (js/obj "prop" "foo")))
     (oget obj "prop"))
 "foo"
 > (oget _ "@@functional/placeholder")
 #t
 > (compile '(oget obj foo-bar))
 "obj[fooBar];"
 > (compile '(oget obj 'foo-bar))
 "obj['fooBar'];"
 > (compile '(oget obj :foo-bar))
 "obj['fooBar'];"
 > (compile '(oget obj "foo-bar"))
 "obj['foo-bar'];"
 > (compile '(oget obj (foo-bar)))
 "obj[fooBar()];"

 ;; `oset!`
 > (describe "oset!")
 _
 > (let ((obj (js/obj)))
     (oset! obj 'foo-bar "baz")
     (oget obj 'foo-bar))
 "baz"
 > (let ((obj (js/obj)))
     (oset! obj :foo-bar "baz")
     (oget obj :foo-bar))
 "baz"
 > (let ((obj (js/obj)))
     (oset! obj "foo-bar" "baz")
     (oget obj "foo-bar"))
 "baz"
 > (compile '(oset! obj foo-bar "baz"))
 "obj[fooBar] = 'baz';"
 > (compile '(oset! obj 'foo-bar "baz"))
 "obj['fooBar'] = 'baz';"
 > (compile '(oset! obj :foo-bar "baz"))
 "obj['fooBar'] = 'baz';"
 > (compile '(oset! obj "foo-bar" "baz"))
 "obj['foo-bar'] = 'baz';"
 > (compile '(oset! obj (foo-bar) "baz"))
 "obj[fooBar()] = 'baz';"

 ;; `send`
 > (describe "send")
 _
 > (let ((obj (js/obj "add" (lambda (x y) (+ x y)))))
     (send obj add 1 1))
 2
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send obj has "foo"))
 #t
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send obj has "bar"))
 #f
 > (compile '(send obj m arg))
 "obj.m(arg);"
 > (compile '(send map get "foo"))
 "map.get('foo');"

 ;; `send/apply`
 > (describe "send/apply")
 _
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send/apply obj has '("foo")))
 #t
 > (compile '(send/apply obj m args))
 "obj.m(...args);"
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

 ;; `new`
 > (describe "new")
 _
 > (let (quux)
     (set! quux
           (new (class ()
                  (define/public (bar)
                    "baz"))))
     (send quux bar))
 "baz"
 > (let (quux)
     (set! quux
           (new (class ()
                  (define/public val 1)
                  (define (constructor x)
                    (set-field! val this x))
                  (define/public (bar)
                    (get-field val this)))
                2))
     (send quux bar))
 2
 > (compile '(new Foo))
 "new Foo();"
 > (compile '(new Foo x))
 "new Foo(x);"

 ;; `new/apply`
 > (describe "new/apply")
 _
 > (compile '(new/apply Foo args))
 "new Foo(...args);"

 ;; `class`
 > (describe "class")
 _
 > ((lambda ()
      (define Foo
        (class object%
          (define/public (bar)
            "baz")))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define/public (bar)
          "baz"))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define bar "baz"))
      (define quux
        (new Foo))
      (get-field bar quux)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define x)
        (define (constructor x)
          (set-field! x this x))
        (define (bar)
          (get-field x this)))
      (define quux
        (new Foo "xyzzy"))
      (send quux bar)))
 "xyzzy"
 > ((lambda ()
      (defclass Foo (Object)
        (define (bar)
          "baz"))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
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
 > ((lambda ()
      (define-class Foo ()
        (define/public (bar)
          "bar"))
      (define foo
        (new Foo))
      (send foo bar)))
 "bar"
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

 ;; `defclass`
 > (describe "defclass")
 _
 > ((lambda ()
      (defclass Foo ()
        (define/public (bar)
          "bar"))
      (define foo
        (new Foo))
      (send foo bar)))
 "bar"
 > (compile '(defclass Foo ()
               (define/public (bar)
                 "bar")))
 "class Foo {
  bar() {
    return 'bar';
  }
}"

 ;; `instance-of?`
 > (describe "instance-of?")
 _
 > (instance-of? (new Map) Map)
 #t
 > (compile '(instance-of? x Foo))
 "x instanceof Foo;"

 ;; `is-a?`
 > (describe "is-a?")
 _
 > (is-a? (new Map) Map)
 #t
 > (compile '(is-a? x Foo))
 "x instanceof Foo;"

 ;; `js/obj`
 > (describe "js/obj")
 _
 > (js/obj)
 (js/obj)
 > (js/obj "foo" "bar")
 (js/obj "foo" "bar")
 > (js/obj "foo" 1 "bar" 2)
 (js/obj "foo" 1 "bar" 2)
 > (compile '(js/obj))
 "({});"
 > (compile '(js/obj "foo" foo))
 "({
  foo
});"
 > (compile '(js/obj "foo" "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj foo foo))
 "({
  [foo]: foo
});"
 > (compile '(js/obj foo "bar"))
 "({
  [foo]: 'bar'
});"
 > (compile '(js/obj 'foo "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj :foo "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj "foo-bar" "baz"))
 "({
  'foo-bar': 'baz'
});"
 > (compile '(js/obj foo-bar "baz"))
 "({
  [fooBar]: 'baz'
});"
 > (compile '(js/obj 'foo-bar "baz"))
 "({
  fooBar: 'baz'
});"
 > (compile '(js/obj :foo-bar "baz"))
 "({
  fooBar: 'baz'
});"
 > (compile '(js/obj "foo bar" "baz"))
 "({
  'foo bar': 'baz'
});"
 > (compile '(js/obj "foo bar" baz))
 "({
  'foo bar': baz
});"
 > (compile '(js/obj "foo bar" 'baz))
 "({
  'foo bar': Symbol.for('baz')
});"
 > (compile '(js/obj "foo bar" :baz))
 "({
  'foo bar': Symbol.for(':baz')
});"
 > (compile '(js/obj "foo" 1 "bar" 2))
 "({
  foo: 1,
  bar: 2
});"
 > (compile '(js/obj "foo" foo "bar" bar))
 "({
  foo,
  bar
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
 > (compile '(js/obj)
            :as 'expression)
 "{}"
 > (compile '(js/obj "foo" "bar")
            :as 'expression)
 "{
  foo: 'bar'
}"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as 'expression)
 "{
  foo: 1,
  bar: 2
}"
 > (compile '(js/obj)
            :as 'return)
 "return {};"
 > (compile '(js/obj "foo" "bar")
            :as 'return)
 "return {
  foo: 'bar'
};"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as 'return)
 "return {
  foo: 1,
  bar: 2
};"

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
 > (js/keys (js/obj))
 '()
 > (js/keys (js/obj "foo" "bar"))
 '("foo")
 > (js/keys (js/obj "foo" "bar"
                    "baz" "quux"))
 '("foo" "baz")
 > (compile '(js/keys x))
 "Object.keys(x);"

 ;; `js/in`
 > (describe "js/in")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/in "foo" obj))
 #t
 > (compile '(js/in "foo" obj))
 "'foo' in obj;"

 ;; `js/delete`
 > (describe "js/delete")
 _
 > (compile '(js/delete x))
 "delete x;"

 ;; `plist->alist`
 > (describe "plist->alist")
 _
 > (plist->alist '())
 '()
 > (plist->alist '(foo bar))
 '((foo . bar))
 > (plist->alist '(foo bar baz quux))
 '((foo . bar) (baz . quux))

 ;; `plist->object`
 > (describe "plist->object")
 _
 > (plist->object '())
 (js/obj)
 > (plist->object '(foo bar))
 (js/obj "foo" 'bar)
 > (plist->object '(foo bar baz quux))
 (js/obj "foo" 'bar
         "baz" 'quux)

 ;; `module`
 > (describe "module")
 _
 > (module foo bar
     (+ 1 1))
 2
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

 ;; `js/try`
 > (describe "js/try")
 _
 > (js/try
    (/ 1 2)
    (catch e
        (display "there was an error"))
    (finally
      (display "finally")))
 0.5
 > (js/try
    (/ 1 3)
    (/ 1 2)
    (catch e
        (display "there was an error"))
    (finally
      (display "finally")))
 0.5
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
              (/ 1 2)
              (catch e
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
}"

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

 ;; `call/cc`
 > (describe "call/cc")
 _
 > (+ 5
      (call/cc
       (lambda (x)
         (* 10 3))))
 35
 > (+ 5
      (call/cc
       (lambda (x)
         (* 10 (x 3)))))
 8
 > (+ 5 (call/cc
         (lambda (x)
           (x 10)
           3)))
 15
 > (+ 5
      (call/cc
       (lambda (x)
         (x 10)
         (error "error"))))
 15
 > (it "(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)"
       (let ((result 0))
         (try
           (set! result
                 (+ 5 (call/cc
                       (lambda (x)
                         (error "error")))))
           (catch Object e))
         result))
 0

 ;; `define-values`
 > (describe "define-values")
 _
 > ((lambda ()
      (define-values (x y)
        (values 1 2))))
 #u
 > ((lambda ()
      (define-values (x y)
        (values 1 2))
      (list x y)))
 '(1 2)
 > ((lambda ()
      (define-values (x y)
        (values 1 2))
      x))
 1
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
    '(define-values (#f #f value)
       (foo bar baz))
    :to 'typescript)
 "let [, , value]: any[] = foo(bar, baz);"
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
 > ((lambda ()
      (define-fields (x)
        (js/obj "x" 1))
      x))
 1
 > ((lambda ()
      (define-fields (foo)
        (js/obj "foo" "bar"))
      foo))
 "bar"
 > ((lambda ()
      (define-fields ((foo bar))
        (js/obj "foo" "bar"))
      bar))
 "bar"
 > (compile '(define-fields (prop) obj))
 "let {prop} = obj;"
 > (compile '(define-fields (prop) obj))
 "let {prop} = obj;"
 > (compile '(define-fields ((x y) z) obj))
 "let {x: y, z} = obj;"
 > (compile '(define-fields ((x y) z) obj))
 "let {x: y, z} = obj;"
 > (compile '(define-fields (foo)
               (js/obj "foo" "bar")))
 "let {foo} = {
  foo: 'bar'
};"
 > (compile '(define-fields ((foo bar))
               (js/obj "foo" "bar")))
 "let {foo: bar} = {
  foo: 'bar'
};"
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

 ;; `set!-values`
 > (describe "set!-values")
 _
 > (let (x y)
     (set!-values (x y) (values 1 2))
     x)
 1
 > (let (x y)
     (set!-values (x y) (values 1 2))
     (list x y))
 '(1 2)
 > (compile '(set!-values (x y) (values 1 2)))
 "[x, y] = [1, 2];"
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

 ;; `set!-fields`
 > (describe "set!-fields")
 _
 > ((lambda ()
      (let (x)
        (set!-fields (x) (js/obj "x" 1))
        x)))
 1
 > (compile '(set!-fields (prop) obj))
 "({prop} = obj);"
 > (compile '(set!-fields (x) (js/obj "x" 1)))
 "({x} = {
  x: 1
});"

 ;; `destructuring-bind`
 > (describe "destructuring-bind")
 _
 > (destructuring-bind (x y)
                       '(1 2)
                       (list x y))
 '(1 2)
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

 ;; `hash`
 > (describe "hash")
 _
 > (hash)
 (new Map)
 > (hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (compile '(hash))
 "new Map();"
 > (compile '(hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"

 ;; `make-hash`
 > (describe "make-hash")
 _
 > (make-hash)
 (new Map)
 > (make-hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (compile '(make-hash))
 "new Map();"
 > (compile '(make-hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"
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

 ;; `hash?`
 > (describe "hash?")
 _
 > (hash? (make-hash))
 #t
 > (hash? 0)
 #f
 > (compile '(hash? x))
 "x instanceof Map;"

 ;; `hash-clear`
 > (describe "hash-clear")
 _
 > (hash-clear
    (make-hash
     '(("foo" . "bar"))))
 (new Map)

 ;; `hash-clear!`
 > (describe "hash-clear!")
 _
 > (let ((ht (make-hash '(("foo" . "bar")))))
     (hash-clear! ht)
     ht)
 (new Map)
 > (compile '(hash-clear! x))
 "x.clear();"

 ;; `hash-copy`
 > (describe "hash-copy")
 _
 > (hash-copy
    (make-hash
     '(("foo" . "bar"))))
 (new Map '(("foo" "bar")))
 > (compile '(hash-copy x))
 "new Map(x);"

 ;; `hash-keys`
 > (describe "hash-keys")
 _
 > (hash-keys
    (make-hash
     '(("foo" . "bar"))))
 '("foo")
 > (compile '(hash-keys x))
 "[...x.keys()];"

 ;; `hash-values`
 > (describe "hash-values")
 _
 > (hash-values
    (make-hash
     '(("foo" . "bar"))))
 '("bar")
 > (compile '(hash-values x))
 "[...x.values()];"

 ;; `hash->list`
 > (describe "hash->list")
 _
 > (hash->list
    (make-hash
     '(("foo" . "bar"))))
 '(("foo" . "bar"))

 ;; `hash-set`
 > (describe "hash-set")
 _
 > (hash-set
    (make-hash)
    "foo"
    "bar")
 (new Map
      '(("foo" "bar")))

 ;; `hash-set!`
 > (describe "hash-set!")
 _
 > (let ((ht (make-hash)))
     (hash-set! ht "foo" "bar")
     ht)
 (new Map
      '(("foo" "bar")))
 > (compile '(hash-set! ht key val))
 "ht.set(key, val);"

 ;; `hash-ref`
 > (describe "hash-ref")
 _
 > (hash-ref
    (make-hash
     '(("foo" . "bar")))
    "foo")
 "bar"
 > (hash-ref (make-hash) "quux" #f)
 #f
 > (compile '(hash-ref ht "foo"))
 "ht.get('foo');"

 ;; `hash-has-key?`
 > (describe "hash-has-key?")
 _
 > (hash-has-key?
    (make-hash
     '(("foo" . "bar")))
    "foo")
 #t
 > (hash-has-key? (make-hash) "quux")
 #f
 > (compile '(hash-has-key? ht "quux"))
 "ht.has('quux');"

 ;; `Map`
 > (describe "Map")
 _
 > (new Map)
 (new Map)
 > (~> (new Map '((1 2)))
       (send _ entries)
       (send Array from _))
 '((1 2))

 ;; `+`
 > (describe "+")
 _
 > (+)
 0
 > (+ 1)
 1
 > (+ 1 2)
 3
 > (+ 2 2)
 4
 > (+ 1 2 3)
 6
 > (+ 1 2 4)
 7
 > (+ (+ 1 1) (+ 1 1))
 4
 > (let ((x 2))
     (+ x x))
 4
 > (apply + '(1 2))
 3
 > (compile '(+ 1))
 "1;"
 > (compile '(+ 1 1))
 "1 + 1;"
 > (compile '(+ 1 1 1))
 "1 + 1 + 1;"
 > (compile '(+ x 1))
 "x + 1;"
 > (compile '(+ x 1 2))
 "x + 1 + 2;"

 ;; `js/+`
 > (describe "js/+")
 _
 > (js/+)
 0
 > (js/+ 1)
 1
 > (js/+ 1 2)
 3
 > (js/+ 2 2)
 4
 > (js/+ 1 2 3)
 6
 > (js/+ 1 2 4)
 7
 > (js/+ (js/+ 1 1) (js/+ 1 1))
 4
 > (let ((x 2))
     (js/+ x x))
 4
 > (js/+ 1 "")
 "1"
 > (compile '(js/+ 1 1))
 "1 + 1;"
 > (compile '(js/+ 1 1 1))
 "1 + 1 + 1;"

 ;; `-`
 > (describe "-")
 _
 > (-)
 0
 > (- 1)
 -1
 > (- 1 2)
 -1
 > (- 1 2 3)
 -4
 > (- 1 2 4)
 -5
 > (compile '(- 1))
 "-1;"
 > (compile '(- 1 1))
 "1 - 1;"
 > (compile '(- 1 1 1))
 "1 - 1 - 1;"
 > (compile '(- x))
 "-x;"
 > (compile '(- x 1))
 "x - 1;"
 > (compile '(- x 1 2))
 "x - 1 - 2;"
 xit> (compile '(- (- x)))
 "x;"

 ;; `js/-`
 > (describe "js/-")
 _
 > (js/-)
 0
 > (js/- 1)
 -1
 > (js/- 1 2)
 -1
 > (js/- 1 2 3)
 -4
 > (js/- 1 2 4)
 -5
 > (compile '(js/- 1))
 "-1;"
 > (compile '(js/- 1 1))
 "1 - 1;"
 > (compile '(js/- 1 1 1))
 "1 - 1 - 1;"

 ;; `*`
 > (describe "*")
 _
 > (*)
 1
 > (* 1)
 1
 > (* 1 2)
 2
 > (* 1 2 3)
 6
 > (* 1 2 4)
 8
 > (compile '(* 1 1))
 "1 * 1;"
 > (compile '(* 1 1 1))
 "1 * 1 * 1;"

 ;; `js/*`
 > (describe "js/*")
 _
 > (js/*)
 1
 > (js/* 1)
 1
 > (js/* 1 2)
 2
 > (js/* 1 2 3)
 6
 > (js/* 1 2 4)
 8
 > (compile '(js/* 1 1))
 "1 * 1;"
 > (compile '(js/* 1 1 1))
 "1 * 1 * 1;"

 ;; `/`
 > (describe "/")
 _
 > (/)
 #u
 > (/ 1)
 1
 > (/ 1 2)
 0.5
 > (/ 1 2 3)
 (/ 1 2 3)
 > (/ 1 2 4)
 0.125
 > (compile '(/ 1 2))
 "1 / 2;"
 > (compile '(/ 1 2 4))
 "1 / 2 / 4;"

 ;; `js//`
 > (describe "js//")
 _
 > (js//)
 #u
 > (js// 1)
 1
 > (js// 1 2)
 0.5
 > (js// 1 2 3)
 (js// 1 2 3)
 > (/ 1 2 4)
 0.125
 > (compile '(js// 1 2))
 "1 / 2;"
 > (compile '(js// 1 2 4))
 "1 / 2 / 4;"

 ;; `<`
 > (describe "<")
 _
 > (< 1)
 #t
 > (< 1 2)
 #t
 > (< 2 1)
 #f
 > (< 1 2 3)
 #t
 > (< 2 1 3)
 #f
 > (< 1 3 2)
 #f
 > (funcall < 1 2)
 #t
 > (funcall < 2 1)
 #f
 > (funcall < 1 2 3)
 #t
 > (funcall < 2 1 3)
 #f
 > (funcall < 1 3 2)
 #f
 > (compile '(< 1))
 "true;"
 > (compile '(< 1 2))
 "1 < 2;"
 > (compile '(< x y))
 "x < y;"
 > (compile '(< 1 2 3))
 "(1 < 2) && (2 < 3);"
 > (compile '(< x y z))
 "(x < y) && (y < z);"

 ;; `js/<`
 > (describe "js/<")
 _
 > (js/< 1 2)
 #t
 > (js/< 2 1)
 #f
 > (js/< 1 2 3)
 #t
 > (js/< 2 1 3)
 #f
 > (js/< 1 3 2)
 #f
 > (funcall js/< 1 2)
 #t
 > (funcall js/< 2 1)
 #f
 > (funcall js/< 1 2 3)
 #t
 > (funcall js/< 2 1 3)
 #f
 > (funcall js/< 1 3 2)
 #f
 > (compile '(js/< x y))
 "x < y;"
 > (compile '(js/< x y z))
 "(x < y) && (y < z);"

 ;; `<=`
 > (describe "<=")
 _
 > (<= 1 2)
 #t
 > (<= 2 1)
 #f
 > (<= 1 2 3)
 #t
 > (<= 1 1 2)
 #t
 > (<= 2 1 3)
 #f
 > (<= 1 3 2)
 #f
 > (funcall <= 1 2)
 #t
 > (funcall <= 2 1)
 #f
 > (funcall <= 1 2 3)
 #t
 > (funcall <= 1 1 2)
 #t
 > (funcall <= 2 1 3)
 #f
 > (funcall <= 1 3 2)
 #f
 > (compile '(<= x y))
 "x <= y;"
 > (compile '(<= x y z))
 "(x <= y) && (y <= z);"

 ;; `js/<=`
 > (describe "js/<=")
 _
 > (js/<= 1 2)
 #t
 > (js/<= 2 1)
 #f
 > (js/<= 1 2 3)
 #t
 > (js/<= 1 1 2)
 #t
 > (js/<= 2 1 3)
 #f
 > (js/<= 1 3 2)
 #f
 > (funcall js/<= 1 2)
 #t
 > (funcall js/<= 2 1)
 #f
 > (funcall js/<= 1 2 3)
 #t
 > (funcall js/<= 1 1 2)
 #t
 > (funcall js/<= 2 1 3)
 #f
 > (funcall js/<= 1 3 2)
 #f
 > (compile '(js/<= x y))
 "x <= y;"
 > (compile '(js/<= x y z))
 "(x <= y) && (y <= z);"

 ;; `>`
 > (describe ">")
 _
 > (> 1)
 #t
 > (> 2 1)
 #t
 > (> 1 2)
 #f
 > (> 3 2 1)
 #t
 > (> 1 2 3)
 #f
 > (> 2 1 3)
 #f
 > (> 1 3 2)
 #f
 > (funcall > 2 1)
 #t
 > (funcall > 1 2)
 #f
 > (funcall > 3 2 1)
 #t
 > (funcall > 1 2 3)
 #f
 > (funcall > 2 1 3)
 #f
 > (funcall > 1 3 2)
 #f
 > (compile '(> 1))
 "true;"
 > (compile '(> 2 1))
 "2 > 1;"
 > (compile '(> x y))
 "x > y;"
 > (compile '(> 3 2 1))
 "(3 > 2) && (2 > 1);"
 > (compile '(> x y z))
 "(x > y) && (y > z);"

 ;; `js/>`
 > (describe "js/>")
 _
 > (js/> 2 1)
 #t
 > (js/> 1 2)
 #f
 > (js/> 3 2 1)
 #t
 > (js/> 1 2 3)
 #f
 > (js/> 2 1 3)
 #f
 > (js/> 1 3 2)
 #f
 > (funcall js/> 2 1)
 #t
 > (funcall js/> 1 2)
 #f
 > (funcall js/> 3 2 1)
 #t
 > (funcall js/> 1 2 3)
 #f
 > (funcall js/> 2 1 3)
 #f
 > (funcall js/> 1 3 2)
 #f
 > (compile '(js/> x y))
 "x > y;"
 > (compile '(js/> x y z))
 "(x > y) && (y > z);"

 ;; `>=`
 > (describe ">=")
 _
 > (>= 1)
 #t
 > (>= 2 1)
 #t
 > (>= 2 2)
 #t
 > (>= 1 2)
 #f
 > (>= 3 2 1)
 #t
 > (>= 3 2 2)
 #t
 > (>= 1 2 3)
 #f
 > (>= 2 1 3)
 #f
 > (>= 1 3 2)
 #f
 > (funcall >= 2 1)
 #t
 > (funcall >= 2 2)
 #t
 > (funcall >= 1 2)
 #f
 > (funcall >= 3 2 1)
 #t
 > (funcall >= 3 2 2)
 #t
 > (funcall >= 1 2 3)
 #f
 > (funcall >= 2 1 3)
 #f
 > (funcall >= 1 3 2)
 #f
 > (compile '(>= x y))
 "x >= y;"
 > (compile '(>= x y z))
 "(x >= y) && (y >= z);"

 ;; `js/>=`
 > (describe "js/>=")
 _
 > (js/>= 2 1)
 #t
 > (js/>= 2 2)
 #t
 > (js/>= 1 2)
 #f
 > (js/>= 3 2 1)
 #t
 > (js/>= 3 2 2)
 #t
 > (js/>= 1 2 3)
 #f
 > (js/>= 2 1 3)
 #f
 > (js/>= 1 3 2)
 #f
 > (funcall js/>= 2 1)
 #t
 > (funcall js/>= 2 2)
 #t
 > (funcall js/>= 1 2)
 #f
 > (funcall js/>= 3 2 1)
 #t
 > (funcall js/>= 3 2 2)
 #t
 > (funcall js/>= 1 2 3)
 #f
 > (funcall js/>= 2 1 3)
 #f
 > (funcall js/>= 1 3 2)
 #f
 > (compile '(js/>= x y))
 "x >= y;"
 > (compile '(js/>= x y z))
 "(x >= y) && (y >= z);"

 ;; `mod`
 > (describe "mod")
 _
 > (compile '(mod x y))
 "x % y;"

 ;; `js/%`
 > (describe "js/%")
 _
 > (compile '(js/% x y))
 "x % y;"

 ;; `abs`
 > (describe "abs")
 _
 > (abs 1)
 1
 > (abs -1)
 1
 > (compile '(abs x))
 "Math.abs(x);"
 > (compile '(js/abs x))
 "Math.abs(x);"

 ;; `range`
 > (describe "range")
 _
 > (range 1 2)
 '(1)
 > (range 10)
 '(0 1 2 3 4 5 6 7 8 9)
 > (range 10 20)
 '(10 11 12 13 14 15 16 17 18 19)
 > (range 20 40 2)
 '(20 22 24 26 28 30 32 34 36 38)
 > (range 20 10 -1)
 '(20 19 18 17 16 15 14 13 12 11)
 > (range 10 15 1.5)
 '(10 11.5 13.0 14.5)

 ;; `member`
 > (describe "member")
 _
 > (member 2 '(1 2 3 4))
 '(2 3 4)
 > (member 9 '(1 2 3 4))
 #f
 > (member 5
           '(3 5 1 7 2 9)
           (lambda (x y)
             (< x y)))
 '(7 2 9)

 ;; `member?`
 > (describe "member?")
 _
 > (member? 2 '(1 2 3 4))
 #t
 > (member? 9 '(1 2 3 4))
 #f
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

 ;; `memq?`
 > (describe "memq?")
 _
 > (memq? 2 '(1 2 3 4))
 #t
 > (memq? 9 '(1 2 3 4))
 #f
 > (compile '(memq? x lst))
 "lst.includes(x);"
 > (compile '(memq? 2 (list 1 2 3 4)))
 "[1, 2, 3, 4].includes(2);"
 > (compile '(memq? (+ 1 1) (list 1 2 3 4)))
 "[1, 2, 3, 4].includes(1 + 1);"

 ;; `take`
 > (describe "take")
 _
 > (take '(1 2 3 4) 0)
 '()
 > (take '(1 2 3 4) 1)
 '(1)
 > (take '(1 2 3 4) 2)
 '(1 2)

 ;; `drop`
 > (describe "drop")
 _
 > (drop '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop '(1 2 3 4) 1)
 '(2 3 4)
 > (compile '(drop x 1))
 "x.slice(1);"

 ;; `drop-right`
 > (describe "drop-right")
 _
 > (drop-right '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop-right '(1 2 3 4) 1)
 '(1 2 3)
 > (compile '(drop-right x 1))
 "x.slice(0, -1);"

 ;; `map`
 > (describe "map")
 _
 > (map list '(1 2))
 '((1) (2))
 > ((lambda ()
      (define (fact n)
        (if (< n 2)
            1
            (* n (fact (- n 1)))))
      (map fact '(1 2 3 4 5 6))))
 '(1 2 6 24 120 720)
 > (compile '(map f lst))
 "lst.map(function (x) {
  return f(x);
});"
 > (compile '(map (lambda (x) x) lst))
 "lst.map(function (x) {
  return x;
});"
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
 > (foldl cons '() '(1 2 3 4))
 '(4 3 2 1)
 > (compile '(foldl (lambda (x acc) x) v lst))
 "lst.reduce(function (acc, x) {
  return x;
}, v);"
 > (compile '(foldl f v lst))
 "lst.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
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
 > (foldr cons '() '(1 2 3 4))
 '(1 2 3 4)
 > (foldr (lambda (v l)
            (cons (add1 v) l))
          '()
          '(1 2 3 4))
 '(2 3 4 5)
 > (compile '(foldr (lambda (x acc) x) v lst))
 "lst.reduceRight(function (acc, x) {
  return x;
}, v);"
 > (compile '(foldr f v lst))
 "lst.reduceRight(function (acc, x) {
  return f(x, acc);
}, v);"
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

 ;; `filter`
 > (describe "filter")
 _
 > (filter string? '("foo" 1 2 3))
 '("foo")
 > (compile '(filter f lst))
 "lst.filter(f);"

 ;; `string?`
 > (describe "string?")
 _
 > (string? "foo")
 #t
 > (string? 1)
 #f
 > (string? (js/obj))
 #f
 > (string? (list "foo"))
 #f
 > (string? (js/obj "foo" ""))
 #f
 > (string? (js/obj "foo" (quote ())))
 #f
 > (string? (js/obj "foo" (js/obj)))
 #f
 > (string? (js/obj "foo" "foo"))
 #f
 > (string? (quote ()))
 #f
 > (compile '(string? x))
 "typeof x === 'string';"

 ;; `string-length`
 > (describe "string-length")
 _
 > (string-length "foo")
 3
 > (compile '(string-length x))
 "x.length;"

 ;; `string-append`
 > (describe "string-append")
 _
 > (string-append)
 ""
 > (string-append "foo")
 "foo"
 > (string-append "foo" "bar")
 "foobar"
 > (apply string-append '("foo" "bar"))
 "foobar"
 > (compile '(string-append "foo"))
 "'foo';"
 > (compile '(string-append "foo" "bar"))
 "'foo' + 'bar';"
 > (compile '(string-append "foo" "bar" "baz"))
 "'foo' + 'bar' + 'baz';"

 ;; `string-join`
 > (describe "string-join")
 _
 > (string-join '("foo" "bar"))
 "foo bar"
 > (string-join '("foo" "bar") ",")
 "foo,bar"
 > (compile '(string-join '("foo" "bar") ","))
 "['foo', 'bar'].join(',');"

 ;; `string-split`
 > (describe "string-split")
 _
 > (string-split "foo bar  baz")
 '("foo" "bar" "baz")
 > (string-split "foo,bar,baz" ",")
 '("foo" "bar" "baz")
 > (string-split "foo, bar, baz" ", ")
 '("foo" "bar" "baz")
 > (string-split "foo\nbar\nbaz" "\n")
 '("foo" "bar" "baz")
 > (compile '(string-split "foo,bar,baz" ","))
 "'foo,bar,baz'.split(',');"

 ;; `string-trim`
 > (describe "string-trim")
 _
 > (string-trim "  foo bar  baz  ")
 "foo bar  baz"
 > (string-trim "  foo bar  baz \r\n\t")
 "foo bar  baz"
 > (compile '(string-trim x))
 "x.trim();"

 ;; `string-upcase`
 > (describe "string-upcase")
 _
 > (string-upcase "foo")
 "FOO"
 > (compile '(string-upcase x))
 "x.toUpperCase();"

 ;; `string-downcase`
 > (describe "string-downcase")
 _
 > (string-downcase "FOO")
 "foo"
 > (compile '(string-downcase x))
 "x.toLowerCase();"

 ;; `substring`
 > (describe "substring")
 _
 > (substring "Apple" 1 3)
 "pp"
 > (substring "Apple" 1)
 "pple"
 > (compile '(substring str i))
 "str.substring(i);"
 > (compile '(substring str i j))
 "str.substring(i, j);"

 ;; `js/tag`
 > (describe "js/tag")
 _
 > (compile '(js/tag foo "bar"))
 "foo`bar`;"
 xit> (compile (js/tag sexp "\"\\\\s\""))
 "'\\\\s';"

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

 ;; `as~>`
 > (describe "as~>")
 _
 > (as~> 0 _
     (+ _ 1)
     (+ _ 1))
 2
 > (macroexpand '(as~> 0 _
                   (+ _ 1)
                   (+ _ 1)))
 '(+ (+ 0 1) 1)
 > (compile '(as~> 0 _
               (+ _ 1)
               (+ _ 1)))
 "0 + 1 + 1;"

 ;; `ann`
 > (describe "ann")
 _
 > (ann #u Any)
 #u
 > (compile '(ann #t Any))
 "true;"
 > (compile '(ann #t Any) :to 'typescript)
 "true as any;"
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

 ;; `cons?`
 > (describe "cons?")
 _
 > (cons? '())
 #f

 ;; `list?`
 > (describe "list?")
 _
 > (list? '())
 #t
 > (list? '(1 . 2))
 #f
 > (list? '(1 2 . 3))
 #f
 > (list? '(1 . ()))
 #t
 > (list? '(1 . (2 . ())))
 #t

 ;; `vector?`
 > (describe "vector?")
 _
 > (vector? '())
 #t
 > (vector? '(1 . 2))
 #t
 > (vector? '(1 2 . 3))
 #t
 > (vector? '(1 . ()))
 #t
 > (vector? '(1 . (2 . ())))
 #t

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
 xit> (compile '(set! (aref args 0) 1))
 "args[0] = 1;"

 ;; `aset`
 > (describe "aset!")
 _
 > (compile '(aset! args 0 1))
 "args[0] = 1;"

 ;; `array-list?`
 > (describe "array-list?")
 _
 > (array-list? '())
 #t
 > (array-list? '(1 . 2))
 #t
 > (array-list? '(1 2))
 #t
 > (array-list? '(1 2 3))
 #t

 ;; `array-list-length`
 > (describe "array-list-length")
 _
 > (compile '(array-list-length x))
 "x.length;"

 ;; `linked-list?`
 > (describe "linked-list?")
 _
 > (linked-list? '())
 #f
 > (linked-list? '(1 . 2))
 #f
 > (linked-list? '(1 . ()))
 #t
 > (linked-list? '(1 . (2 . ())))
 #t
 > (linked-list? '(1 2 . (3 . ())))
 #t

 ;; `linked-list-link?`
 > (describe "linked-list-link?")
 _
 > (linked-list-link? '())
 #f
 > (linked-list-link? '(1 . 2))
 #t
 > (linked-list-link? '(1 . ()))
 #t
 > (linked-list-link? '(1 . (2 . ())))
 #t
 > (linked-list-link? '(1 2 . (3 . ())))
 #t

 ;; `length`
 > (describe "length")
 _
 > (length '(1 . ()))
 1
 > (length '(1 . (2 . ())))
 2
 > (length '(1 2 . ()))
 2

 ;; `first`
 > (describe "first")
 _
 > (compile '(first x))
 "x[0];"

 ;; `last`
 > (describe "last")
 _
 > (last '(1 . ()))
 1
 > (last '(1 . (2 . ())))
 2
 > (last '(1 2 . ()))
 2
 xit> (compile '(last x))
 "x[x.length - 1];"

 ;; `nth`
 > (describe "nth")
 _
 > (nth 1 '(1 . (2 . ())))
 2
 > (nth 1 '(1 2 . (3 . ())))
 2
 xit> (compile '(nth 1 x))
 "x[1];"
 xit> (compile '(nth 2 (nth 1 x)))
 "x[1][2];"

 ;; `nthcdr`
 > (describe "nthcdr")
 _
 xit> (compile '(nthcdr 1 x))
 "x.slice(1);"

 ;; `cdr`
 > (describe "cdr")
 _
 > (cdr '(1 . (2 . ())))
 '(2 . ())
 > (cdr '(1 2 . (3 . ())))
 '(2 . (3 . ()))

 ;; `set-car!`
 > (describe "set-car!")
 _
 > ((lambda ()
      (define foo '())
      (set-car! foo 'bar)
      foo))
 '()
 > ((lambda ()
      (define foo
        '(foo))
      (set-car! foo 'bar)
      foo))
 '(bar)

 ;; `set-cdr!`
 > (describe "set-cdr!")
 _
 > ((lambda ()
      (define foo '())
      (set-cdr! foo '(bar))
      foo))
 '()
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo '(bar))
      foo))
 '(foo bar)
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo '(baz))
      foo))
 '(foo baz)
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo '(baz . quux))
      foo))
 '(foo baz . quux)
 > ((lambda ()
      (define foo
        '(foo . bar))
      (set-cdr! foo '(baz))
      foo))
 '(foo baz)
 > ((lambda ()
      (define foo
        '(foo . bar))
      (set-cdr! foo '(baz . quux))
      foo))
 '(foo baz . quux)
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo 'bar)
      foo))
 '(foo . bar)
 > ((lambda ()
      (define foo
        '(foo bar . baz))
      (set-cdr! foo '(quux))
      foo))
 '(foo quux)
 > ((lambda ()
      (define foo
        '(foo bar . baz))
      (set-cdr! foo 'quux)
      foo))
 '(foo . quux)

 ;; Dotted lists
 > (describe "Dotted lists")
 _
 > (equal? '(1 2) '(1 . (2 . ())))
 #t

 ;; `dotted-list?`
 > (describe "dotted-list?")
 _
 > (dotted-list? '())
 #f
 > (dotted-list? '(1 . 2))
 #t
 > (dotted-list? '(1 . ()))
 #f
 > (dotted-list? '(1 . (2 . ())))
 #f
 > (dotted-list? '(1 . (2 . 3)))
 #t
 > (dotted-list? '(foo . bar))
 #t
 > (dotted-list? '(foo bar))
 #f

 ;; `dotted-list-length`
 > (describe "dotted-list-length")
 _
 > (dotted-list-length '())
 0
 > (dotted-list-length '(1 . ()))
 1
 > (dotted-list-length '(1 . (2 . ())))
 2

 ;; `dotted-list-head`
 > (describe "dotted-list-head")
 _
 > (dotted-list-head '(foo . bar))
 '(foo)
 > (dotted-list-head '(foo bar . baz))
 '(foo bar)

 ;; `dotted-list-tail`
 > (describe "dotted-list-tail")
 _
 > (dotted-list-tail '(foo . bar))
 'bar
 > (dotted-list-tail '(foo bar . baz))
 'baz

 ;; `dotted-list-last`
 > (describe "dotted-list-last")
 _
 > (dotted-list-last '())
 #u
 > (dotted-list-last '(1 . ()))
 1
 > (dotted-list-last '(1 . (2 . ())))
 2

 ;; `dotted-list-last-cdr`
 > (describe "dotted-list-last-cdr")
 _
 > (dotted-list-last-cdr '())
 '()
 > (dotted-list-last-cdr '(1 . ()))
 '()
 > (dotted-list-last-cdr '(1 . (2 . ())))
 '()

 ;; `dotted-list->proper-list`
 > (describe "dotted-list->proper-list")
 _
 > (dotted-list->proper-list '(foo . bar))
 '(foo bar)
 > (dotted-list->proper-list '(foo bar . baz))
 '(foo bar baz)

 ;; `proper-list?`
 > (describe "proper-list?")
 _
 > (proper-list? '(foo bar))
 #t
 > (proper-list? '(foo . bar))
 #f

 ;; `circular-list?`
 > (describe "circular-list?")
 _
 > ((lambda ()
      (define foo '())
      (circular-list? foo)
      (set-cdr! foo foo)
      (circular-list? foo)))
 #f
 > (circular-list? '(foo))
 #f
 > (circular-list? '(foo . bar))
 #f
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t
 > ((lambda ()
      (define foo
        '(foo . ()))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t

 ;; `proper-list->dotted-list`
 > (describe "proper-list->dotted-list")
 _
 > (proper-list->dotted-list '(foo bar))
 '(foo . bar)
 > (proper-list->dotted-list '(foo bar baz))
 '(foo bar . baz)

 ;; `list*`
 > (describe "list*")
 _
 > (list*)
 #u
 > (list* 1)
 1
 > (list* 1 2)
 '(1 . 2)
 > (list* 1 2 3)
 '(1 2 . 3)
 > (list* 1 2 3 4)
 '(1 2 3 . 4)
 > (list* 1 '())
 '(1)
 > (list* 1 '(2))
 '(1 2)
 > (list* 1 '(2 . 3))
 '(1 2 . 3)

 ;; `flatten`
 > (describe "flatten")
 _
 > (flatten '(1 2 3 4))
 '(1 2 3 4)
 > (flatten '(1 . 2))
 '(1 2)
 > (flatten '((a) b (c (d) . e) ()))
 '(a b c d e)
 > (flatten '((((4)))))
 '(4)

 ;; Cons dot
 > (describe "Cons dot")
 _
 > *cons-dot*
 '.
 > (cons-dot)
 '.
 > (cons-dot? *cons-dot*)
 #t

 ;; `require`
 > (describe "require")
 _
 > (compile '(require "foo"))
 "import * as foo from 'foo';"
 > (compile '(require "foo-bar"))
 "import * as fooBar from 'foo-bar';"
 > (compile '(require foo "bar"))
 "import * as foo from 'bar';"
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
 > (compile '(require "foo")
            :fes-module-interop #t)
 "import foo from 'foo';"
 > (compile '(require "foo-bar")
            :fes-module-interop #t)
 "import fooBar from 'foo-bar';"
 > (compile '(require foo "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require "foo" "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require "foo")
            :fcommonjs #t)
 "let foo = require('foo');"
 > (compile '(require foo "bar")
            :fcommonjs #t)
 "let foo = require('bar');"
 > (compile '(require "foo" "bar")
            :fcommonjs #t)
 "let foo = require('bar');"
 > (compile '(require (only-in "foo"
                               bar))
            :fcommonjs #t)
 "let {bar} = require('foo');"
 > (compile '(require (only-in "foo"
                               (bar baz)))
            :fcommonjs #t)
 "let {bar: baz} = require('foo');"
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
 > (compile '(provide x)
            :fcommonjs #t)
 "module.exports = {
  x
};"
 > (compile '(provide x y)
            :fcommonjs #t)
 "module.exports = {
  x,
  y
};"
 > (compile '(provide foo-bar)
            :fcommonjs #t)
 "module.exports = {
  fooBar
};"
 > (compile '(provide (rename-out (x y)))
            :fcommonjs #t)
 "module.exports = {
  x: y
};"
 > (compile '(provide (all-from-out "foo-bar") baz)
            :fcommonjs #t)
 "module.exports = {
  ...fooBar,
  baz
};"

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

 ;; `js/raw`
 > (describe "js/raw")
 _
 > (compile '(js/raw "1"))
 "1"
 > (compile '(js/raw "function I(x) { return x; }"))
 "function I(x) { return x; }"

 ;; `compile`
 > (describe "compile")
 _
 > (compile #t)
 "true;"
 > (compile #t :to 'javascript)
 "true;"
 > (compile #t :from 'roselisp :to 'javascript)
 "true;"
 > (compile '(ann #t Any) :from 'roselisp :to 'typescript)
 "true as any;"
 > (compile "true" :to 'roselisp)
 #t
 > (compile "true" :from 'javascript :to 'roselisp)
 #t
 > (compile "true as any" :from 'typescript :to 'roselisp)
 '(ann #t Any)

 ;; `decompile`
 > (describe "decompile")
 _
 > (decompile "true")
 #t
 > (decompile "true" :from 'javascript)
 #t
 > (decompile "true" :from 'javascript :to 'roselisp)
 #t
 > (decompile "true as any" :from 'typescript)
 '(ann #t Any)
 > (decompile "true as any" :from 'typescript :to 'roselisp)
 '(ann #t Any)

 ;; `license`
 > (describe "license")
 _
 > license
 'MPL-2.0)

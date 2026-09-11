;;; # Roselisp tests
;;;
;;; Tests of procedures and constructs that are specific to Roselisp.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 :describe "#u"
 > #u
 #u
 > '#u
 #u
 > undefined
 #u
 > (compile #u)
 "undefined;"
 > (compile 'undefined)
 "undefined;"

 :describe "#n"
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

 :describe "NaN"
 > NaN
 NaN
 > (nan? NaN)
 #t
 > (nan? 0)
 #f
 > (compile 'NaN)
 "NaN;"
 > (compile '(nan? x))
 "isNaN(x);"

 :describe "true?"
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

 :describe "false?"
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

 :describe "atom?"
 > (atom? #t)
 #t
 > (atom? '())
 #t
 > (atom? '(1 . 2))
 #f
 > (atom? '(1 2 3))
 #f

 :describe "keyword->string"
 > (keyword->string :foo)
 "foo"

 :describe "string->keyword"
 > (string->keyword "foo")
 ':foo

 :describe "keyword->symbol"
 > (keyword->symbol :foo)
 'foo

 :describe "symbol->keyword"
 > (symbol->keyword 'foo)
 ':foo

 :describe "pair-or-list?"
 > (pair-or-list? #t)
 #f
 > (pair-or-list? '())
 #t
 > (pair-or-list? '(1 . 2))
 #t
 > (pair-or-list? '(1 2 3))
 #t
 > (compile '(pair-or-list? x))
 "Array.isArray(x);"

 :describe "vector?"
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

 :describe "Dotted lists"
 > (equal? '(1 2) '(1 . (2 . ())))
 #t

 :describe "dotted-list?"
 > (dotted-list? '())
 #f
 > (dotted-list? '(1 . 2))
 #t
 > (dotted-list? '(1 . (2 . 3)))
 #t
 > (dotted-list? '(foo . bar))
 #t
 > (dotted-list? '(foo bar))
 #f
 > (compile '(dotted-list? x))
 "Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.'));"

 :describe "dotted-pair?"
 > (dotted-pair? '())
 #f
 > (dotted-pair? '(1 . 2))
 #t
 > (dotted-pair? '(1 . (2 . 3)))
 #t
 > (dotted-pair? '(1 2 . 3))
 #f
 > (dotted-pair? '(foo . bar))
 #t
 > (dotted-pair? '(foo bar))
 #f
 > (compile '(dotted-pair? x))
 "Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'));"

 :describe "dotted-proper-list?"
 > (dotted-proper-list? '())
 #f
 > (dotted-proper-list? '(1 . 2))
 #f
 > (dotted-proper-list? '(1 . ()))
 #t
 > (dotted-proper-list? '(1 . (2 . ())))
 #t
 > (dotted-proper-list? '(1 . (2 . 3)))
 #f
 > (dotted-proper-list? '(foo . bar))
 #f
 > (dotted-proper-list? '(foo bar))
 #f

 :describe "dotted-improper-list?"
 > (dotted-improper-list? '())
 #f
 > (dotted-improper-list? '(1 . 2))
 #t
 > (dotted-improper-list? '(1 . ()))
 #f
 > (dotted-improper-list? '(1 . (2 . ())))
 #f
 > (dotted-improper-list? '(1 . (2 . 3)))
 #t
 > (dotted-improper-list? '(foo . bar))
 #t
 > (dotted-improper-list? '(foo bar))
 #f

 :describe "dotted-list-head"
 > (dotted-list-head '(foo . bar))
 '(foo)
 > (dotted-list-head '(foo bar . baz))
 '(foo bar)
 > (compile '(dotted-list-head x))
 "x.slice(0, -2);"

 :describe "dotted-list-tail"
 > (dotted-list-tail '(foo . bar))
 'bar
 > (dotted-list-tail '(foo bar . baz))
 'baz
 > (compile '(dotted-list-tail x))
 "x[x.length - 1];"

 :describe "dotted-list-parse"
 > (dotted-list-parse '(foo . bar))
 (values '(foo) 'bar)
 > (dotted-list-parse '(foo bar . baz))
 (values '(foo bar) 'baz)

 :describe "dotted-list-length"
 > (dotted-list-length '())
 0
 > (dotted-list-length '(1 . ()))
 1
 > (dotted-list-length '(1 . (2 . ())))
 2

 :describe "dotted-list-ref"
 > (dotted-list-ref '(1 . ()) 0)
 1
 > (dotted-list-ref '(1 . (2 . ())) 1)
 2
 > (dotted-list-ref '(1 2 . ()) 1)
 2
 > (dotted-list-ref '(1 2 . (3 . 4)) 2)
 3

 :describe "dotted-list-set"
 > (dotted-list-set '(1 . ()) 0 2)
 '(2 . ())
 > (dotted-list-set '(1 . (2 . ())) 1 3)
 '(1 . (3 . ()))
 > (dotted-list-set '((1 . 2) . (3 . ())) 0 0 4)
 '((4 . 2) . (3 . ()))
 > (dotted-list-set '(1 2 . (3 . ())) 1 4)
 '(1 4 . (3 . ()))

 :describe "dotted-list-set!"
 > (let ((lst '(1 . ())))
     (dotted-list-set! lst 0 2)
     lst)
 '(2 . ())
 > (let ((lst '(1 . (2 . ()))))
     (dotted-list-set! lst 1 3)
     lst)
 '(1 . (3 . ()))
 > (let ((lst '((1 . 2) . (3 . ()))))
     (dotted-list-set! lst 0 0 4)
     lst)
 '((4 . 2) . (3 . ()))
 > (let ((lst '(1 2 . (3 . ()))))
     (dotted-list-set! lst 1 4)
     lst)
 '(1 4 . (3 . ()))

 :describe "dotted-list-first"
 > (dotted-list-first '(1 . ()))
 1
 > (dotted-list-first '(1 . (2 . ())))
 1
 > (dotted-list-first '(1 2 . ()))
 1

 :describe "dotted-list-second"
 > (dotted-list-second '(1 . (2 . ())))
 2
 > (dotted-list-second '(1 2 . ()))
 2

 :describe "dotted-list-third"
 > (dotted-list-third '(1 . (2 . (3 . ()))))
 3
 > (dotted-list-third '(1 2 3 . ()))
 3

 :describe "dotted-list-fourth"
 > (dotted-list-fourth '(1 . (2 . (3 . (4 . ())))))
 4
 > (dotted-list-fourth '(1 2 3 4 . ()))
 4

 :describe "dotted-list-fifth"
 > (dotted-list-fifth '(1 . (2 . (3 . (4 . (5 . ()))))))
 5
 > (dotted-list-fifth '(1 2 3 4 5 . ()))
 5

 :describe "dotted-list-sixth"
 > (dotted-list-sixth '(1 . (2 . (3 . (4 . (5 . (6 . ())))))))
 6
 > (dotted-list-sixth '(1 2 3 4 5 6 . ()))
 6

 :describe "dotted-list-seventh"
 > (dotted-list-seventh '(1 . (2 . (3 . (4 . (5 . (6 . (7 . ()))))))))
 7
 > (dotted-list-seventh '(1 2 3 4 5 6 7 . ()))
 7

 :describe "dotted-list-eighth"
 > (dotted-list-eighth '(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . ())))))))))
 8
 > (dotted-list-eighth '(1 2 3 4 5 6 7 8 . ()))
 8

 :describe "dotted-list-ninth"
 > (dotted-list-ninth '(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . ()))))))))))
 9
 > (dotted-list-ninth '(1 2 3 4 5 6 7 8 9 . ()))
 9

 :describe "dotted-list-tenth"
 > (dotted-list-tenth '(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (10 . ())))))))))))
 10
 > (dotted-list-tenth '(1 2 3 4 5 6 7 8 9 10 . ()))
 10

 :describe "dotted-list-last"
 > (dotted-list-last '())
 #u
 > (dotted-list-last '(1 . ()))
 1
 > (dotted-list-last '(1 . (2 . ())))
 2

 :describe "dotted-list-last-cdr"
 > (dotted-list-last-cdr '())
 '()
 > (dotted-list-last-cdr '(1 . ()))
 '()
 > (dotted-list-last-cdr '(1 . (2 . ())))
 '()

 :describe "dotted-list->proper-list"
 > (dotted-list->proper-list '(foo . bar))
 '(foo bar)
 > (dotted-list->proper-list '(foo bar . baz))
 '(foo bar baz)

 :describe "proper-list?"
 > (proper-list? '(foo bar))
 #t
 > (proper-list? '(foo . bar))
 #f

 :describe "circular-list?"
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

 :describe "proper-list->dotted-list"
 > (proper-list->dotted-list '(foo bar))
 '(foo . bar)
 > (proper-list->dotted-list '(foo bar baz))
 '(foo bar . baz)

 :describe "define-macro"
 > ((lambda ()
      (define-macro (my-macro x)
        x)
      (my-macro 1)))
 1
 > (compile '(define-macro (my-macro env &rest body)
               `(begin
                  ,env
                  ,@body)))
 "function myMacro(exp, env1) {
  let [env, ...body] = exp.slice(1);
  return [Symbol.for('begin'), env, ...body];
}

myMacro.ftype = 'macro';"
 > (compile '(define-macro (foo (x #u))
               x))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  if (x === undefined) {
    x = undefined;
  }
  return x;
}

foo.ftype = 'macro';"
 > (compile '(define-macro (foo &optional x)
               x))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  if (x === undefined) {
    x = undefined;
  }
  return x;
}

foo.ftype = 'macro';"
 > (compile '(define-macro (my-macro exp &rest body)
               `(begin
                  ,exp
                  ,@body)))
 "function myMacro(exp1, env) {
  let [exp, ...body] = exp1.slice(1);
  return [Symbol.for('begin'), exp, ...body];
}

myMacro.ftype = 'macro';"

 :describe "define-fexpr"
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

 :describe "define-inline"
 > (compile '(define-inline (my-plus x y)
               (+ x y)))
 "function myPlus(x, y) {
  return x + y;
}

myPlus.compilerMacro = (() => {
  let f = function (exp, env) {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();"

 :describe "define-subst"
 > (compile '(define-subst (my-plus x y)
               (+ x y)))
 "function myPlus(x, y) {
  return x + y;
}

myPlus.compilerMacro = (() => {
  let f = function (exp, env) {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();"

 :describe "syntax-macro"
 > (compile '(syntax-macro (x y)
                           `(+ ,x ,y)))
 "let f = function (x, y) {
  return [Symbol.for('+'), x, y];
};

f.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

f;"

 :describe "declare"
 > (compile '(define (foo x)
               (declare (ftype "macro"))
               x))
 "function foo(x) {
  return x;
}

foo.ftype = 'macro';"
 > (compile '(lambda (x)
               (declare (ftype "macro"))
               x))
 "let f = function (x) {
  return x;
};

f.ftype = 'macro';

f;"
 > (compile '(begin
               (define (my-plus x y)
                 (+ x y 0))
               (declare my-plus
                        (compiler-macro
                         (macro (x y)
                           `(+ ,x ,y))))))
 "function myPlus(x, y) {
  return x + y + 0;
}

myPlus.compilerMacro = (() => {
  let f = function (exp, env) {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();"

 :describe "let-fields"
 > (compile '(let-fields (((prop) obj))
                         prop))
 "let {prop} = obj;

prop;"

 :describe "arity"
 > (arity (lambda () 1))
 0
 > (arity (lambda (x) x))
 1
 > (arity (lambda (x y) x))
 2
 > (compile '(arity f))
 "f.length;"

 :describe "break"
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

 :describe "continue"
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

 :describe "return"
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

 :describe "yield"
 > (compile '(yield))
 "yield;"
 > (compile '(yield 0))
 "yield 0;"

 :describe "throw"
 > (compile '(throw (new Error "An error")))
 "throw new Error('An error');"

 :describe "await"
 > (compile '(await (foo)))
 "await foo();"

 :describe "async"
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
            :to "typescript")
 "async function foo(x: any): Promise<any> {
  return x;
}"
 > (compile '(define/async (foo x)
               x))
 "async function foo(x) {
  return x;
}"

 :describe "oget"
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

 :describe "oset!"
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

 :describe "Dot"
 > (compile '(. map get "foo"))
 "map.get('foo');"
 > (compile '(.get map "foo"))
 "map.get('foo');"
 > (compile '(.-length arr))
 "arr.length;"

 :describe "new"
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

 :describe "new/apply"
 > (compile '(new/apply Foo args))
 "new Foo(...args);"

 :describe "class"
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

 :describe "define-class"
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
            :to "typescript")
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
            :to "typescript")
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
            :to "typescript")
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
            :to "typescript")
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
            :to "typescript")
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

 :describe "this"
 > (compile '(lambda (this)
               #u))
 "function () {
  return undefined;
};"
 > (compile '(lambda (this)
               #u)
            :to "typescript")
 "function (this: any): any {
  return undefined;
};"
 > (compile '(lambda (this arg)
               arg))
 "function (arg) {
  return arg;
};"
 > (compile '(lambda (this arg)
               arg)
            :to "typescript")
 "function (this: any, arg: any): any {
  return arg;
};"
 > (compile '(lambda (this . args)
               args))
 "function (...args) {
  return args;
};"
 > (compile '(lambda (this . args)
               args)
            :to "typescript")
 "function (this: any, ...args: any[]): any {
  return args;
};"

 :describe "instance-of?"
 > (instance-of? (new Map) Map)
 #t
 > (compile '(instance-of? x Foo))
 "x instanceof Foo;"

 :describe "plist->alist"
 > (plist->alist '())
 '()
 > (plist->alist '(foo bar))
 '((foo . bar))
 > (plist->alist '(foo bar baz quux))
 '((foo . bar) (baz . quux))

 :describe "plist->object"
 > (plist->object '())
 (js/obj)
 > (plist->object '(foo bar))
 (js/obj "foo" 'bar)
 > (plist->object '(foo bar baz quux))
 (js/obj "foo" 'bar
         "baz" 'quux)

 :describe "define-fields"
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
    :to "typescript")
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
    :to "typescript")
 "function foo(): any {
  let obj: any = {};
  let {rest: r, x} = obj;
  return [r, x];
}"

 :describe "set!-fields"
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

 :describe "Map"
 > (new Map)
 (new Map)
 > (~> (new Map '((1 2)))
       (send _ entries)
       (send Array from _))
 '((1 2))

 :describe "member?"
 > (member? 2 '(1 2 3 4))
 #t
 > (member? 9 '(1 2 3 4))
 #f
 > (compile '(member? 2 (list 1 2 3 4) f))
 "[1, 2, 3, 4].findIndex(function (x) {
  return f(2, x);
}) >= 0;"
 > (compile '(member? (+ 1 1) (list 1 2 3 4) f))
 "let v = 1 + 1;

[1, 2, 3, 4].findIndex(function (x) {
  return f(v, x);
}) >= 0;"
 > (compile '(member? (+ 1 1) (list 1 2 3 4) (memoize f)))
 "let v = 1 + 1;

let isEqual = memoize(f);

[1, 2, 3, 4].findIndex(function (x) {
  return isEqual(v, x);
}) >= 0;"

 :describe "memq?"
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

 :describe "as~>"
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

 :describe "ann"
 > (ann #u Any)
 #u
 > (compile '(ann #t Any))
 "true;"
 > (compile '(ann #t Any) :to "typescript")
 "true as any;"
 > (compile '(ann 1 Number)
            :to "javascript")
 "1;"
 > (compile '(ann 1 Number)
            :to "typescript")
 "1 as number;"
 > (compile '(ann (list) Any)
            :to "typescript")
 "[] as any;"
 > (compile '(ann '() Any)
            :to "typescript")
 "[] as any;"
 > (compile '(ann x (List Any))
            :to "typescript")
 "x as [any];"
 > (compile '(ann x (List Number Any))
            :to "typescript")
 "x as [number, any];"
 > (compile '(ann x NN)
            :to "typescript")
 "x as NN;"
 > (compile '(ann x (NN Any))
            :to "typescript")
 "x as NN<any>;"
 > (compile '(ann x (NN Any Any))
            :to "typescript")
 "x as NN<any,any>;"
 > (compile '((ann (lambda (x) x) Any) 1)
            :to "typescript")
 "(function (x: any): any {
  return x;
} as any)(1);"
 > (compile '(lambda (x) (ann (send x foo) Any))
            :to "typescript")
 "function (x: any): any {
  return x.foo() as any;
};"

 :describe ":"
 > (compile '(begin
               (: x Any)
               (define x 1))
            :to "javascript")
 "let x = 1;"
 > (compile '(begin
               (: x Any)
               (define x 1))
            :to "typescript")
 "let x: any = 1;"
 > (compile '(begin
               (: x String)
               (define x "1"))
            :to "typescript")
 "let x: string = '1';"
 > (compile '(begin
               (: x Number)
               (define x 1))
            :to "typescript")
 "let x: number = 1;"
 > (compile '(begin
               (: x Integer)
               (define x 1))
            :to "typescript")
 "let x: number = 1;"
 > (compile '(begin
               (: x Natural)
               (define x 1))
            :to "typescript")
 "let x: number = 1;"
 > (compile '(begin
               (: x Real)
               (define x 1))
            :to "typescript")
 "let x: number = 1;"
 > (compile '(begin
               (: x Symbol)
               (define x 'x))
            :to "typescript")
 "let x: Symbol = Symbol.for('x');"
 > (compile '(begin
               (: x Boolean)
               (define x #t))
            :to "typescript")
 "let x: boolean = true;"
 > (compile '(begin
               (: x True)
               (define x #t))
            :to "typescript")
 "let x: true = true;"
 > (compile '(begin
               (: x False)
               (define x #f))
            :to "typescript")
 "let x: false = false;"
 > (compile '(begin
               (: x (U Number String))
               (define x 1))
            :to "typescript")
 "let x: number | string = 1;"
 > (compile '(begin
               (: x (U Number String Boolean))
               (define x 1))
            :to "typescript")
 "let x: number | string | boolean = 1;"
 > (compile '(begin
               (: x (U Number (U String Boolean)))
               (define x 1))
            :to "typescript")
 "let x: number | (string | boolean) = 1;"
 > (compile '(begin
               (: x (Listof Number))
               (define x (list 1)))
            :to "typescript")
 "let x: number[] = [1];"
 > (compile '(begin
               (: x (Pairof Number))
               (define x '(1 . 2)))
            :to "typescript")
 "let x: (number | Symbol)[] = [1, Symbol.for('.'), 2];"
 > (compile '(begin
               (: hello-world (-> Void))
               (define (hello-world)
                 (display "Hello world!")))
            :to "javascript")
 "function helloWorld() {
  console.log('Hello world!');
}"
 > (compile '(begin
               (: hello-world (-> Void))
               (define (hello-world)
                 (display "Hello world!")))
            :to "typescript")
 "function helloWorld(): void {
  console.log('Hello world!');
}"
 > (compile '(begin
               (: f (-> Number Number))
               (define (f x)
                 x))
            :to "typescript")
 "function f(x: number): number {
  return x;
}"
 > (compile '(begin
               (: f (-> Number Number))
               (define f
                 (lambda (x)
                   x)))
            :to "typescript")
 "let f: (a: number) => number = function (x: any): any {
  return x;
};"
 > (compile '(begin
               (: f (-> Number Number))
               (define f
                 (foo
                  (lambda (x)
                    x))))
            :to "typescript")
 "let f: (a: number) => number = foo(function (x: any): any {
  return x;
});"
 > (compile '(begin
               (: f (-> Number Number Number))
               (define (f x (y 1))
                 x))
            :to "typescript")
 "function f(x: number, y: number = 1): number {
  return x;
}"
 > (compile '(begin
               (: f (->* (Number) (Number) Number))
               (define f
                 (lambda (x (y 1))
                   x)))
            :to "typescript")
 "let f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {
  return x;
};"
 > (compile '(begin
               (: f (-> Any * Any))
               (define f
                 (lambda x
                   x)))
            :to "typescript")
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (-> :rest Any Any))
               (define f
                 (lambda x
                   x)))
            :to "typescript")
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (->* :rest Any Any))
               (define f
                 (lambda x
                   x)))
            :to "typescript")
 "let f: (...a: any) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: f (->* :rest (Listof Any) Any))
               (define f
                 (lambda x
                   x)))
            :to "typescript")
 "let f: (...a: any[]) => any = function (...x: any[]): any {
  return x;
};"
 > (compile '(begin
               (: x Foo)
               (define x
                 (new Foo)))
            :to "typescript")
 "let x: Foo = new Foo();"
 > (compile '(define f
               (lambda ((x : Number))
                 x))
            :to "typescript")
 "let f: any = function (x: number): any {
  return x;
};"
 > (compile '(define f
               (js/arrow ((x : Number))
                 x))
            :to "typescript")
 "let f: any = (x: number): any => {
  return x;
};"
 > (compile '(define (f (x : Number))
               x)
            :to "typescript")
 "function f(x: number): any {
  return x;
}"
 > (compile '(define (f (x : Number) . args)
               x)
            :to "typescript")
 "function f(x: number, ...args: any[]): any {
  return x;
}"
 > (compile '(define (id (x : Number)) : Number
               x)
            :to "typescript")
 "function id(x: number): number {
  return x;
}"
 > (compile '(define (f (x : Number 1)) : Number
               x)
            :to "typescript")
 "function f(x: number = 1): number {
  return x;
}"
 > (compile '(define (f (options : Any (js/obj))) : Any
               x)
            :to "typescript")
 "function f(options: any = {}): any {
  return x;
}"
 > (compile '(define Foo
               (class object%
                 (define/public x)
                 (define (constructor (x : Number))
                   (set-field! x this x))))
            :to "typescript")
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
            :to "typescript")
 "class Foo {
  x: any;

  constructor(x: number, ...args: any[]) {
    this.x = x;
  }
}"

 :describe "define-type"
 > (compile '(define-type NN (-> Number Number))
            :to "javascript")
 ""
 > (compile '(define-type NN (-> Number Number))
            :to "typescript")
 "type NN = (a: number) => number;"
 > (compile '(begin
               (define-type NN (-> Number Number))
               (: f NN)
               (define f
                 (lambda (x)
                   x)))
            :to "javascript")
 "let f = function (x) {
  return x;
};"
 > (compile '(begin
               (define-type NN (-> Number Number))
               (: f NN)
               (define f
                 (lambda (x)
                   x)))
            :to "typescript")
 "type NN = (a: number) => number;

let f: NN = function (x: any): any {
  return x;
};"

 :describe "require"
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

 :describe "provide"
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

 :describe "compile"
 > (compile #t)
 "true;"
 > (compile #t :to "javascript")
 "true;"
 > (compile #t :from 'roselisp :to "javascript")
 "true;"
 > (compile '(ann #t Any) :from "roselisp" :to "typescript")
 "true as any;"
 > (compile "true" :to "roselisp")
 #t
 > (compile "true" :from "javascript" :to "roselisp")
 #t
 > (compile "true as any" :from "typescript" :to "roselisp")
 '(ann #t Any)

 :describe "decompile"
 > (decompile "true")
 #t
 > (decompile "true" :from "javascript")
 #t
 > (decompile "true" :from "javascript" :to "roselisp")
 #t
 > (decompile "true as any" :from "typescript")
 '(ann #t Any)
 > (decompile "true as any" :from "typescript" :to "roselisp")
 '(ann #t Any)

 :describe "license"
 > license
 'MPL-2.0)

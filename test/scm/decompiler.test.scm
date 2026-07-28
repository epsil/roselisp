(require (only-in "../../src/ts/combinators"
                  I))
(require (only-in "../../src/ts/language"
                  decompile))
(require (only-in "../../src/ts/printer"
                  write-to-string))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `decompile`
 > (describe "decompile")
 _
 > (decompile "true;")
 #t
 > (decompile "false")
 #f
 > (decompile "const foo = undefined;")
 '(define foo undefined)
 > (decompile "const foo = null;")
 '(define foo js/null)
 > (decompile "const foo = this;")
 '(define foo this)
 > (decompile "0")
 0
 > (decompile "1")
 1
 > (decompile "-1")
 -1
 > (decompile "'foo'")
 "foo"
 > (decompile "const foo = `bar`;")
 '(define foo "bar")
 > (decompile "const foo = bar`baz`;")
 '(define foo
    (js/tag bar "baz"))
 > (decompile "const foo = `bar
\\`baz`;")
 '(define foo
    "bar
`baz")
 > (decompile "/foo/")
 '(js/regexp "foo")
 > (decompile "/foo/g")
 '(js/regexp "foo" "g")
 > (decompile "const exp = /.*/;")
 '(define exp
    (js/regexp ".*"))
 > (decompile "const exp = /.*/;")
 '(define exp
    (js/regexp ".*"))
 > (decompile "[]")
 '(list)
 > (decompile "const foo = [];")
 '(define foo
    (list))
 > (decompile "[1, 2, 3]")
 '(list 1 2 3)
 > (decompile "[...x]")
 '(append x)
 > (decompile "[x, ...y]")
 '(append (list x) y)
 > (decompile "x[0]")
 '(aget x 0)
 > (decompile "x[0](a, b)")
 '((aget x 0) a b)
 > (decompile "x[0][1]")
 '(aget x 0 1)
 > (decompile "x[len]")
 '(oget x len)
 > (decompile "x[len - 1]")
 '(oget x (- len 1))
 > (decompile "x[0] = 1")
 '(aset! x 0 1)
 > (decompile "x['foo'] = 1")
 '(oset! x "foo" 1)
 > (decompile "!foo")
 '(not foo)
 > (decompile "1 + 2")
 '(+ 1 2)
 > (decompile "1 + 2 + 3")
 '(+ 1 2 3)
 > (decompile "1 + ''")
 '(string-append 1 "")
 > (decompile "1 + 2 + ''")
 '(string-append 1 2 "")
 > (decompile "-1")
 -1
 > (decompile "-(1)")
 -1
 > (decompile "-x")
 '(- x)
 > (decompile "1 - 2")
 '(- 1 2)
 > (decompile "1 - 2 - 3")
 '(- 1 2 3)
 > (decompile "1 - (2 - 3)")
 '(- 1 (- 2 3))
 > (decompile "1 * 2")
 '(* 1 2)
 > (decompile "1 * 2 * 3")
 '(* 1 2 3)
 > (decompile "1 / 2")
 '(/ 1 2)
 xit> (decompile "1 / 2 / 3")
 '(/ 1 2 3)
 > (decompile "x && y")
 '(and x y)
 > (decompile "x && y && z")
 '(and x y z)
 > (decompile "typeof x === 'number' && typeof y === 'number'")
 '(and (eq? (type-of x) "number")
       (eq? (type-of y) "number"))
 > (decompile "x || y")
 '(or x y)
 > (decompile "x || y || z")
 '(or x y z)
 > (decompile "x === y")
 '(eq? x y)
 > (decompile "x !== y")
 '(not (eq? x y))
 > (decompile "x in y")
 '(js/in x y)
 > (decompile "x instanceof y")
 '(is-a? x y)
 > (decompile "typeof x")
 '(type-of x)
 > (decompile "foo(bar);")
 '(foo bar)
 > (decompile "foo(bar);")
 '(foo bar)
 > (decompile "foo(bar);"
              :module #t)
 '(module m scheme
    (foo bar))
 > (decompile "foo(bar);"
              :module #t)
 '(module m scheme
    (foo bar))
 > (decompile "foo('bar');")
 '(foo "bar")
 > (decompile "foo('bar');")
 '(foo "bar")
 > (decompile "foo('bar', 'baz');")
 '(foo "bar" "baz")
 > (decompile "foo('bar', 'baz');")
 '(foo "bar" "baz")
 > (decompile "foo(1, 2, 3);")
 '(foo 1 2 3)
 > (decompile "foo(...args);")
 '(apply foo args)
 > (decompile "foo(x, ...args);")
 '(apply foo x args)
 > (decompile "foo(...args, x);")
 '(apply foo (append args (list x)))
 > (decompile "foo(x, ...args, y);")
 '(apply foo (append (list x) args (list y)))
 xit> (decompile "// comment
foo(bar);")
 '(foo bar)
 > (decompile "x = 1;")
 '(set! x 1)
 > (decompile "x += 1;")
 '(set! x (+ x 1))
 > (decompile "let x = 1;")
 '(define x 1)
 > (decompile "let x = undefined;")
 '(define x undefined)
 > (decompile "let x;")
 '(define x)
 > (decompile "let x = 1, y = 2;")
 '(begin
    (define x 1)
    (define y 2))
 > (decompile "let x = 1, y = 2;
let z = 3;")
 '(begin
    (define x 1)
    (define y 2)
    (define z 3))
 > (decompile "const x = 1")
 '(define x 1)
 > (decompile "let [x] = arr;")
 '(define-values (x)
    arr)
 > (decompile "let [x, y] = arr;")
 '(define-values (x y)
    arr)
 > (decompile "[x, y] = arr;")
 '(set!-values (x y) arr)
 > (decompile "let [x, ...y] = arr;")
 '(define-values (x . y)
    arr)
 > (decompile "let [, y] = arr;")
 '(define-values (_ y)
    arr)
 > (decompile "let {x, y} = obj;")
 '(define-fields (x y)
    obj)
 > (decompile "({x, y} = obj);")
 '(set!-fields (x y) obj)
 > (decompile "let {x: y, z} = obj;")
 '(define-fields ((x y) z)
    obj)
 > (decompile "x.y;")
 '(get-field y x)
 > (decompile "x?.y;")
 '(js/?. x y)
 > (decompile "x?.y();")
 '((js/?. x y))
 > (decompile "x?.y(z);")
 '((js/?. x y) z)
 > (decompile "foo()?.y;")
 '(js/?. (foo) y)
 > (decompile "x?.[0];")
 '(aget (js/?. x) 0)
 > (decompile "x?.[y];")
 '(oget (js/?. x) y)
 > (decompile "x.y = z;")
 '(set-field! y x z)
 > (decompile "x.y();")
 '(send x y)
 > (decompile "x?.y();")
 '((js/?. x y))
 > (decompile "x()?.y();")
 '((js/?. (x) y))
 > (decompile "x.y(z);")
 '(send x y z)
 > (decompile "x.y(...z);")
 '(send/apply x y z)
 > (decompile "function I(x) {
  return x;}")
 '(define (I x)
    x)
 > (decompile "function I(x) {
  foo();
  return x;
}")
 '(define (I x)
    (foo)
    x)
 > (decompile "function I(x: any, y?: any) {
  return x;
}"
              :from 'typescript)
 '(define (I (x : Any) (y undefined))
    x)
 > (decompile "function I(x: any, y: any = true) {
  return x;
}"
              :from 'typescript)
 '(define (I (x : Any) (y : Any #t))
    x)
 > (decompile "function I(x: number, y: number = 1) {
  return x;
}"
              :from 'typescript)
 '(define (I (x : Number) (y : Number 1))
    x)
 > (decompile "function foo(x = 1) {
  return x;
}")
 '(define (foo (x 1))
    x)
 > (decompile "function foo(...args) {
  return args;
}")
 '(define (foo . args)
    args)
 > (decompile "function I(x) {
  if (x) {
    return x;
  } else {
    return false;
  }
}")
 '(define (I x)
    (if x
        x
        #f))
 > (decompile "function I(x) {
  if (x) {
    return x;
  } else if (false) {
    return false;
  } else {
    return false;
  }
}")
 '(define (I x)
    (cond
     (x
      x)
     (#f
      #f)
     (else
      #f)))
 > (decompile "let I = function (x) {
  return x;
};")
 '(define I
    (lambda (x)
      x))
 > (decompile "let foo = function (...args) {
  return args;
};")
 '(define foo
    (lambda args
      args))
 > (decompile "let I = (x) => {
  return x;
};")
 '(define I
    (js/arrow (x)
      x))
 > (decompile "let I = (x: any) => {
  return x;
};"
              :from 'typescript)
 '(define I
    (js/arrow ((x : Any))
      x))
 > (decompile "if (true) {
  foo('bar');
}")
 '(when #t
    (foo "bar"))
 > (decompile "if (!foo) {
  bar('baz');
}")
 '(unless foo
    (bar "baz"))
 > (decompile "if (true) {
  foo('bar');
} else {
  bar('baz');
}")
 '(if #t
      (foo "bar")
      (bar "baz"))
 > (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  bar('baz');
}")
 '(if x
      (when y
        (foo "bar"))
      (bar "baz"))
 > (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  }
}")
 '(cond
   (x
    (when y
      (foo "bar")))
   (z
    (bar "baz")))
 > (decompile "if (x) {
  foo();
  bar();
} else if (y) {
  baz();
  quux();
}")
 '(cond
   (x
    (foo)
    (bar))
   (y
    (baz)
    (quux)))
 > (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  } else {
    baz('quux');
  }
}")
 '(cond
   (x
    (when y
      (foo "bar")))
   (z
    (bar "baz"))
   (else
    (baz "quux")))
 > (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (!z) {
    bar('baz');
  } else {
    baz('quux');
  }
}")
 '(cond
   (x
    (when y
      (foo "bar")))
   ((not z)
    (bar "baz"))
   (else
    (baz "quux")))
 > (decompile "if (x) {
  foo('bar');
} else if (y) {
  bar('baz');
} else {
  baz('quux');
}")
 '(cond
   (x
    (foo "bar"))
   (y
    (bar "baz"))
   (else
    (baz "quux")))
 > (decompile "if (x) {
  foo();
} else {
  bar();
  baz();
}")
 '(cond
   (x
    (foo))
   (else
    (bar)
    (baz)))
 > (decompile "let x = true ? foo : bar;")
 '(define x
    (if #t
        foo
        bar))
 > (decompile "let x = 1 ? foo : 2 ? bar : baz")
 '(define x
    (cond
     (1
      foo)
     (2
      bar)
     (else
      baz)))
 > (decompile "while (foo) {
  bar();}"
              )
 '(do ()
      ((not foo))
    (bar))
 > (decompile "do {
  bar();
} while (foo);")
 '(js/do-while ((bar))
               foo)
 > (decompile "do {
  bar();
  baz();
} while (foo);")
 '(js/do-while ((bar)
                (baz))
               foo)
 > (decompile "for (let i = 0; i < 10; i++) {
  foo();
  break;
}")
 '(for ((i (range 0 10)))
    (foo)
    (break))
 > (decompile "for (i = 0; i < arr.length; i++) {
  foo();
}")
 '(for ((i (range 0 (js/length arr))))
    (foo))
 > (decompile "for (let i = 10; i > 0; i--) {
  foo();
}")
 '(for ((i (range 10 0 -1)))
    (foo))
 > (decompile "for (let i = 0, j = 0; i < 10; i++, j++) {
  foo();
  break bar;
}")
 '(do ((i 0 (+ i 1))
       (j 0 (+ j 1)))
      ((not (< i 10)))
    (foo)
    (break bar))
 > (decompile "for (let x of foo) {
  bar();
  continue;
}")
 '(for ((x foo))
    (bar)
    (continue))
 > (decompile "for (const [name, value] of entries) {
  result.insert(value, [name]);
}")
 '(for ((x entries))
    (define-values (name value)
      x)
    (send result insert value (list name)))
 > (decompile "for (const [name, value] of x) {
  result.insert(value, [name]);
}")
 '(for ((x1 x))
    (define-values (name value)
      x1)
    (send result insert value (list name)))
 > (decompile "for (let x in foo) {
  bar();
}")
 '(for ((x (js/keys foo)))
    (bar))
 > (decompile "new Foo();")
 '(new Foo)
 > (decompile "new Foo('bar');")
 '(new Foo "bar")
 > (decompile "new Foo(...args);")
 '(apply new Foo args)
 > (decompile "new Foo(x, ...args);")
 '(apply new Foo x args)
 > (decompile "delete x")
 '(js/delete x)
 > (decompile "throw new Error('An error')")
 '(throw
   (new Error
        "An error"))
 xit> (decompile "try {
}")
 '(try)
 > (decompile "try {
  x = 2 / 1;
} finally {
  foo();
}")
 '(try
    (set! x (/ 2 1))
    (finally
      (foo)))
 > (decompile "try {
  x = 2 / 1;
} catch {
  foo();
}")
 '(try
    (set! x (/ 2 1))
    (catch Object _
      (foo)))
 > (decompile "try {
  x = 2 / 1;
} catch (e) {
  foo();
} finally {
  bar();
}")
 '(try
    (set! x (/ 2 1))
    (catch Object e
      (foo))
    (finally
      (bar)))
 > (decompile "const I = async function (x) {
  return x;
};")
 '(define I
    (async
     (lambda (x)
       x)))
 > (decompile "async function I(x) {
  return x;
}"
              :module #t)
 '(module m scheme
    (define I
      (async
       (lambda (x)
         x))))
 > (decompile "import 'foo';")
 '(require "foo")
 > (decompile "import * as foo from 'bar';")
 '(require foo "bar")
 > (decompile "import { foo } from 'bar';")
 '(require (only-in "bar"
                    foo))
 > (decompile "import { foo as bar } from 'baz';")
 '(require (only-in "baz"
                    (foo bar)))
 > (decompile "export {};")
 '(provide)
 > (decompile "export {
  foo
};")
 '(provide
    foo)
 > (decompile "export {
  foo as bar
};")
 '(provide
    (rename-out (foo bar)))
 > (decompile "export * from 'foo';")
 '(provide (all-from-out "foo"))
 > (decompile "const foo = {};")
 '(define foo
    (js/obj))
 > (decompile "const foo = { bar: true };")
 '(define foo
    (js/obj "bar" #t))
 > (decompile "const foo = { ...{ bar: true } };")
 '(define foo
    (js/obj-append (js/obj "bar" #t)))
 > (decompile "class Foo {
}")
 '(define-class Foo ())
 > (decompile "class Foo extends Bar {
}")
 '(define-class Foo (Bar))
 > (decompile "class Foo {
  bar = 1;
}")
 '(define-class Foo ()
    (define/public bar 1))
 > (decompile "class Foo {
  bar() {
    return 1;
  }
}")
 '(define-class Foo ()
    (define/public (bar)
      1))
 > (decompile "class Foo {
  bar;

  constructor() {
    this.bar = 1;
  }
}")
 '(define-class Foo ()
    (define/public bar)
    (define (constructor)
      (set-field! bar this 1)))
 > (decompile "class Foo extends Bar {
  constructor() {
    super();
  }
}")
 '(define-class Foo (Bar)
    (define (constructor)
      (super)))
 > (decompile "class Foo extends Bar {
  constructor(x) {
    super(x);
  }
}")
 '(define-class Foo (Bar)
    (define (constructor x)
      (super x)))
 > (decompile "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *generator() {
    for (let x of this.arr) {
      yield x;
    }
  }
}")
 '(define-class Foo ()
    (define/public arr)
    (define (constructor arr)
      (set-field! arr this arr))
    (define/generator (generator)
      (for ((x (get-field arr this)))
        (yield x))))
 > (decompile "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *[Symbol.iterator]() {
    for (let x of this.arr) {
      yield x;
    }
  }
}")
 '(define-class Foo ()
    (define/public arr)
    (define (constructor arr)
      (set-field! arr this arr))
    (define/generator ((get-field iterator Symbol))
      (for ((x (get-field arr this)))
        (yield x))))
 > (decompile "foo as any"
              :from 'typescript)
 '(ann foo Any)
 > (decompile "foo as number"
              :from 'typescript)
 '(ann foo Number)
 > (decompile "foo as boolean"
              :from 'typescript)
 '(ann foo Boolean)
 > (decompile "foo as true"
              :from 'typescript)
 '(ann foo True)
 > (decompile "foo as MyClass"
              :from 'typescript)
 '(ann foo MyClass)
 > (decompile "foo as MyClass<Any>"
              :from 'typescript')
 '(ann foo (MyClass Any))
 > (decompile "foo as any[]"
              :from 'typescript)
 '(ann foo (Listof Any))
 > (decompile "foo as [any]"
              :from 'typescript)
 '(ann foo (List Any))
 > (decompile "foo as [number, any]"
              :from 'typescript)
 '(ann foo (List Number Any))
 > (decompile "foo as number | string"
              :from 'typescript)
 '(ann foo (U Number String))
 > (decompile "foo as string | undefined"
              :from 'typescript)
 '(ann foo (U String Undefined))
 > (decompile "foo as (a: any) => void"
              :from 'typescript)
 '(ann foo (-> Any Void))
 > (decompile "type NN = number;"
              :from 'typescript)
 '(define-type NN Number))

;;; # JavaScript
;;;
;;; Tests of JavaScript constructs.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 :describe "js/\[\]"
 > (compile '(js/\[\] x y))
 "x[y];"

 :describe "js/var"
 > (compile '(js/var x))
 "var x;"
 > (compile '(js/var x 0))
 "var x = 0;"
 > (compile '(js/var x 0 y 0))
 "var x = 0, y = 0;"

 :describe "js/let"
 > (compile '(js/let x))
 "let x;"
 > (compile '(js/let x 0))
 "let x = 0;"
 > (compile '(js/let x 0 y 0))
 "let x = 0, y = 0;"

 :describe "js/const"
 > (compile '(js/const x))
 "const x;"
 > (compile '(js/const x 0))
 "const x = 0;"
 > (compile '(js/const x 0 y 0))
 "const x = 0, y = 0;"

 :describe "js/function"
 > (compile '(js/function ()
               0))
 "function () {
  return 0;
};"
 > (compile '(js/function () : Number
                          0)
            :to "typescript")
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
            :to "typescript")
 "function foo(): number {
  return 0;
}"

 :describe "js/arrow"
 > (compile '(js/arrow ()
               0))
 "() => {
  return 0;
};"
 > (compile '(js/arrow () : Number
                       0)
            :to "typescript")
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
            :to "typescript")
 "let foo: any = (): number => {
  return 0;
};"

 :describe "js/arrow?"
 > (js/arrow? (js/arrow (x) x))
 #t
 > (js/arrow? (lambda (x) x))
 #f

 :describe "js/=>"
 > (compile '(js/=> () 0))
 "() => {
  return 0;
};"

 :describe "js/iife"
 > (compile '(js/iife (js/arrow (x y)
                        (+ x y))
                      (list 1 2))
            :as "expression")
 "((x, y) => {
  return x + y;
})(1, 2)"
 > (compile '(js/iife (js/arrow (x . y)
                        (+ x (first y)))
                      (list* a b))
            :as "expression")
 "((x, ...y) => {
  return x + y[0];
})(a, ...b)"
 > (compile '(js/iife (js/arrow (x y)
                        (+ x y))
                      (list 1 2))
            :as "statement")
 "let x = 1;

let y = 2;

x + y;"
 > (compile '(js/iife (js/arrow (x . y)
                        (+ x (first y)))
                      (list a b c))
            :as "statement")
 "let y = [b, c];

a + y[0];"
 > (compile '(js/iife (js/arrow (x y)
                        (+ x y))
                      (list 1 2))
            :as "return")
 "let x = 1;

let y = 2;

return x + y;"

 :describe "js/\(\)"
 > (compile '(js/\(\) x))
 "x();"
 > (compile '(js/\(\) x y))
 "x(y);"
 > (compile '(js/\(\) x y z))
 "x(y, z);"

 :describe "js/="
 > (compile '(js/= x y))
 "x = y;"
 > (compile '(js/= (aget x i) y))
 "x[i] = y;"
 > (compile '(js/= (list-ref x i) y))
 "x[i] = y;"
 > (compile '(js/= '(x y) z))
 "[x, y] = z;"
 > (compile '(js/= '((x) y) z))
 "[[x], y] = z;"
 > (compile '(js/= '(x . y) z))
 "[x, ...y] = z;"
 > (compile '(module m scheme
               (js/= '(length) x)))
 "[length] = x;"
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
 > (compile '(js/= (list-set! x i) y))
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

 :describe "js/\,"
 > (compile '(js/\, x))
 "x;"
 > (compile '(js/\, x y))
 "x, y;"
 > (compile '(js/\, x y z))
 "x, y, z;"

 :describe "js/\;"
 > (compile '(js/\; x))
 "x;"
 > (compile '(js/\; x y))
 "x;

y;"
 > (compile '(js/\; x y z))
 "x;

y;

z;"

 :describe "js/block"
 > (compile '(js/block x))
 "{
  x;
}"
 > (compile '(js/block x y))
 "{
  x;
  y;
}"
 > (compile '(js/block x y z))
 "{
  x;
  y;
  z;
}"

 :describe "js/\{\}"
 > (compile '(js/\{\} x))
 "{
  x;
}"
 > (compile '(js/\{\} x y))
 "{
  x;
  y;
}"
 > (compile '(js/\{\} x y z))
 "{
  x;
  y;
  z;
}"

 :describe "js/?"
 > (compile '(js/? x y))
 "x ? y : undefined;"
 > (compile '(js/? x y z))
 "x ? y : z;"
 > (compile '(js/? x y (js/? z w)))
 "x ? y : (z ? w : undefined);"
 > (compile '(js/? x y z)
            :as "statement")
 "x ? y : z;"
 > (compile '(js/? x y z)
            :as "return")
 "return x ? y : z;"
 > (compile '(js/? x y z)
            :as "expression")
 "x ? y : z"

 :describe "js/if"
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
            :as "statement")
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(js/if x y z)
            :as "return")
 "if (x) {
  return y;
} else {
  return z;
}"
 it> (compile '(js/if x y z)
              :as "expression")
 "(() => {
  if (x) {
    return y;
  } else {
    return z;
  }
})()"

 :describe "js/switch"
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
    :as "return")
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
    :as "return")
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
    :as "expression")
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

 :describe "js/!"
 > (js/! #f)
 #t
 > (js/! #t)
 #f
 > (compile '(js/! x))
 "!x;"

 :describe "js/&&"
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

 :describe "js/\|\|"
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

 :describe "js/op"
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

 :describe "js/while"
 > (compile '(js/while (< (length result) 3)
               (display result)))
 "while (result.length < 3) {
  console.log(result);
}"
 > (compile '(js/while (begin
                         (set! x (- x 1))
                         (> x 0))
               (display x)))
 "while (x--, x > 0) {
  console.log(x);
}"

 :describe "js/do-while"
 > (compile '(js/do-while ((display result))
                          (< (length result) 3)))
 "do {
  console.log(result);
} while (result.length < 3);"
 > (compile '(js/do-while ((foo)
                           (display result))
                          (< (length result) 3)))
 "do {
  foo();
  console.log(result);
} while (result.length < 3);"

 :describe "js/for"
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
 > (compile '(js/for ((js/define i 0 j 0)
                      (and (< i 10) (< j 10))
                      (begin (set! i (+ i 1)) (set! j (+ j 1))))
                     (foo)))
 "for (let i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"
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

 :describe "js/for-in"
 > (compile '(js/for-in ((i obj))
                        (foo)))
 "for (let i in obj) {
  foo();
}"

 :describe "js/for-of"
 > (compile '(js/for-of ((i lst))
                        (foo)))
 "for (let i of lst) {
  foo();
}"

 :describe "js/."
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

 :describe "js/?."
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

 :describe "js/obj"
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
            :as "expression")
 "{}"
 > (compile '(js/obj "foo" "bar")
            :as "expression")
 "{
  foo: 'bar'
}"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as "expression")
 "{
  foo: 1,
  bar: 2
}"
 > (compile '(js/obj)
            :as "return")
 "return {};"
 > (compile '(js/obj "foo" "bar")
            :as "return")
 "return {
  foo: 'bar'
};"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as "return")
 "return {
  foo: 1,
  bar: 2
};"

 :describe "js/obj?"
 > (compile '(js/obj? x))
 "(x !== null) && (typeof x === 'object');"

 :describe "js/obj-append"
 > (compile '(js/obj-append
              obj
              (js/obj "foo" "bar")))
 "({
  ...obj,
  foo: 'bar'
});"

 :describe "js/keys"
 > (js/keys (js/obj))
 '()
 > (js/keys (js/obj "foo" "bar"))
 '("foo")
 > (js/keys (js/obj "foo" "bar"
                    "baz" "quux"))
 '("foo" "baz")
 > (compile '(js/keys x))
 "Object.keys(x);"

 :describe "js/in"
 > (let ((obj (js/obj "foo" "bar")))
     (js/in "foo" obj))
 #t
 > (compile '(js/in "foo" obj))
 "'foo' in obj;"

 :describe "js/delete"
 > (compile '(js/delete x))
 "delete x;"

 :describe "js/try"
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

 :describe "js/+"
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

 :describe "js/-"
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

 :describe "js/*"
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

 :describe "js//"
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

 :describe "js/<"
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

 :describe "js/<="
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

 :describe "js/>"
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

 :describe "js/>="
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

 :describe "js/%"
 > (compile '(js/% x y))
 "x % y;"

 :describe "js/abs"
 > (compile '(js/abs x))
 "Math.abs(x);"

 :describe "js/tag"
 > (compile '(js/tag foo "bar"))
 "foo`bar`;"

 :describe "js/rename"
 > (compile '(js/rename ((x y))
                        x))
 "y;"

 :describe "js/statement-or-expression"
 > (compile '(js/statement-or-expression
              :statement 1
              :expression 2)
            :as "statement")
 "1;"
 > (compile '(js/statement-or-expression
              :statement 1
              :expression 2)
            :as "expression")
 "2"
 > (compile '(js/statement-or-expression
              :statement 1
              :expression 2
              :return 3)
            :as "return")
 "return 3;"
 > (compile '(js/statement-or-expression
              :statement 1
              :expression 2)
            :as "return")
 "return 2;"
 > (compile '(js/statement-or-expression
              :statement 1)
            :as "return")
 "return 1;"

 :describe "js/raw"
 > (compile '(js/raw "1"))
 "1"
 > (compile '(js/raw "function I(x) { return x; }"))
 "function I(x) { return x; }")

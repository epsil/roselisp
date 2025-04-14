(require chai "chai")
(require (only-in "../../src/ts/combinators"
                  I))
(require (only-in "../../src/ts/decompiler"
                  decompile))
(require (only-in "../../src/ts/printer"
                  write-to-string))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal))

(describe "decompile"
  (fn ()
    (describe "boolean values"
      (fn ()
        (it "true"
            (fn ()
              (define actual
                (decompile "true;"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t)))
              (assert-equal
               actual
               (js/tag sexp "#t"))))
        (it "false"
            (fn ()
              (assert-equal
               (decompile "false"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '#f)))))
    (describe "undefined"
      (fn ()
        (it "undefined"
            (fn ()
              (assert-equal
               (decompile "const foo = undefined;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo undefined))))))
    (describe "null"
      (fn ()
        (it "null"
            (fn ()
              (assert-equal
               (decompile "const foo = null;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo js/null))))))
    (describe "this"
      (fn ()
        (it "this"
            (fn ()
              (assert-equal
               (decompile "const foo = this;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo this))))))
    (describe "numbers"
      (fn ()
        (it "0"
            (fn ()
              (assert-equal
               (decompile "0"
                          (js-obj "language" "JavaScript"))
               "0")))
        (it "1"
            (fn ()
              (assert-equal
               (decompile "1"
                          (js-obj "language" "JavaScript"))
               "1")))
        (it "-1"
            (fn ()
              (assert-equal
               (decompile "-1"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               -1)))))
    (describe "strings"
      (fn ()
        (it "'foo'"
            (fn ()
              (assert-equal
               (decompile "'foo'"
                          (js-obj "language" "JavaScript"))
               "\"foo\"")))
        (it "'foo'"
            (fn ()
              (assert-equal
               (decompile "const foo = `bar`;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo "bar"))))
        (it "bar`baz`"
            (fn ()
              (assert-equal
               (decompile "const foo = bar`baz`;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo
                  (js/tag bar "baz")))))
        (it "'foo'"
            (fn ()
              (assert-equal
               (decompile "const foo = `bar
\\`baz`;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo
                  "bar
`baz"))))))
    (describe "regexps"
      (fn ()
        (it "/foo/"
            (fn ()
              (assert-equal
               (decompile "/foo/"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(js/regexp "foo"))))
        (it "/foo/g"
            (fn ()
              (assert-equal
               (decompile "/foo/g"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(js/regexp "foo" "g"))))
        (it "const exp = /.*/;, JS"
            (fn ()
              (assert-equal
               (decompile "const exp = /.*/;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define exp
                  (js/regexp ".*")))))
        (it "const exp = /.*/;, TS"
            (fn ()
              (assert-equal
               (decompile "const exp = /.*/;"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(define exp
                  (js/regexp ".*")))))))
    (describe "arrays"
      (fn ()
        (it "[]"
            (fn ()
              (assert-equal
               (decompile "[]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(list))))
        (it "const foo = [];"
            (fn ()
              (assert-equal
               (decompile "const foo = [];"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo
                  (list)))))
        (it "[1, 2, 3]"
            (fn ()
              (assert-equal
               (decompile "[1, 2, 3]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(list 1 2 3))))
        (it "[...x]"
            (fn ()
              (assert-equal
               (decompile "[...x]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(append x))))
        (it "[x, ...y]"
            (fn ()
              (assert-equal
               (decompile "[x, ...y]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(append (list x) y))))
        (it "x[0]"
            (fn ()
              (assert-equal
               (decompile "x[0]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(aget x 0))))
        (it "x[0](a, b)"
            (fn ()
              (assert-equal
               (decompile "x[0](a, b)"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '((aget x 0) a b))))
        (it "x[0][1]"
            (fn ()
              (assert-equal
               (decompile "x[0][1]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(aget x 0 1))))
        (it "x[len]"
            (fn ()
              (assert-equal
               (decompile "x[len]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(oget x len))))
        (it "x[len - 1]"
            (fn ()
              (assert-equal
               (decompile "x[len - 1]"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(oget x (- len 1)))))
        (it "x[0] = 1"
            (fn ()
              (assert-equal
               (decompile "x[0] = 1"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(aset! x 0 1))))
        (it "x['foo'] = 1"
            (fn ()
              (assert-equal
               (decompile "x['foo'] = 1"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(oset! x "foo" 1))))))
    (describe "!"
      (fn ()
        (it "!foo"
            (fn ()
              (assert-equal
               (decompile "!foo"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(not foo))))))
    (describe "+"
      (fn ()
        (it "1 + 2"
            (fn ()
              (assert-equal
               (decompile "1 + 2"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(+ 1 2))))
        (it "1 + 2 + 3"
            (fn ()
              (assert-equal
               (decompile "1 + 2 + 3"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(+ 1 2 3))))
        (it "1 + ''"
            (fn ()
              (assert-equal
               (decompile "1 + ''"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(string-append 1 ""))))
        (it "1 + 2 + ''"
            (fn ()
              (assert-equal
               (decompile "1 + 2 + ''"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(string-append 1 2 ""))))))
    (describe "-"
      (fn ()
        (it "-1"
            (fn ()
              (assert-equal
               (decompile "-1"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               -1)))
        (it "-(1)"
            (fn ()
              (assert-equal
               (decompile "-(1)"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               -1)))
        (it "-x"
            (fn ()
              (assert-equal
               (decompile "-x"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(- x))))
        (it "1 - 2"
            (fn ()
              (assert-equal
               (decompile "1 - 2"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(- 1 2))))
        (it "1 - 2 - 3"
            (fn ()
              (assert-equal
               (decompile "1 - 2 - 3"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(- 1 2 3))))
        (it "1 - (2 - 3)"
            (fn ()
              (assert-equal
               (decompile "1 - (2 - 3)"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(- 1 (- 2 3)))))))
    (describe "*"
      (fn ()
        (it "1 * 2"
            (fn ()
              (assert-equal
               (decompile "1 * 2"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(* 1 2))))
        (it "1 * 2 * 3"
            (fn ()
              (assert-equal
               (decompile "1 * 2 * 3"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(* 1 2 3))))))
    (describe "/"
      (fn ()
        (it "1 / 2"
            (fn ()
              (assert-equal
               (decompile "1 / 2"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(/ 1 2))))
        (xit "1 / 2 / 3"
             (fn ()
               (assert-equal
                (decompile "1 / 2 / 3"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t))
                '(/ 1 2 3))))))
    (describe "&&"
      (fn ()
        (it "x && y"
            (fn ()
              (assert-equal
               (decompile "x && y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(and x y))))
        (it "x && y && z"
            (fn ()
              (assert-equal
               (decompile "x && y && z"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(and x y z))))
        (it "typeof x === 'number' && typeof y === 'number'"
            (fn ()
              (assert-equal
               (decompile "typeof x === 'number' && typeof y === 'number'"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(and (eq? (type-of x) "number")
                     (eq? (type-of y) "number")))))))
    (describe "||"
      (fn ()
        (it "x || y"
            (fn ()
              (assert-equal
               (decompile "x || y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(or x y))))
        (it "x || y || z"
            (fn ()
              (assert-equal
               (decompile "x || y || z"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(or x y z))))))
    (describe "==="
      (fn ()
        (it "x === y"
            (fn ()
              (assert-equal
               (decompile "x === y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(eq? x y))))))
    (describe "!=="
      (fn ()
        (it "x !== y"
            (fn ()
              (assert-equal
               (decompile "x !== y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(not (eq? x y)))))))
    (describe "in"
      (fn ()
        (it "x in y"
            (fn ()
              (assert-equal
               (decompile "x in y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(js/in x y))))))
    (describe "instanceof"
      (fn ()
        (it "x instanceof y"
            (fn ()
              (assert-equal
               (decompile "x instanceof y"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(is-a? x y))))))
    (describe "typeof"
      (fn ()
        (it "typeof x"
            (fn ()
              (assert-equal
               (decompile "typeof x"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(type-of x))))))
    (describe "function call"
      (fn ()
        (it "foo(bar);, JS"
            (fn ()
              (assert-equal
               (decompile "foo(bar);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(foo bar))))
        (it "foo(bar);, TS"
            (fn ()
              (assert-equal
               (decompile "foo(bar);"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(foo bar))))
        (it "foo(bar);, JS, module"
            (fn ()
              (assert-equal
               (decompile "foo(bar);"
                          (js-obj "language" "JavaScript"
                                  "module" #t
                                  "sexp" #t))
               '(module m scheme
                  (foo bar)))))
        (it "foo(bar);, TS, module"
            (fn ()
              (assert-equal
               (decompile "foo(bar);"
                          (js-obj "language" "TypeScript"
                                  "module" #t
                                  "sexp" #t))
               '(module m scheme
                  (foo bar)))))
        (it "foo('bar');, JS"
            (fn ()
              (assert-equal
               (decompile "foo('bar');"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(foo "bar"))))
        (it "foo('bar');, TS"
            (fn ()
              (assert-equal
               (decompile "foo('bar');"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(foo "bar"))))
        (it "foo('bar', 'baz');, JS"
            (fn ()
              (assert-equal
               (decompile "foo('bar', 'baz');"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(foo "bar" "baz"))))
        (it "foo('bar', 'baz');, TS"
            (fn ()
              (assert-equal
               (decompile "foo('bar', 'baz');"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(foo "bar" "baz"))))
        (it "foo(1, 2, 3);, JS"
            (fn ()
              (assert-equal
               (decompile "foo(1, 2, 3);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(foo 1 2 3))))
        (it "foo(...args);"
            (fn ()
              (assert-equal
               (decompile "foo(...args);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply foo args))))
        (it "foo(x, ...args);"
            (fn ()
              (assert-equal
               (decompile "foo(x, ...args);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply foo x args))))
        (it "foo(...args, x);"
            (fn ()
              (assert-equal
               (decompile "foo(...args, x);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply foo (append args (list x))))))
        (it "foo(x, ...args, y);"
            (fn ()
              (assert-equal
               (decompile "foo(x, ...args, y);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply foo (append (list x) args (list y))))))
        (xit "// comment
foo(bar);, TS"
             (fn ()
               (assert-equal
                (decompile "// comment
foo(bar);"
                           (js-obj "language" "TypeScript"
                                   "sexp" #t))
                '(foo bar))))))
    (describe "assignment"
      (fn ()
        (it "x = 1;"
            (fn ()
              (assert-equal
               (decompile "x = 1;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(set! x 1))))
        (it "x += 1;"
            (fn ()
              (assert-equal
               (decompile "x += 1;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(set! x (+ x 1)))))))
    (describe "variables"
      (fn ()
        (it "let x = 1;"
            (fn ()
              (assert-equal
               (decompile "let x = 1;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x 1))))
        (it "let x = undefined;"
            (fn ()
              (assert-equal
               (decompile "let x = undefined;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x undefined))))
        (it "let x;"
            (fn ()
              (assert-equal
               (decompile "let x;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x))))
        (it "let x = 1, y = 2;"
            (fn ()
              (assert-equal
               (decompile "let x = 1, y = 2;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(begin
                  (define x 1)
                  (define y 2)))))
        (it "let x = 1, y = 2; let z = 3;"
            (fn ()
              (assert-equal
               (decompile "let x = 1, y = 2;
let z = 3;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(begin
                  (define x 1)
                  (define y 2)
                  (define z 3)))))
        (it "const x = 1;"
            (fn ()
              (assert-equal
               (decompile "const x = 1"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x 1))))
        (it "let [x] = arr;"
            (fn ()
              (assert-equal
               (decompile "let [x] = arr;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-values (x)
                  arr))))
        (it "let [x, y] = arr;"
            (fn ()
              (assert-equal
               (decompile "let [x, y] = arr;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-values (x y)
                  arr))))
        (it "[x, y] = arr;"
            (fn ()
              (assert-equal
               (decompile "[x, y] = arr;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(set!-values (x y) arr))))
        (it "let [x, ...y] = arr;"
            (fn ()
              (assert-equal
               (decompile "let [x, ...y] = arr;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-values (x . y)
                  arr))))
        (it "let [, y] = arr;"
            (fn ()
              (assert-equal
               (decompile "let [, y] = arr;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-values (_ y)
                  arr))))
        (it "let {x, y} = obj;"
            (fn ()
              (assert-equal
               (decompile "let {x, y} = obj;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-js-obj (x y)
                  obj))))
        (it "({x, y} = obj);"
            (fn ()
              (assert-equal
               (decompile "({x, y} = obj);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(set!-js-obj (x y) obj))))
        (it "let {x: y, z} = obj;"
            (fn ()
              (assert-equal
               (decompile "let {x: y, z} = obj;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-js-obj ((x y) z)
                  obj))))))
    (describe "fields"
      (fn ()
        (it "x.y;"
            (fn ()
              (assert-equal
               (decompile "x.y;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(get-field y x))))
        (it "x?.y;"
            (fn ()
              (assert-equal
               (decompile "x?.y;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(and (field-bound? y x)
                     (get-field y x)))))
        (it "foo()?.y;"
            (fn ()
              (assert-equal
               (decompile "foo()?.y;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(~> (foo)
                    (and (field-bound? y _)
                         (get-field y _))))))
        (it "x.y = z;"
            (fn ()
              (assert-equal
               (decompile "x.y = z;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(set-field! y x z))))))
    (describe "methods"
      (fn ()
        (it "x.y();"
            (fn ()
              (assert-equal
               (decompile "x.y();"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(send x y))))
        (it "x?.y();"
            (fn ()
              (assert-equal
               (decompile "x?.y();"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(and (field-bound? y x)
                     (send x y)))))
        (it "foo()?.y();"
            (fn ()
              (assert-equal
               (decompile "foo()?.y();"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(~> (foo)
                    (and (field-bound? y _)
                         (send _ y))))))
        (it "x.y(z);"
            (fn ()
              (assert-equal
               (decompile "x.y(z);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(send x y z))))
        (it "x.y(...z);"
            (fn ()
              (assert-equal
               (decompile "x.y(...z);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(send/apply x y z))))))
    (describe "function definitions"
      (fn ()
        (it "function I(x) { return x; }"
            (fn ()
              (assert-equal
               (decompile "function I(x) {
  return x;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (I x)
                  x))))
        (it "function I(x) { foo(); return x; }"
            (fn ()
              (assert-equal
               (decompile "function I(x) {
  foo();
  return x;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (I x)
                  (foo)
                  x))))
        (it "function I(x: any, y?: any) { return x; }"
            (fn ()
              (assert-equal
               (decompile "function I(x: any, y?: any) {
  return x;
}"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(define (I (x : Any) (y undefined))
                  x))))
        (it "function I(x: any, y: any = true) { return x; }"
            (fn ()
              (assert-equal
               (decompile "function I(x: any, y: any = true) {
  return x;
}"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(define (I (x : Any) (y : Any #t))
                  x))))
        (it "function I(x: number, y: number = 1) { return x; }"
            (fn ()
              (assert-equal
               (decompile "function I(x: number, y: number = 1) {
  return x;
}"
                          (js-obj "language" "TypeScript"
                                  "sexp" #t))
               '(define (I (x : Number) (y : Number 1))
                  x))))
        (it "function foo(x = 1) { x; }"
            (fn ()
              (assert-equal
               (decompile "function foo(x = 1) {
  return x;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (foo (x 1))
                  x))))
        (it "function foo(...args) { return args; }"
            (fn ()
              (assert-equal
               (decompile "function foo(...args) {
  return args;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (foo . args)
                  args))))
        (it "function I(x) { if (x) { return x; } else { return false; } }"
            (fn ()
              (assert-equal
               (decompile "function I(x) {
  if (x) {
    return x;
  } else {
    return false;
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (I x)
                  (if x
                      x
                      #f)))))
        (it "function I(x) { if (x) { return x; } else if (false) { return false; } else { return false; } }"
            (fn ()
              (assert-equal
               (decompile "function I(x) {
  if (x) {
    return x;
  } else if (false) {
    return false;
  } else {
    return false;
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define (I x)
                  (cond
                   (x
                    x)
                   (#f
                    #f)
                   (else
                    #f))))))))
    (describe "function expressions"
      (fn ()
        (it "let I = function (x) { return x; };"
            (fn ()
              (assert-equal
               (decompile "let I = function (x) {
  return x;
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define I
                  (lambda (x)
                    x)))))
        (it "function foo(...args) { return args; }"
            (fn ()
              (assert-equal
               (decompile "let foo = function (...args) {
  return args;
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define foo
                  (lambda args
                    args)))))))
    (describe "arrow functions"
      (fn ()
        (it "let I = function (x) { return x; };"
            (fn ()
              (assert-equal
               (decompile "let I = (x) => {
  return x;
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define I
                  (js/arrow (x)
                    x)))))
        (it "let I = function (x: any) { return x; };"
            (fn ()
              (assert-equal
               (decompile "let I = (x: any) => {
  return x;
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define I
                  (js/arrow ((x : Any))
                    x)))))))
    (describe "if"
      (fn ()
        (it "if (true) { foo('bar'); }"
            (fn ()
              (assert-equal
               (decompile "if (true) {
  foo('bar');
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(when #t
                  (foo "bar")))))
        (it "if (!foo) { bar('baz'); }"
            (fn ()
              (assert-equal
               (decompile "if (!foo) {
  bar('baz');
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(unless foo
                  (bar "baz")))))
        (it "if (true) { foo('bar'); } else { bar('baz'); }"
            (fn ()
              (assert-equal
               (decompile "if (true) {
  foo('bar');
} else {
  bar('baz');
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(if #t
                    (foo "bar")
                    (bar "baz")))))
        (it "if (x) { if (y) { foo('bar'); } } else { bar('baz'); }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  bar('baz');
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(if x
                    (when y
                      (foo "bar"))
                    (bar "baz")))))
        (it "if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (when y
                    (foo "bar")))
                 (z
                  (bar "baz"))))))
        (it "if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  foo();
  bar();
} else if (y) {
  baz();
  quux();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (foo)
                  (bar))
                 (y
                  (baz)
                  (quux))))))
        (it "if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } else { baz('quux'); } }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  } else {
    baz('quux');
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (when y
                    (foo "bar")))
                 (z
                  (bar "baz"))
                 (else
                  (baz "quux"))))))
        (it "if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } else { baz('quux'); } }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (!z) {
    bar('baz');
  } else {
    baz('quux');
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (when y
                    (foo "bar")))
                 ((not z)
                  (bar "baz"))
                 (else
                  (baz "quux"))))))
        (it "if (x) { foo('bar'); } else if (y) { bar('baz'); } else { baz('quux'); }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  foo('bar');
} else if (y) {
  bar('baz');
} else {
  baz('quux');
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (foo "bar"))
                 (y
                  (bar "baz"))
                 (else
                  (baz "quux"))))))
        (it "if (x) { foo(); } else { bar(); baz(); }"
            (fn ()
              (assert-equal
               (decompile "if (x) {
  foo();
} else {
  bar();
  baz();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(cond
                 (x
                  (foo))
                 (else
                  (bar)
                  (baz))))))))
    (describe "?"
      (fn ()
        (it "let x = true ? foo : bar;"
            (fn ()
              (assert-equal
               (decompile "let x = true ? foo : bar;"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x
                  (if #t
                      foo
                      bar)))))
        (it "let x = 1 ? foo : 2 ? bar : baz"
            (fn ()
              (assert-equal
               (decompile "let x = 1 ? foo : 2 ? bar : baz"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define x
                  (cond
                   (1
                    foo)
                   (2
                    bar)
                   (else
                    baz))))))))
    (describe "while"
      (fn ()
        (it "while (foo) { bar(); }"
            (fn ()
              (assert-equal
               (decompile "while (foo) {
  bar();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(do ()
                    ((not foo))
                  (bar)))))))
    (describe "do...while"
      (fn ()
        (it "do { bar(); } while (foo)"
            (fn ()
              (assert-equal
               (decompile "do {
  bar();
} while (foo);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(js/do-while (bar)
                             foo))))))
    (describe "for"
      (fn ()
        (it "for (let i = 0; i < 10; i++) { foo(); }"
            (fn ()
              (assert-equal
               (decompile "for (let i = 0; i < 10; i++) {
  foo();
  break;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((i (range 0 10)))
                  (foo)
                  (break)))))
        (it "for (i = 0; i < arr.length; i++) { foo(); }"
            (fn ()
              (assert-equal
               (decompile "for (i = 0; i < arr.length; i++) {
  foo();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((i (range 0 (get-field length arr))))
                  (foo)))))
        (it "for (let i = 10; i > 0; i--) { foo(); }"
            (fn ()
              (assert-equal
               (decompile "for (let i = 10; i > 0; i--) {
  foo();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((i (range 10 0 -1)))
                  (foo))
               )))
        (it "for (let i = 0, j = 0; i < 10; i++, j++) { foo(); }"
            (fn ()
              (assert-equal
               (decompile "for (let i = 0, j = 0; i < 10; i++, j++) {
  foo();
  break bar;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(do ((i 0 (+ i 1))
                     (j 0 (+ j 1)))
                    ((not (< i 10)))
                  (foo)
                  (break bar)))))))
    (describe "for...of"
      (fn ()
        (it "for (let x of foo) { bar(); }"
            (fn ()
              (assert-equal
               (decompile "for (let x of foo) {
  bar();
  continue;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((x foo))
                  (bar)
                  (continue)))))
        (it "for (const [name, value] of entries) { ... }"
            (fn ()
              (assert-equal
               (decompile "for (const [name, value] of entries) {
  result.insert(value, [name]);
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((x entries))
                  (define-values (name value)
                    x)
                  (send result insert value (list name))))))
        (it "for (const [name, value] of x) { ... }"
            (fn ()
              (assert-equal
               (decompile "for (const [name, value] of x) {
  result.insert(value, [name]);
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((x1 x))
                  (define-values (name value)
                    x1)
                  (send result insert value (list name))))))))
    (describe "for...in"
      (fn ()
        (it "for (let x in foo) { bar(); }"
            (fn ()
              (assert-equal
               (decompile "for (let x in foo) {
  bar();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(for ((x (js-keys foo)))
                  (bar)))))))
    (describe "new"
      (fn ()
        (it "new Foo()"
            (fn ()
              (assert-equal
               (decompile "new Foo();"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(new Foo))))
        (it "new Foo('bar')"
            (fn ()
              (assert-equal
               (decompile "new Foo('bar');"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(new Foo "bar"))))
        (it "new Foo(...args);"
            (fn ()
              (assert-equal
               (decompile "new Foo(...args);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply new Foo args))))
        (it "new Foo(x, ...args);"
            (fn ()
              (assert-equal
               (decompile "new Foo(x, ...args);"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(apply new Foo x args))))))
    (describe "delete"
      (fn ()
        (it "delete x"
            (fn ()
              (assert-equal
               (decompile "delete x"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(js/delete x))))))
    (describe "throw"
      (fn ()
        (it "throw new Error('An error')"
            (fn ()
              (assert-equal
               (decompile "throw new Error('An error')"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(throw
                 (new Error
                      "An error")))))))
    (describe "try"
      (fn ()
        (xit "try {}"
             (fn ()
               (assert-equal
                (decompile "try {
}"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t))
                '(try))))
        (it "try { x = 2 / 1; } finally { foo(); }"
            (fn ()
              (assert-equal
               (decompile "try {
  x = 2 / 1;
} finally {
  foo();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(try
                  (set! x (/ 2 1))
                  (finally
                    (foo))))))
        (it "try { x = 2 / 1; } catch { foo(); }"
            (fn ()
              (assert-equal
               (decompile "try {
  x = 2 / 1;
} catch {
  foo();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(try
                  (set! x (/ 2 1))
                  (catch Object _
                    (foo))))))
        (it "try { x = 2 / 1; } catch (e) { foo(); } finally { bar(); }"
            (fn ()
              (assert-equal
               (decompile "try {
  x = 2 / 1;
} catch (e) {
  foo();
} finally {
  bar();
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(try
                  (set! x (/ 2 1))
                  (catch Object e
                    (foo))
                  (finally
                    (bar))))))))
    (describe "async"
      (fn ()
        (it "const I = async function (x) { return x; };"
            (fn ()
              (assert-equal
               (decompile "const I = async function (x) {
  return x;
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define I
                  (async
                   (lambda (x)
                     x))))))
        (it "async function I(x) { return x; }"
            (fn ()
              (assert-equal
               (decompile "async function I(x) {
  return x;
}"
                          (js-obj "language" "JavaScript"
                                  "module" #t "sexp" #t))
               '(module m scheme
                  (define I
                    (async
                     (lambda (x)
                       x)))))))))
    (describe "import"
      (fn ()
        (it "import 'foo';"
            (fn ()
              (assert-equal
               (decompile "import 'foo';"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(require "foo"))))
        (it "import * as foo from 'bar';"
            (fn ()
              (assert-equal
               (decompile "import * as foo from 'bar';"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(require foo "bar"))))
        (it "import { foo } from 'bar';"
            (fn ()
              (assert-equal
               (decompile "import { foo } from 'bar';"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(require (only-in "bar"
                                  foo)))))
        (it "import { foo as bar } from 'baz';"
            (fn ()
              (assert-equal
               (decompile "import { foo as bar } from 'baz';"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(require (only-in "baz"
                                  (foo bar))))))))
    (describe "export"
      (fn ()
        (it "export {};"
            (fn ()
              (assert-equal
               (decompile "export {};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(provide))))
        (it "export { foo };"
            (fn ()
              (assert-equal
               (decompile "export {
  foo
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(provide
                  foo))))
        (it "export { foo as bar };"
            (fn ()
              (assert-equal
               (decompile "export {
  foo as bar
};"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(provide
                  (rename-out (foo bar))))))
        (it "export * from 'foo';"
            (fn ()
              (assert-equal
               (decompile "export * from 'foo';"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(provide (all-from-out "foo")))))))
    (describe "objects"
      (fn ()
        (it "{}"
            (fn ()
              (define actual
                (decompile "const foo = {};"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t)))
              (assert-equal
               actual
               '(define foo
                  (js-obj)))))
        (it "{ bar: 'baz' }"
            (fn ()
              (define actual
                (decompile "const foo = { bar: true };"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t)))
              (assert-equal
               actual
               '(define foo
                  (js-obj "bar" #t)))))
        (it "{ ...{ bar: true } }"
            (fn ()
              (define actual
                (decompile "const foo = { ...{ bar: true } };"
                           (js-obj "language" "JavaScript"
                                   "sexp" #t)))
              (assert-equal
               actual
               '(define foo
                  (js-obj-append (js-obj "bar" #t))))))))
    (describe "classes"
      (fn ()
        (it "class Foo {}"
            (fn ()
              (assert-equal
               (decompile "class Foo {
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()))))
        (it "class Foo extends Bar {}"
            (fn ()
              (assert-equal
               (decompile "class Foo extends Bar {
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo (Bar)))))
        (it "class Foo { bar = 1; }"
            (fn ()
              (assert-equal
               (decompile "class Foo {
  bar = 1;
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()
                  (define/public bar 1)))))
        (it "class Foo { bar() { return 1; } }"
            (fn ()
              (assert-equal
               (decompile "class Foo {
  bar() {
    return 1;
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()
                  (define/public (bar)
                    1)))))
        (it "class Foo { bar; constructor() { this.bar = 1; } }"
            (fn ()
              (assert-equal
               (decompile "class Foo {
  bar;

  constructor() {
    this.bar = 1;
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()
                  (define/public bar)
                  (define (constructor)
                    (set-field! bar this 1))))))
        (it "class Foo extends Bar { constructor() { super(); } }"
            (fn ()
              (assert-equal
               (decompile "class Foo extends Bar {
  constructor() {
    super();
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo (Bar)
                  (define (constructor)
                    (super))))))
        (it "class Foo extends Bar { constructor(x) { super(x); } }"
            (fn ()
              (assert-equal
               (decompile "class Foo extends Bar {
  constructor(x) {
    super(x);
  }
}"
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo (Bar)
                  (define (constructor x)
                    (super x))))))
        (it "class Foo { *generator() { ... } }"
            (fn ()
              (assert-equal
               (decompile "class Foo {
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
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()
                  (define/public arr)
                  (define (constructor arr)
                    (set-field! arr this arr))
                  (define/generator (generator)
                    (for ((x (get-field arr this)))
                      (yield x)))))))
        (it "class Foo { *[Symbol.iterator]() { ... } }"
            (fn ()
              (assert-equal
               (decompile "class Foo {
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
                          (js-obj "language" "JavaScript"
                                  "sexp" #t))
               '(define-class Foo ()
                  (define/public arr)
                  (define (constructor arr)
                    (set-field! arr this arr))
                  (define/generator ((get-field iterator Symbol))
                    (for ((x (get-field arr this)))
                      (yield x)))))))))
    (describe "TypeScript"
      (fn ()
        (describe "as"
          (fn ()
            (it "foo as any"
                (fn ()
                  (assert-equal
                   (decompile "foo as any"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo Any))))
            (it "foo as number"
                (fn ()
                  (assert-equal
                   (decompile "foo as number"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo Number))))
            (it "foo as boolean"
                (fn ()
                  (assert-equal
                   (decompile "foo as boolean"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo Boolean))))
            (it "foo as false"
                (fn ()
                  (assert-equal
                   (decompile "foo as true"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo True))))
            (it "foo as MyClass"
                (fn ()
                  (assert-equal
                   (decompile "foo as MyClass"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo MyClass))))
            (it "foo as MyClass<Any>"
                (fn ()
                  (assert-equal
                   (decompile "foo as MyClass<Any>"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (MyClass Any)))))
            (it "foo as any[]"
                (fn ()
                  (assert-equal
                   (decompile "foo as any[]"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (Listof Any)))))
            (it "foo as [any]"
                (fn ()
                  (assert-equal
                   (decompile "foo as [any]"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (List Any)))))
            (it "foo as [number, any]"
                (fn ()
                  (assert-equal
                   (decompile "foo as [number, any]"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (List Number Any)))))
            (it "foo as number | string"
                (fn ()
                  (assert-equal
                   (decompile "foo as number | string"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (U Number String)))))
            (it "foo as string | undefined"
                (fn ()
                  (assert-equal
                   (decompile "foo as string | undefined"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (U String Undefined)))))
            (it "foo as (a: any) => void"
                (fn ()
                  (assert-equal
                   (decompile "foo as (a: any) => void"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(ann foo (-> Any Void)))))))
        (describe "type"
          (fn ()
            (it "type NN = number"
                (fn ()
                  (assert-equal
                   (decompile "type NN = number;"
                              (js-obj "language" "TypeScript"
                                      "sexp" #t))
                   '(define-type NN Number))))))))))

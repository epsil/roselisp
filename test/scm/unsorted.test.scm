;;; # Various unsorted tests
;;;
;;; This file functions as an "inbox" for incoming tests.

(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t
 :describe "To do"

 :describe "Dotted lists"
 xit> '(1 . 2)
 '(1 . 2)
 xit> '(1 . ())
 '(1)
 xit> '(1 . (2 . ()))
 '(1 2)
 xit> '(1 . (2 . 3))
 '(1 2 . 3)
 xit> (dotted-list? '(1 . ()))
 #f
 xit> (dotted-list? '(1 . (2 . ())))
 #f
 xit> (dotted-pair? '(1 . ()))
 #f
 xit> (dotted-pair? '(1 . (2 . ())))
 #f
 xit> (compile '(module m scheme
                  '(1 . ()))
               :fdottedlists #t)
 "import {
  normalizeList
} from 'roselisp';

normalizeList([1, Symbol.for('.'), []);"
 xit> (compile '(module m scheme
                  (define (normalize-list x)
                    '(1 . x)))
               :fdottedlists #t)
 "import {
  normalizeList1
} from 'roselisp';

function normalizeList(x) {
  normalizeList1([1, Symbol.for('.'), x);
}"

 :describe "js/iife"
 xit> (compile '(js/iife (js/arrow (x . y)
                           (+ x (first y)))
                         (list* a b))
               :as 'statement)
 "let x = a;

let y = b;

x + y[0];"

 :describe "gensym"
 xit> (compile `(module m scheme
                  (define ,(gensym "length")
                    length)))
 "import {
  length
} from 'roselisp';

let length1 = length;"

 :describe "for-each"
 xit> (compile '(for-each (lambda (x)
                            x)
                          lst))
 "lst.forEach(function (x) {
  return x;
});"

 :describe "interpret"
 xit> (interpret '(length '(1 . ()))
                 :fdottedlists #t)
 1

 :describe "Assignment operators"
 xit> (compile '(js/+= x y))
 "x += y;"
 xit> (compile '(js/-= x y))
 "x -= y;"
 xit> (compile '(js/*= x y))
 "x *= y;"
 xit> (compile '(js//= x y))
 "x /= y;"
 xit> (compile '(js/^= x y))
 "x ^= y;"
 xit> (compile '(js/&= x y))
 "x &= y;"
 xit> (compile '(js/\|= x y))
 "x |= y;"
 xit> (compile '(js/<<= x y))
 "x <<= y;"
 xit> (compile '(js/>>= x y))
 "x >>= y;"
 xit> (compile '(js/>>>= x y))
 "x >>>= y;"

 :describe "define-syntax"
 xit> (compile '(module m scheme
                  (define x 1)
                  (define-syntax (foo x)
                    (syntax
                     (begin
                       (define x 2)
                       x)))
                  (foo)))
 "import {
  datumToSyntax
} from 'roselisp';

let x = 1;

function foo(x) {
  return datumToSyntax(false, [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]);
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

let x1 = 2;

x1;"

 ;; :describe "Other tests"
 ;; xit> (list? '(1 . ()))
 ;; #t
 ;; xit> (list? '(1 . (2 . ())))
 ;; #t
 ;;
 ;; :describe "Cons dot"
 ;; > *cons-dot*
 ;; '.
 ;; > (cons-dot)
 ;; '.
 ;; > (cons-dot? *cons-dot*)
 ;; #t
 ;;
 ;; :describe "array-list?"
 ;; > (array-list? '())
 ;; #t
 ;; > (array-list? '(1 . 2))
 ;; #t
 ;; > (array-list? '(1 2))
 ;; #t
 ;; > (array-list? '(1 2 3))
 ;; #t
 ;;
 ;; :describe "array-list-length"
 ;; > (compile '(array-list-length x))
 ;; "x.length;"
 ;; xit> (compile "\\t")
 ;; "	;"
 ;; xit> (compile 'js-undefined)
 ;; "undefined;"
 ;; xit> (compile 'js-null)
 ;; "null;"
 ;;  xit> (compile '(let ((a 1))
 ;;                   (+ (let ((a 2)) a) a)))
 ;;  "let a = 1;
 ;;
 ;; (() => {
 ;;   let a = 2;
 ;;   return a;
 ;; })() + a;"
 ;;  xit> (compile '(lambda (this arg)
 ;;                arg))
 ;;  "function (arg) {
 ;;   return arg;
 ;; };"
 ;;  xit> (compile '(lambda (this . args)
 ;;                args))
 ;;  "function (...args) {
 ;;   return args;
 ;; };"
 ;;  xit> (compile '(lambda (this arg)
 ;;                arg))
 ;;  "function (arg) {
 ;;   return arg;
 ;; };"
 ;;  xit> (compile '(lambda (this . args)
 ;;                args))
 ;;  "function (...args) {
 ;;   return args;
 ;; };"
 ;;  xit> (compile '(apply send obj method args))
 ;;  "obj.method(...args);"
 ;;  xit> (compile '(for ((i (range 0 10))
 ;;                       (j (range 0 10)))
 ;;                   (foo)))
 ;;  "for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
 ;;   foo();
 ;; }"
 ;;  xit> (compile '(do ((*do-result* (display result)))
 ;;                     ((not (< (array-list-length result) 3)))))
 ;;  "do {
 ;;   console.log(result);
 ;; } while (result.length < 3);"
 ;;  xit> (compile '(get-field (- len 1) arr))
 ;;  "arr[len - 1];"
 ;;  xit> (compile '(module m lisp
 ;;                   (define (I x) x)
 ;;                   (define x 1)
 ;;                   (define *lisp-map* #t)))
 ;;  "function I(x) {
 ;;   return x;
 ;; }
 ;;
 ;; I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];
 ;;
 ;; let x = 1;"
 ;;  xit> (compile '(module m lisp
 ;;                   (require (only-in "./combinators"
 ;;                                     I))
 ;;                   (define x 1)
 ;;                   (define *lisp-map* #t)))
 ;;  "import {
 ;;   I
 ;; } from './combinators';
 ;;
 ;; let x = 1;"
 ;;  xit> (compile '(module m lisp
 ;;                   (require (only-in "./combinators"
 ;;                                     I))
 ;;                   (define x 1)
 ;;                   (define *lisp-map* #t)))
 ;;  "import {
 ;;   I
 ;; } from './combinators';
 ;;
 ;; let x: any = 1;"
 ;;  xit> (compile '(make-hash
 ;;                  (append
 ;;                   `(("foo" . "bar")
 ;;                     ("baz" . "quux"))
 ;;                   (hash->list xyzzy))))
 ;;  "new Map([...[['foo', 'bar'], ['baz', 'quux']], ...xyzzy.entries()]);"
 ;;  xit> (compile '(- (- x)))
 ;;  "x;"
 ;;  xit> (compile '(foldr cons '() '(1 2 3 4)))
 ;;  "[1, 2, 3, 4].reduceRight((function (f) {
 ;;   return function (x, y) {
 ;;     return f(y, x);
 ;;   };
 ;; })(cons), []);"
 ;;  xit> (compile (js/tag sexp "\"\\\\s\""))
 ;;  "'\\\\s';"
 ;;  xit> (compile '(begin
 ;;                   (define-type NN (-> Number Number))
 ;;                   (: f NN)
 ;;                   (define (f x)
 ;;                     x))
 ;;                :to 'typescript)
 ;;  "type NN = (a: number) => number;
 ;;
 ;; function f(x: number): number {
 ;;   return x;
 ;; };"
 ;;  xit> (compile '(last x))
 ;;  "x[x.length - 1];"
 ;;  xit> (compile '(nth 1 x))
 ;;  "x[1];"
 ;;  xit> (compile '(nth 2 (nth 1 x)))
 ;;  "x[1][2];"
 ;;  xit> (compile '(nthcdr 1 x))
 ;;  "x.slice(1);"
 ;;  xit> (compile '(require 'foo "bar"))
 ;;  "import foo from 'bar';"
 ;;  xit> (compile '(require foo :as bar))
 ;;  "import bar from 'foo';"
 ;;  xit> (compile '(require (foo :as bar)))
 ;;  "import bar from 'foo';"
 ;;  xit> (compile '(require ("foo" :as "bar")))
 ;;  "import bar from 'foo';"
 ;; xit> (compile
 ;;       '(lambda (env (options (js/obj)))
 ;;          (let ((language (oget options "language")))
 ;;            (set! language (or language default-language))
 ;;            (let ((compilation-env (or (.get compilation-map
 ;;                                             language)
 ;;                                       javascript-env)))
 ;;              (new CompilationEvaluator
 ;;                   env
 ;;                   compilation-env
 ;;                   options)))))
 ;;  "function (env: any, options: any = {}): any {
 ;;   let language: any = options['language'];
 ;;   language = language || (default-language);
 ;;   {
 ;;     {
 ;;       let compilation-env: any = (compilation-map.get(language)) || (javascript-env);
 ;;       return new CompilationEvaluator(env, compilation-env, options);
 ;;     }
 ;;   }
 ;; }"
 ;;  xit> (compile
 ;;        '(define-values (_ regexp)
 ;;           (rl/sandbox
 ;;            ((js/arrow ()
 ;;               (define __
 ;;                 (js/obj "@@functional/placeholder" #t))
 ;;               (define (js/regexp_ input (flags #u))
 ;;                 (if (eq? (type-of input) "string")
 ;;                     (new RegExp input flags)
 ;;                     input))
 ;;               (values __ js/regexp_)))))
 ;;        :finline-functions #t)
 ;;  "let [, regexp] = (() => {
 ;;   let __ = {
 ;;     '@@functional/placeholder': true
 ;;   };
 ;;   function jsRegexp_(input, flags = undefined) {
 ;;     if (typeof input === 'string') {
 ;;       return new RegExp(input, flags);
 ;;     } else {
 ;;       return input;
 ;;     }
 ;;   }
 ;;   return [__, jsRegexp_];
 ;; })();"
 ;;  xit> (compile '(module m lisp
 ;;                   (define (my-foldl f v l)
 ;;                     (foldl f v l))
 ;;                   (define bar
 ;;                     (my-foldl + 0 '(1 2 3 4))))
 ;;                :finline-functions #t)
 ;;  "let [add] = (function () {
 ;;   function add(...args) {
 ;;     return args.reduce(function (y, x) {
 ;;       return y + x;
 ;;     }, 0);
 ;;   }
 ;;   return [add];
 ;; })();
 ;;
 ;; function myFoldl(f, v, l) {
 ;;   return l.reduce(function (acc, x) {
 ;;     return f(x, acc);
 ;;   }, v);
 ;; }
 ;;
 ;; let bar = myFoldl(add, 0, [1, 2, 3, 4]);"
 ;;  xit> (compile '(module m lisp
 ;;                   (define (my-foldl f v l)
 ;;                     (foldl f v l)))
 ;;                :finline-functions #t)
 ;;  "let [foldl] = (function () {
 ;;   function foldl(f, v, lst) {
 ;;     return lst.reduce(function (acc, x) {
 ;;       return f(x, acc);
 ;;     }, v);
 ;;   }
 ;;   return [foldl];
 ;; })();
 ;;
 ;; function myFoldl(f, v, l) {
 ;;   return foldl(f, v, l);
 ;; }"
 ;;  xit> (compile '(module m lisp
 ;;                   (define (foo f x y)
 ;;                     (f x y))
 ;;                   (define (my-push-4 lst x)
 ;;                     (foo push! lst x)))
 ;;                :finline-functions #t)
 ;;  "let [pushX] = (function () {
 ;;   function pushX(lst, x) {
 ;;     lst.unshift(x);
 ;;     return lst;
 ;;   }
 ;;   return [pushX];
 ;; })();
 ;;
 ;; function foo(f, x, y) {
 ;;   return f(x, y);
 ;; }
 ;;
 ;; function myPush4(lst, x) {
 ;;   return foo(pushX, lst, x);
 ;; }"
 ;;  xit> (compile '(module m lisp
 ;;                   (define (get-push-function)
 ;;                     push!)
 ;;                   (define (my-push-4 lst x)
 ;;                     ((get-push-function) lst x)))
 ;;                :finline-functions #t)
 ;;  "let [pushX] = (function () {
 ;;   function pushX(lst, x) {
 ;;     lst.unshift(x);
 ;;     return lst;
 ;;   }
 ;;   return [pushX];
 ;; })();
 ;;
 ;; function getPushFunction() {
 ;;   return pushX;
 ;; }
 ;;
 ;; function myPush4(lst, x) {
 ;;   return getPushFunction()(lst, x);
 ;; }"
 ;;  xit> '(begin
 ;;          (setq a 1 b 2 c 3)
 ;;          (list a b c))
 ;;  '(1 2 3)
 ;;  xit> (begin
 ;;         (define ((((my-add) x) y) z)
 ;;           (+ x y z))
 ;;         ((my-add _ 2 3) 1))
 ;;  6
 ;;  xit> (begin
 ;;         (define ((((my-add) x) y) z)
 ;;           (+ x y z))
 ;;         ((my-add _ _ _) 1 2 3))
 ;;  6
 ;;  xit> (begin
 ;;         (define ((((my-add) x) y) z)
 ;;           (+ x y z))
 ;;         (((my-add _ _ 3) 1) 2))
 ;;  6
 ;;  xit> (begin
 ;;         (define ((((my-add) x) y) z)
 ;;           (+ x y z))
 ;;         ((my-add _ _ 3) 1 2))
 ;;  6
 ;;  xit> (begin
 ;;         (define ((my-add x) y)
 ;;           (+ x y))
 ;;         ((my-add 2) 3))
 ;;  5
 ;;  xit> (begin
 ;;         (define (((my-add) x) y)
 ;;           (+ x y))
 ;;         (((my-add) 2) 3))
 ;;  5
 ;;  xit> (begin
 ;;         (define ((my-add x y) z)
 ;;           (+ x y z))
 ;;         (my-add 1 2 3))
 ;;  6
 ;;  xit> ((lambda ()
 ;;          (define-macro (foo x)
 ;;            x)
 ;;          (foo '(foo 1))))
 ;;  '(foo 1)
 ;;  xit> ((lambda ()
 ;;          (define-macro (foo x)
 ;;            `(+ ,x ,x))
 ;;          (foo 1)))
 ;;  2
 ;;  xit> ((lambda ()
 ;;          (define-macro my-macro (x)
 ;;            `(begin ,x))
 ;;          (my-macro 1)))
 ;;  1
 ;;  xit> ((lambda ()
 ;;          (defmacro foo (x)
 ;;            x)
 ;;          (foo '(foo 1))))
 ;;  '(foo 1)
 ;;  xit> ((lambda ()
 ;;          (defmacro my-macro (&environment env)
 ;;            (send env has '+))
 ;;          (my-macro)))
 ;;  #t
 ;;  xit> ((lambda ()
 ;;          (defmacro my-macro (&environment env-arg)
 ;;            (send env-arg has '+))
 ;;          (my-macro)))
 ;;  #t
 ;;  xit> ((lambda ()
 ;;          (defmacro (my-macro x)
 ;;            `(begin ,x))
 ;;          (my-macro 1)))
 ;;  1
 ;;  xit> (begin
 ;;         (setq a 1 b 2 c 3)
 ;;         (define f
 ;;           (nlambda (x y z)
 ;;                    (list x y z)))
 ;;         (f a b c))
 ;;  '(a b c)
 ;;  xit> (begin
 ;;         (setq a 1 b 2 c 3)
 ;;         ((nlambda (x y z) (list x y z)) a b c))
 ;;  '(a b c)
 ;;  xit> (if "" 1 2)
 ;;  1
 ;;  xit> (begin
 ;;         (define ((my-curried-unit) x) x)
 ;;         (my-curried-unit '_))
 ;;  '_
 ;;  xit> (let ((result '()))
 ;;         (for ((x '(1 2 3))
 ;;               (y '(4 5 6)))
 ;;           (set! result (cons x result))
 ;;           (set! result (cons y result)))
 ;;         result)
 ;;  '(6 3 5 2 4 1)
 ;;  xit> (send obj 'add 1 1)
 ;;  2
 ;;  xit> (it "(defclass Foo ...) with no arguments"
 ;;           (let (quux)
 ;;             (defclass Foo ()
 ;;               (define x "wobble")
 ;;               (define (constructor x)
 ;;                 (set! (.-x this) x))
 ;;               (define (bar)
 ;;                 (.-x this)))
 ;;             (set! quux (new Foo))
 ;;             (.bar quux)))
 ;;  "wobble"
 ;;  xit> (string-split "  foo bar  baz \r\n\t")
 ;;  '("foo" "bar" "baz")
 ;;  xit> (apply send (make-hash) 'has '("foo"))
 ;;  #f
 ;;  xit> (apply send (make-hash) '(has "foo"))
 ;;  #f
 ;;  xit> ((ann #u Any))
 ;;  #u
 ;;  xit> (interpret 'js/eval #u (js/obj :eval #f))
 ;;  #u
 ;;
 ;; :describe "linked-list?"
 ;; > (linked-list? '())
 ;; #f
 ;; > (linked-list? '(1 . 2))
 ;; #f
 ;; > (linked-list? '(1 . ()))
 ;; #t
 ;; > (linked-list? '(1 . (2 . ())))
 ;; #t
 ;; > (linked-list? '(1 2 . (3 . ())))
 ;; #t
 ;;
 ;; :describe "linked-list-link?"
 ;; > (linked-list-link? '())
 ;; #f
 ;; > (linked-list-link? '(1 . 2))
 ;; #t
 ;; > (linked-list-link? '(1 . ()))
 ;; #t
 ;; > (linked-list-link? '(1 . (2 . ())))
 ;; #t
 ;; > (linked-list-link? '(1 2 . (3 . ())))
 ;; #t
 )

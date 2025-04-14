# Interpreter

Roselisp provides an interpreter. The interpreter can be loaded as a Node library, or interacted with through Roselisp's {ref}`REPL <repl>`.

(repl)=
## REPL

Roselisp provides a simple language shell in the form of a REPL (read--eval--print loop). To interact with the REPL, invoke `roselisp` with zero arguments:

```console
$ roselisp
```

This will start a shell asking the user for input:

```
;; Roselisp version 0.0.1.
;; Type ,h for help and ,q to quit.
>
```

For example, to calculate 1 + 1, enter the expression `(+ 1 1)`:

```
> (+ 1 1)
2
```

For help, type `,h`. To quit the REPL, type `,q`.

## Interpreting files

Roselisp can be used to execute Scheme programs. To begin, we will make a simple ["hello, world"](http://en.wikipedia.org/wiki/%22Hello,_World!%22_program) program in the style of [Kernighan and Ritchie](http://en.wikipedia.org/wiki/The_C_Programming_Language). Create the following file named `hello.scm`:

```scheme
(define (main)
  (display "hello, world")
  0)

(main)
```

We can run this file by passing it to `roselisp`:

```console
$ roselisp hello.scm
hello, world
```

## Scripts

We can also create self-executing Roselisp scripts by adding a [shebang directive](http://en.wikipedia.org/wiki/Shebang_%28Unix%29):

```scheme
#!/usr/bin/env roselisp
(define (main)
  (display "hello, world")
  0)

(main)
```

Then set the file permissions to executable:

```console
$ chmod +x hello.scm
```

It is now possible to run `hello.scm` as a script:

```console
$ ./hello.scm
hello, world
```

## Library

Roselisp can be used as a Node library, and is designed to make it easy to evaluate Lisp code in a JavaScript context. Roselisp S-expressions are made out of JavaScript arrays, with JavaScript's `Symbol` data type being used to represent Lisp symbols. Thus, the S-expression `(+ 1 1)` would in JavaScript be represented as:

```javascript
const exp = [Symbol.for('+'), 1, 1]; // (+ 1 1)
```

To simplify the task of creating symbols, Roselisp provides a shorthand, `s`, which is a special function that supports JavaScript's template string syntax:

```javascript
import {s} from 'roselisp';

const exp = [s`+`, 1, 1]; // (+ 1 1)
```

Alternatively, we can write the same expression as an S-expression by using the `sexp` function:

```javascript
import {sexp} from 'roselisp';

const exp = sexp`(+ 1 1)`; // [s`+`, 1, 1]
```

To evaluate this expression, pass it to Roselisp's `interpret` function:

```javascript
import {interpret, sexp} from 'roselisp';

const exp = sexp`(+ 1 1)`;
const val = interpret(exp); // 2
```

When passed a single argument, `interpret` evaluates the given S-expression in a standard environment. It is also possible to pass a custom environment as the second argument to `interpret`.

### Environments

To create a custom Lisp environment, instantiate the `LispEnvironment` class. The `LispEnvironment` constructor is passed an array of environment bindings. For example, the following creates an environment that binds the symbols `inc` and `square` to functions that increment and square a value, respectively. The expression `(square (inc 1))` is then evaluated in that environment:

```javascript
import {interpret, LispEnvironment, s, sexp} from 'roselisp';

function inc(x) {
  return x + 1;
}

function square(x) {
  return x * x;
}

const env = new LispEnvironment([
  [s`inc`, inc, 'function'],
  [s`square`, square, 'function']
]);

const exp = sexp`(square (inc 1))`;

const val = interpret(exp, env); // 4
```

When we instantiate `LispEnvironment` with an array of bindings, each binding is an array `[sym, val, type]`, where `sym` is a symbol to bind to a value, `val` is the value to bind it to, and `type` is one of `'variable'`, `'function'`, `'macro'` or `'special'`. Here, the binding type is `'function'` since `inc` and `square` are functions.

The custom environment is stacked on top of a standard Lisp environment, so it is possible to use bindings from both. For example, we can evaluate `(+ (square (inc 1)) 1)` and get `5`; here, `+` is provided by the standard environment, while `square` and `inc` are provided by the custom environment. (There is also a low-level evaluation function, `eval_`, which does not do any stacking and requires the caller to provide a complete language environment, but it is rarely what you want. When in doubt, use `interpret`, not `eval_`.)

Thus far, we have interacted with Roselisp's interpreter from JavaScript, in order to call attention to the way Lisp code is represented in JavaScript. But we could easily express the same program in Lisp:

```scheme
(require (only-in "roselisp" interpret LispEnvironment))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define env
  (new LispEnvironment
       `((inc ,inc "function")
         (square ,square "function"))))

(define exp
  '(square (inc 1)))

(define val
  (interpret exp env)) ; 4
```

In the {doc}`Compiler <compiler>` chapter, we will see how we can compile such Lisp code to the equivalent JavaScript code.

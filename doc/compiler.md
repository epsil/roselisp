# Compiler

Roselisp provides a compiler which can be used to compile Scheme programs to JavaScript or TypeScript. Roselisp's compiler is a *source-to-source compiler* or *transcompiler*, what is sometimes called a *transpiler*. Whenever we speak of compilation here, we mean source-to-source compilation, also known as transcompilation or transpilation.

## JavaScript

We assume that we have a file named `hello.scm` that contains the same ["hello, world"](http://en.wikipedia.org/wiki/%22Hello,_World!%22_program) program we have seen before:

```scheme
(define (main)
  (display "hello, world")
  0)

(main)
```

To compile this program to JavaScript, run `roselisp` with the `-c` option:

```console
$ roselisp -c hello.scm
Compiled hello.scm to hello.js
```

This will create a file named `hello.js` in the same directory, which contains the following JavaScript code:

```javascript
function main() {
  console.log('hello, world');
  return 0;
}

main();
```

If we execute this file with `node`, it will print `hello, world` to standard output:

```console
$ node hello.js
hello, world
```

By default, all Roselisp functions return a value, which is why we have made the `main` function return `0`. We can change this behavior by adding a type annotation. As Roselisp takes after [Typed Racket](http://docs.racket-lang.org/ts-guide/), we need to specify a return type of `Void`. Change `hello.scm` to:

```scheme
(: main (-> Void))
(define (main)
  (display "hello, world"))

(main)
```

Run `roselisp -c hello.scm` again. `hello.js` now changes to:

```javascript
function main() {
  console.log('hello, world');
}

main();
```

Since the function's return type has been declared as `Void`, the compiled code does not contain a `return` statement.

## TypeScript

To compile a Roselisp file to TypeScript, add `--language typescript`:

```console
$ roselisp hello.scm -c --language typescript
```

This will create a file named `hello.ts` in the same directory:

```
function main(): void {
  console.log('hello, world');
}

main();
```

Note the `void` type annotation, which is TypeScript syntax. Roselisp has translated the Typed Racket type `Void` to the TypeScript type `void`.

This file, `hello.ts`, can be then be compiled to JavaScript with TypeScript's `tsc` compiler:

```console
$ tsc hello.ts
```

This creates a JavaScript file `hello.js`, which is produced from the TypeScript file `hello.ts`.

For more on TypeScript and `tsc`, see [*The TypeScript Handbook*](http://www.typescriptlang.org/docs/handbook/intro.html). For more on type annotations in Scheme, see [*The Typed Racket Guide*](http://docs.racket-lang.org/ts-guide/).

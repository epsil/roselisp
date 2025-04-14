# Miscellaneous

## Roselisp design goals

Roselisp translates comments as well. Comments in the Lisp source code are not stripped away, but are translated to JavaScript comments.

The goal of Roselisp is to translate Scheme programs to JavaScript programs in a natural way that preserves comments. This lets one use Scheme as a tool for writing JavaScript libraries.

Roselisp translates types as well, if they are specified. Typed Racket types are translated to TypeScript types.

Roselisp translates Typed Racket to TypeScript.

Roselisp translates a subset of Typed Racket to TypeScript.

Roselisp also supports translation of Typed Racket types to TypeScript types.

## Differences from Racket

Roselisp approximates the design of Racket Scheme, and takes after it as much as possible. However, by design, Roselisp is concerned with that core of Racket Scheme which is similar to the core of other programming languages.

When it is difficult to translate

Roselisp straddles two worlds

Another way to put it is that Roselisp places a premium on *translatability*. Roselisp concerns itself with that subset of Scheme which is easily translated to other languages.

More genereally, we may say that Roselisp concerns itself with that subset of Racket Scheme that is easily translatable to other languages.

Roselisp attempts to bridge the gap between Lisp and JavaScript. In practice, Roselisp translates a subset of Typed Racket to a subset of TypeScript.

Roselisp is subject to the following constraints:

-   Scheme programs should be translated to JavaScript in a natural way.
-   S-expressions are represented as arrays in JavaScript, with JavaScript's own `Symbol` class used to represent symbols. This makes it easy to write S-expressions in JavaScript.

There are currently some differences from Racket:

-   In addition to the function `member`, Roselisp defines a nonstandard function `member?`, which is similar to the standard function except that its return value is `#t` or `#f`. The benefit of `member?` is that it translates directory to JavaScript's `.includes` method.
-   Classes in Roselisp can define a constructor with `(define/public (constructor ...) ...)`. The constructor is invoked when the class is instantiated.

*Constructors.* Roselisp permits classes to define a constructor by defining a public member method named `constructor`, i.e., `(define/public (constructor ...) ...)`. The constructor is invoked when the class is instantiated. Objects instantiated with `make-object` or `new` pass values in the order specified by the constructor.

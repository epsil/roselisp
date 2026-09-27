/**
 * # Unsorted code
 *
 * This file functions as an "inbox" or scratchpad for new code, as
 * well as an "outbox" for legacy code that is not needed anymore and
 * may be deleted.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Indent a string by prepending each line with `n` spaces.
 */
declare function indentString(str: any, n?: any, options?: any): any;
declare namespace indentString {
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | symbol[])[] | (number | symbol)[])[])[];
}
/**
 * # Trampoline
 *
 * Trampoline implementation.
 *
 * ## Description
 *
 * As a starting point, consider Clojure's
 * [`trampoline()`][clj:trampoline] function, which is invoked with a
 * function and its arguments, e.g., `(trampoline f 10)`. The
 * `trampoline` function takes the supplied function, `f`, and calls
 * it with the supplied argument, `10`. Then, as long as the return
 * value is a function, it takes that function and calls it with zero
 * arguments, and it keeps on doing so until a non-functional value
 * is returned. At this point, the trampolining stops, and the
 * non-functional value is the return value of the `trampoline` call.
 *
 * This simple implementation suffices for a number of cases, but it
 * has some limitations: there is no way to pass arguments to the
 * trampolined function, nor is it possible to return a functional
 * value. A solution to this is to have the trampolined function
 * return a data structure that represents a function call. Suppose,
 * for example, that we represent the function call `f(x, y)` as the
 * array `[f, x, y]`:
 *
 *     // Return a trampolined function call `f(x, y)`
 *     return [f, x, y];
 *
 * Now we can pass arguments to trampolined functions by returning
 * what is essentially an S-expression. It works, but there is a
 * problem: since we use arrays to represent function calls, we have
 * no way to return array values. This can be solved by defining a
 * special class, `TrampolineCall`, for representing function calls.
 * This class is just a wrapper around the expression array; its
 * purpose is to allow us to distinguish between function calls and
 * array values. Thus, the function call `f(x, y)` can be represented
 * as an instance of `TrampolineCall`, which is now distinct from the
 * array value `[f, x, y]`:
 *
 *     // Return a trampolined function call `f(x, y)`
 *     return new TrampolineCall(f, x, y);
 *     // Return the array `[f, x, y]`
 *     return [f, x, y];
 *
 * It is also easy to add support for nested function calls:
 *
 *     // Return the nested function call `f(g(x), h(y))`
 *     return new TrampolineCall(
 *       f,
 *       new TrampolineCall(g, x),
 *       new TrampolineCall(h, y)
 *     );
 *
 * A short-hand way of creating trampoline calls is provided by the
 * `trampolineCall()` function:
 *
 *     // Return the nested function call `f(g(x))`
 *     return trampolineCall(f, trampolineCall(g, x));
 *
 * The trampolining stops once a non-`TrampolineCall` value is
 * returned. For instance:
 *
 *     // Return the number `1`
 *     return 1;
 *     // Return the string `'1'`
 *     return '1';
 *     // Return the array `[1, 2, 3]`
 *     return [1, 2, 3];
 *
 * Let us consider an example. The [Fibonacci sequence][w:Fibonacci
 * sequence] 0, 1, 1, 2, 3, 5, 8, ... can be defined by the recursive
 * function:
 *
 *     function fibonacci(n) {
 *       if (n < 2) {
 *         return n;
 *       } else {
 *         return fibonacci(n - 1) + fibonacci(n - 2);
 *       }
 *     }
 *
 * This function can be turned into a trampolined function,
 * `fibonacciT()`, by returning a trampolined function call:
 *
 *     function fibonacciT(n) {
 *       if (n < 2) {
 *         return n;
 *       } else {
 *         return trampolineCall(
 *           add,
 *           trampolineCall(fibonacciT, n - 1),
 *           trampolineCall(fibonacciT, n - 2)
 *         );
 *       }
 *     }
 *
 * Here, `add` is a function wrapper around `+` and may be defined as
 * `(x, y) => x + y`. Now `fibonacci()` can be implemented in terms
 * of `fibonacciT()` with a call to `trampoline()`:
 *
 *     function fibonacci(n) {
 *       return trampoline(fibonacciT, n);
 *     }
 *
 * This behaves similarly to the recursive implementation, but does
 * not depend on JavaScript's call stack for recursion. Therefore, it
 * scales better, and can handle cases that would exceed the limits
 * of JavaScript's call stack.
 *
 * For more on trampolines, see the [Wikipedia article][w:Trampoline
 * (computing)] on the subject, as well as the articles ["On
 * Recursion, Continuations and Trampolines"][blog:Bendersky17] by
 * Eli Bendersky and ["Lisp-style trampolines in Common Lisp, C, Ada,
 * Oberon-2, and Revised Oberon"][blog:Bond22] by T. Kurt Bond.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla
 * Public License, v. 2.0. If a copy of the MPL was not distributed
 * with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 *
 * [clj:trampoline]: https://clojuredocs.org/clojure.core/trampoline
 * [w:Fibonacci sequence]: https://en.wikipedia.org/wiki/Fibonacci_sequence
 * [w:Trampoline (computing)]: https://en.wikipedia.org/wiki/Trampoline_(computing)
 * [blog:Bendersky17]: https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/
 * [blog:Bond22]: https://tkurtbond.github.io/posts/2022/06/14/lisp-style-trampolines-in-common-lisp-c-ada-oberon-2-and-revised-oberon/
 */

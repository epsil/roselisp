// SPDX-License-Identifier: MPL-2.0
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

/**
 * Trampoline class.
 *
 * Contains a call stack and a value stack. The call stack is stepped
 * through until it is exhausted, and the returned result is the
 * topmost entry on the value stack.
 */
class Trampoline {
  /**
   * Call stack.
   */
  calls: any = [];

  /**
   * Value stack.
   */
  values: any = [];

  /**
   * Internal stack symbol, used to reference the value
   * on the top of the value stack.
   */
  valueSymbol: any = Symbol('value');

  /**
   * Create a new trampoline.
   * An initial function call may be specified
   * with `f` and `args`; `args`  are here the
   * arguments to the function `f`.
   */
  constructor(f: any = undefined, ...args: any[]) {
    if (f) {
      const initialCall: any = new TrampolineCall(f, ...args);
      this.pushCall(initialCall);
    }
  }

  /**
   * Whether the trampoline is empty,
   * i.e., there are no trampoline calls left.
   */
  isEmpty(): any {
    return this.size() === 0;
  }

  /**
   * Pop a function call off the call stack.
   */
  popCall(): any {
    if (this.calls.length === 0) {
      return undefined;
    } else {
      return this.calls.shift();
    }
  }

  /**
   * Pop a value off the value stack.
   */
  popValue(): any {
    if (this.values.length === 0) {
      return undefined;
    } else {
      return this.values.shift();
    }
  }

  /**
   * Push a function call onto the call stack.
   */
  pushCall(call: any): any {
    this.calls.unshift(call);
    return this;
  }

  /**
   * Push a value onto the value stack.
   */
  pushValue(value: any): any {
    this.values.unshift(value);
    return this;
  }

  /**
   * Pop and evaluate function calls off the call stack
   * until it is exhausted. Returns the value returned by
   * the final call.
   */
  run(): any {
    return this.runUntil(0);
  }

  /**
   * Pop and evaluate function calls off the call stack
   * until it reaches size `size`.
   */
  runUntil(size: any = 0): any {
    while (this.size() > size) {
      this.step();
    }
    return this.popValue();
  }

  /**
   * The number of function calls on the call stack.
   */
  size(): any {
    return this.calls.length;
  }

  /**
   * Pop a single function call off the call stack
   * and evaluate it. The value thus obtained is
   * pushed onto the value stack.
   */
  step(): any {
    const tramp: any = this;
    const nestedCalls: any = [];
    const call: any = tramp.popCall();
    // Iterate over the function call right-to-left so that
    // values get fetched from the values stack in the right
    // order: the rightmost value is on the top of the value
    // stack, while the leftmost value is underneath the other
    // values.
    function f(exp: any): any {
      if (exp instanceof TrampolineCall) {
        // It is tempting to call `.runUntil()` here and
        // evaluate the nested call right away, but that
        // would create the kind of dependency on
        // JavaScript's call stack that we are trying to
        // avoid. So instead, we create a new function call
        // that contains the special value `value-symbol`,
        // which, when evaluated, will instruct the
        // trampoline to fetch the value that the nested call
        // evaluated to from the value stack.
        nestedCalls.push(exp);
        return tramp.valueSymbol;
      } else if (exp === tramp.valueSymbol) {
        // The special value `value-symbol` instructs the
        // trampoline to pop a value off the value stack.
        return tramp.popValue();
      } else {
        return exp;
      }
    }
    const call1: any = call.mapRight(f);
    if (nestedCalls.length > 0) {
      tramp.pushCall(call1);
      for (let nestedCall of nestedCalls) {
        tramp.pushCall(nestedCall);
      }
    } else {
      const value: any = call1.evaluate();
      if (value instanceof TrampolineCall) {
        return tramp.pushCall(value);
      } else {
        return tramp.pushValue(value);
      }
    }
  }
}

/**
 * Trampolined function call.
 *
 * A wrapper around an array representing the call.
 */
class TrampolineCall {
  /**
   * An array where the first element is the function
   * and the other elements are the arguments to it.
   */
  call: any;

  /**
   * Create a trampolined function call.
   *
   * `call` is an array where the first element is the function
   * and the remaining elements are the arguments to it.
   */
  constructor(...call: any[]) {
    this.call = call;
  }

  /**
   * Evaluate the function call.
   */
  evaluate(): any {
    if (this.call.length === 0) {
      return undefined;
    } else {
      const [f, ...args]: any[] = this.call;
      if (f instanceof Function) {
        return f(...args);
      } else {
        return undefined;
      }
    }
  }

  /**
   * Map a function over the function call
   * (left-to-right).
   */
  map(f: any): any {
    return this.mapLeft(f);
  }

  /**
   * Map a function over the function call,
   * from left to right.
   */
  mapLeft(f: any): any {
    return new TrampolineCall(...this.call.map(function (x: any): any {
      return f(x);
    }));
  }

  /**
   * Map a function over the function call,
   * from right to left.
   */
  mapRight(f: any): any {
    const call: any = [];
    const _start: any = this.size() - 1;
    for (let i: any = _start; i > -1; i--) {
      call.unshift(f((this.call as any)[i]));
    }
    return new TrampolineCall(...call);
  }

  /**
   * Pop a value off the call
   * (off the end of the call).
   */
  pop(): any {
    return this.popRight();
  }

  /**
   * Pop a value off the beginning of the call.
   */
  popLeft(): any {
    return this.call.shift();
  }

  /**
   * Pop a value off the end of the call.
   */
  popRight(): any {
    return this.call.pop();
  }

  /**
   * Push a value onto the call
   * (the end of the call).
   */
  push(value: any): any {
    return this.pushRight(value);
  }

  /**
   * Push a value onto the beginning of the call.
   */
  pushLeft(value: any): any {
    this.call.unshift(value);
    return this;
  }

  /**
   * Push a value onto the end of the call.
   */
  pushRight(value: any): any {
    this.call.push(value);
    return this;
  }

  /**
   * Return the size of the call
   * (i.e., number of arguments plus one).
   */
  size(): any {
    return this.call.length;
  }
}

/**
 * Run a trampolined function.
 *
 * The function may return an instance of {@link TrampolineCall}
 * (e.g., by calling {@link trampolineCall}) to represent a
 * trampolined function calls. Other values are treated as final
 * values.
 */
function trampoline(f: any, ...args: any[]): any {
  const trampolineInstance: any = new Trampoline(f, ...args);
  return trampolineInstance.run();
}

/**
 * Create a trampolined function call.
 */
function tcall(f: any, ...args: any[]): any {
  return new TrampolineCall(f, ...args);
}

export {
  tcall as tCall,
  tcall as trampolineCall,
  trampoline as runTrampoline,
  trampoline as trampolineRun,
  Trampoline,
  TrampolineCall,
  tcall,
  trampoline
};
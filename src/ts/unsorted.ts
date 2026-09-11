// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
function indentString(str: any, n: any = 2, options: any = {}): any {
  const whitespaceOption: any = options['whitespace'];
  const whitespace: any = whitespaceOption || ' ';
  const includeEmptyLinesOption: any = options['includeEmptyLines'];
  const pattern: any = includeEmptyLinesOption ? new RegExp('^', 'gm') : new RegExp('^(?!s*$)', 'gm');
  const indentation: any = whitespace.repeat(n);
  return str.replace(pattern, indentation);
}

indentString.fsource = [Symbol.for('define'), [Symbol.for('indent-string'), Symbol.for('str'), [Symbol.for('n'), 2], [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('whitespace-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':whitespace')]], [Symbol.for('define'), Symbol.for('whitespace'), [Symbol.for('or'), Symbol.for('whitespace-option'), ' ']], [Symbol.for('define'), Symbol.for('include-empty-lines-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':include-empty-lines')]], [Symbol.for('define'), Symbol.for('pattern'), [Symbol.for('if'), Symbol.for('include-empty-lines-option'), [Symbol.for('regexp'), '^', 'gm'], [Symbol.for('regexp'), '^(?!s*$)', 'gm']]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), Symbol.for('whitespace'), Symbol.for('n')]], [Symbol.for('regexp-replace'), Symbol.for('pattern'), Symbol.for('str'), Symbol.for('indentation')]];

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
    f.fsource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('TrampolineCall')], [Symbol.for('push-right!'), Symbol.for('nested-calls'), Symbol.for('exp')], [Symbol.for('get-field'), Symbol.for('value-symbol'), Symbol.for('tramp')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('get-field'), Symbol.for('value-symbol'), Symbol.for('tramp')]], [Symbol.for('send'), Symbol.for('tramp'), Symbol.for('pop-value')]], [Symbol.for('else'), Symbol.for('exp')]]];
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

trampoline.fsource = [Symbol.for('define'), [Symbol.for('trampoline'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('trampoline-instance'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('Trampoline'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('send'), Symbol.for('trampoline-instance'), Symbol.for('run')]];

/**
 * Create a trampolined function call.
 */
function tcall(f: any, ...args: any[]): any {
  return new TrampolineCall(f, ...args);
}

tcall.fsource = [Symbol.for('define'), [Symbol.for('tcall'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('new'), Symbol.for('TrampolineCall'), Symbol.for('f'), Symbol.for('args')]];

// ;;; # Special forms
// ;;;
// ;;; This file contains unused interpreter procedures for
// ;;; special forms.
// ;;;
// ;;; ## Description
// ;;;
// ;;; The default approach for the interpreter is to compile the Lisp
// ;;; code to JavaScript code and then evaluate the JavaScript code.
// ;;; That is to say that the S-expression is compiled to an ESTree
// ;;; tree, which is then evaluated.
// ;;;
// ;;; However, it is possible to make the intepretation more efficient
// ;;; by skipping the compilation step and interpreting the S-expression
// ;;; directly. This file defines interpreter procedures which do just
// ;;; that.
// ;;;
// ;;; Care must be taken to implement these functions correctly, as
// ;;; their behavior should be identical to the standard behavior of
// ;;; compiling the code to an ESTree tree and then evaluating. In other
// ;;; words, the only purpose of the code in this file is to make
// ;;; interpretation faster.
// ;;;
// ;;; The functions defined in this file are *special forms*, and must
// ;;; be typed as such in the language environment. A special form
// ;;; receives its own form unevaluated and returns a value that is used
// ;;; directly. It is similar to a macro, except that the value returned
// ;;; by a macro is re-evaluated, while the value returned by a special
// ;;; form is used as-is.
// ;;;
// ;;; Alternatively, one could rewrite these functions to fexprs, which
// ;;; is almost the same thing.
//
// (require (only-in "./env"
//                   EnvironmentStack
//                   LispEnvironment
//                   current-environment))
// (require (only-in "./eval"
//                   (eval_ eval-t)
//                   ;; eval-t
//                   eval_))
// (require (only-in "./exception"
//                   BreakException
//                   ContinueException
//                   ReturnException))
// (require (only-in "./list"
//                   (list-ref_ aget_)
//                   (list-set!_ aset_)))
// (require (only-in "./rose"
//                   datum->syntax
//                   syntax->datum
//                   syntax?
//                   transfer-comments))
// (require (only-in "./util"
//                   begin-wrap
//                   form?
//                   lambda->let
//                   quasiquote?
//                   quote?
//                   tagged-list?
//                   text-of-quotation
//                   unquote-splicing?
//                   unquote?))
// (require (only-in "./procedures"
//                   (funcall tcall)))
// ;; (require (only-in "./trampoline"
// ;;                   tcall))
//
// ;;; Evaluate a `(quote ...)` form.
// (define (quote-special_ exp env)
//   (text-of-quotation exp))
//
// ;;; Evaluate a `(quasiquote ...)` form.
// (define (quasiquote-special_ exp env)
//   (quasiquote-helper (second exp) env))
//
// ;;; Helper function for `quasiquote-special_`.
// (define (quasiquote-helper exp env)
//   (cond
//    ((not (list? exp))
//     exp)
//    (else
//     (define result '())
//     (for ((x exp))
//       (cond
//        ((unquote? x)
//         (define val
//           (eval_ (second x) env))
//         (push-right! result val))
//        ((unquote-splicing? x)
//         (define val
//           (eval_ (second x) env))
//         (unless (list? val)
//           (error "Wrong type of argument: expected list"))
//         (set! result (append result val)))
//        ((quasiquote? x)
//         (push-right! result x))
//        ((list? x)
//         (define val
//           (quasiquote-helper x env))
//         (push-right! result val))
//        (else
//         (push-right! result x))))
//     result)))
//
// ;;; Evaluate a `(setq ...)` form.
// (define (setq-special_ exp env)
//   (define assignments '())
//   (for ((i (range 1 (- (length exp) 1) 2)))
//     (define sym
//       (list-ref exp i))
//     (define val
//       (list-ref exp (+ i 1)))
//     (define assignment
//       `(set ',sym ,val))
//     (push-right! assignments assignment))
//   (define set-exp '())
//   (if (> (length assignments) 1)
//       (set! set-exp `(begin ,@assignments))
//       (set! set-exp (first assignments)))
//   (eval_ set-exp env))
//
// ;;; Evaluate a `(set ...)` form.
// (define (set-special_ exp env)
//   (define params
//     (rest exp))
//   (define sym
//     (first params))
//   (set! sym (eval_ sym env))
//   (cond
//    ((form? sym env aget_)
//     (eval_ `(,aset_ ,@(rest sym) ,(second params)) env))
//    (else
//     (define val
//       (eval_ (second params) env))
//     (cond
//      ((and (list? sym)
//            (= (length sym) 2))
//       (define prop
//         (first sym))
//       (define obj
//         (eval_ (second sym) env))
//       (cond
//        ((symbol? prop)
//         (define match)
//         (when (set! match
//                     (regexp-match (regexp "^\\.-(.*)$")
//                                   (symbol->string prop)))
//           (set! prop (second match))
//           (set! (oget obj prop) val)))))
//      (else
//       (send env set! sym val 'Any)))
//     val)))
//
// ;;; Evaluate a `(fset ...)` form.
// (define (fset-special_ exp env)
//   (define params
//     (rest exp))
//   (define sym
//     (eval_ (first params) env))
//   (define val
//     (eval_ (second params) env))
//   (send env set! sym val '(->* :rest Any Any))
//   val)
//
// ;;; Evaluate a `(module ...)` form.
// (define (module-special_ exp env)
//   (tcall eval-t `(begin ,@(drop exp 3)) env))
//
// ;;; Evaluate a `(begin ...)` form.
// (define (begin-special_ exp env)
//   (begin-helper (rest exp) env #u))
//
// ;;; Helper function for `begin-special_`.
// (define (begin-helper expressions env val)
//   (if (= (length expressions) 0)
//       val
//       (tcall begin-helper
//              (rest expressions)
//              env
//              (tcall eval-t (first expressions) env))))
//
// ;;; Evaluate a `(let* ...)` form.
// (define (let-star-special_ exp env)
//   (define params
//     (rest exp))
//   (define var-exps
//     (first params))
//   (define body
//     (rest params))
//   (define bindings '())
//   (define init-exps '())
//   (for ((var-exp var-exps))
//     (cond
//      ((symbol? var-exp)
//       (push-right! bindings
//                    (list var-exp #u 'Any)))
//
//      (else
//       (push-right! bindings
//                    (list (first var-exp) #u 'Any))
//       (define init-exp
//         `(setq ,@var-exp))
//       (push-right! init-exps init-exp))))
//   (define let-env
//     (new LispEnvironment bindings))
//   (define combined-env
//     (new EnvironmentStack let-env env))
//   (define begin-exp
//     `(begin ,@init-exps ,@body))
//   (tcall eval-t begin-exp combined-env))
//
// ;;; Evaluate a `(let-values ...)` form.
// (define (let-values-special_ exp env)
//   (define bindings
//     (second exp))
//   (define body
//     (drop exp 2))
//   (define let-bindings '())
//   (for ((i (range 0 (length bindings))))
//     (define result
//       (gensym
//        (string-append
//         "let-values-result-"
//         (number->string
//          (+ i 1)))))
//     (define binding
//       (list-ref bindings i))
//     (define binding-vars
//       (first binding))
//     (define binding-exp
//       (second binding))
//     (define regular-bindings '())
//     (define rest-binding #u)
//     (cond
//      ((symbol? binding-vars)
//       (set! rest-binding binding-vars))
//      ((dotted-list? binding-vars)
//       (define binding-list
//         (flatten binding-vars))
//       (set! regular-bindings
//             (drop-right binding-list 1))
//       (set! rest-binding
//             (last binding-list)))
//      (else
//       (set! regular-bindings binding-vars)))
//     (push-right! let-bindings (list result binding-exp))
//     (for ((j (range 0 (length regular-bindings))))
//       (push-right! let-bindings
//                    (list (list-ref regular-bindings j)
//                          `(list-ref ,result ,j))))
//     (when rest-binding
//       (push-right! let-bindings
//                    (list rest-binding
//                          `(nthcdr ,(length
//                                     regular-bindings)
//                                   ,result)))))
//   (eval_ `(let* ,let-bindings
//             ,@body)
//          env))
//
// ;;; Evaluate a `(define-values ...)` form.
// (define (define-values-special_ exp env)
//   (define ids
//     (second exp))
//   (define val
//     (third exp))
//   (define regular-bindings '())
//   (define rest-binding #u)
//   (define result)
//   (cond
//    ((symbol? ids)
//     (set! rest-binding ids))
//    ((dotted-list? ids)
//     (define binding-list
//       (flatten ids))
//     (set! regular-bindings
//           (drop-right binding-list 1))
//     (set! rest-binding
//           (last binding-list)))
//    (else
//     (set! regular-bindings ids)))
//   (set! result (eval_ val env))
//   (for ((i (range 0 (length regular-bindings))))
//     (eval_ `(define ,(list-ref regular-bindings i)
//               ',(list-ref result i))
//            env))
//   (when rest-binding
//     (eval_ `(define ,rest-binding
//               ',(nthcdr (length
//                          regular-bindings)
//                         result))
//            env))
//   #u)
//
// ;;; Evaluate a `(set!-values ...)` form.
// (define (set-values-special_ exp env)
//   (define ids
//     (second exp))
//   (define val
//     (third exp))
//   (define regular-bindings '())
//   (define rest-binding #u)
//   (define result)
//   (cond
//    ((symbol? ids)
//     (set! rest-binding ids))
//    ((dotted-list? ids)
//     (define binding-list
//       (flatten ids))
//     (set! regular-bindings
//           (drop-right binding-list 1))
//     (set! rest-binding
//           (last binding-list)))
//    (else
//     (set! regular-bindings ids)))
//   (set! result (eval_ val env))
//   (for ((i (range 0 (length regular-bindings))))
//     (eval_ `(set! ,(list-ref regular-bindings i)
//                   ',(list-ref result i))
//            env))
//   (when rest-binding
//     (eval_ `(set! ,rest-binding
//                   ',(nthcdr (length
//                              regular-bindings)
//                             result))
//            env))
//   #u)
//
// ;;; Evaluate a `(define ...)` form.
// (define (define-special_ exp env)
//   (define name
//     (second exp))
//   (define body
//     (drop exp 2))
//   (cond
//    ;; Function definition.
//    ((pair-or-list? name)
//     (define name-and-params name)
//     (define f-name
//       (first name-and-params))
//     (define params
//       (rest name-and-params))
//     (cond
//      ;; Curried function definition.
//      ((pair-or-list? f-name)
//       ;; Curry the function. (The following is more similar to
//       ;; currying in Haskell than currying in Racket because the
//       ;; whole function is curried, not just some arguments.)
//       (define curried-name-and-params
//         (send name-and-params flat Infinity))
//       (define curried-name
//         (first curried-name-and-params))
//       (define curried-params
//         (rest curried-name-and-params))
//       (define curried-arity
//         (length curried-params))
//       (define curried-function-exp
//         `(curry
//           (lambda ,curried-params
//             ,@body)
//           ,curried-arity))
//       (define val
//         (eval_ curried-function-exp env))
//       (send env set-local! curried-name val)
//       val)
//      ;; Uncurried function definition.
//      (else
//       (define lambda-exp
//         `(lambda ,params
//            ,@body))
//       (define val
//         (eval_ lambda-exp env))
//       (send env set-local! f-name val)
//       val)))
//    ;; Class definition.
//    ((and (= (length exp) 3)
//          ;; (form? (third exp) env define-class_)
//          (tagged-list? (third exp) 'define-class))
//     (eval_ (define->define-class exp) env))
//    ;; Variable definition.
//    (else
//     (define val-exp
//       (third exp))
//     (define val
//       (eval_ val-exp env))
//     (send env set-local! name val)
//     val)))
//
// ;;; Convert a `(define ... (class ...))` form to
// ;;; a `(define-class ...)` form.
// (define (define->define-class node)
//   (cond
//    ((syntax? node)
//     (define superclass
//       (send (send node get 2) get 1))
//     (define superclass-exp
//       (syntax->datum superclass))
//     (define superclass-list
//       (if (or (eq? superclass-exp 'object%)
//               (eq? superclass-exp 'object)
//               (eq? superclass-exp 'Object))
//           '()
//           (list superclass)))
//     (transfer-comments
//      node
//      (datum->syntax
//       #f
//       `(define-class ,(send node get 1)
//          ,(datum->syntax #f superclass-list)
//          ,@(send (send node get 2) drop 2)))))
//    (else
//     (~> node
//         (datum->syntax #f _)
//         (define->define-class _)
//         (syntax->datum _)))))
//
// ;;; Evaluate a `(define/public ...)` form.
// (define (define-public-special_ exp env)
//   (define-special_ exp env))
//
// ;;; Evaluate a `(define/generator ...)` form.
// (define (define-generator-special_ exp env)
//   (define-special_ exp env))
//
// ;;; Evaluate a `(define/async ...)` form.
// (define (define-async-special_ exp env)
//   (define-special_ exp env))
//
// ;;; Evaluate a `(defmacro ...)` form.
// (define (defmacro-special_ exp env)
//   (define name
//     (second exp))
//   (when (list? name)
//     (set! name (first name)))
//   (define macro-fn
//     (defmacro->fn exp env))
//   (send env set! name macro-fn '(->macro :rest Any Any))
//   ;; name
//   macro-fn)
//
// ;;; Create a macro function on the basis of a
// ;;; `(defmacro ...)` form.
// (define (defmacro->fn exp env)
//   (define macro-fn
//     (defmacro->lambda-form exp))
//   (eval_ macro-fn env))
//
// ;;; Create a `(lambda ...)` form for a macro function
// ;;; on the basis of a `(defmacro ...)` form.
// (define (defmacro->lambda-form exp)
//   (define name
//     (second exp))
//   (define args
//     (third exp))
//   (define body
//     (drop exp 3))
//   (define env 'env)
//   (define macro-args '())
//   (when (list? name)
//     (set! args (rest name))
//     (set! name (first name))
//     (set! body (drop exp 2)))
//   (cond
//    ((list? args)
//     (for ((i (range 0 (length args))))
//       (define arg
//         (list-ref args i))
//       (cond
//        ((eq arg '&environment)
//         (set! env (list-ref args (+ i 1)))
//         (set! i (+ i 2)))
//        (else
//         (push-right! macro-args arg)))))
//    (else
//     (set! macro-args args)))
//   (cond
//    ((null? macro-args)
//     `(lambda (exp ,env)
//        ,@body))
//    (else
//     `(lambda (exp ,env)
//        (let-values ((,macro-args (rest exp)))
//          ,@body)))))
//
// ;;; Evaluate a `(define-macro ...)` form.
// (define (define-macro-special_ exp env)
//   (define name
//     (car (second exp)))
//   (define macro-fn
//     (define-macro->fn exp env))
//   (send env set! name macro-fn '(->macro :rest Any Any))
//   name)
//
// ;;; Create a macro function on the basis of a
// ;;; `(define-macro ...)` form.
// (define (define-macro->fn exp env)
//   (define macro-fn
//     (define-macro->lambda-form exp))
//   (eval_ macro-fn env))
//
// ;;; Create a `(lambda ...)` form for a macro function
// ;;; on the basis of a `(define-macro ...)` form.
// (define (define-macro->lambda-form exp)
//   (define name-and-args
//     (second exp))
//   (define name
//     (car name-and-args))
//   (define args
//     (cdr name-and-args))
//   (define body
//     (drop exp 2))
//   (define env 'env)
//   (define macro-args '())
//   (cond
//    ((list? args)
//     (for ((i (range 0 (length args))))
//       (define arg
//         (list-ref args i))
//       (cond
//        ((eq arg '&environment)
//         (set! env (list-ref args (+ i 1)))
//         (set! i (+ i 2)))
//        (else
//         (push-right! macro-args arg)))))
//    (else
//     (set! macro-args args)))
//   (cond
//    ((null? macro-args)
//     `(lambda (exp ,env)
//        ,@body))
//    (else
//     `(lambda (exp ,env)
//        (let-values ((,macro-args (rest exp)))
//          ,@body)))))
//
// ;;; Evaluate a `(for ...)` form.
// (define (for-special_ exp env)
//   (define decls
//     (second exp))
//   (define body
//     (drop exp 2))
//   (define-values (decl1)
//     decls)
//   (define-values (sym values-expr)
//     decl1)
//   (define values
//     (eval_ values-expr env))
//   (define result #u)
//   (try
//     (for ((value values))
//       (try
//         (set! result
//               (eval_ `(let ((,sym ,value))
//                         ,@body)
//                      env))
//         (catch ContinueException e)))
//     (catch BreakException e
//       (set! result #u)))
//   result)
//
// ;;; Evaluate a `(js/while ...)` form.
// (define (js/while-special_ exp env)
//   (define test
//     `(truep ,(second exp)))
//   (define body
//     (begin-wrap (drop exp 2)))
//   (define result #u)
//   (try
//     (while (eval_ test env)
//       (try
//         (set! result (eval_ body env))
//         (catch ContinueException e)))
//     (catch BreakException e
//       (set! result #u)))
//   result)
//
// ;;; Evaluate a `(js/do-while ...)` form.
// (define (js/do-while-special_ exp env)
//   (define body (second exp))
//   (define test (third exp))
//   (define begin-exp
//     `(begin
//        ,body
//        (while ,test
//          ,body)))
//   (eval_ begin-exp env))
//
// ;;; Evaluate a `(break)` form.
// (define (break-special_ exp env)
//   (throw (new BreakException)))
//
// ;;; Evaluate a `(continue)` form.
// (define (continue-special_ exp env)
//   (throw (new ContinueException)))
//
// ;;; Evaluate a `(yield ...)` form.
// (define (yield-special_ exp env)
//   (define val
//     (eval_ (second exp) env))
//   val)
//
// ;;; Evaluate a `(return ...)` form.
// (define (return-special_ exp env)
//   (define val
//     (eval_ (second exp) env))
//   (throw (new ReturnException val)))
//
// ;;; Evaluate a `(throw ...)` form.
// (define (throw-special_ exp env)
//   (define val
//     (eval_ (second exp) env))
//   (throw val))
//
// ;;; Evaluate an `(async ...)` form.
// (define (async-special_ exp env)
//   (async
//    (lambda args
//      (apply (eval_ (second exp) env)
//             args))))
//
// ;;; Evaluate an `(await ...)` form.
// (define await-special_
//   (async
//    (lambda (exp env)
//      (await (eval_ (second exp) env)))))
//
// ;;; Evaluate a `(lambda ...)` form.
// (define (lambda-special_ exp env)
//   (js/function-special_ exp env))
//
// ;;; Evaluate a `(js/function ...)` form.
// (define (js/function-special_ exp env)
//   (define f
//     (js/function
//         args
//       (define let-exp
//         (lambda->let exp args))
//       (define result)
//       (try
//         (set! result (eval_ let-exp env))
//         (catch ReturnException e
//           (set! result (get-field value e))))
//       result))
//   (set-field! lisp-info f (list exp env))
//   f)
//
// ;;; Evaluate a `(js/arrow ...)` form.
// (define (js/arrow-special_ exp env)
//   (define f
//     (js/arrow
//         args
//       (define let-exp
//         (lambda->let exp args))
//       (define result)
//       (try
//         (set! result (eval_ let-exp env))
//         (catch ReturnException e
//           (set! result (get-field value e))))
//       result))
//   (set-field! lisp-info f (list exp env))
//   f)
//
// ;;; Evaluate a `(cond ...)` form.
// (define (cond-special_ exp env)
//   (cond
//    ((<= (length exp) 1)
//     #f)
//    (else
//     (define clause
//       (second exp))
//     (define clauses
//       (drop exp 2))
//     (define condition
//       (first clause))
//     (define then-expr
//       (begin-wrap
//        (rest clause)))
//     (tcall cond-helper
//            (or (eq? condition 'else)
//                (tcall eval-t `(truep ,condition) env))
//            then-expr
//            clauses
//            env))))
//
// ;;; Helper function for `cond-special_`.
// (define (cond-helper condition then-expr clauses env)
//   (cond
//    (condition
//     (tcall eval-t then-expr env))
//    ((= (length clauses) 0)
//     #u)
//    (else
//     (define clause1
//       (first clauses))
//     (define clauses1
//       (rest clauses))
//     (define condition1
//       (first clause1))
//     (define then-expr-1
//       (begin-wrap (rest clause1)))
//     (tcall cond-helper
//            (or (eq? condition1 'else)
//                (tcall eval-t `(truep ,condition1) env))
//            then-expr-1
//            clauses1
//            env))))
//
// ;;; Evaluate an `(and ...)` form.
// (define (and-special_ exp env)
//   (define params
//     (rest exp))
//   (define result #t)
//   (for ((operand params))
//     (set! result (eval_ operand env))
//     (unless (eval_ `(truep ',result) env)
//       (return #f)))
//   result)
//
// ;;; Evaluate an `(or ...)` form.
// (define (or-special_ exp env)
//   (define params
//     (rest exp))
//   (define result #f)
//   (for ((operand params))
//     (set! result (eval_ operand env))
//     (when (eval_ `(truep ',result) env)
//       (return result)))
//   result)
//
// ;;; Evaluate a `(send ...)` form.
// (define (send-special_ exp env)
//   (define obj
//     (second exp))
//   (set! obj (eval_ obj env))
//   (define method
//     (third exp))
//   (when (quote? method)
//     (set! method (eval_ method env)))
//   (define args
//     (drop exp 3))
//   (set! args
//         (map (lambda (x)
//                (eval_ x env))
//              args))
//   (apply send-method obj method args))
//
// ;;; Evaluate a `(send/apply ...)` form.
// (define (send-apply-special_ exp env)
//   (define obj
//     (second exp))
//   (set! obj (eval_ obj env))
//   (define method
//     (third exp))
//   (when (quote? method)
//     (set! method (eval_ method env)))
//   (define args
//     (drop exp 3))
//   (set! args
//         (map (lambda (x)
//                (eval_ x env))
//              args))
//   (when (> (length args) 0)
//     (set! args (append (drop-right args 1)
//                        (last args))))
//   (apply send-method obj method args))
//
// ;;; Call a method on an object.
// ;;;
// ;;; Helper function for `send-special_` and `send-apply-special_`.
// (define (send-method . args)
//   (define-values (obj method . rest-args)
//     args)
//   (cond
//    ((eq? (type-of method) "symbol")
//     (apply send-method obj (symbol->string method) rest-args))
//    ((eq? (type-of method) "string")
//     (apply send-method obj (oget obj method) rest-args))
//    ((is-a? method Function)
//     (send/apply method call obj rest-args))
//    (else
//     (throw (new Error
//                 (string-append "Not a method: "
//                                method))))))
//
// ;;; Evaluate a `(. ...)` form.
// (define (dot-special_ exp env)
//   (define obj
//     (eval_ (second exp) env))
//   (define method
//     (third exp))
//   (define field)
//   (define match)
//   (cond
//    ((and (symbol? method)
//          (set! match
//                (regexp-match (regexp "^-(.*)$")
//                              (symbol->string method))))
//     (set! field (second match))
//     (eval_ `(get-field ,(string->symbol field)
//                        ,obj)
//            env))
//    (else
//     (send-special_ exp env))))
//
// ;;; Evaluate a `(get-field ...)` form.
// (define (get-field-special_ exp env)
//   (define field
//     (second exp))
//   (define field-name
//     (symbol->string field))
//   (define obj
//     (third exp))
//   (~> (eval_ obj env)
//       (oget _ field-name)))
//
// ;;; Evaluate a `(js/optional-chaining ...)` form.
// (define (js/optional-chaining-special_ exp env)
//   (define obj
//     (second exp))
//   (define field
//     (third exp))
//   (define field-name
//     (symbol->string field))
//   (~> (eval_ obj env)
//       (oget _ field-name)))
//
// ;;; Evaluate a `(set-field! ...)` form.
// (define (set-field-special_ exp env)
//   (define field
//     (second exp))
//   (define obj
//     (third exp))
//   (define val
//     (fourth exp))
//   (oset! (eval_ obj env)
//          (symbol->string field)
//          (eval_ val env)))
//
// ;;; Evaluate a `(define-class ...)` form.
// (define (define-class-special_ exp env)
//   (define fields '())
//   (define methods '())
//   (define constructors
//     (make-hash))
//   (define constructor
//     (lambda (this . args)
//       ;; Initialize fields.
//       (for ((field fields))
//         (define name
//           (symbol->string (second field)))
//         (define exp
//           (third field))
//         (set! (oget this name)
//               (eval_ `(let ((this ',this))
//                         ,exp)
//                      env)))
//       (define arity
//         (length args))
//       (define constructor-fn
//         (send constructors get arity))
//       (when (procedure? constructor-fn)
//         (send/apply constructor-fn call this args))))
//   (define params
//     (rest exp))
//   (define definitions params)
//   ;; Get name of the class and, if specified, the base class it extends.
//   ;; If no class name is specified, an anonymous class is created.
//   (define class-name-symbol
//     (first params))
//   (define class-name
//     (if (symbol? class-name-symbol)
//         (symbol->string class-name-symbol)
//         ""))
//   (define base-class #u)
//   (unless (eq? class-name "")
//     (set! definitions (rest definitions)))
//   (define super-classes
//     (first definitions))
//   (when (and (list? super-classes)
//              (not (tagged-list? super-classes 'define)))
//     ;; The first form is a list of superclasses.
//     (set! definitions (rest definitions))
//     ;; JavaScript supports single inheritance only,
//     ;; so only the first class is used.
//     (when (> (length super-classes) 0)
//       (set! base-class (eval_ (first super-classes) env))))
//   (when base-class
//     ;; <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
//     (set! (oget constructor :prototype)
//           (send Object create (oget base-class :prototype))))
//   ;; Sort field definitions from method definitions.
//   (for ((definition definitions))
//     (if (symbol? (second definition))
//         (push-right! fields definition)
//         (push-right! methods definition)))
//   (for ((method methods))
//     (define def-params
//       (rest method))
//     (define def-name-and-args
//       (first def-params))
//     (define def-name
//       (symbol->string
//        (first def-name-and-args)))
//     (define def-args
//       (rest def-name-and-args))
//     (define def-body
//       (rest def-params))
//     (define arity
//       (length def-args))
//     ;; Create a method function that binds JavaScript's
//     ;; `this` value to the Lisp symbol `this`.
//     (define method-fn
//       (lambda (this . args)
//         (define var-exps '())
//         (for ((i (range 0 arity)))
//           (define arg-exp
//             (list-ref def-args i))
//           (define name
//             (if (list? arg-exp)
//                 (first arg-exp)
//                 arg-exp))
//           (define value
//             (if (>= i (length args))
//                 #u
//                 (list-ref args i)))
//           (define var-exp
//             `(,name ',value))
//           (push-right! var-exps var-exp))
//         (define this-exp
//           `(this ',this))
//         (push-right! var-exps this-exp)
//         (define let-exp
//           `(let* ,var-exps
//              ,@def-body))
//         (eval_ let-exp env)))
//     ;; A form on the form `(define (constructor ...) ...)`
//     ;; or `(define/public (constructor ...) ...)` is
//     ;; understood to define a constructor.
//     ;; Cf. the constructor syntax of TypeScript:
//     ;; <https://www.typescriptlang.org/docs/handbook/2/classes.html#constructors>
//     (if (or (eq? def-name "constructor")
//             (eq? def-name class-name))
//         (send constructors set arity method-fn)
//         (set! (oget (get-field prototype constructor) def-name)
//               method-fn)))
//   (unless (eq? class-name "")
//     (send env set! class-name-symbol constructor 'Any))
//   constructor)
//
// ;;; Evaluate a `(try ...)` form.
// (define (try-special_ exp env)
//   (define body-clauses '())
//   (define catch-clauses '())
//   (define finally-clauses '())
//   (define result #u)
//   (define body)
//   (for ((x (drop exp 1)))
//     (cond
//      ((tagged-list? x 'catch)
//       (push-right! catch-clauses x))
//      ((tagged-list? x 'finally)
//       (push-right! finally-clauses x))
//      (else
//       (push-right! body-clauses x))))
//   (set! body
//         (if (= (length body-clauses) 1)
//             (first body-clauses)
//             `(begin ,@body-clauses)))
//   (try
//     (set! result (eval_ body env))
//     (catch Object err
//       (for ((clause catch-clauses))
//         (when (is-a? err (eval_ (second clause) env))
//           (set! result
//                 (eval_ `(let ((,(third clause) ',err))
//                           ,@(drop clause 3))
//                        env))
//           (break))))
//     (finally
//       (when (> (length finally-clauses) 0)
//         (eval_ `(begin
//                   ,@(drop (first finally-clauses) 1))
//                env))))
//   result)
//
// ;;; Evaluate a `(provide ...)` form.
// (define (provide-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(require ...)` form.
// (define (require-special_ exp env)
//   #u)
//
// ;;; Evaluate an `(ann ...)` form.
// (define (ann-special_ exp env)
//   (eval_ (second exp) env))
//
// ;;; Evaluate a `(colon ...)` form.
// (define (colon-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(define-type ...)` form.
// (define (define-type-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(let-fields ...)` form.
// (define (let-fields-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(define-fields ...)` form.
// (define (define-fields-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(set!-fields ...)` form.
// (define (set-fields-special_ exp env)
//   #u)
//
// ;;; Evaluate a `(let-env ...)` form.
// (define (let-env-special_ exp env)
//   ;; Legacy function.
//   (define params
//     (rest exp))
//   (define body
//     (rest params))
//   (define let-env
//     (eval_ (first params) env))
//   (define combined-env
//     (new EnvironmentStack let-env env))
//   (eval_ `(begin ,@body) combined-env))
//
// ;;; Evaluate a `(macrop ...)` form.
// ;;;
// ;;; Whether `macro` is a macro in the environment `env`.
// ;;; `macro` may be a symbol, a string, or a macro function.
// (define (macrop-special_ val)
//   (eval_ `(macrop ,val) (current-environment)))
//
// ;;; Evaluate an `(nlambda ...)` form.
// (define (nlambda-special_ exp env)
//   (define f
//     (lambda-special_ exp env))
//   (set-field! fexpr f #t)
//   f)

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
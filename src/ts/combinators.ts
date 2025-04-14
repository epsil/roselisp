// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Combinators
 *
 * Combinatory logic.
 *
 * ## Description
 *
 * A simple combinator library.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  __,
  curryN,
  dashify
} from './curry';

/**
 * **A1** combinator.
 *
 *     A1 f x = f x
 *
 * This is a function application combinator that accepts a
 * single argument.
 *
 * See also {@link A2 **A2**}.
 */
function A1(f: any, x: any): any {
  return f(x);
}

A1.lispSource = [Symbol.for('define'), [Symbol.for('A1'), Symbol.for('f'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]];

/**
 * **A2** combinator.
 *
 *     A2 f = f
 *     A2 f x = f x
 *     A2 f x y = f x y
 *     A2 f x y z = f x y z
 *
 * This is a variadic function application combinator that accepts
 * any number of arguments.
 *
 * See also {@link A1 **A1**}.
 */
function A2(f: any, ...args: any[]): any {
  return f(...args);
}

A2.lispSource = [Symbol.for('define'), [Symbol.for('A2'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]];

/**
 * Curried **A1** combinator.
 */
const A1c: any = curryN(2, A1);

/**
 * Partially applicable **A2** combinator.
 */
const A2c: any = dashify(A2, __);

/**
 * **A** combinator.
 *
 *     A f = f
 *     A f x = f x
 *     A f x y = f x y
 *     A f x y z = f x y z
 *
 * The special Ramda value [`R.__`][r:dash], here written as `_`,
 * can be used to partially apply any function:
 *
 *     A add _ 1 = λx. add x 1
 *
 * This combinator is also known as [`R.call()`][r:call],
 * [`funcall()`][cl:funcall] and [`$`][hs:dollar]. The JavaScript
 * equivalent is [`Function.prototype.call()`][js:call]. A similar
 * function is [`apply()`][cl:apply], which corresponds to
 * [`Function.prototype.apply()`][js:apply].
 *
 * [r:call]: https://ramdajs.com/docs/#call
 * [r:dash]: https://ramdajs.com/docs/#__
 * [cl:funcall]: http://clhs.lisp.se/Body/f_funcal.htm
 * [hs:dollar]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:-36-
 * [js:call]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/call
 * [cl:apply]: http://clhs.lisp.se/Body/f_apply.htm
 * [js:apply]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/apply
 */
const A: any = A2c;

/**
 * **B1** combinator.
 *
 *     B1 f g x = f (g x)
 *
 * It can be considered a right-to-left function composition
 * combinator with function application "baked in".
 *
 * This combinator is also known as [**Z**][paper:Schonfinkel24] and
 * [`bluebird()`][hs:bluebird]. See {@link Q1 **Q1**} for the
 * left-to-right version.
 *
 * See also {@link B2 **B2**}.
 *
 * [paper:Schonfinkel24]: https://content.wolfram.com/uploads/sites/43/2020/12/Schonfinkel-OnTheBuildingBlocksOfMathematicalLogic.pdf
 * [hs:bluebird]: https://hackage.haskell.org/package/data-aviary/docs/Data-Aviary-Birds.html#v:bluebird
 */
function B1(f: any, g: any, x: any): any {
  return f(g(x));
}

B1.lispSource = [Symbol.for('define'), [Symbol.for('B1'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]]];

/**
 * **B2** combinator.
 *
 *     B2 x = x
 *     B2 f x = f x
 *     B2 f g x = f (g x)
 *     B2 f g h x = f (g (h x))
 *
 * This is a variadic version of the **B** combinator that has been
 * generalized to accept any number of arguments.
 *
 * Right-to-left function composition is the type of composition that
 * is commonly encountered in the literature. By convention, the term
 * "function composition" denotes right-to-left composition unless
 * otherwise specified. It corresponds to the Ramda function
 * [`compose()`][r:compose]. One can derive `compose` from **B** by
 * means of partial application:
 *
 *     compose f g h = B f g h _
 *
 * Here, `_` is [`R.__`][r:dash], a placeholder value for partial
 * application.
 *
 * See also {@link B1 **B1**}.
 *
 * [r:compose]: https://ramdajs.com/docs/#compose
 * [r:dash]: https://ramdajs.com/docs/#__
 */
function B2(...args: any[]): any {
  switch (args.length) {
    case 0: {
      return undefined;
      break;
    }
    case 1: {
      return args[0];
      break;
    }
    default: {
      const fs: any = args.slice(0, -1);
      const x: any = args[args.length - 1];
      // Right-to-left function composition
      // corresponds to a right fold.
      return fs.reduceRight(function (acc: any, x: any): any {
        return A(x, acc);
      }, x);
    }
  }
}

B2.lispSource = [Symbol.for('define'), [Symbol.for('B2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('fs'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('foldr'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];

/**
 * Curried **B1** combinator.
 */
const B1c: any = curryN(3, B1);

/**
 * Partially applicable **B2** combinator.
 */
const B2c: any = dashify(B2, __);

/**
 * **B** combinator.
 *
 *     B x = x
 *     B f x = f x
 *     B f g x = f (g x)
 *     B f g h x = f (g (h x))
 *
 * This is a variadic **B** combinator that has been generalized to
 * accept any number of arguments.
 *
 * It can be considered a right-to-left function composition
 * combinator with function application "baked in".
 *
 * This combinator is also known as [**Z**][paper:Schonfinkel24] and
 * [`bluebird()`][hs:bluebird]. See {@link Q **Q**} for the
 * left-to-right version.
 *
 * [paper:Schonfinkel24]: https://content.wolfram.com/uploads/sites/43/2020/12/Schonfinkel-OnTheBuildingBlocksOfMathematicalLogic.pdf
 * [hs:bluebird]: https://hackage.haskell.org/package/data-aviary/docs/Data-Aviary-Birds.html#v:bluebird
 */
const B: any = B2c;

/**
 * **C1** combinator.
 *
 *     C1 f x y = f y x
 *
 * Swaps the arguments of a binary function. Also known as
 * [`flip()`][hs:flip].
 *
 * See also {@link C2 **C2**}.
 *
 * [hs:flip]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:flip
 */
function C1(f: any, x: any, y: any): any {
  return f(y, x);
}

C1.lispSource = [Symbol.for('define'), [Symbol.for('C1'), Symbol.for('f'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]];

/**
 * **C2** combinator.
 *
 *     C2 f x = f x
 *     C2 f x y = f y x
 *     C2 f x y z = f y z x
 *     C2 f x y z w = f y z w x
 *
 * Rotates the arguments of a variadic function.
 *
 * See also {@link C3 **C3**}.
 */
function C2(f: any, ...args: any[]): any {
  if (args.length < 2) {
    return f(...args);
  } else {
    return f(...[...args.slice(1), args[0]]);
  }
}

C2.lispSource = [Symbol.for('define'), [Symbol.for('C2'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('args')]], [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('args')]]]]]]]];

/**
 * **C3** combinator.
 *
 *     C3 f x = f x
 *     C3 f x y = f y x
 *     C3 f x y z = f z x y
 *     C3 f x y z w = f w x y z
 *
 * Rotates the arguments of a variadic function.
 *
 * See also {@link C2 **C2**}.
 */
function C3(f: any, ...args: any[]): any {
  if (args.length < 2) {
    return f(...args);
  } else {
    return f(...[args[args.length - 1], ...args.slice(0, -1)]);
  }
}

C3.lispSource = [Symbol.for('define'), [Symbol.for('C3'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop-right'), Symbol.for('args'), 1]]]]]]]];

/**
 * Curried **C1** combinator.
 */
const C1c: any = curryN(3, C1);

/**
 * Partially applicable **C2** combinator.
 */
const C2c: any = dashify(C2, __);

/**
 * Partially applicable **C3** combinator.
 */
const C3c: any = dashify(C3, __);

/**
 * **C** combinator.
 *
 *     C f x y = f y x
 *
 * Swaps the arguments of a binary function. Also known as
 * [`flip()`][hs:flip].
 *
 * [hs:flip]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:flip
 */
const C: any = C1c;

/**
 * **I1** combinator.
 *
 *     I1 x = x
 *
 * The identity function, implemented plainly. Also known as
 * [`identity()`][rkt:identity] and [`id()`][hs:id].
 *
 * [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
 * [hs:id]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:id
 */
function I1(x: any): any {
  return x;
}

I1.lispSource = [Symbol.for('define'), [Symbol.for('I1'), Symbol.for('x')], Symbol.for('x')];

/**
 * **I2** combinator.
 *
 *     I1 x = x
 *     I2 x y = x
 *     I2 x y z = x
 *
 * The identity function, implemented as a variadic function.
 */
function I2(x: any, ...args: any[]): any {
  return x;
}

I2.lispSource = [Symbol.for('define'), [Symbol.for('I2'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')];

/**
 * Curried **I1** combinator.
 */
const I1c: any = curryN(1, I1);

/**
 * Partially applicable **I2** combinator.
 */
const I2c: any = dashify(I2, __);

/**
 * **I** combinator.
 *
 *     I x = x
 *
 * The identity function. Permits partial application with
 * [`R.__`][r:dash]. Also known as [`identity()`][rkt:identity]
 * and [`id()`][hs:id].
 *
 * [r:dash]: https://ramdajs.com/docs/#__
 * [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
 * [hs:id]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:id
 */
const I: any = I2c;

/**
 * **K1** combinator.
 *
 *     K1 x y = x
 *
 * See also {@link K2 **K2**}.
 */
function K1(x: any, y: any): any {
  return x;
}

K1.lispSource = [Symbol.for('define'), [Symbol.for('K1'), Symbol.for('x'), Symbol.for('y')], Symbol.for('x')];

/**
 * **K2** combinator.
 *
 *     K2 x = x
 *     K2 x y = x
 *     K2 x y z = x
 *
 * See also {@link K1 **K1**}.
 */
function K2(x: any, ...args: any[]): any {
  return x;
}

K2.lispSource = [Symbol.for('define'), [Symbol.for('K2'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')];

/**
 * Curried **K1** combinator.
 */
const K1c: any = curryN(2, K1);

/**
 * Partially applicable **K2** combinator.
 */
const K2c: any = dashify(K2);

/**
 * **K** combinator.
 *
 *     K x y = x
 *
 * In Henry G. Baker's memorable phrase, "The **S** combinator
 * cheerfully copies ... the **K** combinator knowingly kills."
 * (From ["NREVERSAL of Fortune---the Thermodynamics of Garbage
 * Collection"][paper:Baker92], 1992.)
 *
 * Also known as [`const()`][hs:const].
 *
 * [paper:Baker92]: https://www.semanticscholar.org/paper/NREVERSAL-of-Fortune-The-Thermodynamics-of-Garbage-Baker/4248073bcdb7c0ed9af9f93f8048ddc0c9f01966
 * [hs:const]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:const
 */
const K: any = K1c;

/**
 * **Ki1** combinator.
 *
 *     Ki1 x y = y
 *
 * This is a flipped version of {@link K1 **K1**}, which returns the
 * second argument rather than the first. It may be derived from **K1**
 * by means of {@link C **C**}:
 *
 *     Ki1 = C K1
 *
 * Also known as [`kite()`][hs:kite].
 *
 * [hs:kite]: https://hackage.haskell.org/package/data-aviary/docs/Data-Aviary-Birds.html#v:kite
 */
function Ki1(x: any, y: any): any {
  return y;
}

Ki1.lispSource = [Symbol.for('define'), [Symbol.for('Ki1'), Symbol.for('x'), Symbol.for('y')], Symbol.for('y')];

/**
 * Curried **Ki** combinator.
 */
const Ki1c: any = curryN(2, Ki1);

/**
 * **Ki** combinator.
 *
 *     Ki x y = y
 *
 * This is a flipped version of {@link K **K**}, which returns the
 * second argument rather than the first. It may be derived from **K**
 * by means of {@link C **C**}:
 *
 *     Ki = C K
 *
 * It is also known as [`kite()`][hs:kite].
 *
 * [hs:kite]: https://hackage.haskell.org/package/data-aviary/docs/Data-Aviary-Birds.html#v:kite
 */
const Ki: any = Ki1c;

/**
 * **Q1** combinator.
 *
 *     Q1 f g x = g (f x)
 *
 * Similar to {@link T1 **T1**}, except that the `x` argument comes
 * last.
 *
 * See also {@link Q2 **Q2**} and {@link B1 **B1**}.
 */
function Q1(f: any, g: any, x: any): any {
  return g(f(x));
}

Q1.lispSource = [Symbol.for('define'), [Symbol.for('Q1'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [Symbol.for('g'), [Symbol.for('f'), Symbol.for('x')]]];

/**
 * **Q2** combinator.
 *
 * Variadic combinator which accepts any number of arguments:
 *
 *     Q2 x = x
 *     Q2 f x = f x
 *     Q2 f g x = g (f x)
 *     Q2 f g h x = h (g (f x))
 *
 * Similar to the Ramda function [`pipe()`][r:pipe]. One can derive
 * `pipe` from **Q2** by means of partial application:
 *
 *     pipe f g h = Q2 f g h _
 *
 * Here, `_` is [`R.__`][r:dash], a placeholder value for partial
 * application.
 *
 * See also {@link Q1 **Q1**} and {@link B2 **B2**}.
 *
 * [r:pipe]: https://ramdajs.com/docs/#pipe
 * [r:dash]: https://ramdajs.com/docs/#__
 */
function Q2(...args: any[]): any {
  switch (args.length) {
    case 0: {
      return undefined;
      break;
    }
    case 1: {
      return args[0];
      break;
    }
    default: {
      const fs: any = args.slice(0, -1);
      const x: any = args[args.length - 1];
      // Left-to-right function composition
      // corresponds to a left fold.
      return fs.reduce(function (acc: any, x: any): any {
        return A(x, acc);
      }, x);
    }
  }
}

Q2.lispSource = [Symbol.for('define'), [Symbol.for('Q2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('fs'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('foldl'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];

/**
 * Curried **Q1** combinator.
 */
const Q1c: any = curryN(3, Q1);

/**
 * Partially applicable **Q2** combinator.
 */
const Q2c: any = dashify(Q2, __);

/**
 * **Q** combinator.
 *
 * Variadic combinator which accepts any number of arguments:
 *
 *     Q x = x
 *     Q f x = f x
 *     Q f g x = g (f x)
 *     Q f g h x = h (g (f x))
 *
 * Left-to-right function composition is a less common form of
 * function composition that is sometimes useful in a programming
 * context. It corresponds to the Ramda function [`pipe()`][r:pipe].
 * One can derive `pipe` from **Q** by mean of partial application:
 *
 *     pipe f g h = Q f g h _
 *
 * Here, `_` is [`R.__`][r:dash], a placeholder value for partial
 * application.
 *
 * See also {@link B **B**} and {@link T **T**}.
 *
 * [r:pipe]: https://ramdajs.com/docs/#pipe
 * [r:dash]: https://ramdajs.com/docs/#__
 */
const Q: any = Q2c;

/**
 * **S1** combinator.
 *
 *     S1 f g x = (f x) (g x)
 *
 * Classical **S** combinator as defined by Moses Schönfinkel in
 * ["On the Building Blocks of Mathematical Logic"
 * (1924)][paper:Schonfinkel24].
 *
 * See also {@link S2 **S2**}.
 *
 * [paper:Schonfinkel24]: https://content.wolfram.com/uploads/sites/43/2020/12/Schonfinkel-OnTheBuildingBlocksOfMathematicalLogic.pdf
 */
function S1(f: any, g: any, x: any): any {
  return f(x)(g(x));
}

S1.lispSource = [Symbol.for('define'), [Symbol.for('S1'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [[Symbol.for('f'), Symbol.for('x')], [Symbol.for('g'), Symbol.for('x')]]];

/**
 * **S2** combinator.
 *
 *     S2 f g x = f x (g x)
 *
 * Alternate **S** combinator as defined by some software libraries
 * (e.g., Haskell's [Data.Aviary.Birds][hs:data.aviary.birds:starling]).
 *
 * See also {@link S1 **S1**}.
 *
 * [hs:data.aviary.birds:starling]: https://hackage.haskell.org/package/data-aviary/docs/Data-Aviary-Birds.html#v:starling
 */
function S2(f: any, g: any, x: any): any {
  return f(x, g(x));
}

S2.lispSource = [Symbol.for('define'), [Symbol.for('S2'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x'), [Symbol.for('g'), Symbol.for('x')]]];

/**
 * Curried **S1** combinator.
 */
const S1c: any = curryN(3, S1);

/**
 * Curried **S2** combinator.
 */
const S2c: any = curryN(3, S2);

/**
 * **S** combinator.
 *
 *     S f g x = (f x) (g x)
 *
 * Classical **S** combinator as defined by Moses Schönfinkel in
 * the paper ["On the Building Blocks of Mathematical
 * Logic"][paper:Schonfinkel24].
 *
 * See also {@link I **I**} and {@link K **K**}. Together, they form
 * the building blocks of the [**SKI** combinator calculus][w:SKI],
 * which can be thought of as an extremely simple Turing-complete
 * language.
 *
 * [paper:Schonfinkel24]: https://content.wolfram.com/uploads/sites/43/2020/12/Schonfinkel-OnTheBuildingBlocksOfMathematicalLogic.pdf
 * [w:SKI]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
 */
const S: any = S1c;

/**
 * **T1** combinator.
 *
 *     T1 x f = f x
 *
 * See also {@link T2 **T2**} and {@link Q1 **Q1**}.
 */
function T1(x: any, f: any): any {
  return f(x);
}

T1.lispSource = [Symbol.for('define'), [Symbol.for('T1'), Symbol.for('x'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('x')]];

/**
 * **T2** combinator.
 *
 *     T2 x = x
 *     T2 x f = f x
 *     T2 x f g = g (f x)
 *     T2 x f g h = h (g (f x))
 *
 * Variadic combinator which accepts any number of arguments.
 *
 * See also {@link T1 **T1**}. See also {@link Q2 **Q2**}, which is
 * similar to this combinator except that the `x` argument comes last.
 */
function T2(...args: any[]): any {
  switch (args.length) {
    case 0: {
      return undefined;
      break;
    }
    case 1: {
      return args[0];
      break;
    }
    default: {
      const [x, ...fs]: any[] = args;
      return fs.reduce(function (acc: any, x: any): any {
        return A(x, acc);
      }, x);
    }
  }
}

T2.lispSource = [Symbol.for('define'), [Symbol.for('T2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')], Symbol.for('args')], [Symbol.for('foldl'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];

/**
 * Curried **T1** combinator.
 */
const T1c: any = curryN(2, T1);

/**
 * Partially applicable **T2** combinator.
 */
const T2c: any = dashify(T2, __);

/**
 * **T** combinator.
 *
 *     T x = x
 *     T x f = f x
 *     T x f g = g (f x)
 *     T x f g h = h (g (f x))
 *
 * Variadic combinator which accepts any number of arguments.
 *
 * It also supports partial application. For example, the similar
 * Ramda function [`pipe()`][r:pipe] can be derived from **T** with:
 *
 *     pipe f g h = T _ f g h
 *
 * Here, `_` is [`R.__`][r:dash], a placeholder value for partial
 * application.
 *
 * See also {@link Q **Q**} and {@link B **B**}.
 *
 * [r:pipe]: https://ramdajs.com/docs/#pipe
 * [r:dash]: https://ramdajs.com/docs/#__
 */
const T: any = T2c;

/**
 * **U1** combinator.
 *
 *     U1 f = f f
 *
 * Not to be confused with {@link UT1 **UT1**}.
 */
function U1(f: any): any {
  return f(f);
}

U1.lispSource = [Symbol.for('define'), [Symbol.for('U1'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('f')]];

/**
 * Curried **U1** combinator.
 */
const U1c: any = curryN(1, U1);

/**
 * **U** combinator.
 *
 *     U f = f f
 *
 * The [**U** combinator][www:ucombinator] applies a function to
 * itself. Not to be confused with {@link UT **UT**}, the
 * "Turing bird".
 *
 * [www:ucombinator]: https://www.ucombinator.org/
 */
const U: any = U1c;

/**
 * **UT1** combinator.
 *
 *     UT1 x y = y (x x y)
 *
 * Also known as the "Turing bird" combinator, as it is named in
 * Raymond M. Smullyan's book [*To Mock a Mockingbird and Other Logic
 * Puzzles*][book:Smullyan85]. It is attributed to Alan Turing, who is
 * said to have discovered it in 1937. The definition given in the
 * book is `Uxy = y(xxy)`.
 *
 * It is here named **UT1** to avoid confusion with {@link U1 **U1**}.
 *
 * [book:Smullyan85]: https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
 */
function UT1(x: any, y: any): any {
  return y(x(x, y));
}

UT1.lispSource = [Symbol.for('define'), [Symbol.for('UT1'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('y'), [Symbol.for('x'), Symbol.for('x'), Symbol.for('y')]]];

/**
 * Curried **UT1** combinator.
 */
const UT1c: any = curryN(2, UT1);

/**
 * **UT** combinator.
 *
 *     UT x y = y (x x y)
 *
 * Also known as the "Turing bird" combinator, as it is named in
 * Raymond M. Smullyan's book [*To Mock a Mockingbird and Other Logic
 * Puzzles*][book:Smullyan85]. It is attributed to Alan Turing, who is
 * said to have discovered it in 1937.
 *
 * It is here named **UT** to avoid confusion with the
 * {@link U **U** combinator}.
 *
 * [book:Smullyan85]: https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
 */
const UT: any = UT1c;

/**
 * **Y1** combinator.
 *
 *     Y1 f = (λx. f (λa. (x x) a))
 *            (λx. f (λa. (x x) a))
 *
 * The definition used here is adapted from the one found in the book
 * [*The Little LISPer*][book:Friedman74] by Daniel P. Friedman and
 * Matthias Felleisen.
 *
 * [book:Friedman74]: https://www.goodreads.com/book/show/734117.The_Little_LISPer
 */
function Y1(f: any): any {
  // The names of the function arguments (`future`, `arg`)
  // are the same as in the book *The Little LISPer*, with
  // the exception of `f`, which is named `M` in the book.
  return (function (future: any): any {
    return f(function (arg: any): any {
      return future(future)(arg);
    });
  })(function (future: any): any {
    return f(function (arg: any): any {
      return future(future)(arg);
    });
  });
}

Y1.lispSource = [Symbol.for('define'), [Symbol.for('Y1'), Symbol.for('f')], [[Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]], [Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]]]];

/**
 * Curried **Y1** combinator.
 */
const Y1c: any = curryN(1, Y1);

/**
 * **Y** combinator.
 *
 *     Y f = (λx. f (λa. (x x) a))
 *           (λx. f (λa. (x x) a))
 *
 * The classical definition of **Y** does not work in JavaScript
 * because of strict evaluation:
 *
 *     Y f = (λx. f (x x))
 *           (λx. f (x x))
 *
 * Therefore, we use the first definition, which delays the
 * application. Both definitions obey the property:
 *
 *     Y f = f (Y f)
 *
 * The **Y** combinator allows one to implement recursion in a
 * language without native support for it. The use of this combinator
 * to recursively compute the factorial of 6 might look like:
 *
 *     const factorialY = Y(function (factorial) {
 *       return function (x) {
 *         if (x === 0) {
 *           return 1;
 *         } else {
 *           return x * factorial(x - 1);
 *         }
 *       };
 *     });
 *     const factorialOfSix = factorialY(6); // 6!
 *
 * As noted, there are several definitions of **Y**. The definition
 * used here is adapted from the one found in the book [*The Little
 * LISPer*][book:Friedman74] by Daniel P. Friedman and Matthias
 * Felleisen. See also the article ["Fixed-point combinators in
 * JavaScript: Memoizing recursive functions"][article:Might08] by
 * Matt Might.
 *
 * [book:Friedman74]: https://www.goodreads.com/book/show/734117.The_Little_LISPer
 * [article:Might08]: https://matt.might.net/articles/implementation-of-recursive-fixed-point-y-combinator-in-javascript-for-memoization/
 */
const Y: any = Y1c;

const curried: any = {
  A: A1c,
  B: B1c,
  C: C1c,
  I: I1c,
  K: K1c,
  Ki: Ki1c,
  Q: Q1c,
  S: S1c,
  T: T1c,
  U: U1c,
  UT: UT1c,
  Y: Y1c
};

const variadic: any = {
  A: A2c,
  B: B2c,
  C: C2c,
  I: I2c,
  K: K2c,
  Ki: Ki1c,
  Q: Q2c,
  S: S1c,
  T: T2c,
  U: U1c,
  UT: UT1c,
  Y: Y1c
};

export {
  __,
  __ as _,
  curried,
  variadic,
  A,
  A1,
  A1c,
  A2,
  A2c,
  B,
  B1,
  B1c,
  B2,
  B2c,
  C,
  C1,
  C2,
  C3,
  C1c,
  C2c,
  C3c,
  I,
  I1,
  I1c,
  K,
  K1,
  K1c,
  K2,
  K2c,
  Ki,
  Ki1,
  Ki1c,
  Q,
  Q1,
  Q1c,
  Q2,
  Q2c,
  S,
  S1,
  S1c,
  S2,
  S2c,
  T,
  T1,
  T1c,
  T2,
  T2c,
  U,
  U1,
  U1c,
  UT,
  UT1,
  UT1c,
  Y,
  Y1,
  Y1c
};
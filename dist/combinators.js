"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.U1c = exports.U1 = exports.U = exports.T2c = exports.T2 = exports.T1c = exports.T1 = exports.T = exports.S2c = exports.S2 = exports.S1c = exports.S1 = exports.S = exports.Q2c = exports.Q2 = exports.Q1c = exports.Q1 = exports.Q = exports.Ki1c = exports.Ki1 = exports.Ki = exports.K2c = exports.K2 = exports.K1c = exports.K1 = exports.K = exports.I1c = exports.I1 = exports.I = exports.C3c = exports.C2c = exports.C1c = exports.C3 = exports.C2 = exports.C1 = exports.C = exports.B2c = exports.B2 = exports.B1c = exports.B1 = exports.B = exports.A2c = exports.A2 = exports.A1c = exports.A1 = exports.A = exports.variadic = exports.curried = exports._ = exports.__ = void 0;
exports.Y1c = exports.Y1 = exports.Y = exports.UT1c = exports.UT1 = exports.UT = void 0;
const curry_1 = require("./curry");
Object.defineProperty(exports, "__", { enumerable: true, get: function () { return curry_1.__; } });
Object.defineProperty(exports, "_", { enumerable: true, get: function () { return curry_1.__; } });
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
function A1(f, x) {
    return f(x);
}
exports.A1 = A1;
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
function A2(f, ...args) {
    return f(...args);
}
exports.A2 = A2;
A2.lispSource = [Symbol.for('define'), [Symbol.for('A2'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]];
/**
 * Curried **A1** combinator.
 */
const A1c = (0, curry_1.curryN)(2, A1);
exports.A1c = A1c;
/**
 * Partially applicable **A2** combinator.
 */
const A2c = (0, curry_1.dashify)(A2, curry_1.__);
exports.A2c = A2c;
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
const A = A2c;
exports.A = A;
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
function B1(f, g, x) {
    return f(g(x));
}
exports.B1 = B1;
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
function B2(...args) {
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
            const fs = args.slice(0, -1);
            const x = args[args.length - 1];
            // Right-to-left function composition
            // corresponds to a right fold.
            return fs.reduceRight(function (acc, x) {
                return A(x, acc);
            }, x);
        }
    }
}
exports.B2 = B2;
B2.lispSource = [Symbol.for('define'), [Symbol.for('B2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('fs'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('foldr'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];
/**
 * Curried **B1** combinator.
 */
const B1c = (0, curry_1.curryN)(3, B1);
exports.B1c = B1c;
/**
 * Partially applicable **B2** combinator.
 */
const B2c = (0, curry_1.dashify)(B2, curry_1.__);
exports.B2c = B2c;
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
const B = B2c;
exports.B = B;
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
function C1(f, x, y) {
    return f(y, x);
}
exports.C1 = C1;
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
function C2(f, ...args) {
    if (args.length < 2) {
        return f(...args);
    }
    else {
        return f(...[...args.slice(1), args[0]]);
    }
}
exports.C2 = C2;
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
function C3(f, ...args) {
    if (args.length < 2) {
        return f(...args);
    }
    else {
        return f(...[args[args.length - 1], ...args.slice(0, -1)]);
    }
}
exports.C3 = C3;
C3.lispSource = [Symbol.for('define'), [Symbol.for('C3'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop-right'), Symbol.for('args'), 1]]]]]]]];
/**
 * Curried **C1** combinator.
 */
const C1c = (0, curry_1.curryN)(3, C1);
exports.C1c = C1c;
/**
 * Partially applicable **C2** combinator.
 */
const C2c = (0, curry_1.dashify)(C2, curry_1.__);
exports.C2c = C2c;
/**
 * Partially applicable **C3** combinator.
 */
const C3c = (0, curry_1.dashify)(C3, curry_1.__);
exports.C3c = C3c;
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
const C = C1c;
exports.C = C;
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
function I1(x) {
    return x;
}
exports.I1 = I1;
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
function I2(x, ...args) {
    return x;
}
I2.lispSource = [Symbol.for('define'), [Symbol.for('I2'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')];
/**
 * Curried **I1** combinator.
 */
const I1c = (0, curry_1.curryN)(1, I1);
exports.I1c = I1c;
/**
 * Partially applicable **I2** combinator.
 */
const I2c = (0, curry_1.dashify)(I2, curry_1.__);
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
const I = I2c;
exports.I = I;
/**
 * **K1** combinator.
 *
 *     K1 x y = x
 *
 * See also {@link K2 **K2**}.
 */
function K1(x, y) {
    return x;
}
exports.K1 = K1;
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
function K2(x, ...args) {
    return x;
}
exports.K2 = K2;
K2.lispSource = [Symbol.for('define'), [Symbol.for('K2'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')];
/**
 * Curried **K1** combinator.
 */
const K1c = (0, curry_1.curryN)(2, K1);
exports.K1c = K1c;
/**
 * Partially applicable **K2** combinator.
 */
const K2c = (0, curry_1.dashify)(K2);
exports.K2c = K2c;
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
const K = K1c;
exports.K = K;
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
function Ki1(x, y) {
    return y;
}
exports.Ki1 = Ki1;
Ki1.lispSource = [Symbol.for('define'), [Symbol.for('Ki1'), Symbol.for('x'), Symbol.for('y')], Symbol.for('y')];
/**
 * Curried **Ki** combinator.
 */
const Ki1c = (0, curry_1.curryN)(2, Ki1);
exports.Ki1c = Ki1c;
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
const Ki = Ki1c;
exports.Ki = Ki;
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
function Q1(f, g, x) {
    return g(f(x));
}
exports.Q1 = Q1;
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
function Q2(...args) {
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
            const fs = args.slice(0, -1);
            const x = args[args.length - 1];
            // Left-to-right function composition
            // corresponds to a left fold.
            return fs.reduce(function (acc, x) {
                return A(x, acc);
            }, x);
        }
    }
}
exports.Q2 = Q2;
Q2.lispSource = [Symbol.for('define'), [Symbol.for('Q2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('fs'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('foldl'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];
/**
 * Curried **Q1** combinator.
 */
const Q1c = (0, curry_1.curryN)(3, Q1);
exports.Q1c = Q1c;
/**
 * Partially applicable **Q2** combinator.
 */
const Q2c = (0, curry_1.dashify)(Q2, curry_1.__);
exports.Q2c = Q2c;
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
const Q = Q2c;
exports.Q = Q;
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
function S1(f, g, x) {
    return f(x)(g(x));
}
exports.S1 = S1;
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
function S2(f, g, x) {
    return f(x, g(x));
}
exports.S2 = S2;
S2.lispSource = [Symbol.for('define'), [Symbol.for('S2'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x'), [Symbol.for('g'), Symbol.for('x')]]];
/**
 * Curried **S1** combinator.
 */
const S1c = (0, curry_1.curryN)(3, S1);
exports.S1c = S1c;
/**
 * Curried **S2** combinator.
 */
const S2c = (0, curry_1.curryN)(3, S2);
exports.S2c = S2c;
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
const S = S1c;
exports.S = S;
/**
 * **T1** combinator.
 *
 *     T1 x f = f x
 *
 * See also {@link T2 **T2**} and {@link Q1 **Q1**}.
 */
function T1(x, f) {
    return f(x);
}
exports.T1 = T1;
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
function T2(...args) {
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
            const [x, ...fs] = args;
            return fs.reduce(function (acc, x) {
                return A(x, acc);
            }, x);
        }
    }
}
exports.T2 = T2;
T2.lispSource = [Symbol.for('define'), [Symbol.for('T2'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('case'), [Symbol.for('array-list-length'), Symbol.for('args')], [[0], Symbol.for('undefined')], [[1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')], Symbol.for('args')], [Symbol.for('foldl'), Symbol.for('A'), Symbol.for('x'), Symbol.for('fs')]]]];
/**
 * Curried **T1** combinator.
 */
const T1c = (0, curry_1.curryN)(2, T1);
exports.T1c = T1c;
/**
 * Partially applicable **T2** combinator.
 */
const T2c = (0, curry_1.dashify)(T2, curry_1.__);
exports.T2c = T2c;
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
const T = T2c;
exports.T = T;
/**
 * **U1** combinator.
 *
 *     U1 f = f f
 *
 * Not to be confused with {@link UT1 **UT1**}.
 */
function U1(f) {
    return f(f);
}
exports.U1 = U1;
U1.lispSource = [Symbol.for('define'), [Symbol.for('U1'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('f')]];
/**
 * Curried **U1** combinator.
 */
const U1c = (0, curry_1.curryN)(1, U1);
exports.U1c = U1c;
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
const U = U1c;
exports.U = U;
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
function UT1(x, y) {
    return y(x(x, y));
}
exports.UT1 = UT1;
UT1.lispSource = [Symbol.for('define'), [Symbol.for('UT1'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('y'), [Symbol.for('x'), Symbol.for('x'), Symbol.for('y')]]];
/**
 * Curried **UT1** combinator.
 */
const UT1c = (0, curry_1.curryN)(2, UT1);
exports.UT1c = UT1c;
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
const UT = UT1c;
exports.UT = UT;
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
function Y1(f) {
    // The names of the function arguments (`future`, `arg`)
    // are the same as in the book *The Little LISPer*, with
    // the exception of `f`, which is named `M` in the book.
    return (function (future) {
        return f(function (arg) {
            return future(future)(arg);
        });
    })(function (future) {
        return f(function (arg) {
            return future(future)(arg);
        });
    });
}
exports.Y1 = Y1;
Y1.lispSource = [Symbol.for('define'), [Symbol.for('Y1'), Symbol.for('f')], [[Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]], [Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]]]];
/**
 * Curried **Y1** combinator.
 */
const Y1c = (0, curry_1.curryN)(1, Y1);
exports.Y1c = Y1c;
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
const Y = Y1c;
exports.Y = Y;
const curried = {
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
exports.curried = curried;
const variadic = {
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
exports.variadic = variadic;

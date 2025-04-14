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
import { __ } from './curry';
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
declare function A1(f: any, x: any): any;
declare namespace A1 {
    var lispSource: (symbol | symbol[])[];
}
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
declare function A2(f: any, ...args: any[]): any;
declare namespace A2 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Curried **A1** combinator.
 */
declare const A1c: any;
/**
 * Partially applicable **A2** combinator.
 */
declare const A2c: any;
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
declare const A: any;
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
declare function B1(f: any, g: any, x: any): any;
declare namespace B1 {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
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
declare function B2(...args: any[]): any;
declare namespace B2 {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | number[])[] | (symbol[] | number[])[])[])[];
}
/**
 * Curried **B1** combinator.
 */
declare const B1c: any;
/**
 * Partially applicable **B2** combinator.
 */
declare const B2c: any;
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
declare const B: any;
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
declare function C1(f: any, x: any, y: any): any;
declare namespace C1 {
    var lispSource: (symbol | symbol[])[];
}
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
declare function C2(f: any, ...args: any[]): any;
declare namespace C2 {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | (symbol | symbol[])[][])[])[])[])[])[];
}
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
declare function C3(f: any, ...args: any[]): any;
declare namespace C3 {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[][])[])[])[])[])[];
}
/**
 * Curried **C1** combinator.
 */
declare const C1c: any;
/**
 * Partially applicable **C2** combinator.
 */
declare const C2c: any;
/**
 * Partially applicable **C3** combinator.
 */
declare const C3c: any;
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
declare const C: any;
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
declare function I1(x: any): any;
declare namespace I1 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Curried **I1** combinator.
 */
declare const I1c: any;
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
declare const I: any;
/**
 * **K1** combinator.
 *
 *     K1 x y = x
 *
 * See also {@link K2 **K2**}.
 */
declare function K1(x: any, y: any): any;
declare namespace K1 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * **K2** combinator.
 *
 *     K2 x = x
 *     K2 x y = x
 *     K2 x y z = x
 *
 * See also {@link K1 **K1**}.
 */
declare function K2(x: any, ...args: any[]): any;
declare namespace K2 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Curried **K1** combinator.
 */
declare const K1c: any;
/**
 * Partially applicable **K2** combinator.
 */
declare const K2c: any;
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
declare const K: any;
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
declare function Ki1(x: any, y: any): any;
declare namespace Ki1 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Curried **Ki** combinator.
 */
declare const Ki1c: any;
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
declare const Ki: any;
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
declare function Q1(f: any, g: any, x: any): any;
declare namespace Q1 {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
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
declare function Q2(...args: any[]): any;
declare namespace Q2 {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | number[])[] | (symbol[] | number[])[])[])[];
}
/**
 * Curried **Q1** combinator.
 */
declare const Q1c: any;
/**
 * Partially applicable **Q2** combinator.
 */
declare const Q2c: any;
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
declare const Q: any;
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
declare function S1(f: any, g: any, x: any): any;
declare namespace S1 {
    var lispSource: (symbol | symbol[] | symbol[][])[];
}
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
declare function S2(f: any, g: any, x: any): any;
declare namespace S2 {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Curried **S1** combinator.
 */
declare const S1c: any;
/**
 * Curried **S2** combinator.
 */
declare const S2c: any;
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
declare const S: any;
/**
 * **T1** combinator.
 *
 *     T1 x f = f x
 *
 * See also {@link T2 **T2**} and {@link Q1 **Q1**}.
 */
declare function T1(x: any, f: any): any;
declare namespace T1 {
    var lispSource: (symbol | symbol[])[];
}
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
declare function T2(...args: any[]): any;
declare namespace T2 {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | number[])[] | (symbol[] | number[])[])[])[];
}
/**
 * Curried **T1** combinator.
 */
declare const T1c: any;
/**
 * Partially applicable **T2** combinator.
 */
declare const T2c: any;
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
declare const T: any;
/**
 * **U1** combinator.
 *
 *     U1 f = f f
 *
 * Not to be confused with {@link UT1 **UT1**}.
 */
declare function U1(f: any): any;
declare namespace U1 {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Curried **U1** combinator.
 */
declare const U1c: any;
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
declare const U: any;
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
declare function UT1(x: any, y: any): any;
declare namespace UT1 {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Curried **UT1** combinator.
 */
declare const UT1c: any;
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
declare const UT: any;
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
declare function Y1(f: any): any;
declare namespace Y1 {
    var lispSource: (symbol | symbol[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[][])[];
}
/**
 * Curried **Y1** combinator.
 */
declare const Y1c: any;
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
declare const Y: any;
declare const curried: any;
declare const variadic: any;
export { __, __ as _, curried, variadic, A, A1, A1c, A2, A2c, B, B1, B1c, B2, B2c, C, C1, C2, C3, C1c, C2c, C3c, I, I1, I1c, K, K1, K1c, K2, K2c, Ki, Ki1, Ki1c, Q, Q1, Q1c, Q2, Q2c, S, S1, S1c, S2, S2c, T, T1, T1c, T2, T2c, U, U1, U1c, UT, UT1, UT1c, Y, Y1, Y1c };

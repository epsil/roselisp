/**
 * # Currying
 *
 * Currying and partial application.
 *
 * ## Description
 *
 * [Ramda][r:curry]-compatible implementation of currying and partial
 * application of functions.
 *
 * This file defines a placeholder value, [`__`][r:dash], which is
 * compatible with Ramda.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [r:curry]: https://ramdajs.com/docs/#curry
 * [r:dash]: https://ramdajs.com/docs/#__
 */
/**
 * Ramda-compatible placeholder value.
 *
 * See [`R.__`][r:dash] in Ramda.
 *
 * [r:dash]: https://ramdajs.com/docs/#__
 */
declare const __: any;
/**
 * Whether a value is the placeholder value, `__`.
 */
declare function isPlaceholder(x: any, placeholder?: any): any;
declare namespace isPlaceholder {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Creates a function that accepts arguments of `f` and either invokes `f`
 * returning its result, if at least `arity` number of arguments have been
 * provided, or returns a function that accepts the remaining `f`
 * arguments, and so on. The arity of `f` may be specified if `f.length`
 * is not sufficient.
 *
 * Loosely based on [`curry` from Ramda][r:curry].
 *
 * [r:curry]: https://ramdajs.com/docs/#curry
 */
declare function curry(f: any, arity?: any): any;
declare namespace curry {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Make a curried function. `f` is the function to call, `arity` is
 * the arity of the function, and `placeholder` is a placeholder value
 * like {@link __ `R.__`}. `received` is used internally and is an
 * array of the arguments received thus far.
 *
 * Loosely based on [`curryN` from Ramda][r:curryn].
 *
 * [r:curryn]: https://ramdajs.com/docs/#curryN
 */
declare function curryN(arity: any, f: any, received?: any): any;
declare namespace curryN {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (number | symbol)[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[][])[])[])[])[])[])[];
}
/**
 * Add support for partial application with
 * a placeholder value like {@link __ `R.__`}.
 */
declare function dashify(f: any, placeholder?: any): any;
declare namespace dashify {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[][])[] | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | symbol[] | (number | symbol | symbol[])[][])[])[])[])[])[])[];
}
export { __, __ as _, __ as placeholder, curry, curryN, dashify, isPlaceholder };

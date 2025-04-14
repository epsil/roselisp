/**
 * # S-expressions
 *
 * Utilities for creating S-expressions in JavaScript.
 *
 * ## Description
 *
 * This file defines some functions for making it easier to write
 * S-expressions in JavaScript code. The JavaScript expression:
 *
 *     sexp`(x y)`
 *
 * Is the same as the JavaScript expression:
 *
 *     [s`x`, s`y`]
 *
 * Which is the same as the JavaScript expression:
 *
 *     [Symbol.for('x'), Symbol.for('y')]
 *
 * Note that these functions come with a small runtime cost. However,
 * they might aid readability in contexts where this is not a
 * practical issue. In contexts where it *is* an issue, one should
 * write `[Symbol.for('x'), ...]` instead.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Shorthand for creating symbols.
 *
 * Implemented as a [tagged template][js:Template literals].
 * For example, the expression `(x (y z))` can be written as:
 *
 *     [s`x`, [s`y`, s`z`]]
 *
 * This amounts to the same as:
 *
 *     [Symbol.for('x'), [Symbol.for('y'), Symbol.for('z')]]
 *
 * Alternatively, `s` can be invoked as a regular function:
 *
 *     [s('x'), [s('y'), s('z')]]
 *
 * This comes in handy when the string to convert is stored in a
 * variable:
 *
 *     const str = 'foo';
 *     const sym = s(str);
 *
 * `s` creates symbols in the same way as `string->symbol`, i.e.,
 * with `Symbol.for()`, which is part of JavaScript's built-in
 * [`Symbol`][js:Symbol] implementation. To create unique symbols,
 * use `gensym` instead.
 *
 * [js:Template literals]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 * [js:Symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
 */
declare function s(x: any, y?: any): any;
/**
 * Convert the input to an S-expression.
 *
 * For example:
 *
 *     sexp`(exp)`   = [Symbol.for('exp')]
 *     sexp('(exp)') = [Symbol.for('exp')]
 *
 * As shown, `sexp` works both as a regular function and
 * as a [tagged template literal][js:Template literals].
 *
 * [js:Template literals]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
declare function sexp(input: any, ...args: any[]): any;
export { s, sexp };

"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Symbols
 *
 * Symbol implementation.
 *
 * ## Description
 *
 * This file maps Roselisp's "symbol" concept onto JavaScript's
 * [`Symbol`][js:Symbol] construct. It provides utilities for
 * working with symbols.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [js:Symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.symbolp_ = exports.symbolToString_ = exports.stringToSymbol_ = exports.gensym_ = exports.gensymp_ = exports.intern_ = void 0;
/**
 * Whether something is a symbol.
 *
 * Similar to [`symbol?` in Racket][rkt:symbolp] and
 * [`symbolp` in Common Lisp][cl:symbolp].
 *
 * [rkt:symbolp]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol~3f%29%29
 * [cl:symbolp]: http://clhs.lisp.se/Body/f_symbol.htm#symbolp
 */
function symbolp_(obj) {
    return typeof obj === 'symbol';
}
exports.symbolp_ = symbolp_;
symbolp_.lispSource = [Symbol.for('define'), [Symbol.for('symbol?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'symbol']];
/**
 * Convert a symbol to a string.
 *
 * Similar to [`symbol->string` in Racket][rkt:symbol-to-string]
 * and [`symbol-name` in Common Lisp][cl:symbol-name].
 *
 * [rkt:symbol-to-string]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol-~3estring%29%29
 * [cl:symbol-name]: http://clhs.lisp.se/Body/f_symb_2.htm#symbol-name
 */
function symbolToString_(sym) {
    return sym.description;
}
exports.symbolToString_ = symbolToString_;
symbolToString_.lispSource = [Symbol.for('define'), [Symbol.for('symbol->string_'), Symbol.for('sym')], [Symbol.for('ann'), [Symbol.for('get-field'), Symbol.for('description'), Symbol.for('sym')], Symbol.for('String')]];
/**
 * Convert a string to a symbol.
 *
 * Similar to [`string->symbol` in Racket][rkt:string-to-symbol]
 * and [`intern` in Common Lisp][cl:intern].
 *
 * [rkt:string-to-symbol]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._string-~3esymbol%29%29
 * [cl:intern]: http://clhs.lisp.se/Body/f_intern.htm#intern
 */
function stringToSymbol_(str) {
    // `Symbol.for()` returns the same symbol for a given string,
    // similar to `string->symbol`.
    return Symbol.for(str);
}
exports.intern_ = stringToSymbol_;
exports.stringToSymbol_ = stringToSymbol_;
stringToSymbol_.lispSource = [Symbol.for('define'), [Symbol.for('string->symbol_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('Symbol'), Symbol.for('for'), Symbol.for('str')]];
/**
 * Create a unique symbol.
 *
 * Similar to [`gensym` in Racket][rkt:gensym] and
 * [`gensym` in Common Lisp][cl:gensym].
 *
 * [rkt:gensym]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._gensym%29%29
 * [cl:gensym]: http://clhs.lisp.se/Body/f_gensym.htm#gensym
 */
function gensym_(str) {
    // `Symbol()` returns a unique symbol for any string,
    // similar to `gensym`.
    return Symbol(str);
}
exports.gensym_ = gensym_;
gensym_.lispSource = [Symbol.for('define'), [Symbol.for('gensym_'), Symbol.for('str')], [Symbol.for('Symbol'), Symbol.for('str')]];
/**
 * Whether something is a unique symbol.
 */
function gensymp_(obj) {
    // It is a unique symbol if it is a symbol that is different
    // from the one returned by `string->symbol`.
    return (typeof obj === 'symbol') && (obj !== Symbol.for(obj.description));
}
exports.gensymp_ = gensymp_;
gensymp_.lispSource = [Symbol.for('define'), [Symbol.for('gensym?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('obj')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('obj'), [Symbol.for('string->symbol'), [Symbol.for('symbol->string'), Symbol.for('obj')]]]]]];

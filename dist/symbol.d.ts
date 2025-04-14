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
/**
 * Whether something is a symbol.
 *
 * Similar to [`symbol?` in Racket][rkt:symbolp] and
 * [`symbolp` in Common Lisp][cl:symbolp].
 *
 * [rkt:symbolp]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol~3f%29%29
 * [cl:symbolp]: http://clhs.lisp.se/Body/f_symbol.htm#symbolp
 */
declare function symbolp_(obj: any): any;
declare namespace symbolp_ {
    var lispSource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Convert a symbol to a string.
 *
 * Similar to [`symbol->string` in Racket][rkt:symbol-to-string]
 * and [`symbol-name` in Common Lisp][cl:symbol-name].
 *
 * [rkt:symbol-to-string]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol-~3estring%29%29
 * [cl:symbol-name]: http://clhs.lisp.se/Body/f_symb_2.htm#symbol-name
 */
declare function symbolToString_(sym: any): any;
declare namespace symbolToString_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Convert a string to a symbol.
 *
 * Similar to [`string->symbol` in Racket][rkt:string-to-symbol]
 * and [`intern` in Common Lisp][cl:intern].
 *
 * [rkt:string-to-symbol]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._string-~3esymbol%29%29
 * [cl:intern]: http://clhs.lisp.se/Body/f_intern.htm#intern
 */
declare function stringToSymbol_(str: any): any;
declare namespace stringToSymbol_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Create a unique symbol.
 *
 * Similar to [`gensym` in Racket][rkt:gensym] and
 * [`gensym` in Common Lisp][cl:gensym].
 *
 * [rkt:gensym]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._gensym%29%29
 * [cl:gensym]: http://clhs.lisp.se/Body/f_gensym.htm#gensym
 */
declare function gensym_(str: any): any;
declare namespace gensym_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether something is a unique symbol.
 */
declare function gensymp_(obj: any): any;
declare namespace gensymp_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
export { stringToSymbol_ as intern_, gensymp_, gensym_, stringToSymbol_, symbolToString_, symbolp_ };

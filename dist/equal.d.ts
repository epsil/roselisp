/**
 * # Equality
 *
 * Equality algorithms.
 *
 * ## Description
 *
 * This file provides various functions for checking if two values
 * are equal.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Strict equality.
 *
 * Similar to [`eq?` in Racket][rkt:eqp] and
 * [`eq` in Common Lisp][cl:eq].
 *
 * [rkt:eqp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eq~3f%29%29
 * [cl:eq]: http://clhs.lisp.se/Body/f_eq.htm#eq
 */
declare function eqp_(x: any, y: any): any;
declare namespace eqp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Loose equality.
 *
 * Similar to [`eqv?` in Racket][rkt:eqvp] and
 * [`eql` in Common Lisp][cl:eql].
 *
 * [rkt:eqvp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eqv~3f%29%29
 * [cl:eql]: http://clhs.lisp.se/Body/f_eql.htm#eql
 */
declare function eqvp_(x: any, y: any): any;
declare namespace eqvp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Structural equality.
 *
 * Similar to [`equal?` in Racket][rkt:equalp] and
 * [`equal` in Common Lisp][cl:equal].
 *
 * [rkt:equalp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._equal~3f%29%29
 * [cl:equal]: http://clhs.lisp.se/Body/f_equal.htm#equal
 */
declare function equalp_(x: any, y: any): any;
declare namespace equalp_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[][] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[];
}
export { eqp_ as eq, eqp_ as eqp, eqp_ as eq_, equalp_ as equalp, equalp_ as equal_, eqvp_ as eql, eqvp_ as eqlp, eqvp_ as eqlp_, eqvp_ as eql_, eqvp_ as eqv, eqvp_ as eqvp, eqvp_ as eqv_, eqp_, equalp_, eqvp_ };

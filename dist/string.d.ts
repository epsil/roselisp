/**
 * # Strings
 *
 * String utilities.
 *
 * ## Description
 *
 * Various functions for working with strings.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Whether something is a string.
 *
 * Similar to [`string?` in Racket][rkt:stringp] and
 * [`stringp` in Common Lisp][cl:stringp].
 *
 * [rkt:stringp]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string~3f%29%29
 * [cl:stringp]: http://clhs.lisp.se/Body/f_stgp.htm#stringp
 */
declare function stringp_(obj: any): any;
declare namespace stringp_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether something is a string primitive.
 */
declare function stringPrimitiveP_(obj: any): any;
declare namespace stringPrimitiveP_ {
    var lispSource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Whether something is a string object.
 */
declare function stringObjectP_(obj: any): any;
declare namespace stringObjectP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Concatenate one or more strings together.
 *
 * Similar to [`string-append` in Racket][rkt:string-append].
 *
 * [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
 */
declare function stringAppend_(...args: any[]): any;
declare namespace stringAppend_ {
    var lispSource: (symbol | (string | symbol | (symbol | symbol[])[])[])[];
}
/**
 * Get the character at a particular position in a string.
 *
 * Similar to [`string-ref` in Racket][rkt:string-ref].
 *
 * [rkt:string-ref]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-ref%29%29
 */
declare function stringRef_(str: any, n: any): any;
declare namespace stringRef_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Trim whitespace from the beginning and end of a string.
 *
 * Similar to [`string-trim` in Racket][rkt:string-trim].
 *
 * [rkt:string-trim]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-trim%29%29
 */
declare function stringTrim_(str: any, sep?: any, ...options: any[]): any;
declare namespace stringTrim_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (string | symbol | (string | symbol)[])[])[] | (symbol | (string | symbol | (symbol | (string | symbol)[])[])[])[])[])[])[];
}
/**
 * Repeat a string `n` times.
 */
declare function stringRepeat_(str: any, n: any): any;
declare namespace stringRepeat_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Join a list of strings, using `sep` as the separator.
 *
 * Similar to [`string-join` in Racket][rkt:string-join].
 *
 * [rkt:string-join]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-join%29%29
 */
declare function stringJoin_(lst: any, sep?: any): any;
declare namespace stringJoin_ {
    var lispSource: (symbol | (symbol | (string | symbol)[])[])[];
}
/**
 * Split a string into a list of strings.
 *
 * Similar to [`string-split` in Racket][rkt:string-split].
 *
 * [rkt:string-split]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29
 */
declare function stringSplit_(str: any, sep?: any): any;
declare namespace stringSplit_ {
    var lispSource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
}
/**
 * Return a copy of `str` where `from` is replaced with `to`.
 *
 * Similar to [`string-replace`][rkt:string-replace] in Racket.
 *
 * [rkt:string-replace]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-replace%29%29
 */
declare function stringReplace_(str: any, from: any, to: any): any;
declare namespace stringReplace_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Convert string to upper case.
 *
 * Similar to [`string-upcase` in Racket][rkt:string-upcase].
 *
 * [rkt:string-upcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-upcase%29%29
 */
declare function stringUpcase_(str: any): any;
declare namespace stringUpcase_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Convert string to lower case.
 *
 * Similar to [`string-downcase` in Racket][rkt:string-downcase].
 *
 * [rkt:string-downcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-downcase%29%29
 */
declare function stringDowncase_(str: any): any;
declare namespace stringDowncase_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return a substring of `str`, from `start` to `end`.
 *
 * Similar to [`substring` in Racket][rkt:substring].
 *
 * [rkt:substring]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._substring%29%29
 */
declare function substring_(str: any, start: any, end?: any): any;
declare namespace substring_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Convert a string to a number.
 *
 * Similar to [`string->number` in Racket][rkt:string-to-number].
 *
 * [rkt:string-to-number]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._string-~3enumber%29%29
 */
declare function stringToNumber_(str: any): any;
declare namespace stringToNumber_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Convert a number to a string.
 *
 * Similar to [`number->string` in Racket][rkt:number-to-string].
 *
 * [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
 */
declare function numberToString_(n: any): any;
declare namespace numberToString_ {
    var lispSource: (symbol | (string | symbol)[])[];
}
/**
 * Indent a string by prepending each line with `n` spaces.
 */
declare function indentString(str: any, n?: any, options?: any): any;
declare namespace indentString {
    var lispSource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | symbol[])[] | (number | symbol)[])[])[];
}
export { numberToString_, stringToNumber_, stringAppend_ as stringAppend, stringObjectP_, stringPrimitiveP_, stringReplace_ as stringReplace, stringp_ as stringp, stringp_, substring_ as substring, indentString, stringAppend_, stringDowncase_, stringJoin_, stringRef_, stringRepeat_, stringReplace_, stringSplit_, stringTrim_, stringUpcase_, substring_ };

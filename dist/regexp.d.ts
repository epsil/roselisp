/**
 * # Regular expressions
 *
 * Regular expression utilities.
 *
 * ## Description
 *
 * For the sake of simplicity, all regexps are JavaScript regexps,
 * which is to say that `(regexp ...)` behaves the same as
 * `(js/regexp ...)`. (One may look into writing a `rkt/regexp` macro
 * that translates Racket regexps to JavaScript regexps.)
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Convert `input` to a [regular expression][mdn:Regular Expressions] object.
 *
 * [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
 */
declare function regexp_(input: any, flags?: any): any;
declare namespace regexp_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether `obj` is a regular expression.
 */
declare function regexpp_(obj: any): any;
declare namespace regexpp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Make a regexp string suitable for matching the given string.
 * Pass the regexp string to `regexp` to make a regular expression.
 *
 * Similar to [`regexp-quote` in Racket][rkt:regexp-quote].
 *
 * [rkt:regexp-quote]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-quote%29%29
 */
declare function regexpQuote_(str: any): any;
declare namespace regexpQuote_ {
    var lispSource: (symbol | (string | symbol | (string | symbol)[])[])[];
}
/**
 * Match `pattern` against `input`.
 *
 * `pattern` is a [regular expression][mdn:Regular Expressions]
 * object.
 *
 * This function is implemented in terms of
 * [`String.prototype.match()`][mdn:String.prototype.match] and is
 * similar to [`regexp-match`][rkt:regexp-match] in Racket.
 *
 * [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
 * [mdn:String.prototype.match]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match
 * [rkt:regexp-match]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28quote._~23~25kernel%29._regexp-match%29%29
 */
declare function regexpMatch_(pattern: any, input: any): any;
declare namespace regexpMatch_ {
    var lispSource: (symbol | symbol[])[];
}
declare function regexpMatchP_(pattern: any, input: any): any;
declare namespace regexpMatchP_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Match `pattern` against `input` and replace with `insert`.
 * `pattern` is a [regular expression][mdn:Regular Expressions]
 * object. This function is implemented in terms of
 * [`String.prototype.replace()`][mdn:String.prototype.replace] and is
 * similar to [`regexp-replace`][rkt:regexp-replace] in Racket.
 *
 * [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
 * [mdn:String.prototype.replace]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
 * [rkt:regexp-replace]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28quote._~23~25kernel%29._regexp-replace%29%29
 */
declare function regexpReplace_(pattern: any, input: any, insert: any): any;
declare namespace regexpReplace_ {
    var lispSource: (symbol | symbol[])[];
}
export { regexpMatchP_ as jsRegexpMatchP_, regexpMatchP_, regexpMatch_ as jsRegexpMatch_, regexpQuote_ as jsRegexpQuote_, regexpReplace_ as jsRegexpReplace_, regexpp_ as jsRegexpP_, regexpp_ as regexpP_, regexp_ as jsRegexp_, regexpMatch_, regexpQuote_, regexpReplace_, regexpp_, regexp_ };

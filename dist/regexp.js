"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.regexp_ = exports.regexpp_ = exports.regexpReplace_ = exports.regexpQuote_ = exports.regexpMatch_ = exports.jsRegexp_ = exports.regexpP_ = exports.jsRegexpP_ = exports.jsRegexpReplace_ = exports.jsRegexpQuote_ = exports.jsRegexpMatch_ = exports.regexpMatchP_ = exports.jsRegexpMatchP_ = void 0;
/**
 * Convert `input` to a [regular expression][mdn:Regular Expressions] object.
 *
 * [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
 */
function regexp_(input, flags = undefined) {
    if (typeof input === 'string') {
        return new RegExp(input, flags);
    }
    else {
        return input;
    }
}
exports.jsRegexp_ = regexp_;
exports.regexp_ = regexp_;
regexp_.lispSource = [Symbol.for('define'), [Symbol.for('regexp_'), Symbol.for('input'), [Symbol.for('flags'), Symbol.for('undefined')]], [Symbol.for('if'), [Symbol.for('string?'), Symbol.for('input')], [Symbol.for('new'), Symbol.for('RegExp'), Symbol.for('input'), Symbol.for('flags')], Symbol.for('input')]];
/**
 * Whether `obj` is a regular expression.
 */
function regexpp_(obj) {
    return obj instanceof RegExp;
}
exports.jsRegexpP_ = regexpp_;
exports.regexpP_ = regexpp_;
exports.regexpp_ = regexpp_;
regexpp_.lispSource = [Symbol.for('define'), [Symbol.for('regexp?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('RegExp')]];
/**
 * Make a regexp string suitable for matching the given string.
 * Pass the regexp string to `regexp` to make a regular expression.
 *
 * Similar to [`regexp-quote` in Racket][rkt:regexp-quote].
 *
 * [rkt:regexp-quote]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-quote%29%29
 */
function regexpQuote_(str) {
    // Based on `escapeRegExp()` from
    // <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping>.
    return str.replace(new RegExp('[.*+?^${}()|[\\]\\\\]', 'g'), '\\$&');
}
exports.jsRegexpQuote_ = regexpQuote_;
exports.regexpQuote_ = regexpQuote_;
regexpQuote_.lispSource = [Symbol.for('define'), [Symbol.for('regexp-quote_'), Symbol.for('str')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '[.*+?^${}()|[\\]\\\\]', 'g'], Symbol.for('str'), '\\$&']];
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
function regexpMatch_(pattern, input) {
    return input.match(pattern);
}
exports.jsRegexpMatch_ = regexpMatch_;
exports.regexpMatch_ = regexpMatch_;
regexpMatch_.lispSource = [Symbol.for('define'), [Symbol.for('regexp-match_'), Symbol.for('pattern'), Symbol.for('input')], [Symbol.for('send'), Symbol.for('input'), Symbol.for('match'), Symbol.for('pattern')]];
function regexpMatchP_(pattern, input) {
    return input.match(pattern) !== null;
}
exports.jsRegexpMatchP_ = regexpMatchP_;
exports.regexpMatchP_ = regexpMatchP_;
regexpMatchP_.lispSource = [Symbol.for('define'), [Symbol.for('regexp-match?_'), Symbol.for('pattern'), Symbol.for('input')], [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('regexp-match'), Symbol.for('pattern'), Symbol.for('input')], Symbol.for('js/null')]]];
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
function regexpReplace_(pattern, input, insert) {
    return input.replace(pattern, insert);
}
exports.jsRegexpReplace_ = regexpReplace_;
exports.regexpReplace_ = regexpReplace_;
regexpReplace_.lispSource = [Symbol.for('define'), [Symbol.for('regexp-replace_'), Symbol.for('pattern'), Symbol.for('input'), Symbol.for('insert')], [Symbol.for('send'), Symbol.for('input'), Symbol.for('replace'), Symbol.for('pattern'), Symbol.for('insert')]];

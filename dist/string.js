"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.substring_ = exports.stringUpcase_ = exports.stringTrim_ = exports.stringSplit_ = exports.stringReplace_ = exports.stringRepeat_ = exports.stringRef_ = exports.stringJoin_ = exports.stringDowncase_ = exports.stringAppend_ = exports.indentString = exports.substring = exports.stringp_ = exports.stringp = exports.stringReplace = exports.stringPrimitiveP_ = exports.stringObjectP_ = exports.stringAppend = exports.stringToNumber_ = exports.numberToString_ = void 0;
const [plistGet] = (() => {
    function plistGet_(plist, prop) {
        let val = undefined;
        const _end = plist.length;
        for (let i = 0; i < _end; i = i + 2) {
            if (plist[i] === prop) {
                val = plist[i + 1];
                break;
            }
        }
        return val;
    }
    return [plistGet_];
})();
/**
 * Whether something is a string.
 *
 * Similar to [`string?` in Racket][rkt:stringp] and
 * [`stringp` in Common Lisp][cl:stringp].
 *
 * [rkt:stringp]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string~3f%29%29
 * [cl:stringp]: http://clhs.lisp.se/Body/f_stgp.htm#stringp
 */
function stringp_(obj) {
    return (typeof obj === 'string') || (obj instanceof String);
}
exports.stringp = stringp_;
exports.stringp_ = stringp_;
stringp_.lispSource = [Symbol.for('define'), [Symbol.for('string?_'), Symbol.for('obj')], [Symbol.for('or'), [Symbol.for('string-primitive?'), Symbol.for('obj')], [Symbol.for('string-object?'), Symbol.for('obj')]]];
/**
 * Whether something is a string primitive.
 */
function stringPrimitiveP_(obj) {
    return typeof obj === 'string';
}
exports.stringPrimitiveP_ = stringPrimitiveP_;
stringPrimitiveP_.lispSource = [Symbol.for('define'), [Symbol.for('string-primitive?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'string']];
/**
 * Whether something is a string object.
 */
function stringObjectP_(obj) {
    return obj instanceof String;
}
exports.stringObjectP_ = stringObjectP_;
stringObjectP_.lispSource = [Symbol.for('define'), [Symbol.for('string-object?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('String')]];
/**
 * Concatenate one or more strings together.
 *
 * Similar to [`string-append` in Racket][rkt:string-append].
 *
 * [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
 */
function stringAppend_(...args) {
    return args.reduce(function (acc, x) {
        return acc + x;
    }, '');
}
exports.stringAppend = stringAppend_;
exports.stringAppend_ = stringAppend_;
stringAppend_.lispSource = [Symbol.for('define'), [Symbol.for('string-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('string-append'), Symbol.for('acc'), Symbol.for('x')]], '', Symbol.for('args')]];
/**
 * Get the character at a particular position in a string.
 *
 * Similar to [`string-ref` in Racket][rkt:string-ref].
 *
 * [rkt:string-ref]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-ref%29%29
 */
function stringRef_(str, n) {
    return str.charAt(n);
}
exports.stringRef_ = stringRef_;
stringRef_.lispSource = [Symbol.for('define'), [Symbol.for('string-ref_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('charAt'), Symbol.for('n')]];
/**
 * Trim whitespace from the beginning and end of a string.
 *
 * Similar to [`string-trim` in Racket][rkt:string-trim].
 *
 * [rkt:string-trim]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-trim%29%29
 */
function stringTrim_(str, sep = undefined, ...options) {
    if (sep) {
        const repeatOption = plistGet(options, Symbol.for(':repeat?'));
        const patternStr = '(' + sep.replace(new RegExp('[.*+?^${}()|[\\]\\\\]', 'g'), '\\$&') + ')' + (repeatOption ? '+' : '');
        return str.replace(new RegExp('^' + patternStr), '').replace(new RegExp(patternStr + '$'), '');
    }
    else {
        return str.trim();
    }
}
exports.stringTrim_ = stringTrim_;
stringTrim_.lispSource = [Symbol.for('define'), [Symbol.for('string-trim_'), Symbol.for('str'), [Symbol.for('sep'), Symbol.for('undefined')], Symbol.for('.'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('sep'), [Symbol.for('define'), Symbol.for('repeat-option'), [Symbol.for('plist-get'), Symbol.for('options'), [Symbol.for('quote'), Symbol.for(':repeat?')]]], [Symbol.for('define'), Symbol.for('pattern-str'), [Symbol.for('string-append'), '(', [Symbol.for('regexp-quote'), Symbol.for('sep')], ')', [Symbol.for('if'), Symbol.for('repeat-option'), '+', '']]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), '^', Symbol.for('pattern-str')]], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), Symbol.for('pattern-str'), '$']], Symbol.for('_'), '']]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('trim')]]]];
/**
 * Repeat a string `n` times.
 */
function stringRepeat_(str, n) {
    return str.repeat(n);
}
exports.stringRepeat_ = stringRepeat_;
stringRepeat_.lispSource = [Symbol.for('define'), [Symbol.for('string-repeat_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('repeat'), Symbol.for('n')]];
/**
 * Join a list of strings, using `sep` as the separator.
 *
 * Similar to [`string-join` in Racket][rkt:string-join].
 *
 * [rkt:string-join]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-join%29%29
 */
function stringJoin_(lst, sep = ' ') {
    return lst.join(sep);
}
exports.stringJoin_ = stringJoin_;
stringJoin_.lispSource = [Symbol.for('define'), [Symbol.for('string-join_'), Symbol.for('lst'), [Symbol.for('sep'), ' ']], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('join'), Symbol.for('sep')]];
/**
 * Split a string into a list of strings.
 *
 * Similar to [`string-split` in Racket][rkt:string-split].
 *
 * [rkt:string-split]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29
 */
function stringSplit_(str, sep = new RegExp('\\s+', 'g')) {
    return str.split(sep);
}
exports.stringSplit_ = stringSplit_;
stringSplit_.lispSource = [Symbol.for('define'), [Symbol.for('string-split_'), Symbol.for('str'), [Symbol.for('sep'), [Symbol.for('regexp'), '\\s+', 'g']]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('split'), Symbol.for('sep')]];
/**
 * Return a copy of `str` where `from` is replaced with `to`.
 *
 * Similar to [`string-replace`][rkt:string-replace] in Racket.
 *
 * [rkt:string-replace]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-replace%29%29
 */
function stringReplace_(str, from, to) {
    return str.replace(from, to);
}
exports.stringReplace = stringReplace_;
exports.stringReplace_ = stringReplace_;
stringReplace_.lispSource = [Symbol.for('define'), [Symbol.for('string-replace_'), Symbol.for('str'), Symbol.for('from'), Symbol.for('to')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('from'), Symbol.for('to')]];
/**
 * Convert string to upper case.
 *
 * Similar to [`string-upcase` in Racket][rkt:string-upcase].
 *
 * [rkt:string-upcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-upcase%29%29
 */
function stringUpcase_(str) {
    return str.toUpperCase();
}
exports.stringUpcase_ = stringUpcase_;
stringUpcase_.lispSource = [Symbol.for('define'), [Symbol.for('string-upcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toUpperCase')]];
/**
 * Convert string to lower case.
 *
 * Similar to [`string-downcase` in Racket][rkt:string-downcase].
 *
 * [rkt:string-downcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-downcase%29%29
 */
function stringDowncase_(str) {
    return str.toLowerCase();
}
exports.stringDowncase_ = stringDowncase_;
stringDowncase_.lispSource = [Symbol.for('define'), [Symbol.for('string-downcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toLowerCase')]];
/**
 * Return a substring of `str`, from `start` to `end`.
 *
 * Similar to [`substring` in Racket][rkt:substring].
 *
 * [rkt:substring]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._substring%29%29
 */
function substring_(str, start, end = undefined) {
    return str.substring(start, end);
}
exports.substring = substring_;
exports.substring_ = substring_;
substring_.lispSource = [Symbol.for('define'), [Symbol.for('substring_'), Symbol.for('str'), Symbol.for('start'), [Symbol.for('end'), Symbol.for('undefined')]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('substring'), Symbol.for('start'), Symbol.for('end')]];
/**
 * Convert a string to a number.
 *
 * Similar to [`string->number` in Racket][rkt:string-to-number].
 *
 * [rkt:string-to-number]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._string-~3enumber%29%29
 */
function stringToNumber_(str) {
    return parseFloat(str);
}
exports.stringToNumber_ = stringToNumber_;
stringToNumber_.lispSource = [Symbol.for('define'), [Symbol.for('string->number_'), Symbol.for('str')], [Symbol.for('parseFloat'), Symbol.for('str')]];
/**
 * Convert a number to a string.
 *
 * Similar to [`number->string` in Racket][rkt:number-to-string].
 *
 * [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
 */
function numberToString_(n) {
    return n + '';
}
exports.numberToString_ = numberToString_;
numberToString_.lispSource = [Symbol.for('define'), [Symbol.for('number->string_'), Symbol.for('n')], [Symbol.for('string-append'), Symbol.for('n'), '']];
/**
 * Indent a string by prepending each line with `n` spaces.
 */
function indentString(str, n = 2, options = {}) {
    const whitespaceOption = options['whitespace'];
    const whitespace = whitespaceOption || ' ';
    const includeEmptyLinesOption = options['includeEmptyLines'];
    const pattern = includeEmptyLinesOption ? new RegExp('^', 'gm') : new RegExp('^(?!s*$)', 'gm');
    const indentation = whitespace.repeat(n);
    return str.replace(pattern, indentation);
}
exports.indentString = indentString;
indentString.lispSource = [Symbol.for('define'), [Symbol.for('indent-string'), Symbol.for('str'), [Symbol.for('n'), 2], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('whitespace-option'), [Symbol.for('oget'), Symbol.for('options'), 'whitespace']], [Symbol.for('define'), Symbol.for('whitespace'), [Symbol.for('or'), Symbol.for('whitespace-option'), ' ']], [Symbol.for('define'), Symbol.for('include-empty-lines-option'), [Symbol.for('oget'), Symbol.for('options'), 'includeEmptyLines']], [Symbol.for('define'), Symbol.for('pattern'), [Symbol.for('if'), Symbol.for('include-empty-lines-option'), [Symbol.for('regexp'), '^', 'gm'], [Symbol.for('regexp'), '^(?!s*$)', 'gm']]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), Symbol.for('whitespace'), Symbol.for('n')]], [Symbol.for('regexp-replace'), Symbol.for('pattern'), Symbol.for('str'), Symbol.for('indentation')]];

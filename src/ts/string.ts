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

const [plistGet]: any[] = ((): any => {
  function plistGet_(plist: any, prop: any): any {
    let val: any = undefined;
    const _end: any = plist.length;
    for (let i: any = 0; i < _end; i = i + 2) {
      if ((plist as any)[i] === prop) {
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
function stringp_(obj: any): any {
  return (typeof obj === 'string') || (obj instanceof String);
}

stringp_.lispSource = [Symbol.for('define'), [Symbol.for('string?_'), Symbol.for('obj')], [Symbol.for('or'), [Symbol.for('string-primitive?'), Symbol.for('obj')], [Symbol.for('string-object?'), Symbol.for('obj')]]];

/**
 * Whether something is a string primitive.
 */
function stringPrimitiveP_(obj: any): any {
  return typeof obj === 'string';
}

stringPrimitiveP_.lispSource = [Symbol.for('define'), [Symbol.for('string-primitive?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'string']];

/**
 * Whether something is a string object.
 */
function stringObjectP_(obj: any): any {
  return obj instanceof String;
}

stringObjectP_.lispSource = [Symbol.for('define'), [Symbol.for('string-object?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('String')]];

/**
 * Concatenate one or more strings together.
 *
 * Similar to [`string-append` in Racket][rkt:string-append].
 *
 * [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
 */
function stringAppend_(...args: any[]): any {
  return args.reduce(function (acc: any, x: any): any {
    return acc + x;
  }, '');
}

stringAppend_.lispSource = [Symbol.for('define'), [Symbol.for('string-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('string-append'), Symbol.for('acc'), Symbol.for('x')]], '', Symbol.for('args')]];

/**
 * Get the character at a particular position in a string.
 *
 * Similar to [`string-ref` in Racket][rkt:string-ref].
 *
 * [rkt:string-ref]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-ref%29%29
 */
function stringRef_(str: any, n: any): any {
  return str.charAt(n);
}

stringRef_.lispSource = [Symbol.for('define'), [Symbol.for('string-ref_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('charAt'), Symbol.for('n')]];

/**
 * Trim whitespace from the beginning and end of a string.
 *
 * Similar to [`string-trim` in Racket][rkt:string-trim].
 *
 * [rkt:string-trim]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-trim%29%29
 */
function stringTrim_(str: any, sep: any = undefined, ...options: any[]): any {
  if (sep) {
    const repeatOption: any = plistGet(options, Symbol.for(':repeat?'));
    const patternStr: any = '(' + sep.replace(new RegExp('[.*+?^${}()|[\\]\\\\]', 'g'), '\\$&') + ')' + (repeatOption ? '+' : '');
    return str.replace(new RegExp('^' + patternStr), '').replace(new RegExp(patternStr + '$'), '');
  } else {
    return str.trim();
  }
}

stringTrim_.lispSource = [Symbol.for('define'), [Symbol.for('string-trim_'), Symbol.for('str'), [Symbol.for('sep'), Symbol.for('undefined')], Symbol.for('.'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('sep'), [Symbol.for('define'), Symbol.for('repeat-option'), [Symbol.for('plist-get'), Symbol.for('options'), [Symbol.for('quote'), Symbol.for(':repeat?')]]], [Symbol.for('define'), Symbol.for('pattern-str'), [Symbol.for('string-append'), '(', [Symbol.for('regexp-quote'), Symbol.for('sep')], ')', [Symbol.for('if'), Symbol.for('repeat-option'), '+', '']]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), '^', Symbol.for('pattern-str')]], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), Symbol.for('pattern-str'), '$']], Symbol.for('_'), '']]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('trim')]]]];

/**
 * Repeat a string `n` times.
 */
function stringRepeat_(str: any, n: any): any {
  return str.repeat(n);
}

stringRepeat_.lispSource = [Symbol.for('define'), [Symbol.for('string-repeat_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('repeat'), Symbol.for('n')]];

/**
 * Join a list of strings, using `sep` as the separator.
 *
 * Similar to [`string-join` in Racket][rkt:string-join].
 *
 * [rkt:string-join]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-join%29%29
 */
function stringJoin_(lst: any, sep: any = ' '): any {
  return lst.join(sep);
}

stringJoin_.lispSource = [Symbol.for('define'), [Symbol.for('string-join_'), Symbol.for('lst'), [Symbol.for('sep'), ' ']], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('join'), Symbol.for('sep')]];

/**
 * Split a string into a list of strings.
 *
 * Similar to [`string-split` in Racket][rkt:string-split].
 *
 * [rkt:string-split]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29
 */
function stringSplit_(str: any, sep: any = new RegExp('\\s+', 'g')): any {
  return str.split(sep);
}

stringSplit_.lispSource = [Symbol.for('define'), [Symbol.for('string-split_'), Symbol.for('str'), [Symbol.for('sep'), [Symbol.for('regexp'), '\\s+', 'g']]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('split'), Symbol.for('sep')]];

/**
 * Return a copy of `str` where `from` is replaced with `to`.
 *
 * Similar to [`string-replace`][rkt:string-replace] in Racket.
 *
 * [rkt:string-replace]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-replace%29%29
 */
function stringReplace_(str: any, from: any, to: any): any {
  return str.replace(from, to);
}

stringReplace_.lispSource = [Symbol.for('define'), [Symbol.for('string-replace_'), Symbol.for('str'), Symbol.for('from'), Symbol.for('to')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('from'), Symbol.for('to')]];

/**
 * Convert string to upper case.
 *
 * Similar to [`string-upcase` in Racket][rkt:string-upcase].
 *
 * [rkt:string-upcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-upcase%29%29
 */
function stringUpcase_(str: any): any {
  return str.toUpperCase();
}

stringUpcase_.lispSource = [Symbol.for('define'), [Symbol.for('string-upcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toUpperCase')]];

/**
 * Convert string to lower case.
 *
 * Similar to [`string-downcase` in Racket][rkt:string-downcase].
 *
 * [rkt:string-downcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-downcase%29%29
 */
function stringDowncase_(str: any): any {
  return str.toLowerCase();
}

stringDowncase_.lispSource = [Symbol.for('define'), [Symbol.for('string-downcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toLowerCase')]];

/**
 * Return a substring of `str`, from `start` to `end`.
 *
 * Similar to [`substring` in Racket][rkt:substring].
 *
 * [rkt:substring]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._substring%29%29
 */
function substring_(str: any, start: any, end: any = undefined): any {
  return str.substring(start, end);
}

substring_.lispSource = [Symbol.for('define'), [Symbol.for('substring_'), Symbol.for('str'), Symbol.for('start'), [Symbol.for('end'), Symbol.for('undefined')]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('substring'), Symbol.for('start'), Symbol.for('end')]];

/**
 * Convert a string to a number.
 *
 * Similar to [`string->number` in Racket][rkt:string-to-number].
 *
 * [rkt:string-to-number]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._string-~3enumber%29%29
 */
function stringToNumber_(str: any): any {
  return parseFloat(str);
}

stringToNumber_.lispSource = [Symbol.for('define'), [Symbol.for('string->number_'), Symbol.for('str')], [Symbol.for('parseFloat'), Symbol.for('str')]];

/**
 * Convert a number to a string.
 *
 * Similar to [`number->string` in Racket][rkt:number-to-string].
 *
 * [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
 */
function numberToString_(n: any): any {
  return n + '';
}

numberToString_.lispSource = [Symbol.for('define'), [Symbol.for('number->string_'), Symbol.for('n')], [Symbol.for('string-append'), Symbol.for('n'), '']];

/**
 * Indent a string by prepending each line with `n` spaces.
 */
function indentString(str: any, n: any = 2, options: any = {}): any {
  const whitespaceOption: any = options['whitespace'];
  const whitespace: any = whitespaceOption || ' ';
  const includeEmptyLinesOption: any = options['includeEmptyLines'];
  const pattern: any = includeEmptyLinesOption ? new RegExp('^', 'gm') : new RegExp('^(?!s*$)', 'gm');
  const indentation: any = whitespace.repeat(n);
  return str.replace(pattern, indentation);
}

indentString.lispSource = [Symbol.for('define'), [Symbol.for('indent-string'), Symbol.for('str'), [Symbol.for('n'), 2], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('whitespace-option'), [Symbol.for('oget'), Symbol.for('options'), 'whitespace']], [Symbol.for('define'), Symbol.for('whitespace'), [Symbol.for('or'), Symbol.for('whitespace-option'), ' ']], [Symbol.for('define'), Symbol.for('include-empty-lines-option'), [Symbol.for('oget'), Symbol.for('options'), 'includeEmptyLines']], [Symbol.for('define'), Symbol.for('pattern'), [Symbol.for('if'), Symbol.for('include-empty-lines-option'), [Symbol.for('regexp'), '^', 'gm'], [Symbol.for('regexp'), '^(?!s*$)', 'gm']]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), Symbol.for('whitespace'), Symbol.for('n')]], [Symbol.for('regexp-replace'), Symbol.for('pattern'), Symbol.for('str'), Symbol.for('indentation')]];

export {
  numberToString_,
  stringToNumber_,
  stringAppend_ as stringAppend,
  stringObjectP_,
  stringPrimitiveP_,
  stringReplace_ as stringReplace,
  stringp_ as stringp,
  stringp_,
  substring_ as substring,
  indentString,
  stringAppend_,
  stringDowncase_,
  stringJoin_,
  stringRef_,
  stringRepeat_,
  stringReplace_,
  stringSplit_,
  stringTrim_,
  stringUpcase_,
  substring_
};
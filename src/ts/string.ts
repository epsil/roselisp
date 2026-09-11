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

import {
  currentCompilationOptions
} from './env';

const [plistGet]: any[] = ((): any => {
  function plistGet_(plst: any, prop: any): any {
    let val: any = undefined;
    const _end: any = plst.length;
    for (let i: any = 0; i < _end; i = i + 2) {
      if ((plst as any)[i] === prop) {
        val = plst[i + 1];
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
function stringp_(x: any): any {
  return (typeof x === 'string') || (x instanceof String);
}

stringp_.fsource = [Symbol.for('define'), [Symbol.for('string?_'), Symbol.for('x')], [Symbol.for('js/string?'), Symbol.for('x')]];

/**
 * Compiler macro for `(string? ...)` expressions.
 */
stringp_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [x]: any[] = exp.slice(1);
    const {fstringobjects} = currentCompilationOptions();
    if (fstringobjects) {
      return [Symbol.for('funcall'), Symbol.for('string?'), x];
    } else {
      return [Symbol.for('js/string-literal?'), x];
    }
  };
  f.ftype = 'macro';
  return f;
})();

/**
 * The length of a string.
 */
function stringLength_(x: any): any {
  return x.length;
}

stringLength_.fsource = [Symbol.for('define'), [Symbol.for('string-length_'), Symbol.for('x')], [Symbol.for('js/length'), Symbol.for('x')]];

stringLength_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('js/length'), x];
  };
  f.ftype = 'macro';
  return f;
})();

/**
 * Concatenate one or more strings together.
 *
 * Similar to [`string-append` in Racket][rkt:string-append].
 *
 * [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
 */
function stringAppend_(...args: any[]): any {
  let result: any = '';
  for (let x of args) {
    result = result + x;
  }
  return result;
}

stringAppend_.fsource = [Symbol.for('define'), [Symbol.for('string-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), '']], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/string-concat'), Symbol.for('result'), Symbol.for('x')]]], Symbol.for('result')]];

/**
 * Compiler macro for `(string-append ...)` expressions.
 */
stringAppend_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const args: any = exp.slice(1);
    return [Symbol.for('js/string-concat'), ...args];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringRef_.fsource = [Symbol.for('define'), [Symbol.for('string-ref_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('charAt'), Symbol.for('n')]];

stringRef_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str, n]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('charAt'), n];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringTrim_.fsource = [Symbol.for('define'), [Symbol.for('string-trim_'), Symbol.for('str'), [Symbol.for('sep'), undefined], Symbol.for('.'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('sep'), [Symbol.for('define'), Symbol.for('repeat-option'), [Symbol.for('plist-get'), Symbol.for('options'), Symbol.for(':repeat?')]], [Symbol.for('define'), Symbol.for('pattern-str'), [Symbol.for('string-append'), '(', [Symbol.for('regexp-quote'), Symbol.for('sep')], ')', [Symbol.for('if'), Symbol.for('repeat-option'), '+', '']]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), '^', Symbol.for('pattern-str')]], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), [Symbol.for('string-append'), Symbol.for('pattern-str'), '$']], Symbol.for('_'), '']]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('trim')]]]];

/**
 * Compiler macro for `(string-trim ...)` expressions.
 */
stringTrim_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str, ...args]: any[] = exp.slice(1);
    if (Array.isArray(args) && (args.length === 0)) {
      return [Symbol.for('send'), str, Symbol.for('trim')];
    } else {
      return [Symbol.for('funcall'), Symbol.for('string-trim'), str, ...args];
    }
  };
  f.ftype = 'macro';
  return f;
})();

/**
 * Repeat a string `n` times.
 */
function stringRepeat_(str: any, n: any): any {
  return str.repeat(n);
}

stringRepeat_.fsource = [Symbol.for('define'), [Symbol.for('string-repeat_'), Symbol.for('str'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('repeat'), Symbol.for('n')]];

stringRepeat_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str, n]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('repeat'), n];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringJoin_.fsource = [Symbol.for('define'), [Symbol.for('string-join_'), Symbol.for('lst'), [Symbol.for('sep'), ' ']], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('join'), Symbol.for('sep')]];

stringJoin_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    let [lst, sep]: any[] = exp.slice(1);
    if (sep === undefined) {
      sep = ' ';
    }
    return [Symbol.for('send'), lst, Symbol.for('join'), sep];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringSplit_.fsource = [Symbol.for('define'), [Symbol.for('string-split_'), Symbol.for('str'), [Symbol.for('sep'), [Symbol.for('regexp'), '\\s+', 'g']]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('split'), Symbol.for('sep')]];

stringSplit_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    let [str, sep]: any[] = exp.slice(1);
    if (sep === undefined) {
      sep = [Symbol.for('regexp'), '\\s+', 'g'];
    }
    return [Symbol.for('send'), str, Symbol.for('split'), sep];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringReplace_.fsource = [Symbol.for('define'), [Symbol.for('string-replace_'), Symbol.for('str'), Symbol.for('from'), Symbol.for('to')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('from'), Symbol.for('to')]];

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

stringUpcase_.fsource = [Symbol.for('define'), [Symbol.for('string-upcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toUpperCase')]];

stringUpcase_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('toUpperCase')];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringDowncase_.fsource = [Symbol.for('define'), [Symbol.for('string-downcase_'), Symbol.for('str')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('toLowerCase')]];

stringDowncase_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('toLowerCase')];
  };
  f.ftype = 'macro';
  return f;
})();

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

substring_.fsource = [Symbol.for('define'), [Symbol.for('substring_'), Symbol.for('str'), Symbol.for('start'), [Symbol.for('end'), undefined]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('substring'), Symbol.for('start'), Symbol.for('end')]];

/**
 * Compiler macro for `(substring ...)` expressions.
 */
substring_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str, ...args]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('substring'), ...args];
  };
  f.ftype = 'macro';
  return f;
})();

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

stringToNumber_.fsource = [Symbol.for('define'), [Symbol.for('string->number_'), Symbol.for('str')], [Symbol.for('js/parse-float'), Symbol.for('str')]];

stringToNumber_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [str]: any[] = exp.slice(1);
    return [Symbol.for('js/parse-float'), str];
  };
  f.ftype = 'macro';
  return f;
})();

/**
 * Convert a number to a string.
 *
 * Similar to [`number->string` in Racket][rkt:number-to-string].
 *
 * [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
 */
function numberToString_(n: any): any {
  return n.toString();
}

numberToString_.fsource = [Symbol.for('define'), [Symbol.for('number->string_'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('n'), Symbol.for('toString')]];

numberToString_.compilerMacro = ((): any => {
  const f: any = function (exp: any, env: any): any {
    const [n]: any[] = exp.slice(1);
    return [Symbol.for('send'), n, Symbol.for('toString')];
  };
  f.ftype = 'macro';
  return f;
})();

export {
  numberToString_ as numberToString,
  stringToNumber_ as stringToNumber,
  stringAppend_ as stringAppend,
  stringReplace_ as stringReplace,
  stringp_ as stringp,
  substring_ as substring,
  numberToString_,
  stringAppend_,
  stringDowncase_,
  stringJoin_,
  stringLength_,
  stringRef_,
  stringRepeat_,
  stringReplace_,
  stringSplit_,
  stringToNumber_,
  stringTrim_,
  stringUpcase_,
  stringp_,
  substring_
};
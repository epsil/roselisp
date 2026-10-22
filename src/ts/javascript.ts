// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # JavaScript
 *
 * Basic JavaScript constructs.
 *
 * ## Description
 *
 * This file defines functions for various basic
 * JavaScript constructs.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  join
} from 'path';

import {
  cwd
} from 'process';

import {
  getNodeModulesPath
} from './util';

/**
 * JavaScript's [`eval` function][js:eval].
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
function jsEval_(str: any): any {
  return eval(str);
}

jsEval_.fsource = [Symbol.for('define'), [Symbol.for('js/eval_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];

/**
 * JavaScript [strict equality][js:strict-equality],
 * i.e., the [`===`][js:strict-equality-operator] operator.
 *
 * [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
 * [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
 */
function jsStrictlyEqualP_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left === right);
}

jsStrictlyEqualP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('==='), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsStrictlyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/strictly-equal?_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('==='), Symbol.for('args')]];

/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
function jsLooselyEqualP_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left == right);
}

jsLooselyEqualP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('=='), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsLooselyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/loosely-equal?_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('=='), Symbol.for('args')]];

/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
function jsSameValueP_(x: any, y: any): any {
  return Object.is(x, y);
}

jsSameValueP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x, y]: any[] = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('Object'), Symbol.for('is'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();

jsSameValueP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('is'), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
function jsSameValueZeroP_(x: any, y: any): any {
  return Object.is(x, y) || (isNaN(x) && isNaN(y));
}

jsSameValueZeroP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value-zero?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), [Symbol.for('js/same-value?'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('and'), [Symbol.for('js/nan?'), Symbol.for('x')], [Symbol.for('js/nan?'), Symbol.for('y')]]]];

/**
 * Whether something is JavaScript's `null`.
 */
function jsNullP_(x: any): any {
  return x === null;
}

jsNullP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('eq?'), x, null];
  };
  f.ftype = 'macro';
  return f;
})();

jsNullP_.fsource = [Symbol.for('define'), [Symbol.for('js/null?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), Symbol.for('x'), null]];

/**
 * Whether a number is JavaScript's [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
function jsIsNaN_(x: any): any {
  return isNaN(x);
}

jsIsNaN_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('isNaN'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsIsNaN_.fsource = [Symbol.for('define'), [Symbol.for('js/is-NaN_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('isNaN'), Symbol.for('x')]];

/**
 * Whether `x` is a JavaScript function.
 */
function jsFunctionP_(x: any): any {
  // In JavaScript, every function is a
  // [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
  return x instanceof Function;
}

jsFunctionP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('js/function-object?'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsFunctionP_.fsource = [Symbol.for('define'), [Symbol.for('js/function?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/function-object?'), Symbol.for('x')]];

/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
function jsFunctionObjectP_(x: any): any {
  return x instanceof Function;
}

jsFunctionObjectP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('is-a?'), x, Symbol.for('Function')];
  };
  f.ftype = 'macro';
  return f;
})();

jsFunctionObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Function')]];

/**
 * Whether `obj` is of type `"function"`.
 */
function jsFunctionTypeP_(x: any): any {
  return typeof x === 'function';
}

jsFunctionTypeP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'function'];
  };
  f.ftype = 'macro';
  return f;
})();

jsFunctionTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-type?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'function']];

/**
 * Whether `obj` is an arrow function.
 */
function jsArrowP_(x: any): any {
  if ((x instanceof Function) && (x + '').match(/^(\([^)]*\)|[^=]*) *=>/)) {
    return true;
  } else {
    return false;
  }
}

jsArrowP_.fsource = [Symbol.for('define'), [Symbol.for('js/arrow?_'), Symbol.for('x')], [Symbol.for('true?'), [Symbol.for('and'), [Symbol.for('js/function?'), Symbol.for('x')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^(\\([^)]*\\)|[^=]*) *=>'], [Symbol.for('js/source'), Symbol.for('x')]]]]];

/**
 * Returns the JavaScript source of a function.
 */
function jsSource_(f: any): any {
  return f + '';
}

jsSource_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [f]: any[] = exp.slice(1);
    return [Symbol.for('js/to-string'), f];
  };
  f.ftype = 'macro';
  return f;
})();

jsSource_.fsource = [Symbol.for('define'), [Symbol.for('js/source_'), Symbol.for('f')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/to-string'), Symbol.for('f')]];

/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
function jsTypeof_(x: any): any {
  return typeof x;
}

jsTypeof_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('js/op'), Symbol.for('typeof'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsTypeof_.fsource = [Symbol.for('define'), [Symbol.for('js/typeof_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('typeof'), Symbol.for('x')]];

/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
function jsInstanceof_(x: any, y: any): any {
  return x instanceof y;
}

jsInstanceof_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x, y]: any[] = exp.slice(1);
    return [Symbol.for('js/op'), Symbol.for('instanceof'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();

jsInstanceof_.fsource = [Symbol.for('define'), [Symbol.for('js/instanceof_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('instanceof'), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
function jsIn_(prop: any, obj: any): any {
  return prop in obj;
}

jsIn_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [prop, obj]: any[] = exp.slice(1);
    return [Symbol.for('js/op'), Symbol.for('in'), prop, obj];
  };
  f.ftype = 'macro';
  return f;
})();

jsIn_.fsource = [Symbol.for('define'), [Symbol.for('js/in_'), Symbol.for('prop'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('in'), Symbol.for('prop'), Symbol.for('obj')]];

/**
 * Variadic version of JavaScript's `+` operator.
 *
 * Performs [addition][js:add] or [string concatenation][js:concat],
 * depending on the types.
 *
 * [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
 * [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
 */
function jsPlus_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left + right);
}

jsPlus_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('+'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsPlus_.fsource = [Symbol.for('define'), [Symbol.for('js/plus_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('+'), Symbol.for('args')]];

/**
 * Return the absolute value of `x`.
 */
function jsAbs_(x: any): any {
  return Math.abs(x);
}

jsAbs_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsAbs_.fsource = [Symbol.for('define'), [Symbol.for('js/abs_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('x')]];

/**
 * Find the index of a list element matching a predicate.
 *
 * Like `findf-index`, but returns `-1` rather than `#f`
 * if there is no match.
 */
function jsFindIndex_(proc: any, seq: any): any {
  // This construct maps neatly onto
  // [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
  return seq.findIndex(proc);
}

jsFindIndex_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [proc, seq]: any[] = exp.slice(1);
    return [Symbol.for('send'), seq, Symbol.for('findIndex'), proc];
  };
  f.ftype = 'macro';
  return f;
})();

jsFindIndex_.fsource = [Symbol.for('define'), [Symbol.for('js/find-index_'), Symbol.for('proc'), Symbol.for('seq')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('seq'), Symbol.for('findIndex'), Symbol.for('proc')]];

/**
 * Placeholder function for JavaScript's
 * [tagged template][js:tagged-template] construct.
 *
 * [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
function jsTaggedTemplate_(tag: any, ...args: any[]): any {
  return tag;
}

jsTaggedTemplate_.fsource = [Symbol.for('define'), [Symbol.for('js/tagged-template_'), Symbol.for('tag'), Symbol.for('.'), Symbol.for('args')], Symbol.for('tag')];

/**
 * Placeholder function for JavaScript's
 * [`delete`][js:delete] operator.
 *
 * [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
 */
function jsDelete_(x: any): any {
  // This function does nothing by itself, but a call to it
  // will be compiled to a `UnaryExpression` ESTree node
  // invoking `delete`.

  // TODO: Define as a fexpr instead.
  return undefined;
}

jsDelete_.fsource = [Symbol.for('define'), [Symbol.for('js/delete_'), Symbol.for('x')], undefined];

/**
 * Whether something is a JavaScript array.
 */
function jsArrayP_(x: any): any {
  return Array.isArray(x);
}

jsArrayP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsArrayP_.fsource = [Symbol.for('define'), [Symbol.for('js/array?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('x')]];

/**
 * Return the length of a JavaScript string or array.
 */
function jsLength_(x: any): any {
  return x.length;
}

jsLength_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('get-field'), Symbol.for('length'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsLength_.fsource = [Symbol.for('define'), [Symbol.for('js/length_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('x')]];

/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
function jsRef_(obj: any, key: any): any {
  return (obj as any)[key];
}

jsRef_.fsource = [Symbol.for('define'), [Symbol.for('js/ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('js/ref'), Symbol.for('obj'), Symbol.for('key')]];

/**
 * Look up the property `prop` in the JavaScript object `obj`.
 */
function jsDot_(obj: any, prop: any): any {
  return obj.prop;
}

jsDot_.fsource = [Symbol.for('define'), [Symbol.for('js/dot_'), Symbol.for('obj'), Symbol.for('prop')], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop')]];

/**
 * Look up properties `args` in the JavaScript object `obj`,
 * using optional chaining.
 */
function jsOptionalChaining_(obj: any, ...args: any[]): any {
  let result: any = obj;
  for (let x of args) {
    if (x in result) {
      result = (result as any)[x];
    } else {
      result = undefined;
      break;
    }
  }
  return result;
}

jsOptionalChaining_.fsource = [Symbol.for('define'), [Symbol.for('js/optional-chaining_'), Symbol.for('obj'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), Symbol.for('obj')]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('cond'), [[Symbol.for('js/in'), Symbol.for('x'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/ref'), Symbol.for('result'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), undefined], [Symbol.for('break')]]]], Symbol.for('result')]];

/**
 * Slice a JavaScript array.
 */
function jsSlice_(arr: any, ...args: any[]): any {
  return arr.slice(...args);
}

jsSlice_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [arr, ...args]: any[] = exp.slice(1);
    return [Symbol.for('send/apply'), arr, Symbol.for('slice'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsSlice_.fsource = [Symbol.for('define'), [Symbol.for('js/slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];

/**
 * Fold up a JavaScript array left to right.
 */
function jsReduce_(arr: any, ...args: any[]): any {
  return arr.reduce(...args);
}

jsReduce_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [arr, ...args]: any[] = exp.slice(1);
    return [Symbol.for('send/apply'), arr, Symbol.for('reduce'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsReduce_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduce'), Symbol.for('args')]];

/**
 * Fold up a JavaScript array right to left.
 */
function jsReduceRight_(arr: any, ...args: any[]): any {
  return arr.reduceRight(...args);
}

jsReduceRight_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [arr, ...args]: any[] = exp.slice(1);
    return [Symbol.for('send/apply'), arr, Symbol.for('reduceRight'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsReduceRight_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce-right_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduceRight'), Symbol.for('args')]];

/**
 * Whether something is a JavaScript string.
 */
function jsStringP_(x: any): any {
  return (typeof x === 'string') || (x instanceof String);
}

jsStringP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    if (!(Array.isArray(x) && (x.length > 0))) {
      return [Symbol.for('or'), [Symbol.for('js/string-literal?'), x], [Symbol.for('js/string-object?'), x]];
    } else {
      const x1: any = Symbol('x');
      return [Symbol.for('let'), [[x1, x]], ((x: any): any => [Symbol.for('or'), [Symbol.for('js/string-literal?'), x], [Symbol.for('js/string-object?'), x]])(x1)];
    }
  };
  f.ftype = 'macro';
  return f;
})();

jsStringP_.fsource = [Symbol.for('define'), [Symbol.for('js/string?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('or'), [Symbol.for('js/string-literal?'), Symbol.for('x')], [Symbol.for('js/string-object?'), Symbol.for('x')]]];

/**
 * Whether something is a JavaScript string literal.
 */
function jsStringLiteralP_(x: any): any {
  return typeof x === 'string';
}

jsStringLiteralP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'string'];
  };
  f.ftype = 'macro';
  return f;
})();

jsStringLiteralP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-literal?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'string']];

/**
 * Whether something is a JavaScript string object.
 */
function jsStringObjectP_(x: any): any {
  return x instanceof String;
}

jsStringObjectP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('is-a?'), x, Symbol.for('String')];
  };
  f.ftype = 'macro';
  return f;
})();

jsStringObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('String')]];

/**
 * Concatenate two or more JavaScript strings together.
 */
function jsStringConcat_(...args: any[]): any {
  let result: any = '';
  for (let x of args) {
    result = result + x;
  }
  return result;
}

jsStringConcat_.fsource = [Symbol.for('define'), [Symbol.for('js/string-concat_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), '']], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/op'), Symbol.for('+'), Symbol.for('result'), Symbol.for('x')]]], Symbol.for('result')]];

/**
 * Compiler macro for `(js/string-concat ...)` expressions.
 */
jsStringConcat_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('+'), [Symbol.for('list'), ...args], Symbol.for(':identity'), ''];
  };
  f.ftype = 'macro';
  return f;
})();

/**
 * Convert `x` to a string.
 */
function jsToString_(x: any): any {
  return x + '';
}

jsToString_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('js/+'), x, ''];
  };
  f.ftype = 'macro';
  return f;
})();

jsToString_.fsource = [Symbol.for('define'), [Symbol.for('js/to-string_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/+'), Symbol.for('x'), '']];

/**
 * Create a JavaScript regular expression.
 */
function jsRegexp_(input: any, flags: any = undefined): any {
  return new RegExp(input, flags);
}

jsRegexp_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp_'), Symbol.for('input'), [Symbol.for('flags'), undefined]], [Symbol.for('new'), Symbol.for('RegExp'), Symbol.for('input'), Symbol.for('flags')]];

/**
 * Whether `obj` is a JavaScript regular expression.
 */
function jsRegexpP_(obj: any): any {
  return obj instanceof RegExp;
}

jsRegexpP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj]: any[] = exp.slice(1);
    return [Symbol.for('is-a?'), obj, Symbol.for('RegExp')];
  };
  f.ftype = 'macro';
  return f;
})();

jsRegexpP_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp?_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('RegExp')]];

/**
 * Match a string or regular expression against
 * a JavaScript string.
 */
function jsRegexpMatch_(str: any, pattern: any): any {
  return str.match(pattern);
}

jsRegexpMatch_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [str, pattern]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('match'), pattern];
  };
  f.ftype = 'macro';
  return f;
})();

jsRegexpMatch_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-match_'), Symbol.for('str'), Symbol.for('pattern')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('match'), Symbol.for('pattern')]];

/**
 * Match a string or regular expression against
 * a JavaScript string and replace the matches
 * with a given string or replacement pattern.
 */
function jsRegexpReplace_(str: any, pattern: any, insert: any): any {
  return str.replace(pattern, insert);
}

jsRegexpReplace_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [str, pattern, insert]: any[] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('replace'), pattern, insert];
  };
  f.ftype = 'macro';
  return f;
})();

jsRegexpReplace_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-replace_'), Symbol.for('str'), Symbol.for('pattern'), Symbol.for('insert')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('pattern'), Symbol.for('insert')]];

/**
 * Create a JavaScript `new` expression.
 */
function jsNew_(x: any, ...args: any[]): any {
  return new x(...args);
}

jsNew_.fsource = [Symbol.for('define'), [Symbol.for('js/new_'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('new/apply'), Symbol.for('x'), Symbol.for('args')]];

/**
 * Create a JavaScript `return` statement.
 */
function jsReturn_(x: any = undefined): any {
  return x;
}

jsReturn_.fsource = [Symbol.for('define'), [Symbol.for('js/return_'), [Symbol.for('x'), undefined]], Symbol.for('x')];

/**
 * Create a JavaScript `yield` expression.
 */
function jsYield_(x: any = undefined): any {
  return x;
}

jsYield_.fsource = [Symbol.for('define'), [Symbol.for('js/yield_'), [Symbol.for('x'), undefined]], Symbol.for('x')];

/**
 * Logical negation.
 */
function jsNot_(x: any): any {
  return !x;
}

jsNot_.fsource = [Symbol.for('define'), [Symbol.for('js/not_'), Symbol.for('x')], [Symbol.for('js/op'), Symbol.for('!'), Symbol.for('x')]];

/**
 * Logical AND.
 */
function jsAnd_(...args: any[]): any {
  return (args.length === 0) ? true : args.reduce((left: any, right: any): any => left && right);
}

jsAnd_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('&&'), [Symbol.for('list'), ...args], Symbol.for(':identity'), true];
  };
  f.ftype = 'macro';
  return f;
})();

jsAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('&&'), Symbol.for('args'), Symbol.for(':identity'), true]];

/**
 * Logical OR.
 */
function jsOr_(...args: any[]): any {
  return (args.length === 0) ? false : args.reduce((left: any, right: any): any => left || right);
}

jsOr_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('||'), [Symbol.for('list'), ...args], Symbol.for(':identity'), false];
  };
  f.ftype = 'macro';
  return f;
})();

jsOr_.fsource = [Symbol.for('define'), [Symbol.for('js/or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('||'), Symbol.for('args'), Symbol.for(':identity'), false]];

/**
 * Bitwise NOT.
 */
function jsBitwiseNot_(x: any): any {
  return ~x;
}

jsBitwiseNot_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('js/op'), Symbol.for('~'), x];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseNot_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-not_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]];

/**
 * Bitwise AND.
 */
function jsBitwiseAnd_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left & right);
}

jsBitwiseAnd_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('&'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('&'), Symbol.for('args')]];

/**
 * Bitwise OR.
 */
function jsBitwiseOr_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left | right);
}

jsBitwiseOr_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('|'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseOr_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('|'), Symbol.for('args')]];

/**
 * Bitwise XOR.
 */
function jsBitwiseXor_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left ^ right);
}

jsBitwiseXor_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('^'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseXor_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-xor_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('^'), Symbol.for('args')]];

/**
 * Bitwise left shift.
 */
function jsBitwiseShiftLeft_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left << right);
}

jsBitwiseShiftLeft_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('<<'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseShiftLeft_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-left_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('<<'), Symbol.for('args')]];

/**
 * Bitwise right shift.
 */
function jsBitwiseShiftRight_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left >> right);
}

jsBitwiseShiftRight_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('>>'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>>'), Symbol.for('args')]];

/**
 * Bitwise unsigned right shift.
 */
function jsUnsignedBitwiseShiftRight_(...args: any[]): any {
  return (args.length === 0) ? undefined : args.reduce((left: any, right: any): any => left >>> right);
}

jsUnsignedBitwiseShiftRight_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('>>>'), [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();

jsUnsignedBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/unsigned-bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>>>'), Symbol.for('args')]];

/**
 * Less than comparison.
 */
function jsLt_(...args: any[]): any {
  if (args.length <= 1) {
    return true;
  } else {
    let result: any = true;
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      if (!(args[i - 1] < (args as any)[i])) {
        result = false;
        break;
      }
    }
    return result;
  }
}

jsLt_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('<'), [Symbol.for('list'), ...args], Symbol.for(':fold'), Symbol.for('chain')];
  };
  f.ftype = 'macro';
  return f;
})();

jsLt_.fsource = [Symbol.for('define'), [Symbol.for('js/lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('<'), Symbol.for('args'), Symbol.for(':fold'), Symbol.for('chain')]];

/**
 * Less than or equal comparison.
 */
function jsLte_(...args: any[]): any {
  if (args.length <= 1) {
    return true;
  } else {
    let result: any = true;
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      if (!(args[i - 1] <= (args as any)[i])) {
        result = false;
        break;
      }
    }
    return result;
  }
}

jsLte_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('<='), [Symbol.for('list'), ...args], Symbol.for(':fold'), Symbol.for('chain')];
  };
  f.ftype = 'macro';
  return f;
})();

jsLte_.fsource = [Symbol.for('define'), [Symbol.for('js/lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('<='), Symbol.for('args'), Symbol.for(':fold'), Symbol.for('chain')]];

/**
 * Greater than comparison.
 */
function jsGt_(...args: any[]): any {
  if (args.length <= 1) {
    return true;
  } else {
    let result: any = true;
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      if (!(args[i - 1] > (args as any)[i])) {
        result = false;
        break;
      }
    }
    return result;
  }
}

jsGt_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('>'), [Symbol.for('list'), ...args], Symbol.for(':fold'), Symbol.for('chain')];
  };
  f.ftype = 'macro';
  return f;
})();

jsGt_.fsource = [Symbol.for('define'), [Symbol.for('js/gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>'), Symbol.for('args'), Symbol.for(':fold'), Symbol.for('chain')]];

/**
 * Greater than or equal comparison.
 */
function jsGte_(...args: any[]): any {
  if (args.length <= 1) {
    return true;
  } else {
    let result: any = true;
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      if (!(args[i - 1] >= (args as any)[i])) {
        result = false;
        break;
      }
    }
    return result;
  }
}

jsGte_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('>='), [Symbol.for('list'), ...args], Symbol.for(':fold'), Symbol.for('chain')];
  };
  f.ftype = 'macro';
  return f;
})();

jsGte_.fsource = [Symbol.for('define'), [Symbol.for('js/gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>='), Symbol.for('args'), Symbol.for(':fold'), Symbol.for('chain')]];

/**
 * Addition.
 */
function jsAdd_(...args: any[]): any {
  return (args.length === 0) ? 0 : args.reduce((left: any, right: any): any => left + right);
}

jsAdd_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('+'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 0];
  };
  f.ftype = 'macro';
  return f;
})();

jsAdd_.fsource = [Symbol.for('define'), [Symbol.for('js/add_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('+'), Symbol.for('args'), Symbol.for(':identity'), 0]];

/**
 * Subtraction.
 */
function jsSub_(...args: any[]): any {
  if (args.length === 0) {
    return 0;
  } else if (args.length === 1) {
    return -args[0];
  } else {
    return args.reduce((left: any, right: any): any => left - right);
  }
}

jsSub_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('-'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 0, Symbol.for(':unary'), Symbol.for('pre')];
  };
  f.ftype = 'macro';
  return f;
})();

jsSub_.fsource = [Symbol.for('define'), [Symbol.for('js/sub_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('-'), Symbol.for('args'), Symbol.for(':identity'), 0, Symbol.for(':unary'), Symbol.for('pre')]];

/**
 * Multiplication.
 */
function jsMul_(...args: any[]): any {
  return (args.length === 0) ? 1 : args.reduce((left: any, right: any): any => left * right);
}

jsMul_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('*'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 1];
  };
  f.ftype = 'macro';
  return f;
})();

jsMul_.fsource = [Symbol.for('define'), [Symbol.for('js/mul_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('*'), Symbol.for('args'), Symbol.for(':identity'), 1]];

/**
 * Division.
 */
function jsDiv_(...args: any[]): any {
  if (args.length === 0) {
    return 1;
  } else if (args.length === 1) {
    return 1 / args[0];
  } else {
    return args.reduce((left: any, right: any): any => left / right);
  }
}

jsDiv_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('/'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 1, Symbol.for(':unary'), Symbol.for('right')];
  };
  f.ftype = 'macro';
  return f;
})();

jsDiv_.fsource = [Symbol.for('define'), [Symbol.for('js/div_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('/'), Symbol.for('args'), Symbol.for(':identity'), 1, Symbol.for(':unary'), Symbol.for('right')]];

/**
 * Modulo operation.
 */
function jsMod_(x: any, y: any): any {
  return x % y;
}

jsMod_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x, y]: any[] = exp.slice(1);
    return [Symbol.for('js/op'), Symbol.for('%'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();

jsMod_.fsource = [Symbol.for('define'), [Symbol.for('js/mod_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('%'), Symbol.for('x'), Symbol.for('y')]];

/**
 * Exponentiation.
 */
function jsExpt_(...args: any[]): any {
  return (args.length === 0) ? 1 : args.reduceRight((right: any, left: any): any => left ** right);
}

jsExpt_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const args: any = exp.slice(1);
    return [Symbol.for('js/op/apply'), Symbol.for('**'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 1, Symbol.for(':fold'), Symbol.for('right')];
  };
  f.ftype = 'macro';
  return f;
})();

jsExpt_.fsource = [Symbol.for('define'), [Symbol.for('js/expt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('**'), Symbol.for('args'), Symbol.for(':identity'), 1, Symbol.for(':fold'), Symbol.for('right')]];

/**
 * Convert a string to a number.
 */
function jsParseFloat_(str: any): any {
  return parseFloat(str);
}

jsParseFloat_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [str]: any[] = exp.slice(1);
    return [Symbol.for('parseFloat'), str];
  };
  f.ftype = 'macro';
  return f;
})();

jsParseFloat_.fsource = [Symbol.for('define'), [Symbol.for('js/parse-float_'), Symbol.for('str')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('parseFloat'), Symbol.for('str')]];

/**
 * Create a JavaScript `Promise`.
 */
function jsPromise_(f: any): any {
  return new Promise(f);
}

jsPromise_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [f]: any[] = exp.slice(1);
    return [Symbol.for('new'), Symbol.for('Promise'), f];
  };
  f.ftype = 'macro';
  return f;
})();

jsPromise_.fsource = [Symbol.for('define'), [Symbol.for('js/promise_'), Symbol.for('f')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('new'), Symbol.for('Promise'), Symbol.for('f')]];

/**
 * Whether something is a JavaScript `Promise`.
 */
function jsPromiseP_(x: any): any {
  return x instanceof Promise;
}

jsPromiseP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('is-a?'), x, Symbol.for('Promise')];
  };
  f.ftype = 'macro';
  return f;
})();

jsPromiseP_.fsource = [Symbol.for('define'), [Symbol.for('js/promise?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Promise')]];

/**
 * Import a module with [Node's `require`][nodejs:require].
 * Returns `#f` if not found.
 *
 * Note that `require` is a declarative construct which is not really
 * suited for being called as a function, because
 * [Node's `require` algorithm][nodejs:require-algorithm] interprets
 * the `require` call as relative to the file containing it (i.e.,
 * this file), rather than the current working directory. To make
 * `js/require` more practical when called interactively, the
 * approach taken here is to convert a relative path to an absolute
 * paths, which is then required.
 *
 * [nodejs:require]: https://nodejs.org/api/modules.html#requireid
 * [nodejs:require-algorithm]: https://nodejs.org/api/modules.html#all-together
 */
function jsRequire_(id: any): any {
  let result: any = false;
  if (id.match(new RegExp('^\\.+/'))) {
    // If the module path is relative (starts with `./` or `../`), then
    // interpret it as relative to the current working directory and
    // rewrite it to an absolute path.
    const modulePath: any = join(cwd(), id);
    try {
      result = require(modulePath);
    } catch {
    }
  } else {
    // If the module path is not relative, then it probably references
    // a module located in `node_modules/`, so rewrite it to
    // "`/absolute/path/to/node_modules/id`".
    const nodeModulesPath: any = getNodeModulesPath(cwd());
    if (nodeModulesPath) {
      const modulePath: any = join(nodeModulesPath, id);
      try {
        result = require(modulePath);
      } catch {
      }
    }
    // Final attempt: load the module as a system module.
    if (!result) {
      try {
        result = require(id);
      } catch {
      }
    }
  }
  return result;
}

jsRequire_.fsource = [Symbol.for('define'), [Symbol.for('js/require_'), Symbol.for('id')], [Symbol.for('define'), Symbol.for('result'), false], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.+/'], Symbol.for('id')], [Symbol.for('define'), Symbol.for('module-path'), [Symbol.for('join'), [Symbol.for('cwd')], Symbol.for('id')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('module-path')]], [Symbol.for('catch')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('node-modules-path'), [Symbol.for('get-node-modules-path'), [Symbol.for('cwd')]]], [Symbol.for('when'), Symbol.for('node-modules-path'), [Symbol.for('define'), Symbol.for('module-path'), [Symbol.for('join'), Symbol.for('node-modules-path'), Symbol.for('id')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('module-path')]], [Symbol.for('catch')]]], [Symbol.for('unless'), Symbol.for('result'), [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('id')]], [Symbol.for('catch')]]]]], Symbol.for('result')];

export {
  jsAdd_ as _add,
  jsSub_ as _sub,
  jsMul_ as _mul,
  jsDiv_ as _div,
  jsAdd_,
  jsSub_,
  jsMul_,
  jsDiv_,
  jsAbs_,
  jsAnd_,
  jsArrayP_,
  jsArrowP_,
  jsBitwiseAnd_,
  jsBitwiseNot_,
  jsBitwiseOr_,
  jsBitwiseShiftLeft_,
  jsBitwiseShiftRight_,
  jsBitwiseXor_,
  jsDelete_,
  jsDot_,
  jsEval_,
  jsExpt_,
  jsFindIndex_,
  jsFunctionObjectP_,
  jsFunctionTypeP_,
  jsFunctionP_,
  jsGt_,
  jsGte_,
  jsIn_,
  jsInstanceof_,
  jsIsNaN_,
  jsLength_,
  jsLooselyEqualP_,
  jsLt_,
  jsLte_,
  jsMod_,
  jsNew_,
  jsNot_,
  jsNullP_,
  jsOptionalChaining_,
  jsOr_,
  jsParseFloat_,
  jsPlus_,
  jsPromiseP_,
  jsPromise_,
  jsReduceRight_,
  jsReduce_,
  jsRef_,
  jsRegexpMatch_,
  jsRegexpReplace_,
  jsRegexpP_,
  jsRegexp_,
  jsRequire_,
  jsReturn_,
  jsSameValueZeroP_,
  jsSameValueP_,
  jsSlice_,
  jsSource_,
  jsStrictlyEqualP_,
  jsStringConcat_,
  jsStringLiteralP_,
  jsStringObjectP_,
  jsStringP_,
  jsTaggedTemplate_,
  jsToString_,
  jsTypeof_,
  jsUnsignedBitwiseShiftRight_,
  jsYield_
};
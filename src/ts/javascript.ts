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
function jsStrictlyEqualP_(x: any, y: any): any {
  return x === y;
}

jsStrictlyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/strictly-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
function jsLooselyEqualP_(x: any, y: any): any {
  return x == y;
}

jsLooselyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/loosely-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/=='), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
function jsSameValueP_(x: any, y: any): any {
  return Object.is(x, y);
}

jsSameValueP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('is'), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
function jsSameValueZeroP_(x: any, y: any): any {
  return (x === y) || (Number.isNaN(x) && Number.isNaN(y));
}

jsSameValueZeroP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value-zero?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')], [Symbol.for('and'), [Symbol.for('js/nan?'), Symbol.for('x')], [Symbol.for('js/nan?'), Symbol.for('y')]]]];

/**
 * Whether something is JavaScript's `null`.
 */
function jsNullP_(obj: any): any {
  return obj === null;
}

jsNullP_.fsource = [Symbol.for('define'), [Symbol.for('js/null?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), null]];

/**
 * Whether a number is JavaScript's [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
function jsNanP_(x: any, y: any): any {
  return Number.isNaN(x);
}

jsNanP_.fsource = [Symbol.for('define'), [Symbol.for('js/nan?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isNaN'), Symbol.for('x')]];

/**
 * Whether `obj` is a JavaScript function.
 */
function jsFunctionP_(obj: any): any {
  // In JavaScript, every function is a
  // [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
  return obj instanceof Function;
}

jsFunctionP_.fsource = [Symbol.for('define'), [Symbol.for('js/function?_'), Symbol.for('obj')], [Symbol.for('js/function-object?'), Symbol.for('obj')]];

/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
function jsFunctionObjectP_(obj: any): any {
  return obj instanceof Function;
}

jsFunctionObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-object?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Function')]];

/**
 * Whether `obj` is of type `"function"`.
 */
function jsFunctionTypeP_(obj: any): any {
  return typeof obj === 'function';
}

jsFunctionTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-type?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'function']];

/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
function jsTypeOf_(x: any): any {
  return typeof x;
}

jsTypeOf_.fsource = [Symbol.for('define'), [Symbol.for('js/type-of_'), Symbol.for('x')], [Symbol.for('js/type-of'), Symbol.for('x')]];

/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
function jsInstanceOfP_(x: any, y: any): any {
  return x instanceof y;
}

jsInstanceOfP_.fsource = [Symbol.for('define'), [Symbol.for('js/instance-of?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/instance-of?'), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
function jsIn_(prop: any, obj: any): any {
  return prop in obj;
}

jsIn_.fsource = [Symbol.for('define'), [Symbol.for('js/in_'), Symbol.for('prop'), Symbol.for('obj')], [Symbol.for('js/op'), Symbol.for('in'), Symbol.for('prop'), Symbol.for('obj')]];

/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function jsObj_(...args: any[]): any {
  const entries: any = [];
  const _end: any = args.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    entries.push([(args as any)[i], args[i + 1]]);
  }
  return Object.fromEntries(entries);
}

jsObj_.fsource = [Symbol.for('define'), [Symbol.for('js/obj_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('entries'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('args')], 2]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')], [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]]];

/**
 * Whether something is a JavaScript object.
 */
function jsObjP_(x: any): any {
  // This function avoids regarding JavaScript's `null` value as an
  // object (even if JavaScript does), because it has no properties;
  // and unlike the empty object, attempting to access a property on
  // it causes an error to be thrown. This is more trouble than it is
  // worth, so only non-`null` object values are considered to be
  // proper objects here.
  return (x !== null) && (typeof x === 'object');
}

jsObjP_.fsource = [Symbol.for('define'), [Symbol.for('js/obj?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), Symbol.for('x')]], [Symbol.for('js/object-type?'), Symbol.for('x')]]];

/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function jsObjectTypeP_(x: any): any {
  return typeof x === 'object';
}

jsObjectTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/object-type?_'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];

/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function jsObjAppend_(...args: any[]): any {
  return Object.assign({}, ...args);
}

jsObjAppend_.fsource = [Symbol.for('define'), [Symbol.for('js/obj-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('js/obj')], Symbol.for('args')]];

/**
 * Spread a JavaScript object into another.
 */
function jsObjSpread_(x: any): any {
  return x;
}

jsObjSpread_.fsource = [Symbol.for('define'), [Symbol.for('js/obj-spread_'), Symbol.for('x')], Symbol.for('x')];

/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function jsKeys_(obj: any): any {
  return Object.keys(obj);
}

jsKeys_.fsource = [Symbol.for('define'), [Symbol.for('js/keys_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), Symbol.for('obj')]];

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
  if (args.length === 0) {
    return undefined;
  } else {
    let result: any = args[0];
    for (let x of args.slice(1)) {
      result = result + x;
    }
    return result;
  }
}

jsPlus_.fsource = [Symbol.for('define'), [Symbol.for('js/plus_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('zero?'), [Symbol.for('length'), Symbol.for('args')]], undefined], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('rest'), Symbol.for('args')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/+'), Symbol.for('result'), Symbol.for('x')]]], Symbol.for('result')]]]];

/**
 * Return the absolute value of `x`.
 */
function jsAbs_(x: any): any {
  return Math.abs(x);
}

jsAbs_.fsource = [Symbol.for('define'), [Symbol.for('js/abs_'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('x')]];

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

jsFindIndex_.fsource = [Symbol.for('define'), [Symbol.for('js/find-index_'), Symbol.for('proc'), Symbol.for('seq')], [Symbol.for('send'), Symbol.for('seq'), Symbol.for('findIndex'), Symbol.for('proc')]];

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
  return undefined;
}

jsDelete_.fsource = [Symbol.for('define'), [Symbol.for('js/delete_'), Symbol.for('x')], undefined];

/**
 * Whether something is a JavaScript array.
 */
function jsArrayP_(x: any): any {
  return Array.isArray(x);
}

jsArrayP_.fsource = [Symbol.for('define'), [Symbol.for('js/array?_'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('x')]];

/**
 * Return the length of a JavaScript string or array.
 */
function jsLength_(x: any): any {
  return x.length;
}

jsLength_.fsource = [Symbol.for('define'), [Symbol.for('js/length_'), Symbol.for('x')], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('x')]];

/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
function jsGet_(obj: any, key: any): any {
  return (obj as any)[key];
}

jsGet_.fsource = [Symbol.for('define'), [Symbol.for('js/get_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('js/get'), Symbol.for('obj'), Symbol.for('key')]];

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

jsOptionalChaining_.fsource = [Symbol.for('define'), [Symbol.for('js/optional-chaining_'), Symbol.for('obj'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), Symbol.for('obj')]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('cond'), [[Symbol.for('js/in'), Symbol.for('x'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/get'), Symbol.for('result'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), undefined], [Symbol.for('break')]]]], Symbol.for('result')]];

/**
 * Slice a JavaScript array.
 */
function jsSlice_(arr: any, ...args: any[]): any {
  return arr.slice(...args);
}

jsSlice_.fsource = [Symbol.for('define'), [Symbol.for('js/slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];

/**
 * Fold up a JavaScript array left to right.
 */
function jsReduce_(arr: any, ...args: any[]): any {
  return arr.reduce(...args);
}

jsReduce_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduce'), Symbol.for('args')]];

/**
 * Fold up a JavaScript array right to left.
 */
function jsReduceRight_(arr: any, ...args: any[]): any {
  return arr.reduceRight(...args);
}

jsReduceRight_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce-right_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduceRight'), Symbol.for('args')]];

/**
 * Whether something is a JavaScript string.
 */
function jsStringP_(x: any): any {
  return (typeof x === 'string') || (x instanceof String);
}

jsStringP_.fsource = [Symbol.for('define'), [Symbol.for('js/string?_'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('js/string-literal?'), Symbol.for('x')], [Symbol.for('js/string-object?'), Symbol.for('x')]]];

/**
 * Whether something is a JavaScript string literal.
 */
function jsStringLiteralP_(x: any): any {
  return typeof x === 'string';
}

jsStringLiteralP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-literal?_'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'string']];

/**
 * Whether something is a JavaScript string object.
 */
function jsStringObjectP_(x: any): any {
  return x instanceof String;
}

jsStringObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-object?_'), Symbol.for('x')], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('String')]];

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

jsRegexpP_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('RegExp')]];

/**
 * Match a string or regular expression against
 * a JavaScript string.
 */
function jsRegexpMatch_(str: any, pattern: any): any {
  return str.match(pattern);
}

jsRegexpMatch_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-match_'), Symbol.for('str'), Symbol.for('pattern')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('match'), Symbol.for('pattern')]];

/**
 * Match a string or regular expression against
 * a JavaScript string and replace the matches
 * with a given string or replacement pattern.
 */
function jsRegexpReplace_(str: any, pattern: any, insert: any): any {
  return str.replace(pattern, insert);
}

jsRegexpReplace_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-replace_'), Symbol.for('str'), Symbol.for('pattern'), Symbol.for('insert')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('pattern'), Symbol.for('insert')]];

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
 * Less than comparison.
 */
function jsLt_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x < y) === (x >= y)
      if (args[i - 1] >= (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

jsLt_.fsource = [Symbol.for('define'), [Symbol.for('js/lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>='), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];

/**
 * Less than or equal comparison.
 */
function jsLte_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x <= y) === (x > y)
      if (args[i - 1] > (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

jsLte_.fsource = [Symbol.for('define'), [Symbol.for('js/lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];

/**
 * Greater than comparison.
 */
function jsGt_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x > y) === (x <= y)
      if (args[i - 1] <= (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

jsGt_.fsource = [Symbol.for('define'), [Symbol.for('js/gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<='), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];

/**
 * Greater than or equal comparison.
 */
function jsGte_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x >= y) === (x < y)
      if (args[i - 1] < (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

jsGte_.fsource = [Symbol.for('define'), [Symbol.for('js/gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];

/**
 * Modulo operation.
 */
function jsMod_(x: any, y: any): any {
  return x % y;
}

jsMod_.fsource = [Symbol.for('define'), [Symbol.for('js/mod_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/%'), Symbol.for('x'), Symbol.for('y')]];

/**
 * Logical negation.
 */
function jsNot_(x: any): any {
  return !x;
}

jsNot_.fsource = [Symbol.for('define'), [Symbol.for('js/not_'), Symbol.for('x')], [Symbol.for('js/!'), Symbol.for('x')]];

/**
 * Logical AND.
 */
function jsAnd_(...args: any[]): any {
  return args.reduce(function (left: any, right: any): any {
    return left && right;
  }, true);
}

jsAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('&&'), Symbol.for('args'), Symbol.for(':identity'), true]];

/**
 * Logical OR.
 */
function jsOr_(...args: any[]): any {
  return args.reduce(function (left: any, right: any): any {
    return left || right;
  }, false);
}

jsOr_.fsource = [Symbol.for('define'), [Symbol.for('js/or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('||'), Symbol.for('args'), Symbol.for(':identity'), false]];

/**
 * Bitwise NOT.
 */
function jsBitwiseNot_(x: any): any {
  return ~x;
}

jsBitwiseNot_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-not_'), Symbol.for('x')], [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]];

/**
 * Bitwise AND.
 */
function jsBitwiseAnd_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left & right;
  }, args[0]);
}

jsBitwiseAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('&'), Symbol.for('args')]];

/**
 * Bitwise OR.
 */
function jsBitwiseOr_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left | right;
  }, args[0]);
}

jsBitwiseOr_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('|'), Symbol.for('args')]];

/**
 * Bitwise XOR.
 */
function jsBitwiseXor_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left ^ right;
  }, args[0]);
}

jsBitwiseXor_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-xor_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('^'), Symbol.for('args')]];

/**
 * Bitwise left shift.
 */
function jsBitwiseShiftLeft_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left << right;
  }, args[0]);
}

jsBitwiseShiftLeft_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-left_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('<<'), Symbol.for('args')]];

/**
 * Bitwise right shift.
 */
function jsBitwiseShiftRight_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left >> right;
  }, args[0]);
}

jsBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('>>'), Symbol.for('args')]];

/**
 * Bitwise unsigned right shift.
 */
function jsUnsignedBitwiseShiftRight_(...args: any[]): any {
  return args.slice(1).reduce(function (left: any, right: any): any {
    return left >>> right;
  }, args[0]);
}

jsUnsignedBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/unsigned-bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('>>>'), Symbol.for('args')]];

export {
  jsAbs_,
  jsAnd_,
  jsArrayP_,
  jsBitwiseAnd_,
  jsBitwiseNot_,
  jsBitwiseOr_,
  jsBitwiseShiftLeft_,
  jsBitwiseShiftRight_,
  jsBitwiseXor_,
  jsDelete_,
  jsDot_,
  jsEval_,
  jsFindIndex_,
  jsFunctionObjectP_,
  jsFunctionTypeP_,
  jsFunctionP_,
  jsGet_,
  jsGt_,
  jsGte_,
  jsIn_,
  jsInstanceOfP_,
  jsKeys_,
  jsLength_,
  jsLooselyEqualP_,
  jsLt_,
  jsLte_,
  jsMod_,
  jsNanP_,
  jsNew_,
  jsNot_,
  jsNullP_,
  jsObjAppend_,
  jsObjSpread_,
  jsObjP_,
  jsObj_,
  jsObjectTypeP_,
  jsOptionalChaining_,
  jsOr_,
  jsPlus_,
  jsReduceRight_,
  jsReduce_,
  jsRegexpMatch_,
  jsRegexpReplace_,
  jsRegexpP_,
  jsRegexp_,
  jsReturn_,
  jsSameValueZeroP_,
  jsSameValueP_,
  jsSlice_,
  jsStrictlyEqualP_,
  jsStringConcat_,
  jsStringLiteralP_,
  jsStringObjectP_,
  jsStringP_,
  jsTaggedTemplate_,
  jsTypeOf_,
  jsUnsignedBitwiseShiftRight_,
  jsYield_
};
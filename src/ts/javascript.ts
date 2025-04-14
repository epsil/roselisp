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
 * JavaScript [strict equality][js:strict-equality],
 * i.e., the [`===`][js:strict-equality-operator] operator.
 *
 * [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
 * [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
 */
function jsIsStrictlyEqualP_(x: any, y: any): any {
  return x === y;
}

jsIsStrictlyEqualP_.lispSource = [Symbol.for('define'), [Symbol.for('js-is-strictly-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
function jsIsLooselyEqualP_(x: any, y: any): any {
  return x == y;
}

jsIsLooselyEqualP_.lispSource = [Symbol.for('define'), [Symbol.for('js-is-loosely-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/=='), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
function jsSameValueP_(x: any, y: any): any {
  return Object.is(x, y);
}

jsSameValueP_.lispSource = [Symbol.for('define'), [Symbol.for('js-same-value?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('is'), Symbol.for('x'), Symbol.for('y')]];

/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
function jsSameValueZeroP_(x: any, y: any): any {
  return (x === y) || (Number.isNaN(x) && Number.isNaN(y));
}

jsSameValueZeroP_.lispSource = [Symbol.for('define'), [Symbol.for('js-same-value-zero?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')], [Symbol.for('and'), [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isNaN'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isNaN'), Symbol.for('y')]]]];

/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
function jsTypeof_(x: any): any {
  return typeof x;
}

jsTypeof_.lispSource = [Symbol.for('define'), [Symbol.for('js-typeof_'), Symbol.for('x')], [Symbol.for('js/typeof'), Symbol.for('x')]];

/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
function jsInstanceof_(x: any, y: any): any {
  return x instanceof y;
}

jsInstanceof_.lispSource = [Symbol.for('define'), [Symbol.for('js-instanceof_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/instanceof'), Symbol.for('x'), Symbol.for('y')]];

/**
 * Variadic version of JavaScript's `+` operator.
 *
 * Performs [addition][js:add] or [string concatenation][js:concat]
 * depending on the types.
 *
 * [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
 * [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
 */
function jsPlus_(...args: any[]): any {
  const len: any = args.length;
  if (len === 0) {
    return undefined;
  } else {
    let result: any = args[0];
    for (let i: any = 1; i < len; i++) {
      result = result + (args as any)[i];
    }
    return result;
  }
}

jsPlus_.lispSource = [Symbol.for('define'), [Symbol.for('js-plus_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('len'), [Symbol.for('array-length'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('zero?'), Symbol.for('len')], Symbol.for('undefined')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('array-first'), Symbol.for('args')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, Symbol.for('len')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/+'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]]]], Symbol.for('result')]]]];

/**
 * Whether `obj` is a JavaScript function.
 */
function jsFunctionP_(obj: any): any {
  // In JavaScript, every function is a
  // [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
  return obj instanceof Function;
}

jsFunctionP_.lispSource = [Symbol.for('define'), [Symbol.for('js-function?_'), Symbol.for('obj')], [Symbol.for('js/function-object?'), Symbol.for('obj')]];

/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
function jsFunctionObjectP_(obj: any): any {
  return obj instanceof Function;
}

jsFunctionObjectP_.lispSource = [Symbol.for('define'), [Symbol.for('js-function-object?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Function')]];

/**
 * Whether `obj` is of type `"function"`.
 */
function jsFunctionTypeP_(obj: any): any {
  return typeof obj === 'function';
}

jsFunctionTypeP_.lispSource = [Symbol.for('define'), [Symbol.for('js-function-type?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'function']];

/**
 * Whether something is JavaScript's `null`.
 */
function jsNullP_(obj: any): any {
  return obj === null;
}

jsNullP_.lispSource = [Symbol.for('define'), [Symbol.for('js-null?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), Symbol.for('js/null')]];

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

jsFindIndex_.lispSource = [Symbol.for('define'), [Symbol.for('js-find-index_'), Symbol.for('proc'), Symbol.for('seq')], [Symbol.for('send'), Symbol.for('seq'), Symbol.for('findIndex'), Symbol.for('proc')]];

/**
 * JavaScript's [`eval` function][js:eval].
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
function jsEval_(str: any): any {
  return eval(str);
}

jsEval_.lispSource = [Symbol.for('define'), [Symbol.for('js-eval_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];

/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
function jsIn_(prop: any, obj: any): any {
  return prop in obj;
}

jsIn_.lispSource = [Symbol.for('define'), [Symbol.for('js-in_'), Symbol.for('prop'), Symbol.for('obj')], [Symbol.for('js/in'), Symbol.for('prop'), Symbol.for('obj')]];

/**
 * Placeholder function for JavaScript's
 * [tagged template][js:tagged-template] construct.
 *
 * [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
function jsTaggedTemplate_(tag: any, ...args: any[]): any {
  return tag;
}

jsTaggedTemplate_.lispSource = [Symbol.for('define'), [Symbol.for('js-tagged-template_'), Symbol.for('tag'), Symbol.for('.'), Symbol.for('args')], Symbol.for('tag')];

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

jsDelete_.lispSource = [Symbol.for('define'), [Symbol.for('js-delete_'), Symbol.for('x')], Symbol.for('undefined')];

export {
  jsDelete_,
  jsEval_,
  jsFindIndex_,
  jsFunctionObjectP_,
  jsFunctionTypeP_,
  jsFunctionP_,
  jsIn_,
  jsInstanceof_,
  jsIsLooselyEqualP_,
  jsIsStrictlyEqualP_,
  jsNullP_,
  jsPlus_,
  jsSameValueZeroP_,
  jsSameValueP_,
  jsTaggedTemplate_,
  jsTypeof_
};
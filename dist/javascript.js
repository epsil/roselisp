"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.jsRegexp_ = exports.jsRegexpP_ = exports.jsRegexpReplace_ = exports.jsRegexpMatch_ = exports.jsReduce_ = exports.jsReduceRight_ = exports.jsPlus_ = exports.jsOr_ = exports.jsOptionalChaining_ = exports.jsObjectTypeP_ = exports.jsObj_ = exports.jsObjP_ = exports.jsObjAppend_ = exports.jsNullP_ = exports.jsNot_ = exports.jsNinth_ = exports.jsNew_ = exports.jsNanP_ = exports.jsMod_ = exports.jsLte_ = exports.jsLt_ = exports.jsLooselyEqualP_ = exports.jsLength_ = exports.jsLast_ = exports.jsKeys_ = exports.jsInstanceOfP_ = exports.jsIn_ = exports.jsGte_ = exports.jsGt_ = exports.jsGet_ = exports.jsFunctionP_ = exports.jsFunctionTypeP_ = exports.jsFunctionObjectP_ = exports.jsFourth_ = exports.jsFirst_ = exports.jsFindIndex_ = exports.jsFifth_ = exports.jsEval_ = exports.jsEighth_ = exports.jsDot_ = exports.jsDelete_ = exports.jsBitwiseXor_ = exports.jsBitwiseShiftRight_ = exports.jsBitwiseShiftLeft_ = exports.jsBitwiseOr_ = exports.jsBitwiseNot_ = exports.jsBitwiseAnd_ = exports.jsArrayP_ = exports.jsAnd_ = exports.jsAbs_ = void 0;
exports.jsYield_ = exports.jsUnsignedBitwiseShiftRight_ = exports.jsTypeOf_ = exports.jsThird_ = exports.jsTenth_ = exports.jsTake_ = exports.jsTaggedTemplate_ = exports.jsStrictlyEqualP_ = exports.jsSlice_ = exports.jsSixth_ = exports.jsSeventh_ = exports.jsSecond_ = exports.jsSameValueP_ = exports.jsSameValueZeroP_ = exports.jsReverse_ = exports.jsReturn_ = exports.jsRest_ = void 0;
/**
 * JavaScript [strict equality][js:strict-equality],
 * i.e., the [`===`][js:strict-equality-operator] operator.
 *
 * [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
 * [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
 */
function jsStrictlyEqualP_(x, y) {
    return x === y;
}
exports.jsStrictlyEqualP_ = jsStrictlyEqualP_;
jsStrictlyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/strictly-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')]];
/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
function jsLooselyEqualP_(x, y) {
    return x == y;
}
exports.jsLooselyEqualP_ = jsLooselyEqualP_;
jsLooselyEqualP_.fsource = [Symbol.for('define'), [Symbol.for('js/loosely-equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/=='), Symbol.for('x'), Symbol.for('y')]];
/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
function jsSameValueP_(x, y) {
    return Object.is(x, y);
}
exports.jsSameValueP_ = jsSameValueP_;
jsSameValueP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('is'), Symbol.for('x'), Symbol.for('y')]];
/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
function jsSameValueZeroP_(x, y) {
    return (x === y) || (Number.isNaN(x) && Number.isNaN(y));
}
exports.jsSameValueZeroP_ = jsSameValueZeroP_;
jsSameValueZeroP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value-zero?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')], [Symbol.for('and'), [Symbol.for('js/nan?'), Symbol.for('x')], [Symbol.for('js/nan?'), Symbol.for('y')]]]];
/**
 * Whether something is JavaScript's `null`.
 */
function jsNullP_(obj) {
    return obj === null;
}
exports.jsNullP_ = jsNullP_;
jsNullP_.fsource = [Symbol.for('define'), [Symbol.for('js/null?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), null]];
/**
 * Whether a number is JavaScript's [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
function jsNanP_(x, y) {
    return Number.isNaN(x);
}
exports.jsNanP_ = jsNanP_;
jsNanP_.fsource = [Symbol.for('define'), [Symbol.for('js/nan?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isNaN'), Symbol.for('x')]];
/**
 * Whether `obj` is a JavaScript function.
 */
function jsFunctionP_(obj) {
    // In JavaScript, every function is a
    // [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
    return obj instanceof Function;
}
exports.jsFunctionP_ = jsFunctionP_;
jsFunctionP_.fsource = [Symbol.for('define'), [Symbol.for('js/function?_'), Symbol.for('obj')], [Symbol.for('js/function-object?'), Symbol.for('obj')]];
/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
function jsFunctionObjectP_(obj) {
    return obj instanceof Function;
}
exports.jsFunctionObjectP_ = jsFunctionObjectP_;
jsFunctionObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-object?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Function')]];
/**
 * Whether `obj` is of type `"function"`.
 */
function jsFunctionTypeP_(obj) {
    return typeof obj === 'function';
}
exports.jsFunctionTypeP_ = jsFunctionTypeP_;
jsFunctionTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-type?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'function']];
/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
function jsTypeOf_(x) {
    return typeof x;
}
exports.jsTypeOf_ = jsTypeOf_;
jsTypeOf_.fsource = [Symbol.for('define'), [Symbol.for('js/type-of_'), Symbol.for('x')], [Symbol.for('js/type-of'), Symbol.for('x')]];
/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
function jsInstanceOfP_(x, y) {
    return x instanceof y;
}
exports.jsInstanceOfP_ = jsInstanceOfP_;
jsInstanceOfP_.fsource = [Symbol.for('define'), [Symbol.for('js/instance-of?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/instance-of?'), Symbol.for('x'), Symbol.for('y')]];
/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
function jsIn_(prop, obj) {
    return prop in obj;
}
exports.jsIn_ = jsIn_;
jsIn_.fsource = [Symbol.for('define'), [Symbol.for('js/in_'), Symbol.for('prop'), Symbol.for('obj')], [Symbol.for('js/in'), Symbol.for('prop'), Symbol.for('obj')]];
/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function jsObj_(...args) {
    const entries = [];
    const _end = args.length;
    for (let i = 0; i < _end; i = i + 2) {
        entries.push([args[i], args[i + 1]]);
    }
    return Object.fromEntries(entries);
}
exports.jsObj_ = jsObj_;
jsObj_.fsource = [Symbol.for('define'), [Symbol.for('js/obj_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('entries'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('args')], 2]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), [Symbol.for('js/get'), Symbol.for('args'), Symbol.for('i')], [Symbol.for('js/get'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]]];
/**
 * Whether something is a JavaScript object.
 */
function jsObjP_(x) {
    // This function avoids regarding JavaScript's `null` value as an
    // object (even if JavaScript does), because it has no properties;
    // and unlike the empty object, attempting to access a property on
    // it causes an error to be thrown. This is more trouble than it is
    // worth, so only non-`null` object values are considered to be
    // proper objects here.
    return (x !== null) && (typeof x === 'object');
}
exports.jsObjP_ = jsObjP_;
jsObjP_.fsource = [Symbol.for('define'), [Symbol.for('js/obj?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), Symbol.for('x')]], [Symbol.for('js/object-type?'), Symbol.for('x')]]];
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function jsObjectTypeP_(x) {
    return typeof x === 'object';
}
exports.jsObjectTypeP_ = jsObjectTypeP_;
jsObjectTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/object-type?_'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function jsObjAppend_(...args) {
    return Object.assign({}, ...args);
}
exports.jsObjAppend_ = jsObjAppend_;
jsObjAppend_.fsource = [Symbol.for('define'), [Symbol.for('js/obj-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('js/obj')], Symbol.for('args')]];
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function jsKeys_(obj) {
    return Object.keys(obj);
}
exports.jsKeys_ = jsKeys_;
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
function jsPlus_(...args) {
    if (args.length === 0) {
        return undefined;
    }
    else {
        return args.reduce(function (acc, x) {
            return acc + x;
        });
    }
}
exports.jsPlus_ = jsPlus_;
jsPlus_.fsource = [Symbol.for('define'), [Symbol.for('js/plus_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('if'), [Symbol.for('zero?'), [Symbol.for('js/length'), Symbol.for('args')]], undefined, [Symbol.for('js/reduce'), Symbol.for('args'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [Symbol.for('js/+'), Symbol.for('acc'), Symbol.for('x')]]]]];
/**
 * Return the absolute value of `x`.
 */
function jsAbs_(x) {
    return Math.abs(x);
}
exports.jsAbs_ = jsAbs_;
jsAbs_.fsource = [Symbol.for('define'), [Symbol.for('js/abs_'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('x')]];
/**
 * Find the index of a list element matching a predicate.
 *
 * Like `findf-index`, but returns `-1` rather than `#f`
 * if there is no match.
 */
function jsFindIndex_(proc, seq) {
    // This construct maps neatly onto
    // [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
    return seq.findIndex(proc);
}
exports.jsFindIndex_ = jsFindIndex_;
jsFindIndex_.fsource = [Symbol.for('define'), [Symbol.for('js/find-index_'), Symbol.for('proc'), Symbol.for('seq')], [Symbol.for('send'), Symbol.for('seq'), Symbol.for('findIndex'), Symbol.for('proc')]];
/**
 * Placeholder function for JavaScript's
 * [tagged template][js:tagged-template] construct.
 *
 * [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
function jsTaggedTemplate_(tag, ...args) {
    return tag;
}
exports.jsTaggedTemplate_ = jsTaggedTemplate_;
jsTaggedTemplate_.fsource = [Symbol.for('define'), [Symbol.for('js/tagged-template_'), Symbol.for('tag'), Symbol.for('.'), Symbol.for('args')], Symbol.for('tag')];
/**
 * Placeholder function for JavaScript's
 * [`delete`][js:delete] operator.
 *
 * [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
 */
function jsDelete_(x) {
    // This function does nothing by itself, but a call to it
    // will be compiled to a `UnaryExpression` ESTree node
    // invoking `delete`.
    return undefined;
}
exports.jsDelete_ = jsDelete_;
jsDelete_.fsource = [Symbol.for('define'), [Symbol.for('js/delete_'), Symbol.for('x')], undefined];
/**
 * Whether something is a JavaScript array.
 */
function jsArrayP_(obj) {
    return Array.isArray(obj);
}
exports.jsArrayP_ = jsArrayP_;
jsArrayP_.fsource = [Symbol.for('define'), [Symbol.for('js/array?_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('obj')]];
/**
 * Return the last element of a JavaScript array.
 */
function jsLast_(arr) {
    return arr[arr.length - 1];
}
exports.jsLast_ = jsLast_;
jsLast_.fsource = [Symbol.for('define'), [Symbol.for('js/last_'), Symbol.for('arr')], [Symbol.for('js/get'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('arr')], 1]]];
/**
 * Return the length of a JavaScript string or array.
 */
function jsLength_(arr) {
    return arr.length;
}
exports.jsLength_ = jsLength_;
jsLength_.fsource = [Symbol.for('define'), [Symbol.for('js/length_'), Symbol.for('arr')], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('arr')]];
/**
 * Return the first element of a JavaScript array.
 */
function jsFirst_(lst) {
    return lst[0];
}
exports.jsFirst_ = jsFirst_;
jsFirst_.fsource = [Symbol.for('define'), [Symbol.for('js/first_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 0]];
/**
 * Return the second element of a JavaScript array.
 */
function jsSecond_(lst) {
    return lst[1];
}
exports.jsSecond_ = jsSecond_;
jsSecond_.fsource = [Symbol.for('define'), [Symbol.for('js/second_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 1]];
/**
 * Return the third element of a JavaScript array.
 */
function jsThird_(lst) {
    return lst[2];
}
exports.jsThird_ = jsThird_;
jsThird_.fsource = [Symbol.for('define'), [Symbol.for('js/third_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 2]];
/**
 * Return the fourth element of a JavaScript array.
 */
function jsFourth_(lst) {
    return lst[3];
}
exports.jsFourth_ = jsFourth_;
jsFourth_.fsource = [Symbol.for('define'), [Symbol.for('js/fourth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 3]];
/**
 * Return the fifth element of a JavaScript array.
 */
function jsFifth_(lst) {
    return lst[4];
}
exports.jsFifth_ = jsFifth_;
jsFifth_.fsource = [Symbol.for('define'), [Symbol.for('js/fifth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 4]];
/**
 * Return the sixth element of a JavaScript array.
 */
function jsSixth_(lst) {
    return lst[5];
}
exports.jsSixth_ = jsSixth_;
jsSixth_.fsource = [Symbol.for('define'), [Symbol.for('js/sixth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 5]];
/**
 * Return the seventh element of a JavaScript array.
 */
function jsSeventh_(lst) {
    return lst[6];
}
exports.jsSeventh_ = jsSeventh_;
jsSeventh_.fsource = [Symbol.for('define'), [Symbol.for('js/seventh_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 6]];
/**
 * Return the eight element of a JavaScript array.
 */
function jsEighth_(lst) {
    return lst[7];
}
exports.jsEighth_ = jsEighth_;
jsEighth_.fsource = [Symbol.for('define'), [Symbol.for('js/eighth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 7]];
/**
 * Return the ninth element of a JavaScript array.
 */
function jsNinth_(lst) {
    return lst[8];
}
exports.jsNinth_ = jsNinth_;
jsNinth_.fsource = [Symbol.for('define'), [Symbol.for('js/ninth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 8]];
/**
 * Return the tenth element of a JavaScript array.
 */
function jsTenth_(lst) {
    return lst[9];
}
exports.jsTenth_ = jsTenth_;
jsTenth_.fsource = [Symbol.for('define'), [Symbol.for('js/tenth_'), Symbol.for('lst')], [Symbol.for('js/get'), Symbol.for('lst'), 9]];
/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
function jsGet_(obj, key) {
    return obj[key];
}
exports.jsGet_ = jsGet_;
jsGet_.fsource = [Symbol.for('define'), [Symbol.for('js/get_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('js/get'), Symbol.for('obj'), Symbol.for('key')]];
/**
 * Look up the property `prop` in the JavaScript object `obj`.
 */
function jsDot_(obj, prop) {
    return obj.prop;
}
exports.jsDot_ = jsDot_;
jsDot_.fsource = [Symbol.for('define'), [Symbol.for('js/dot_'), Symbol.for('obj'), Symbol.for('prop')], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop')]];
/**
 * Look up properties `args` in the JavaScript object `obj`,
 * using optional chaining.
 */
function jsOptionalChaining_(obj, ...args) {
    return args.reduce(function (obj, prop) {
        return obj === null || obj === void 0 ? void 0 : obj.prop;
    }, obj);
}
exports.jsOptionalChaining_ = jsOptionalChaining_;
jsOptionalChaining_.fsource = [Symbol.for('define'), [Symbol.for('js/optional-chaining_'), Symbol.for('obj'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('prop'), Symbol.for('obj')], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop')]], Symbol.for('obj'), Symbol.for('args')]];
/**
 * Slice a JavaScript array.
 */
function jsSlice_(arr, ...args) {
    return arr.slice(...args);
}
exports.jsSlice_ = jsSlice_;
jsSlice_.fsource = [Symbol.for('define'), [Symbol.for('js/slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];
/**
 * Return the tail of a JavaScript array.
 */
function jsRest_(arr) {
    return arr.slice(1);
}
exports.jsRest_ = jsRest_;
jsRest_.fsource = [Symbol.for('define'), [Symbol.for('js/rest_'), Symbol.for('arr')], [Symbol.for('js/slice'), Symbol.for('arr'), 1]];
/**
 * Reverse the order of a JavaScript array.
 * Returns a new array.
 */
function jsReverse_(arr) {
    return arr.reverse();
}
exports.jsReverse_ = jsReverse_;
jsReverse_.fsource = [Symbol.for('define'), [Symbol.for('js/reverse_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];
/**
 * Take the `n` first elements from
 * the JavaScript array `arr`.
 */
function jsTake_(arr, n) {
    return arr.slice(0, arr.length - n);
}
exports.jsTake_ = jsTake_;
jsTake_.fsource = [Symbol.for('define'), [Symbol.for('js/take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('js/slice'), Symbol.for('arr'), 0, [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('arr')], Symbol.for('n')]]];
/**
 * Fold up a JavaScript array left to right.
 */
function jsReduce_(arr, ...args) {
    return arr.reduce(...args);
}
exports.jsReduce_ = jsReduce_;
jsReduce_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduce'), Symbol.for('args')]];
/**
 * Fold up a JavaScript array right to left.
 */
function jsReduceRight_(arr, ...args) {
    return arr.reduceRight(...args);
}
exports.jsReduceRight_ = jsReduceRight_;
jsReduceRight_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce-right_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduceRight'), Symbol.for('args')]];
/**
 * Create a JavaScript regular expression.
 */
function jsRegexp_(input, flags = undefined) {
    return new RegExp(input, flags);
}
exports.jsRegexp_ = jsRegexp_;
jsRegexp_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp_'), Symbol.for('input'), [Symbol.for('flags'), undefined]], [Symbol.for('new'), Symbol.for('RegExp'), Symbol.for('input'), Symbol.for('flags')]];
/**
 * Whether `obj` is a JavaScript regular expression.
 */
function jsRegexpP_(obj) {
    return obj instanceof RegExp;
}
exports.jsRegexpP_ = jsRegexpP_;
jsRegexpP_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp?_'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('RegExp')]];
/**
 * Match a string or regular expression against
 * a JavaScript string.
 */
function jsRegexpMatch_(str, pattern) {
    return str.match(pattern);
}
exports.jsRegexpMatch_ = jsRegexpMatch_;
jsRegexpMatch_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-match_'), Symbol.for('str'), Symbol.for('pattern')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('match'), Symbol.for('pattern')]];
/**
 * Match a string or regular expression against
 * a JavaScript string and replace the matches
 * with a given string or replacement pattern.
 */
function jsRegexpReplace_(str, pattern, insert) {
    return str.replace(pattern, insert);
}
exports.jsRegexpReplace_ = jsRegexpReplace_;
jsRegexpReplace_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-replace_'), Symbol.for('str'), Symbol.for('pattern'), Symbol.for('insert')], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('pattern'), Symbol.for('insert')]];
/**
 * JavaScript's [`eval` function][js:eval].
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
function jsEval_(str) {
    return eval(str);
}
exports.jsEval_ = jsEval_;
jsEval_.fsource = [Symbol.for('define'), [Symbol.for('js/eval_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];
/**
 * Create a JavaScript `new` expression.
 */
function jsNew_(x, ...args) {
    return new x(...args);
}
exports.jsNew_ = jsNew_;
jsNew_.fsource = [Symbol.for('define'), [Symbol.for('js/new_'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('new/apply'), Symbol.for('x'), Symbol.for('args')]];
/**
 * Create a JavaScript `return` statement.
 */
function jsReturn_(x = undefined) {
    return x;
}
exports.jsReturn_ = jsReturn_;
jsReturn_.fsource = [Symbol.for('define'), [Symbol.for('js/return_'), [Symbol.for('x'), undefined]], Symbol.for('x')];
/**
 * Create a JavaScript `yield` expression.
 */
function jsYield_(x = undefined) {
    return x;
}
exports.jsYield_ = jsYield_;
jsYield_.fsource = [Symbol.for('define'), [Symbol.for('js/yield_'), [Symbol.for('x'), undefined]], Symbol.for('x')];
/**
 * Less than comparison.
 */
function jsLt_(...args) {
    if (args.length < 2) {
        return true;
    }
    else {
        const _end = args.length;
        for (let i = 1; i < _end; i++) {
            // !(x < y) === (x >= y)
            if (args[i - 1] >= args[i]) {
                return false;
            }
        }
        return true;
    }
}
exports.jsLt_ = jsLt_;
jsLt_.fsource = [Symbol.for('define'), [Symbol.for('js/lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('js/length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('js/length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('js/>='), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
/**
 * Less than or equal comparison.
 */
function jsLte_(...args) {
    if (args.length < 2) {
        return true;
    }
    else {
        const _end = args.length;
        for (let i = 1; i < _end; i++) {
            // !(x <= y) === (x > y)
            if (args[i - 1] > args[i]) {
                return false;
            }
        }
        return true;
    }
}
exports.jsLte_ = jsLte_;
jsLte_.fsource = [Symbol.for('define'), [Symbol.for('js/lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('js/length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('js/length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('js/>'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
/**
 * Greater than comparison.
 */
function jsGt_(...args) {
    if (args.length < 2) {
        return true;
    }
    else {
        const _end = args.length;
        for (let i = 1; i < _end; i++) {
            // !(x > y) === (x <= y)
            if (args[i - 1] <= args[i]) {
                return false;
            }
        }
        return true;
    }
}
exports.jsGt_ = jsGt_;
jsGt_.fsource = [Symbol.for('define'), [Symbol.for('js/gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('js/length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('js/length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('js/<='), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
/**
 * Greater than or equal comparison.
 */
function jsGte_(...args) {
    if (args.length < 2) {
        return true;
    }
    else {
        const _end = args.length;
        for (let i = 1; i < _end; i++) {
            // !(x >= y) === (x < y)
            if (args[i - 1] < args[i]) {
                return false;
            }
        }
        return true;
    }
}
exports.jsGte_ = jsGte_;
jsGte_.fsource = [Symbol.for('define'), [Symbol.for('js/gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('js/length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('js/length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('js/<'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
/**
 * Modulo operation.
 */
function jsMod_(x, y) {
    return x % y;
}
exports.jsMod_ = jsMod_;
jsMod_.fsource = [Symbol.for('define'), [Symbol.for('js/mod_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/%'), Symbol.for('x'), Symbol.for('y')]];
/**
 * Logical negation.
 */
function jsNot_(x) {
    return !x;
}
exports.jsNot_ = jsNot_;
jsNot_.fsource = [Symbol.for('define'), [Symbol.for('js/not_'), Symbol.for('x')], [Symbol.for('js/!'), Symbol.for('x')]];
/**
 * Logical AND.
 */
function jsAnd_(...args) {
    return args.reduce(function (left, right) {
        return left && right;
    }, true);
}
exports.jsAnd_ = jsAnd_;
jsAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('&&'), Symbol.for('args'), Symbol.for(':identity'), true]];
/**
 * Logical OR.
 */
function jsOr_(...args) {
    return args.reduce(function (left, right) {
        return left || right;
    }, false);
}
exports.jsOr_ = jsOr_;
jsOr_.fsource = [Symbol.for('define'), [Symbol.for('js/or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('||'), Symbol.for('args'), Symbol.for(':identity'), false]];
/**
 * Bitwise NOT.
 */
function jsBitwiseNot_(x) {
    return ~x;
}
exports.jsBitwiseNot_ = jsBitwiseNot_;
jsBitwiseNot_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-not_'), Symbol.for('x')], [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]];
/**
 * Bitwise AND.
 */
function jsBitwiseAnd_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left & right;
    }, args[0]);
}
exports.jsBitwiseAnd_ = jsBitwiseAnd_;
jsBitwiseAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('&'), Symbol.for('args')]];
/**
 * Bitwise OR.
 */
function jsBitwiseOr_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left | right;
    }, args[0]);
}
exports.jsBitwiseOr_ = jsBitwiseOr_;
jsBitwiseOr_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('|'), Symbol.for('args')]];
/**
 * Bitwise XOR.
 */
function jsBitwiseXor_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left ^ right;
    }, args[0]);
}
exports.jsBitwiseXor_ = jsBitwiseXor_;
jsBitwiseXor_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-xor_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('^'), Symbol.for('args')]];
/**
 * Bitwise left shift.
 */
function jsBitwiseShiftLeft_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left << right;
    }, args[0]);
}
exports.jsBitwiseShiftLeft_ = jsBitwiseShiftLeft_;
jsBitwiseShiftLeft_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-left_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('<<'), Symbol.for('args')]];
/**
 * Bitwise right shift.
 */
function jsBitwiseShiftRight_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left >> right;
    }, args[0]);
}
exports.jsBitwiseShiftRight_ = jsBitwiseShiftRight_;
jsBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('>>'), Symbol.for('args')]];
/**
 * Bitwise unsigned right shift.
 */
function jsUnsignedBitwiseShiftRight_(...args) {
    return args.slice(1).reduce(function (left, right) {
        return left >>> right;
    }, args[0]);
}
exports.jsUnsignedBitwiseShiftRight_ = jsUnsignedBitwiseShiftRight_;
jsUnsignedBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/unsigned-bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('js/op/apply'), Symbol.for('>>>'), Symbol.for('args')]];

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
declare function jsEval_(str: any): any;
declare namespace jsEval_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript [strict equality][js:strict-equality],
 * i.e., the [`===`][js:strict-equality-operator] operator.
 *
 * [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
 * [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
 */
declare function jsStrictlyEqualP_(x: any, y: any): any;
declare namespace jsStrictlyEqualP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
declare function jsLooselyEqualP_(x: any, y: any): any;
declare namespace jsLooselyEqualP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
declare function jsSameValueP_(x: any, y: any): any;
declare namespace jsSameValueP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
declare function jsSameValueZeroP_(x: any, y: any): any;
declare namespace jsSameValueZeroP_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something is JavaScript's `null`.
 */
declare function jsNullP_(obj: any): any;
declare namespace jsNullP_ {
    var fsource: (symbol | (symbol | null)[])[];
}
/**
 * Whether a number is JavaScript's [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
declare function jsNanP_(x: any, y: any): any;
declare namespace jsNanP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is a JavaScript function.
 */
declare function jsFunctionP_(obj: any): any;
declare namespace jsFunctionP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
declare function jsFunctionObjectP_(obj: any): any;
declare namespace jsFunctionObjectP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is of type `"function"`.
 */
declare function jsFunctionTypeP_(obj: any): any;
declare namespace jsFunctionTypeP_ {
    var fsource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
declare function jsTypeOf_(x: any): any;
declare namespace jsTypeOf_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
declare function jsInstanceOfP_(x: any, y: any): any;
declare namespace jsInstanceOfP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
declare function jsIn_(prop: any, obj: any): any;
declare namespace jsIn_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
declare function jsObj_(...args: any[]): any;
declare namespace jsObj_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[];
}
/**
 * Whether something is a JavaScript object.
 */
declare function jsObjP_(x: any): any;
declare namespace jsObjP_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
declare function jsObjectTypeP_(x: any): any;
declare namespace jsObjectTypeP_ {
    var fsource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
declare function jsObjAppend_(...args: any[]): any;
declare namespace jsObjAppend_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Spread a JavaScript object into another.
 */
declare function jsObjSpread_(x: any): any;
declare namespace jsObjSpread_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
declare function jsKeys_(obj: any): any;
declare namespace jsKeys_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Variadic version of JavaScript's `+` operator.
 *
 * Performs [addition][js:add] or [string concatenation][js:concat],
 * depending on the types.
 *
 * [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
 * [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
 */
declare function jsPlus_(...args: any[]): any;
declare namespace jsPlus_ {
    var fsource: (symbol | (symbol | ((symbol | symbol[])[] | undefined)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[])[])[])[])[];
}
/**
 * Return the absolute value of `x`.
 */
declare function jsAbs_(x: any): any;
declare namespace jsAbs_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Find the index of a list element matching a predicate.
 *
 * Like `findf-index`, but returns `-1` rather than `#f`
 * if there is no match.
 */
declare function jsFindIndex_(proc: any, seq: any): any;
declare namespace jsFindIndex_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Placeholder function for JavaScript's
 * [tagged template][js:tagged-template] construct.
 *
 * [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
declare function jsTaggedTemplate_(tag: any, ...args: any[]): any;
declare namespace jsTaggedTemplate_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Placeholder function for JavaScript's
 * [`delete`][js:delete] operator.
 *
 * [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
 */
declare function jsDelete_(x: any): any;
declare namespace jsDelete_ {
    var fsource: (symbol | symbol[] | undefined)[];
}
/**
 * Whether something is a JavaScript array.
 */
declare function jsArrayP_(x: any): any;
declare namespace jsArrayP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the length of a JavaScript string or array.
 */
declare function jsLength_(x: any): any;
declare namespace jsLength_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
declare function jsGet_(obj: any, key: any): any;
declare namespace jsGet_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Look up the property `prop` in the JavaScript object `obj`.
 */
declare function jsDot_(obj: any, prop: any): any;
declare namespace jsDot_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Look up properties `args` in the JavaScript object `obj`,
 * using optional chaining.
 */
declare function jsOptionalChaining_(obj: any, ...args: any[]): any;
declare namespace jsOptionalChaining_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | undefined)[])[] | (symbol | symbol[])[][])[])[])[])[];
}
/**
 * Slice a JavaScript array.
 */
declare function jsSlice_(arr: any, ...args: any[]): any;
declare namespace jsSlice_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Fold up a JavaScript array left to right.
 */
declare function jsReduce_(arr: any, ...args: any[]): any;
declare namespace jsReduce_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Fold up a JavaScript array right to left.
 */
declare function jsReduceRight_(arr: any, ...args: any[]): any;
declare namespace jsReduceRight_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Whether something is a JavaScript string.
 */
declare function jsStringP_(x: any): any;
declare namespace jsStringP_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether something is a JavaScript string literal.
 */
declare function jsStringLiteralP_(x: any): any;
declare namespace jsStringLiteralP_ {
    var fsource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Whether something is a JavaScript string object.
 */
declare function jsStringObjectP_(x: any): any;
declare namespace jsStringObjectP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Concatenate two or more JavaScript strings together.
 */
declare function jsStringConcat_(...args: any[]): any;
declare namespace jsStringConcat_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[][])[])[];
}
/**
 * Create a JavaScript regular expression.
 */
declare function jsRegexp_(input: any, flags?: any): any;
declare namespace jsRegexp_ {
    var fsource: (symbol | (symbol | (symbol | undefined)[])[])[];
}
/**
 * Whether `obj` is a JavaScript regular expression.
 */
declare function jsRegexpP_(obj: any): any;
declare namespace jsRegexpP_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Match a string or regular expression against
 * a JavaScript string.
 */
declare function jsRegexpMatch_(str: any, pattern: any): any;
declare namespace jsRegexpMatch_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Match a string or regular expression against
 * a JavaScript string and replace the matches
 * with a given string or replacement pattern.
 */
declare function jsRegexpReplace_(str: any, pattern: any, insert: any): any;
declare namespace jsRegexpReplace_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Create a JavaScript `new` expression.
 */
declare function jsNew_(x: any, ...args: any[]): any;
declare namespace jsNew_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Create a JavaScript `return` statement.
 */
declare function jsReturn_(x?: any): any;
declare namespace jsReturn_ {
    var fsource: (symbol | (symbol | (symbol | undefined)[])[])[];
}
/**
 * Create a JavaScript `yield` expression.
 */
declare function jsYield_(x?: any): any;
declare namespace jsYield_ {
    var fsource: (symbol | (symbol | (symbol | undefined)[])[])[];
}
/**
 * Less than comparison.
 */
declare function jsLt_(...args: any[]): any;
declare namespace jsLt_ {
    var fsource: (symbol | (symbol | (boolean | (number | symbol | symbol[])[])[] | (boolean | symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[];
}
/**
 * Less than or equal comparison.
 */
declare function jsLte_(...args: any[]): any;
declare namespace jsLte_ {
    var fsource: (symbol | (symbol | (boolean | (number | symbol | symbol[])[])[] | (boolean | symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[];
}
/**
 * Greater than comparison.
 */
declare function jsGt_(...args: any[]): any;
declare namespace jsGt_ {
    var fsource: (symbol | (symbol | (boolean | (number | symbol | symbol[])[])[] | (boolean | symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[];
}
/**
 * Greater than or equal comparison.
 */
declare function jsGte_(...args: any[]): any;
declare namespace jsGte_ {
    var fsource: (symbol | (symbol | (boolean | (number | symbol | symbol[])[])[] | (boolean | symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[];
}
/**
 * Modulo operation.
 */
declare function jsMod_(x: any, y: any): any;
declare namespace jsMod_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Logical negation.
 */
declare function jsNot_(x: any): any;
declare namespace jsNot_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Logical AND.
 */
declare function jsAnd_(...args: any[]): any;
declare namespace jsAnd_ {
    var fsource: (symbol | (boolean | symbol)[])[];
}
/**
 * Logical OR.
 */
declare function jsOr_(...args: any[]): any;
declare namespace jsOr_ {
    var fsource: (symbol | (boolean | symbol)[])[];
}
/**
 * Bitwise NOT.
 */
declare function jsBitwiseNot_(x: any): any;
declare namespace jsBitwiseNot_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise AND.
 */
declare function jsBitwiseAnd_(...args: any[]): any;
declare namespace jsBitwiseAnd_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise OR.
 */
declare function jsBitwiseOr_(...args: any[]): any;
declare namespace jsBitwiseOr_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise XOR.
 */
declare function jsBitwiseXor_(...args: any[]): any;
declare namespace jsBitwiseXor_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise left shift.
 */
declare function jsBitwiseShiftLeft_(...args: any[]): any;
declare namespace jsBitwiseShiftLeft_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise right shift.
 */
declare function jsBitwiseShiftRight_(...args: any[]): any;
declare namespace jsBitwiseShiftRight_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Bitwise unsigned right shift.
 */
declare function jsUnsignedBitwiseShiftRight_(...args: any[]): any;
declare namespace jsUnsignedBitwiseShiftRight_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Immediately invoked function expression (IIFE).
 */
declare function jsIife_(f: any, args: any): any;
declare namespace jsIife_ {
    var fsource: (symbol | symbol[])[];
}
export { jsAbs_, jsAnd_, jsArrayP_, jsBitwiseAnd_, jsBitwiseNot_, jsBitwiseOr_, jsBitwiseShiftLeft_, jsBitwiseShiftRight_, jsBitwiseXor_, jsDelete_, jsDot_, jsEval_, jsFindIndex_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsGet_, jsGt_, jsGte_, jsIife_, jsIn_, jsInstanceOfP_, jsKeys_, jsLength_, jsLooselyEqualP_, jsLt_, jsLte_, jsMod_, jsNanP_, jsNew_, jsNot_, jsNullP_, jsObjAppend_, jsObjSpread_, jsObjP_, jsObj_, jsObjectTypeP_, jsOptionalChaining_, jsOr_, jsPlus_, jsReduceRight_, jsReduce_, jsRegexpMatch_, jsRegexpReplace_, jsRegexpP_, jsRegexp_, jsReturn_, jsSameValueZeroP_, jsSameValueP_, jsSlice_, jsStrictlyEqualP_, jsStringConcat_, jsStringLiteralP_, jsStringObjectP_, jsStringP_, jsTaggedTemplate_, jsTypeOf_, jsUnsignedBitwiseShiftRight_, jsYield_ };

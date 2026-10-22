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
declare function jsStrictlyEqualP_(...args: any[]): any;
declare namespace jsStrictlyEqualP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
declare function jsLooselyEqualP_(...args: any[]): any;
declare namespace jsLooselyEqualP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
declare function jsSameValueP_(x: any, y: any): any;
declare namespace jsSameValueP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
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
declare function jsNullP_(x: any): any;
declare namespace jsNullP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (symbol | null)[])[];
}
/**
 * Whether a number is JavaScript's [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
declare function jsIsNaN_(x: any): any;
declare namespace jsIsNaN_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether `x` is a JavaScript function.
 */
declare function jsFunctionP_(x: any): any;
declare namespace jsFunctionP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
declare function jsFunctionObjectP_(x: any): any;
declare namespace jsFunctionObjectP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether `obj` is of type `"function"`.
 */
declare function jsFunctionTypeP_(x: any): any;
declare namespace jsFunctionTypeP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (string | symbol | symbol[])[])[];
}
/**
 * Whether `obj` is an arrow function.
 */
declare function jsArrowP_(x: any): any;
declare namespace jsArrowP_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[];
}
/**
 * Returns the JavaScript source of a function.
 */
declare function jsSource_(f: any): any;
declare namespace jsSource_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
declare function jsTypeof_(x: any): any;
declare namespace jsTypeof_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
declare function jsInstanceof_(x: any, y: any): any;
declare namespace jsInstanceof_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
declare function jsIn_(prop: any, obj: any): any;
declare namespace jsIn_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
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
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Return the absolute value of `x`.
 */
declare function jsAbs_(x: any): any;
declare namespace jsAbs_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Find the index of a list element matching a predicate.
 *
 * Like `findf-index`, but returns `-1` rather than `#f`
 * if there is no match.
 */
declare function jsFindIndex_(proc: any, seq: any): any;
declare namespace jsFindIndex_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
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
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Return the length of a JavaScript string or array.
 */
declare function jsLength_(x: any): any;
declare namespace jsLength_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
declare function jsRef_(obj: any, key: any): any;
declare namespace jsRef_ {
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
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Fold up a JavaScript array left to right.
 */
declare function jsReduce_(arr: any, ...args: any[]): any;
declare namespace jsReduce_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Fold up a JavaScript array right to left.
 */
declare function jsReduceRight_(arr: any, ...args: any[]): any;
declare namespace jsReduceRight_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether something is a JavaScript string.
 */
declare function jsStringP_(x: any): any;
declare namespace jsStringP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether something is a JavaScript string literal.
 */
declare function jsStringLiteralP_(x: any): any;
declare namespace jsStringLiteralP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (string | symbol | symbol[])[])[];
}
/**
 * Whether something is a JavaScript string object.
 */
declare function jsStringObjectP_(x: any): any;
declare namespace jsStringObjectP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Concatenate two or more JavaScript strings together.
 */
declare function jsStringConcat_(...args: any[]): any;
declare namespace jsStringConcat_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[][])[])[];
    var compilerMacro: any;
}
/**
 * Convert `x` to a string.
 */
declare function jsToString_(x: any): any;
declare namespace jsToString_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (string | symbol)[])[];
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
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Match a string or regular expression against
 * a JavaScript string.
 */
declare function jsRegexpMatch_(str: any, pattern: any): any;
declare namespace jsRegexpMatch_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Match a string or regular expression against
 * a JavaScript string and replace the matches
 * with a given string or replacement pattern.
 */
declare function jsRegexpReplace_(str: any, pattern: any, insert: any): any;
declare namespace jsRegexpReplace_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
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
    var compilerMacro: any;
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Logical OR.
 */
declare function jsOr_(...args: any[]): any;
declare namespace jsOr_ {
    var compilerMacro: any;
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise NOT.
 */
declare function jsBitwiseNot_(x: any): any;
declare namespace jsBitwiseNot_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise AND.
 */
declare function jsBitwiseAnd_(...args: any[]): any;
declare namespace jsBitwiseAnd_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise OR.
 */
declare function jsBitwiseOr_(...args: any[]): any;
declare namespace jsBitwiseOr_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise XOR.
 */
declare function jsBitwiseXor_(...args: any[]): any;
declare namespace jsBitwiseXor_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise left shift.
 */
declare function jsBitwiseShiftLeft_(...args: any[]): any;
declare namespace jsBitwiseShiftLeft_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise right shift.
 */
declare function jsBitwiseShiftRight_(...args: any[]): any;
declare namespace jsBitwiseShiftRight_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Bitwise unsigned right shift.
 */
declare function jsUnsignedBitwiseShiftRight_(...args: any[]): any;
declare namespace jsUnsignedBitwiseShiftRight_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Less than comparison.
 */
declare function jsLt_(...args: any[]): any;
declare namespace jsLt_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Less than or equal comparison.
 */
declare function jsLte_(...args: any[]): any;
declare namespace jsLte_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Greater than comparison.
 */
declare function jsGt_(...args: any[]): any;
declare namespace jsGt_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Greater than or equal comparison.
 */
declare function jsGte_(...args: any[]): any;
declare namespace jsGte_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Addition.
 */
declare function jsAdd_(...args: any[]): any;
declare namespace jsAdd_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (number | symbol)[])[];
}
/**
 * Subtraction.
 */
declare function jsSub_(...args: any[]): any;
declare namespace jsSub_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (number | symbol)[])[];
}
/**
 * Multiplication.
 */
declare function jsMul_(...args: any[]): any;
declare namespace jsMul_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (number | symbol)[])[];
}
/**
 * Division.
 */
declare function jsDiv_(...args: any[]): any;
declare namespace jsDiv_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (number | symbol)[])[];
}
/**
 * Modulo operation.
 */
declare function jsMod_(x: any, y: any): any;
declare namespace jsMod_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Exponentiation.
 */
declare function jsExpt_(...args: any[]): any;
declare namespace jsExpt_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (number | symbol)[])[];
}
/**
 * Convert a string to a number.
 */
declare function jsParseFloat_(str: any): any;
declare namespace jsParseFloat_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Create a JavaScript `Promise`.
 */
declare function jsPromise_(f: any): any;
declare namespace jsPromise_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Whether something is a JavaScript `Promise`.
 */
declare function jsPromiseP_(x: any): any;
declare namespace jsPromiseP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
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
declare function jsRequire_(id: any): any;
declare namespace jsRequire_ {
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | ((symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[])[])[];
}
export { jsAdd_ as _add, jsSub_ as _sub, jsMul_ as _mul, jsDiv_ as _div, jsAdd_, jsSub_, jsMul_, jsDiv_, jsAbs_, jsAnd_, jsArrayP_, jsArrowP_, jsBitwiseAnd_, jsBitwiseNot_, jsBitwiseOr_, jsBitwiseShiftLeft_, jsBitwiseShiftRight_, jsBitwiseXor_, jsDelete_, jsDot_, jsEval_, jsExpt_, jsFindIndex_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsGt_, jsGte_, jsIn_, jsInstanceof_, jsIsNaN_, jsLength_, jsLooselyEqualP_, jsLt_, jsLte_, jsMod_, jsNew_, jsNot_, jsNullP_, jsOptionalChaining_, jsOr_, jsParseFloat_, jsPlus_, jsPromiseP_, jsPromise_, jsReduceRight_, jsReduce_, jsRef_, jsRegexpMatch_, jsRegexpReplace_, jsRegexpP_, jsRegexp_, jsRequire_, jsReturn_, jsSameValueZeroP_, jsSameValueP_, jsSlice_, jsSource_, jsStrictlyEqualP_, jsStringConcat_, jsStringLiteralP_, jsStringObjectP_, jsStringP_, jsTaggedTemplate_, jsToString_, jsTypeof_, jsUnsignedBitwiseShiftRight_, jsYield_ };

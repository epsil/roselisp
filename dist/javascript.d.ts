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
 * Whether a number is [NaN][js:nan].
 *
 * [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
 */
declare function jsNanP_(x: any, y: any): any;
declare namespace jsNanP_ {
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
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | undefined)[])[];
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
 * Whether something is JavaScript's `null`.
 */
declare function jsNullP_(obj: any): any;
declare namespace jsNullP_ {
    var fsource: (symbol | (symbol | null)[])[];
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
 * Return the last element of a JavaScript array.
 */
declare function jsLast_(arr: any): any;
declare namespace jsLast_ {
    var fsource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Return the length of a JavaScript string or array.
 */
declare function jsLength_(arr: any): any;
declare namespace jsLength_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the first element of a JavaScript array.
 */
declare function jsFirst_(lst: any): any;
declare namespace jsFirst_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the second element of a JavaScript array.
 */
declare function jsSecond_(lst: any): any;
declare namespace jsSecond_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the third element of a JavaScript array.
 */
declare function jsThird_(lst: any): any;
declare namespace jsThird_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fourth element of a JavaScript array.
 */
declare function jsFourth_(lst: any): any;
declare namespace jsFourth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fifth element of a JavaScript array.
 */
declare function jsFifth_(lst: any): any;
declare namespace jsFifth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the sixth element of a JavaScript array.
 */
declare function jsSixth_(lst: any): any;
declare namespace jsSixth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the seventh element of a JavaScript array.
 */
declare function jsSeventh_(lst: any): any;
declare namespace jsSeventh_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the eight element of a JavaScript array.
 */
declare function jsEighth_(lst: any): any;
declare namespace jsEighth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the ninth element of a JavaScript array.
 */
declare function jsNinth_(lst: any): any;
declare namespace jsNinth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tenth element of a JavaScript array.
 */
declare function jsTenth_(lst: any): any;
declare namespace jsTenth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
declare function jsGet_(obj: any, key: any): any;
declare namespace jsGet_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Slice a JavaScript array.
 */
declare function jsSlice_(arr: any, ...args: any[]): any;
declare namespace jsSlice_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the tail of a JavaScript array.
 */
declare function jsRest_(arr: any): any;
declare namespace jsRest_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Reverse the order of a JavaScript array.
 * Returns a new array.
 */
declare function jsReverse_(arr: any): any;
declare namespace jsReverse_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Take the `n` first elements from
 * the JavaScript array `arr`.
 */
declare function jsTake_(arr: any, n: any): any;
declare namespace jsTake_ {
    var fsource: (symbol | (number | symbol | (symbol | symbol[])[])[])[];
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
 * JavaScript's [`eval` function][js:eval].
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
declare function jsEval_(str: any): any;
declare namespace jsEval_ {
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
export { jsDelete_, jsEighth_, jsEval_, jsFifth_, jsFindIndex_, jsFirst_, jsFourth_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsGet_, jsIn_, jsInstanceOfP_, jsLast_, jsLength_, jsLooselyEqualP_, jsNanP_, jsNew_, jsNinth_, jsNullP_, jsPlus_, jsReduceRight_, jsReduce_, jsRegexpMatch_, jsRegexpReplace_, jsRegexpP_, jsRegexp_, jsRest_, jsReturn_, jsReverse_, jsSameValueZeroP_, jsSameValueP_, jsSecond_, jsSeventh_, jsSixth_, jsSlice_, jsStrictlyEqualP_, jsTaggedTemplate_, jsTake_, jsTenth_, jsThird_, jsTypeOf_, jsYield_ };

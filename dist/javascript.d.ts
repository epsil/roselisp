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
declare function jsIsStrictlyEqualP_(x: any, y: any): any;
declare namespace jsIsStrictlyEqualP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript [loose equality][js:loose-equality],
 * i.e., the [`==`][js:loose-equality-operator] operator.
 *
 * [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
 * [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
 */
declare function jsIsLooselyEqualP_(x: any, y: any): any;
declare namespace jsIsLooselyEqualP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript [sameValue][js:same-value] equality.
 *
 * [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
 */
declare function jsSameValueP_(x: any, y: any): any;
declare namespace jsSameValueP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript [sameValueZero][js:same-value-zero] equality.
 *
 * [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
declare function jsSameValueZeroP_(x: any, y: any): any;
declare namespace jsSameValueZeroP_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * JavaScript's [`typeof`][js:typeof] operator,
 * as a function.
 *
 * [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
 */
declare function jsTypeof_(x: any): any;
declare namespace jsTypeof_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript's [`instanceof`][js:instanceof] operator,
 * as a function.
 *
 * [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
 */
declare function jsInstanceof_(x: any, y: any): any;
declare namespace jsInstanceof_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Variadic version of JavaScript's `+` operator.
 *
 * Performs [addition][js:add] or [string concatenation][js:concat]
 * depending on the types.
 *
 * [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
 * [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
 */
declare function jsPlus_(...args: any[]): any;
declare namespace jsPlus_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol)[])[][])[])[])[])[])[];
}
/**
 * Whether `obj` is a JavaScript function.
 */
declare function jsFunctionP_(obj: any): any;
declare namespace jsFunctionP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is a [`Function`][js:Function] object.
 *
 * [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
 */
declare function jsFunctionObjectP_(obj: any): any;
declare namespace jsFunctionObjectP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is of type `"function"`.
 */
declare function jsFunctionTypeP_(obj: any): any;
declare namespace jsFunctionTypeP_ {
    var lispSource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Whether something is JavaScript's `null`.
 */
declare function jsNullP_(obj: any): any;
declare namespace jsNullP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Find the index of a list element matching a predicate.
 *
 * Like `findf-index`, but returns `-1` rather than `#f`
 * if there is no match.
 */
declare function jsFindIndex_(proc: any, seq: any): any;
declare namespace jsFindIndex_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript's [`eval` function][js:eval].
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
declare function jsEval_(str: any): any;
declare namespace jsEval_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * JavaScript's [`in`][js:in] operator,
 * as a function.
 *
 * [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
 */
declare function jsIn_(prop: any, obj: any): any;
declare namespace jsIn_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Placeholder function for JavaScript's
 * [tagged template][js:tagged-template] construct.
 *
 * [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
 */
declare function jsTaggedTemplate_(tag: any, ...args: any[]): any;
declare namespace jsTaggedTemplate_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Placeholder function for JavaScript's
 * [`delete`][js:delete] operator.
 *
 * [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
 */
declare function jsDelete_(x: any): any;
declare namespace jsDelete_ {
    var lispSource: (symbol | symbol[])[];
}
export { jsDelete_, jsEval_, jsFindIndex_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsIn_, jsInstanceof_, jsIsLooselyEqualP_, jsIsStrictlyEqualP_, jsNullP_, jsPlus_, jsSameValueZeroP_, jsSameValueP_, jsTaggedTemplate_, jsTypeof_ };

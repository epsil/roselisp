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
exports.jsSource_ = exports.jsSlice_ = exports.jsSameValueP_ = exports.jsSameValueZeroP_ = exports.jsReturn_ = exports.jsRequire_ = exports.jsRegexp_ = exports.jsRegexpP_ = exports.jsRegexpReplace_ = exports.jsRegexpMatch_ = exports.jsRef_ = exports.jsReduce_ = exports.jsReduceRight_ = exports.jsPromise_ = exports.jsPromiseP_ = exports.jsPlus_ = exports.jsParseFloat_ = exports.jsOr_ = exports.jsOptionalChaining_ = exports.jsNullP_ = exports.jsNot_ = exports.jsNew_ = exports.jsMod_ = exports.jsLte_ = exports.jsLt_ = exports.jsLooselyEqualP_ = exports.jsLength_ = exports.jsIsNaN_ = exports.jsInstanceof_ = exports.jsIn_ = exports.jsGte_ = exports.jsGt_ = exports.jsFunctionP_ = exports.jsFunctionTypeP_ = exports.jsFunctionObjectP_ = exports.jsFindIndex_ = exports.jsExpt_ = exports.jsEval_ = exports.jsDot_ = exports.jsDelete_ = exports.jsBitwiseXor_ = exports.jsBitwiseShiftRight_ = exports.jsBitwiseShiftLeft_ = exports.jsBitwiseOr_ = exports.jsBitwiseNot_ = exports.jsBitwiseAnd_ = exports.jsArrowP_ = exports.jsArrayP_ = exports.jsAnd_ = exports.jsAbs_ = void 0;
exports.jsYield_ = exports.jsUnsignedBitwiseShiftRight_ = exports.jsTypeof_ = exports.jsToString_ = exports.jsTaggedTemplate_ = exports.jsStringP_ = exports.jsStringObjectP_ = exports.jsStringLiteralP_ = exports.jsStringConcat_ = exports.jsStrictlyEqualP_ = void 0;
const path_1 = require("path");
const process_1 = require("process");
const util_1 = require("./util");
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
 * JavaScript [strict equality][js:strict-equality],
 * i.e., the [`===`][js:strict-equality-operator] operator.
 *
 * [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
 * [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
 */
function jsStrictlyEqualP_(...args) {
    return args.slice(1).reduce((left, right) => left === right, args[0]);
}
exports.jsStrictlyEqualP_ = jsStrictlyEqualP_;
jsStrictlyEqualP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
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
function jsLooselyEqualP_(...args) {
    return args.slice(1).reduce((left, right) => left == right, args[0]);
}
exports.jsLooselyEqualP_ = jsLooselyEqualP_;
jsLooselyEqualP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
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
function jsSameValueP_(x, y) {
    return Object.is(x, y);
}
exports.jsSameValueP_ = jsSameValueP_;
jsSameValueP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x, y] = exp.slice(1);
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
function jsSameValueZeroP_(x, y) {
    return Object.is(x, y) || (isNaN(x) && isNaN(y));
}
exports.jsSameValueZeroP_ = jsSameValueZeroP_;
jsSameValueZeroP_.fsource = [Symbol.for('define'), [Symbol.for('js/same-value-zero?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), [Symbol.for('js/same-value?'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('and'), [Symbol.for('js/nan?'), Symbol.for('x')], [Symbol.for('js/nan?'), Symbol.for('y')]]]];
/**
 * Whether something is JavaScript's `null`.
 */
function jsNullP_(x) {
    return x === null;
}
exports.jsNullP_ = jsNullP_;
jsNullP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
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
function jsIsNaN_(x) {
    return isNaN(x);
}
exports.jsIsNaN_ = jsIsNaN_;
jsIsNaN_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('isNaN'), x];
    };
    f.ftype = 'macro';
    return f;
})();
jsIsNaN_.fsource = [Symbol.for('define'), [Symbol.for('js/is-NaN_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('isNaN'), Symbol.for('x')]];
/**
 * Whether `x` is a JavaScript function.
 */
function jsFunctionP_(x) {
    // In JavaScript, every function is a
    // [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
    return x instanceof Function;
}
exports.jsFunctionP_ = jsFunctionP_;
jsFunctionP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
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
function jsFunctionObjectP_(x) {
    return x instanceof Function;
}
exports.jsFunctionObjectP_ = jsFunctionObjectP_;
jsFunctionObjectP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('is-a?'), x, Symbol.for('Function')];
    };
    f.ftype = 'macro';
    return f;
})();
jsFunctionObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Function')]];
/**
 * Whether `obj` is of type `"function"`.
 */
function jsFunctionTypeP_(x) {
    return typeof x === 'function';
}
exports.jsFunctionTypeP_ = jsFunctionTypeP_;
jsFunctionTypeP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'function'];
    };
    f.ftype = 'macro';
    return f;
})();
jsFunctionTypeP_.fsource = [Symbol.for('define'), [Symbol.for('js/function-type?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'function']];
/**
 * Whether `obj` is an arrow function.
 */
function jsArrowP_(x) {
    if ((x instanceof Function) && (x + '').match(/^(\([^)]*\)|[^=]*) *=>/)) {
        return true;
    }
    else {
        return false;
    }
}
exports.jsArrowP_ = jsArrowP_;
jsArrowP_.fsource = [Symbol.for('define'), [Symbol.for('js/arrow?_'), Symbol.for('x')], [Symbol.for('true?'), [Symbol.for('and'), [Symbol.for('js/function?'), Symbol.for('x')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^(\\([^)]*\\)|[^=]*) *=>'], [Symbol.for('js/source'), Symbol.for('x')]]]]];
/**
 * Returns the JavaScript source of a function.
 */
function jsSource_(f) {
    return f + '';
}
exports.jsSource_ = jsSource_;
jsSource_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [f] = exp.slice(1);
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
function jsTypeof_(x) {
    return typeof x;
}
exports.jsTypeof_ = jsTypeof_;
jsTypeof_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
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
function jsInstanceof_(x, y) {
    return x instanceof y;
}
exports.jsInstanceof_ = jsInstanceof_;
jsInstanceof_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x, y] = exp.slice(1);
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
function jsIn_(prop, obj) {
    return prop in obj;
}
exports.jsIn_ = jsIn_;
jsIn_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [prop, obj] = exp.slice(1);
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
function jsPlus_(...args) {
    if (args.length === 0) {
        return undefined;
    }
    else {
        let result = args[0];
        for (let x of args.slice(1)) {
            result = result + x;
        }
        return result;
    }
}
exports.jsPlus_ = jsPlus_;
jsPlus_.fsource = [Symbol.for('define'), [Symbol.for('js/plus_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('zero?'), [Symbol.for('length'), Symbol.for('args')]], undefined], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('rest'), Symbol.for('args')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/+'), Symbol.for('result'), Symbol.for('x')]]], Symbol.for('result')]]]];
/**
 * Return the absolute value of `x`.
 */
function jsAbs_(x) {
    return Math.abs(x);
}
exports.jsAbs_ = jsAbs_;
jsAbs_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
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
function jsFindIndex_(proc, seq) {
    // This construct maps neatly onto
    // [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
    return seq.findIndex(proc);
}
exports.jsFindIndex_ = jsFindIndex_;
jsFindIndex_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [proc, seq] = exp.slice(1);
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
    // TODO: Define as a fexpr instead.
    return undefined;
}
exports.jsDelete_ = jsDelete_;
jsDelete_.fsource = [Symbol.for('define'), [Symbol.for('js/delete_'), Symbol.for('x')], undefined];
/**
 * Whether something is a JavaScript array.
 */
function jsArrayP_(x) {
    return Array.isArray(x);
}
exports.jsArrayP_ = jsArrayP_;
jsArrayP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), x];
    };
    f.ftype = 'macro';
    return f;
})();
jsArrayP_.fsource = [Symbol.for('define'), [Symbol.for('js/array?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('x')]];
/**
 * Return the length of a JavaScript string or array.
 */
function jsLength_(x) {
    return x.length;
}
exports.jsLength_ = jsLength_;
jsLength_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('get-field'), Symbol.for('length'), x];
    };
    f.ftype = 'macro';
    return f;
})();
jsLength_.fsource = [Symbol.for('define'), [Symbol.for('js/length_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('x')]];
/**
 * Look up the property `key` in the JavaScript object `obj`.
 */
function jsRef_(obj, key) {
    return obj[key];
}
exports.jsRef_ = jsRef_;
jsRef_.fsource = [Symbol.for('define'), [Symbol.for('js/ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('js/ref'), Symbol.for('obj'), Symbol.for('key')]];
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
    let result = obj;
    for (let x of args) {
        if (x in result) {
            result = result[x];
        }
        else {
            result = undefined;
            break;
        }
    }
    return result;
}
exports.jsOptionalChaining_ = jsOptionalChaining_;
jsOptionalChaining_.fsource = [Symbol.for('define'), [Symbol.for('js/optional-chaining_'), Symbol.for('obj'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), Symbol.for('obj')]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('cond'), [[Symbol.for('js/in'), Symbol.for('x'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/ref'), Symbol.for('result'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), undefined], [Symbol.for('break')]]]], Symbol.for('result')]];
/**
 * Slice a JavaScript array.
 */
function jsSlice_(arr, ...args) {
    return arr.slice(...args);
}
exports.jsSlice_ = jsSlice_;
jsSlice_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [arr, ...args] = exp.slice(1);
        return [Symbol.for('send/apply'), arr, Symbol.for('slice'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsSlice_.fsource = [Symbol.for('define'), [Symbol.for('js/slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];
/**
 * Fold up a JavaScript array left to right.
 */
function jsReduce_(arr, ...args) {
    return arr.reduce(...args);
}
exports.jsReduce_ = jsReduce_;
jsReduce_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [arr, ...args] = exp.slice(1);
        return [Symbol.for('send/apply'), arr, Symbol.for('reduce'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsReduce_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduce'), Symbol.for('args')]];
/**
 * Fold up a JavaScript array right to left.
 */
function jsReduceRight_(arr, ...args) {
    return arr.reduceRight(...args);
}
exports.jsReduceRight_ = jsReduceRight_;
jsReduceRight_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [arr, ...args] = exp.slice(1);
        return [Symbol.for('send/apply'), arr, Symbol.for('reduceRight'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsReduceRight_.fsource = [Symbol.for('define'), [Symbol.for('js/reduce-right_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('reduceRight'), Symbol.for('args')]];
/**
 * Whether something is a JavaScript string.
 */
function jsStringP_(x) {
    return (typeof x === 'string') || (x instanceof String);
}
exports.jsStringP_ = jsStringP_;
jsStringP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        if (!(Array.isArray(x) && (x.length > 0))) {
            return [Symbol.for('or'), [Symbol.for('js/string-literal?'), x], [Symbol.for('js/string-object?'), x]];
        }
        else {
            const x1 = Symbol('x');
            return [Symbol.for('let'), [[x1, x]], ((x) => [Symbol.for('or'), [Symbol.for('js/string-literal?'), x], [Symbol.for('js/string-object?'), x]])(x1)];
        }
    };
    f.ftype = 'macro';
    return f;
})();
jsStringP_.fsource = [Symbol.for('define'), [Symbol.for('js/string?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('or'), [Symbol.for('js/string-literal?'), Symbol.for('x')], [Symbol.for('js/string-object?'), Symbol.for('x')]]];
/**
 * Whether something is a JavaScript string literal.
 */
function jsStringLiteralP_(x) {
    return typeof x === 'string';
}
exports.jsStringLiteralP_ = jsStringLiteralP_;
jsStringLiteralP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'string'];
    };
    f.ftype = 'macro';
    return f;
})();
jsStringLiteralP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-literal?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'string']];
/**
 * Whether something is a JavaScript string object.
 */
function jsStringObjectP_(x) {
    return x instanceof String;
}
exports.jsStringObjectP_ = jsStringObjectP_;
jsStringObjectP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('is-a?'), x, Symbol.for('String')];
    };
    f.ftype = 'macro';
    return f;
})();
jsStringObjectP_.fsource = [Symbol.for('define'), [Symbol.for('js/string-object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('String')]];
/**
 * Concatenate two or more JavaScript strings together.
 */
function jsStringConcat_(...args) {
    let result = '';
    for (let x of args) {
        result = result + x;
    }
    return result;
}
exports.jsStringConcat_ = jsStringConcat_;
jsStringConcat_.fsource = [Symbol.for('define'), [Symbol.for('js/string-concat_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), '']], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/op'), Symbol.for('+'), Symbol.for('result'), Symbol.for('x')]]], Symbol.for('result')]];
/**
 * Compiler macro for `(js/string-concat ...)` expressions.
 */
jsStringConcat_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('+'), [Symbol.for('list'), ...args], Symbol.for(':identity'), ''];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Convert `x` to a string.
 */
function jsToString_(x) {
    return x + '';
}
exports.jsToString_ = jsToString_;
jsToString_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('js/+'), x, ''];
    };
    f.ftype = 'macro';
    return f;
})();
jsToString_.fsource = [Symbol.for('define'), [Symbol.for('js/to-string_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/+'), Symbol.for('x'), '']];
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
jsRegexpP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj] = exp.slice(1);
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
function jsRegexpMatch_(str, pattern) {
    return str.match(pattern);
}
exports.jsRegexpMatch_ = jsRegexpMatch_;
jsRegexpMatch_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [str, pattern] = exp.slice(1);
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
function jsRegexpReplace_(str, pattern, insert) {
    return str.replace(pattern, insert);
}
exports.jsRegexpReplace_ = jsRegexpReplace_;
jsRegexpReplace_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [str, pattern, insert] = exp.slice(1);
        return [Symbol.for('send'), str, Symbol.for('replace'), pattern, insert];
    };
    f.ftype = 'macro';
    return f;
})();
jsRegexpReplace_.fsource = [Symbol.for('define'), [Symbol.for('js/regexp-replace_'), Symbol.for('str'), Symbol.for('pattern'), Symbol.for('insert')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('str'), Symbol.for('replace'), Symbol.for('pattern'), Symbol.for('insert')]];
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
jsLt_.fsource = [Symbol.for('define'), [Symbol.for('js/lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>='), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
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
jsLte_.fsource = [Symbol.for('define'), [Symbol.for('js/lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
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
jsGt_.fsource = [Symbol.for('define'), [Symbol.for('js/gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<='), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
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
jsGte_.fsource = [Symbol.for('define'), [Symbol.for('js/gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('length'), Symbol.for('args')], 2], true], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('-'), Symbol.for('i'), 1]], [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('return'), false]]], true]]];
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
    // TODO: `define-inline`, `js/op`.
    return !x;
}
exports.jsNot_ = jsNot_;
jsNot_.fsource = [Symbol.for('define'), [Symbol.for('js/not_'), Symbol.for('x')], [Symbol.for('js/!'), Symbol.for('x')]];
/**
 * Logical AND.
 */
function jsAnd_(...args) {
    return args.reduce((left, right) => left && right, true);
}
exports.jsAnd_ = jsAnd_;
jsAnd_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('&&'), [Symbol.for('list'), ...args], Symbol.for(':identity'), true];
    };
    f.ftype = 'macro';
    return f;
})();
jsAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('&&'), Symbol.for('args'), Symbol.for(':identity'), true]];
/**
 * Logical OR.
 */
function jsOr_(...args) {
    return args.reduce((left, right) => left || right, false);
}
exports.jsOr_ = jsOr_;
jsOr_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('||'), [Symbol.for('list'), ...args], Symbol.for(':identity'), false];
    };
    f.ftype = 'macro';
    return f;
})();
jsOr_.fsource = [Symbol.for('define'), [Symbol.for('js/or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('||'), Symbol.for('args'), Symbol.for(':identity'), false]];
/**
 * Bitwise NOT.
 */
function jsBitwiseNot_(x) {
    return ~x;
}
exports.jsBitwiseNot_ = jsBitwiseNot_;
jsBitwiseNot_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('js/op'), Symbol.for('~'), x];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseNot_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-not_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]];
/**
 * Bitwise AND.
 */
function jsBitwiseAnd_(...args) {
    return args.slice(1).reduce((left, right) => left & right, args[0]);
}
exports.jsBitwiseAnd_ = jsBitwiseAnd_;
jsBitwiseAnd_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('&'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseAnd_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-and_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('&'), Symbol.for('args')]];
/**
 * Bitwise OR.
 */
function jsBitwiseOr_(...args) {
    return args.slice(1).reduce((left, right) => left | right, args[0]);
}
exports.jsBitwiseOr_ = jsBitwiseOr_;
jsBitwiseOr_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('|'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseOr_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-or_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('|'), Symbol.for('args')]];
/**
 * Bitwise XOR.
 */
function jsBitwiseXor_(...args) {
    return args.slice(1).reduce((left, right) => left ^ right, args[0]);
}
exports.jsBitwiseXor_ = jsBitwiseXor_;
jsBitwiseXor_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('^'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseXor_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-xor_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('^'), Symbol.for('args')]];
/**
 * Bitwise left shift.
 */
function jsBitwiseShiftLeft_(...args) {
    return args.slice(1).reduce((left, right) => left << right, args[0]);
}
exports.jsBitwiseShiftLeft_ = jsBitwiseShiftLeft_;
jsBitwiseShiftLeft_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('<<'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseShiftLeft_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-left_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('<<'), Symbol.for('args')]];
/**
 * Bitwise right shift.
 */
function jsBitwiseShiftRight_(...args) {
    return args.slice(1).reduce((left, right) => left >> right, args[0]);
}
exports.jsBitwiseShiftRight_ = jsBitwiseShiftRight_;
jsBitwiseShiftRight_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('>>'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>>'), Symbol.for('args')]];
/**
 * Bitwise unsigned right shift.
 */
function jsUnsignedBitwiseShiftRight_(...args) {
    return args.slice(1).reduce((left, right) => left >>> right, args[0]);
}
exports.jsUnsignedBitwiseShiftRight_ = jsUnsignedBitwiseShiftRight_;
jsUnsignedBitwiseShiftRight_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('>>>'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
jsUnsignedBitwiseShiftRight_.fsource = [Symbol.for('define'), [Symbol.for('js/unsigned-bitwise-shift-right_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('>>>'), Symbol.for('args')]];
/**
 * Exponentiation.
 */
function jsExpt_(...args) {
    return args.reduceRight((right, left) => left ** right, 1);
}
exports.jsExpt_ = jsExpt_;
jsExpt_.compilerMacro = (() => {
    const f = (exp, env) => {
        const args = exp.slice(1);
        return [Symbol.for('js/op/apply'), Symbol.for('**'), [Symbol.for('list'), ...args], Symbol.for(':identity'), 1, Symbol.for(':fold'), Symbol.for('right')];
    };
    f.ftype = 'macro';
    return f;
})();
jsExpt_.fsource = [Symbol.for('define'), [Symbol.for('js/expt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/op/apply'), Symbol.for('**'), Symbol.for('args'), Symbol.for(':identity'), 1, Symbol.for(':fold'), Symbol.for('right')]];
/**
 * Create a JavaScript `Promise`.
 */
function jsPromise_(f) {
    return new Promise(f);
}
exports.jsPromise_ = jsPromise_;
jsPromise_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [f] = exp.slice(1);
        return [Symbol.for('new'), Symbol.for('Promise'), f];
    };
    f.ftype = 'macro';
    return f;
})();
jsPromise_.fsource = [Symbol.for('define'), [Symbol.for('js/promise_'), Symbol.for('f')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('new'), Symbol.for('Promise'), Symbol.for('f')]];
/**
 * Whether something is a JavaScript `Promise`.
 */
function jsPromiseP_(x) {
    return x instanceof Promise;
}
exports.jsPromiseP_ = jsPromiseP_;
jsPromiseP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('is-a?'), x, Symbol.for('Promise')];
    };
    f.ftype = 'macro';
    return f;
})();
jsPromiseP_.fsource = [Symbol.for('define'), [Symbol.for('js/promise?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Promise')]];
/**
 * Convert a string to a number.
 */
function jsParseFloat_(str) {
    return parseFloat(str);
}
exports.jsParseFloat_ = jsParseFloat_;
jsParseFloat_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [str] = exp.slice(1);
        return [Symbol.for('parseFloat'), str];
    };
    f.ftype = 'macro';
    return f;
})();
jsParseFloat_.fsource = [Symbol.for('define'), [Symbol.for('js/parse-float_'), Symbol.for('str')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('parseFloat'), Symbol.for('str')]];
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
function jsRequire_(id) {
    let result = false;
    if (id.match(new RegExp('^\\.+/'))) {
        // If the module path is relative (starts with `./` or `../`), then
        // interpret it as relative to the current working directory and
        // rewrite it to an absolute path.
        const modulePath = (0, path_1.join)((0, process_1.cwd)(), id);
        try {
            result = require(modulePath);
        }
        catch (_a) {
        }
    }
    else {
        // If the module path is not relative, then it probably references
        // a module located in `node_modules/`, so rewrite it to
        // "`/absolute/path/to/node_modules/id`".
        const nodeModulesPath = (0, util_1.getNodeModulesPath)((0, process_1.cwd)());
        if (nodeModulesPath) {
            const modulePath = (0, path_1.join)(nodeModulesPath, id);
            try {
                result = require(modulePath);
            }
            catch (_b) {
            }
        }
        // Final attempt: load the module as a system module.
        if (!result) {
            try {
                result = require(id);
            }
            catch (_c) {
            }
        }
    }
    return result;
}
exports.jsRequire_ = jsRequire_;
jsRequire_.fsource = [Symbol.for('define'), [Symbol.for('js/require_'), Symbol.for('id')], [Symbol.for('define'), Symbol.for('result'), false], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.+/'], Symbol.for('id')], [Symbol.for('define'), Symbol.for('module-path'), [Symbol.for('join'), [Symbol.for('cwd')], Symbol.for('id')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('module-path')]], [Symbol.for('catch')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('node-modules-path'), [Symbol.for('get-node-modules-path'), [Symbol.for('cwd')]]], [Symbol.for('when'), Symbol.for('node-modules-path'), [Symbol.for('define'), Symbol.for('module-path'), [Symbol.for('join'), Symbol.for('node-modules-path'), Symbol.for('id')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('module-path')]], [Symbol.for('catch')]]], [Symbol.for('unless'), Symbol.for('result'), [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/require'), Symbol.for('id')]], [Symbol.for('catch')]]]]], Symbol.for('result')];

"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Objects
 *
 * JavaScript objects.
 *
 * ## Description
 *
 * Functions for working with JavaScript objects.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.object_ = exports.objectp_ = exports.objectTypeP_ = exports.objectSpread_ = exports.objectSetX_ = exports.objectRef_ = exports.objectKeys_ = exports.objectCopy_ = exports.objectAppend_ = exports.fieldNames_ = exports.oset_ = exports.osetx_ = exports.objectSet_ = exports.oget_ = exports.objectGet_ = void 0;
const [keywordp] = (() => {
    function keywordp_(obj) {
        return (typeof obj === 'symbol') && (obj.description.match(/^:/) ? true : false);
    }
    return [keywordp_];
})();
/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function object_(...args) {
    const entries = [];
    const _end = args.length;
    for (let i = 0; i < _end; i = i + 2) {
        let prop = args[i];
        const val = args[i + 1];
        if (keywordp(prop)) {
            prop = prop.description.replace(/^:/, '');
        }
        if (typeof prop === 'symbol') {
            prop = prop.description;
        }
        entries.push([prop, val]);
    }
    return Object.fromEntries(entries);
}
exports.object_ = object_;
object_.fsource = [Symbol.for('define'), [Symbol.for('object_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('entries'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('args')], 2]]], [Symbol.for('let'), [[Symbol.for('prop'), [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('val'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]], [Symbol.for('when'), [Symbol.for('keyword?'), Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('prop'), [Symbol.for('keyword->string'), Symbol.for('prop')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('prop'), [Symbol.for('symbol->string'), Symbol.for('prop')]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), Symbol.for('prop'), Symbol.for('val')]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]]];
/**
 * Whether something is a JavaScript object.
 */
function objectp_(x) {
    // This function avoids regarding JavaScript's `null` value as an
    // object (even if JavaScript does), because it has no properties;
    // and unlike the empty object, attempting to access a property on
    // it causes an error to be thrown. This is more trouble than it is
    // worth, so only non-`null` object values are considered to be
    // proper objects here.
    return (x !== null) && (typeof x === 'object');
}
exports.objectp_ = objectp_;
objectp_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        if (!(Array.isArray(x) && (x.length > 0))) {
            return [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), x]], [Symbol.for('object-type?'), x]];
        }
        else {
            const x1 = Symbol('x');
            return [Symbol.for('let'), [[x1, x]], ((x) => [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), x]], [Symbol.for('object-type?'), x]])(x1)];
        }
    };
    f.ftype = 'macro';
    return f;
})();
objectp_.fsource = [Symbol.for('define'), [Symbol.for('object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), Symbol.for('x')]], [Symbol.for('object-type?'), Symbol.for('x')]]];
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function objectTypeP_(x) {
    return typeof x === 'object';
}
exports.objectTypeP_ = objectTypeP_;
objectTypeP_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [x] = exp.slice(1);
        return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'object'];
    };
    f.ftype = 'macro';
    return f;
})();
objectTypeP_.fsource = [Symbol.for('define'), [Symbol.for('object-type?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function objectAppend_(...args) {
    return Object.assign({}, ...args);
}
exports.objectAppend_ = objectAppend_;
objectAppend_.fsource = [Symbol.for('define'), [Symbol.for('object-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('object')], Symbol.for('args')]];
/**
 * Copy a JavaScript object.
 */
function objectCopy_(obj) {
    return Object.assign({}, obj);
}
exports.objectCopy_ = objectCopy_;
objectCopy_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj] = exp.slice(1);
        return [Symbol.for('object-append'), obj];
    };
    f.ftype = 'macro';
    return f;
})();
objectCopy_.fsource = [Symbol.for('define'), [Symbol.for('object-copy_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('object-append'), Symbol.for('obj')]];
/**
 * Spread a JavaScript object into another.
 */
function objectSpread_(x) {
    return x;
}
exports.objectSpread_ = objectSpread_;
objectSpread_.fsource = [Symbol.for('define'), [Symbol.for('object-spread_'), Symbol.for('x')], Symbol.for('x')];
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function objectKeys_(obj) {
    return Object.keys(obj);
}
exports.objectKeys_ = objectKeys_;
objectKeys_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj] = exp.slice(1);
        return [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), obj];
    };
    f.ftype = 'macro';
    return f;
})();
objectKeys_.fsource = [Symbol.for('define'), [Symbol.for('object-keys_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), Symbol.for('obj')]];
/**
 * Look up the property `key` in `obj`.
 *
 * Similar to [`object-get` in Racket][rkt:object-get] and
 * [`oget` in ClojureScript][cljs:oget].
 *
 * [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
 * [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectRef_(obj, key) {
    return obj[key];
}
exports.objectGet_ = objectRef_;
exports.oget_ = objectRef_;
exports.objectRef_ = objectRef_;
objectRef_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj, key] = exp.slice(1);
        return [Symbol.for('js/ref'), obj, key];
    };
    f.ftype = 'macro';
    return f;
})();
objectRef_.fsource = [Symbol.for('define'), [Symbol.for('object-ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/ref'), Symbol.for('obj'), Symbol.for('key')]];
/**
 * Set the property `key` in `obj` to `val`.
 *
 * Similar to [`object-set!` in Racket][rkt:object-set] and
 * [`oset!` in ClojureScript][cljs:oset].
 *
 * [rkt:object-set]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-set%21%29%29
 * [cljs:oset]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectSetX_(obj, key, val) {
    return obj[key] = val;
}
exports.objectSet_ = objectSetX_;
exports.osetx_ = objectSetX_;
exports.oset_ = objectSetX_;
exports.objectSetX_ = objectSetX_;
objectSetX_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj, key, val] = exp.slice(1);
        return [Symbol.for('js/='), [Symbol.for('object-ref'), obj, key], val];
    };
    f.ftype = 'macro';
    return f;
})();
objectSetX_.fsource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/='), [Symbol.for('object-ref'), Symbol.for('obj'), Symbol.for('key')], Symbol.for('val')]];
/**
 * Return the keys for an object.
 *
 * Similar to [`field-names` in Racket][rkt:field-names].
 *
 * [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
 */
function fieldNames_(obj) {
    return Object.keys(obj);
}
exports.fieldNames_ = fieldNames_;
fieldNames_.compilerMacro = (() => {
    const f = (exp, env) => {
        const [obj] = exp.slice(1);
        return [Symbol.for('object-keys'), obj];
    };
    f.ftype = 'macro';
    return f;
})();
fieldNames_.fsource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('object-keys'), Symbol.for('obj')]];

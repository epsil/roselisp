"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Currying
 *
 * Currying and partial application.
 *
 * ## Description
 *
 * [Ramda][r:curry]-compatible implementation of currying and partial
 * application of functions.
 *
 * This file defines a placeholder value, [`__`][r:dash], which is
 * compatible with Ramda.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [r:curry]: https://ramdajs.com/docs/#curry
 * [r:dash]: https://ramdajs.com/docs/#__
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.isPlaceholder = exports.dashify = exports.curryN = exports.curry = exports.placeholder = exports._ = exports.__ = void 0;
/**
 * Ramda-compatible placeholder value.
 *
 * See [`R.__`][r:dash] in Ramda.
 *
 * [r:dash]: https://ramdajs.com/docs/#__
 */
const __ = {
    '@@functional/placeholder': true
};
exports.__ = __;
exports._ = __;
exports.placeholder = __;
/**
 * Whether a value is the placeholder value, `__`.
 */
function isPlaceholder(x, placeholder = __) {
    return x === placeholder;
}
exports.isPlaceholder = isPlaceholder;
isPlaceholder.lispSource = [Symbol.for('define'), [Symbol.for('is-placeholder'), Symbol.for('x'), [Symbol.for('placeholder'), Symbol.for('__')]], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('placeholder')]];
/**
 * Creates a function that accepts arguments of `f` and either invokes `f`
 * returning its result, if at least `arity` number of arguments have been
 * provided, or returns a function that accepts the remaining `f`
 * arguments, and so on. The arity of `f` may be specified if `f.length`
 * is not sufficient.
 *
 * Loosely based on [`curry` from Ramda][r:curry].
 *
 * [r:curry]: https://ramdajs.com/docs/#curry
 */
function curry(f, arity = f.length) {
    return curryN(arity, f);
}
exports.curry = curry;
curry.lispSource = [Symbol.for('define'), [Symbol.for('curry'), Symbol.for('f'), [Symbol.for('arity'), [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('f')]]], [Symbol.for('curry-n'), Symbol.for('arity'), Symbol.for('f')]];
/**
 * Make a curried function. `f` is the function to call, `arity` is
 * the arity of the function, and `placeholder` is a placeholder value
 * like {@link __ `R.__`}. `received` is used internally and is an
 * array of the arguments received thus far.
 *
 * Loosely based on [`curryN` from Ramda][r:curryn].
 *
 * [r:curryn]: https://ramdajs.com/docs/#curryN
 */
function curryN(arity, f, received = []) {
    function curriedF(...args) {
        if (args.length === 0) {
            return curriedF;
        }
        else {
            let argsIdx = 0;
            let left = arity;
            const combined = [];
            let combinedIdx = 0;
            let result;
            while ((combinedIdx < received.length) || (argsIdx < args.length)) {
                if ((combinedIdx < received.length) && ((received[combinedIdx] !== __) || (argsIdx >= args.length))) {
                    result = received[combinedIdx];
                }
                else {
                    result = args[argsIdx];
                    argsIdx++;
                }
                combined[combinedIdx] = result;
                if (result !== __) {
                    left--;
                }
                combinedIdx++;
            }
            if (left <= 0) {
                return f(...combined);
            }
            else {
                return curryN(arity, f, combined);
            }
        }
    }
    curriedF.lispSource = [Symbol.for('define'), [Symbol.for('curried-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], Symbol.for('curried-f')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('args-idx'), 0], [Symbol.for('define'), Symbol.for('left'), Symbol.for('arity')], [Symbol.for('define'), Symbol.for('combined'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('combined-idx'), 0], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('while'), [Symbol.for('or'), [Symbol.for('<'), Symbol.for('combined-idx'), [Symbol.for('array-list-length'), Symbol.for('received')]], [Symbol.for('<'), Symbol.for('args-idx'), [Symbol.for('array-list-length'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('<'), Symbol.for('combined-idx'), [Symbol.for('array-list-length'), Symbol.for('received')]], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('received'), Symbol.for('combined-idx')], Symbol.for('__')]], [Symbol.for('>='), Symbol.for('args-idx'), [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('received'), Symbol.for('combined-idx')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('args-idx')]], [Symbol.for('set!'), Symbol.for('args-idx'), [Symbol.for('+'), Symbol.for('args-idx'), 1]]]], [Symbol.for('aset!'), Symbol.for('combined'), Symbol.for('combined-idx'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('result'), Symbol.for('__')], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('-'), Symbol.for('left'), 1]]], [Symbol.for('set!'), Symbol.for('combined-idx'), [Symbol.for('+'), Symbol.for('combined-idx'), 1]]], [Symbol.for('cond'), [[Symbol.for('<='), Symbol.for('left'), 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('combined')]], [Symbol.for('else'), [Symbol.for('curry-n'), Symbol.for('arity'), Symbol.for('f'), Symbol.for('combined')]]]]]];
    return curriedF;
}
exports.curryN = curryN;
curryN.lispSource = [Symbol.for('define'), [Symbol.for('curry-n'), Symbol.for('arity'), Symbol.for('f'), [Symbol.for('received'), [Symbol.for('quote'), []]]], [Symbol.for('define'), [Symbol.for('curried-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], Symbol.for('curried-f')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('args-idx'), 0], [Symbol.for('define'), Symbol.for('left'), Symbol.for('arity')], [Symbol.for('define'), Symbol.for('combined'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('combined-idx'), 0], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('while'), [Symbol.for('or'), [Symbol.for('<'), Symbol.for('combined-idx'), [Symbol.for('array-list-length'), Symbol.for('received')]], [Symbol.for('<'), Symbol.for('args-idx'), [Symbol.for('array-list-length'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('<'), Symbol.for('combined-idx'), [Symbol.for('array-list-length'), Symbol.for('received')]], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('received'), Symbol.for('combined-idx')], Symbol.for('__')]], [Symbol.for('>='), Symbol.for('args-idx'), [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('received'), Symbol.for('combined-idx')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('args-idx')]], [Symbol.for('set!'), Symbol.for('args-idx'), [Symbol.for('+'), Symbol.for('args-idx'), 1]]]], [Symbol.for('aset!'), Symbol.for('combined'), Symbol.for('combined-idx'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('result'), Symbol.for('__')], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('-'), Symbol.for('left'), 1]]], [Symbol.for('set!'), Symbol.for('combined-idx'), [Symbol.for('+'), Symbol.for('combined-idx'), 1]]], [Symbol.for('cond'), [[Symbol.for('<='), Symbol.for('left'), 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('combined')]], [Symbol.for('else'), [Symbol.for('curry-n'), Symbol.for('arity'), Symbol.for('f'), Symbol.for('combined')]]]]]], Symbol.for('curried-f')];
/**
 * Add support for partial application with
 * a placeholder value like {@link __ `R.__`}.
 */
function dashify(f, placeholder = __) {
    // `g` is a wrapper around `f` that adds support for
    // the placeholder value.
    function g(...args) {
        const indices = [];
        const completeArgs = [...args];
        let arg;
        const _end = args.length;
        for (let i = 0; i < _end; i++) {
            arg = args[i];
            if (arg === placeholder) {
                indices.push(i);
            }
        }
        if (indices.length === 0) {
            return f(...args);
        }
        else {
            // `h` is a function that receives remaining arguments.
            // When all arguments have been received, it invokes `f`.
            function h(...remainingArgs) {
                const _end = remainingArgs.length;
                for (let i = 0; i < _end; i++) {
                    if (indices.length === 0) {
                        break;
                    }
                    else if (remainingArgs[i] === placeholder) {
                        continue;
                    }
                    else {
                        const j = indices.shift();
                        completeArgs[j] = remainingArgs[i];
                    }
                }
                if (indices.length === 0) {
                    return f(...completeArgs);
                }
                else {
                    return h;
                }
            }
            h.lispSource = [Symbol.for('define'), [Symbol.for('h'), Symbol.for('.'), Symbol.for('remaining-args')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('remaining-args')]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('break')]], [[Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')], Symbol.for('placeholder')], [Symbol.for('continue')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('j'), [Symbol.for('pop!'), Symbol.for('indices')]], [Symbol.for('aset!'), Symbol.for('complete-args'), Symbol.for('j'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')]]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('complete-args')]], [Symbol.for('else'), Symbol.for('h')]]];
            return h;
        }
    }
    g.lispSource = [Symbol.for('define'), [Symbol.for('g'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('complete-args'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('arg')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('arg'), Symbol.for('placeholder')], [Symbol.for('push-right!'), Symbol.for('indices'), Symbol.for('i')]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), [Symbol.for('h'), Symbol.for('.'), Symbol.for('remaining-args')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('remaining-args')]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('break')]], [[Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')], Symbol.for('placeholder')], [Symbol.for('continue')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('j'), [Symbol.for('pop!'), Symbol.for('indices')]], [Symbol.for('aset!'), Symbol.for('complete-args'), Symbol.for('j'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')]]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('complete-args')]], [Symbol.for('else'), Symbol.for('h')]]], Symbol.for('h')]]];
    return g;
}
exports.dashify = dashify;
dashify.lispSource = [Symbol.for('define'), [Symbol.for('dashify'), Symbol.for('f'), [Symbol.for('placeholder'), Symbol.for('__')]], [Symbol.for('define'), [Symbol.for('g'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('complete-args'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('arg')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('arg'), Symbol.for('placeholder')], [Symbol.for('push-right!'), Symbol.for('indices'), Symbol.for('i')]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), [Symbol.for('h'), Symbol.for('.'), Symbol.for('remaining-args')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('remaining-args')]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('break')]], [[Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')], Symbol.for('placeholder')], [Symbol.for('continue')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('j'), [Symbol.for('pop!'), Symbol.for('indices')]], [Symbol.for('aset!'), Symbol.for('complete-args'), Symbol.for('j'), [Symbol.for('aget'), Symbol.for('remaining-args'), Symbol.for('i')]]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('indices')], 0], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('complete-args')]], [Symbol.for('else'), Symbol.for('h')]]], Symbol.for('h')]]], Symbol.for('g')];

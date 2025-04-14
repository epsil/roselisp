"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Procedures
 *
 * Various procedures.
 *
 * ## Description
 *
 * This file defines various procedures that are part of the language
 * environment.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.sub = exports.minus = exports._sub = exports.sub1 = exports.range = exports.procedurep = exports.functionp = exports.pipe = exports.numberp = exports.not = exports.mul = exports._mul = exports.memf = exports.memq = exports.member = exports.memberp_ = exports.memberp = exports.memberP_ = exports.memberP = exports.mapcar = exports.map = exports.lte = exports.lt = exports.keywordp = exports.isAP = exports.instanceofp = exports.instanceOf_ = exports.instanceOfP_ = exports.instanceOfP = exports.instanceOf = exports.intersection = exports.gte = exports.gt = exports.funcall = exports.foldr = exports.foldl = exports.findf = exports.findfIndex = exports.fexprp = exports.falsep_ = exports.error = exports.div = exports._div = exports.display = exports.compose = exports.apply = exports.plus = exports.add = exports._add = exports.add1 = void 0;
exports.range_ = exports.procedurep_ = exports.pipe_ = exports.onep_ = exports.oddp_ = exports.numberp_ = exports.not_ = exports.mul_ = exports.modulo_ = exports.memq_ = exports.memqp_ = exports.memf_ = exports.memfp_ = exports.member_ = exports.map_ = exports.lte_ = exports.lt_ = exports.keywordp_ = exports.isAP_ = exports.intersection_ = exports.indexWhere_ = exports.indexOf_ = exports.identity_ = exports.gte_ = exports.gt_ = exports.funcall_ = exports.foldr_ = exports.foldl_ = exports.findf_ = exports.findfIndex_ = exports.filter_ = exports.fexprp_ = exports.falsep = exports.evenp_ = exports.error_ = exports.div_ = exports.display_ = exports.const_ = exports.compose_ = exports.booleanp_ = exports.assert_ = exports.apply_ = exports.add_ = exports.add1_ = exports.zerop = exports.values = exports.union = exports.typeOf = exports.truep_ = exports.subtract = void 0;
exports.zerop_ = exports.values_ = exports.union_ = exports.undefinedp_ = exports.typeOf_ = exports.truep = exports.sub_ = exports.sub1_ = exports.selfEvaluatingP_ = void 0;
const [equalp, keywordp] = (() => {
    function equalp_(x, y) {
        if (x === y) {
            return true;
        }
        else if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && Array.isArray(y)) {
            const cdrX = (Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
            if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (x.length === 3) && !Array.isArray(cdrX) && !(Array.isArray(cdrX) && (cdrX.length >= 3) && (cdrX[cdrX.length - 2] === Symbol.for('.')))) {
                return false;
            }
            else if (equalp_(x[0], y[0])) {
                return equalp_(cdrX, (() => {
                    function cdr_(lst) {
                        if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
                            return lst[2];
                        }
                        else {
                            return lst.slice(1);
                        }
                    }
                    return cdr_;
                })()(y));
            }
            else {
                return false;
            }
        }
        else if (Array.isArray(x) && Array.isArray(y) && (y.length >= 3) && (y[y.length - 2] === Symbol.for('.'))) {
            return equalp_(y, x);
        }
        else if (Array.isArray(x) && Array.isArray(y)) {
            if (x.length !== y.length) {
                return false;
            }
            const _end = x.length;
            for (let i = 0; i < _end; i++) {
                if (!equalp_(x[i], y[i])) {
                    return false;
                }
            }
            return true;
        }
        else if ((x instanceof Map) && (y instanceof Map)) {
            if (x.size !== y.size) {
                return false;
            }
            for (let entry of x.entries()) {
                const [key1, value1] = entry;
                const value2 = y.get(key1);
                if (!equalp_(value1, value2)) {
                    return false;
                }
            }
            return true;
        }
        else if ((x !== null) && (typeof x === 'object') && (y !== null) && (typeof y === 'object')) {
            if (Object.keys(x).length !== Object.keys(y).length) {
                return false;
            }
            for (let key of Object.keys(x)) {
                if (!equalp_(x[key], y[key])) {
                    return false;
                }
            }
            return true;
        }
        else {
            return false;
        }
    }
    function keywordp_(obj) {
        return (typeof obj === 'symbol') && obj.description.match(new RegExp('^:'));
    }
    function cdr_(lst) {
        if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
            return lst[2];
        }
        else {
            return lst.slice(1);
        }
    }
    return [equalp_, keywordp_];
})();
/**
 * Call `f` with `args`, using the last arg as a list of args.
 * Returns the value `f` returns.
 *
 * Similar to [`apply` in Racket][rkt:apply] and
 * [`apply` in Common Lisp][cl:apply].
 *
 * [rkt:apply]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29
 * [cl:apply]: http://clhs.lisp.se/Body/f_apply.htm#apply
 */
function apply_(f, ...args) {
    if (args.length > 0) {
        args = [...args.slice(0, -1), ...args[args.length - 1]];
    }
    return f.apply(null, args);
}
exports.apply = apply_;
exports.apply_ = apply_;
apply_.lispSource = [Symbol.for('define'), [Symbol.for('apply_'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('append'), [Symbol.for('drop-right'), Symbol.for('args'), 1], [Symbol.for('array-list-last'), Symbol.for('args')]]]], [Symbol.for('send'), Symbol.for('f'), Symbol.for('apply'), Symbol.for('js/null'), Symbol.for('args')]];
/**
 * Call `f` with `args`.
 * Returns the value `f` returns.
 *
 * Similar to [`funcall` in Common Lisp][cl:funcall].
 *
 * [cl:funcall]: http://clhs.lisp.se/Body/f_funcal.htm#funcall
 */
function funcall_(f, ...args) {
    return f.call(null, ...args);
}
exports.funcall = funcall_;
exports.funcall_ = funcall_;
funcall_.lispSource = [Symbol.for('define'), [Symbol.for('funcall_'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('f'), Symbol.for('call'), Symbol.for('js/null'), Symbol.for('args')]];
/**
 * Whether `obj` is a procedure (i.e., a function).
 *
 * Similar to [`procedure?` in Racket][rkt:procedurep] and
 * [`functionp` in Common Lisp][cl:functionp].
 *
 * [rkt:procedurep]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28quote._~23~25kernel%29._procedure~3f%29%29
 * [cl:functionp]: http://clhs.lisp.se/Body/f_fnp.htm#functionp
 */
function procedurep_(obj) {
    return obj instanceof Function;
}
exports.functionp = procedurep_;
exports.procedurep = procedurep_;
exports.procedurep_ = procedurep_;
procedurep_.lispSource = [Symbol.for('define'), [Symbol.for('procedure?_'), Symbol.for('obj')], [Symbol.for('js/function?'), Symbol.for('obj')]];
/**
 * Whether `obj` is a fexpr, that is, a procedure that
 * does not evaluate its arguments.
 */
function fexprp_(obj) {
    return (obj instanceof Function) && obj.fexpr;
}
exports.fexprp = fexprp_;
exports.fexprp_ = fexprp_;
fexprp_.lispSource = [Symbol.for('define'), [Symbol.for('fexpr?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('procedure?'), Symbol.for('obj')], [Symbol.for('get-field'), Symbol.for('fexpr'), Symbol.for('obj')]]];
/**
 * Logical negation.
 *
 * Similar to [`not` in Racket][rkt:not] and
 * [`not` in Common Lisp][cl:not].
 *
 * [rkt:not]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._not%29%29
 * [cl:not]: http://clhs.lisp.se/Body/f_not.htm
 */
function not_(x) {
    return !x;
}
exports.not = not_;
exports.not_ = not_;
not_.lispSource = [Symbol.for('define'), [Symbol.for('not_'), Symbol.for('x')], [Symbol.for('not'), Symbol.for('x')]];
/**
 * Map a procedure over a list.
 *
 * Similar to [`map` in Racket][rkt:map] and
 * [`mapcar` in Common Lisp][cl:mapcar].
 *
 * [rkt:map]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
 * [cl:mapcar]: http://clhs.lisp.se/Body/f_mapc_.htm#mapcar
 */
function map_(f, seq) {
    return seq.map(function (x) {
        return f(x);
    });
}
exports.map = map_;
exports.mapcar = map_;
exports.map_ = map_;
map_.lispSource = [Symbol.for('define'), [Symbol.for('map_'), Symbol.for('f'), Symbol.for('seq')], [Symbol.for('map'), Symbol.for('f'), Symbol.for('seq')]];
/**
 * Less than operator.
 *
 * Similar to [`<` in Racket][rkt:lt] and [`<` in Common Lisp][cl:lt].
 *
 * [rkt:lt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c%29%29
 * [cl:lt]: http://clhs.lisp.se/Body/f_eq_sle.htm#LT
 */
function lt_(...args) {
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
exports.lt = lt_;
exports.lt_ = lt_;
lt_.lispSource = [Symbol.for('define'), [Symbol.for('lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>='), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];
/**
 * Less than or equal operator.
 *
 * Similar to [`<=` in Racket][rkt:lte] and [`<=` in Common Lisp][cl:lte].
 *
 * [rkt:lte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c~3d%29%29
 * [cl:lte]: http://clhs.lisp.se/Body/f_eq_sle.htm#LTEQ
 */
function lte_(...args) {
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
exports.lte = lte_;
exports.lte_ = lte_;
lte_.lispSource = [Symbol.for('define'), [Symbol.for('lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];
/**
 * Greater than operator.
 *
 * Similar to [`>` in Racket][rkt:gt] and [`>` in Common Lisp][cl:gt].
 *
 * [rkt:gt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e%29%29
 * [cl:gt]: http://clhs.lisp.se/Body/f_eq_sle.htm#GT
 */
function gt_(...args) {
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
exports.gt = gt_;
exports.gt_ = gt_;
gt_.lispSource = [Symbol.for('define'), [Symbol.for('gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<='), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];
/**
 * Greater than or equal operator.
 *
 * Similar to [`>=` in Racket][rkt:gte] and [`>=` in Common Lisp][cl:gte].
 *
 * [rkt:gte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e~3d%29%29
 * [cl:gte]: http://clhs.lisp.se/Body/f_eq_sle.htm#GTEQ
 */
function gte_(...args) {
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
exports.gte = gte_;
exports.gte_ = gte_;
gte_.lispSource = [Symbol.for('define'), [Symbol.for('gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<'), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];
/**
 * Modulo operation.
 *
 * Similar to [`modulo` in Racket][rkt:modulo] and
 * [`mod` in Common Lisp][cl:mod].
 *
 * [rkt:modulo]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._modulo%29%29
 * [cl:mod]: http://clhs.lisp.se/Body/f_mod_r.htm
 */
function modulo_(x, y) {
    return x % y;
}
exports.modulo_ = modulo_;
modulo_.lispSource = [Symbol.for('define'), [Symbol.for('modulo_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('modulo'), Symbol.for('x'), Symbol.for('y')]];
/**
 * Addition.
 *
 * Similar to [`+` in Racket][rkt:add] and [`+` in Common Lisp][cl:add].
 *
 * [rkt:add]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2B%29%29
 * [cl:add]: http://clhs.lisp.se/Body/f_pl.htm
 */
function add_(...args) {
    let result = 0;
    for (let arg of args) {
        result = result + arg;
    }
    return result;
}
exports._add = add_;
exports.add = add_;
exports.plus = add_;
exports.add_ = add_;
add_.lispSource = [Symbol.for('define'), [Symbol.for('add_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), 0]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('+'), Symbol.for('result'), Symbol.for('arg')]]], Symbol.for('result')]];
/**
 * Return `(+ x 1)`.
 *
 * Similar to [`add1` in Racket][rkt:add1].
 *
 * [rkt:add1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29
 */
function add1_(x) {
    return x + 1;
}
exports.add1 = add1_;
exports.add1_ = add1_;
add1_.lispSource = [Symbol.for('define'), [Symbol.for('add1_'), Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]];
/**
 * Subtraction.
 *
 * Similar to [`-` in Racket][rkt:sub] and [`-` in Common Lisp][cl:sub].
 *
 * [rkt:sub]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._-%29%29
 * [cl:sub]: http://clhs.lisp.se/Body/f__.htm
 */
function sub_(...args) {
    const len = args.length;
    if (len === 0) {
        return 0;
    }
    else if (len === 1) {
        return -args[0];
    }
    else {
        let result = args[0];
        for (let i = 1; i < len; i++) {
            result = result - args[i];
        }
        return result;
    }
}
exports._sub = sub_;
exports.minus = sub_;
exports.sub = sub_;
exports.subtract = sub_;
exports.sub_ = sub_;
sub_.lispSource = [Symbol.for('define'), [Symbol.for('sub_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('len'), [Symbol.for('array-list-length'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('zero?'), Symbol.for('len')], 0], [[Symbol.for('one?'), Symbol.for('len')], [Symbol.for('-'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, Symbol.for('len')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('-'), Symbol.for('result'), [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]]]], Symbol.for('result')]]]]];
/**
 * Return `(- x 1)`.
 *
 * Similar to [`sub1` in Racket][rkt:sub1].
 *
 * [rkt:sub1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._sub1%29%29
 */
function sub1_(x) {
    return x - 1;
}
exports.sub1 = sub1_;
exports.sub1_ = sub1_;
sub1_.lispSource = [Symbol.for('define'), [Symbol.for('sub1_'), Symbol.for('x')], [Symbol.for('-'), Symbol.for('x'), 1]];
/**
 * Multiplication.
 *
 * Similar to [`*` in Racket][rkt:mul] and [`*` in Common Lisp][cl:mul].
 *
 * [rkt:mul]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2A%29%29
 * [cl:mul]: http://clhs.lisp.se/Body/f_st.htm
 */
function mul_(...args) {
    let result = 1;
    for (let arg of args) {
        result = result * arg;
    }
    return result;
}
exports._mul = mul_;
exports.mul = mul_;
exports.mul_ = mul_;
mul_.lispSource = [Symbol.for('define'), [Symbol.for('mul_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), 1]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('*'), Symbol.for('result'), Symbol.for('arg')]]], Symbol.for('result')]];
/**
 * Division.
 *
 * Similar to [`/` in Racket][rkt:div] and [`/` in Common Lisp][cl:div].
 *
 * [rkt:div]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2F%29%29
 * [cl:div]: http://clhs.lisp.se/Body/f_sl.htm
 */
function div_(...args) {
    if (args.length === 1) {
        return 1 / args[0];
    }
    else {
        let result = args[0];
        const _end = args.length;
        for (let i = 1; i < _end; i++) {
            result = result / args[i];
        }
        return result;
    }
}
exports._div = div_;
exports.div = div_;
exports.div_ = div_;
div_.lispSource = [Symbol.for('define'), [Symbol.for('div_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 1], [Symbol.for('/'), 1, [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('/'), Symbol.for('result'), [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]]]], Symbol.for('result')]]]];
/**
 * Whether a value is the number zero.
 *
 * Similar to [`zerop` in Racket][rkt:zerop] and
 * [`zerop` in Common Lisp][cl:zerop].
 *
 * [rkt:zerop]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._zero~3f%29%29
 * [cl:zerop]: http://clhs.lisp.se/Body/f_zerop.htm#zerop
 */
function zerop_(n) {
    return n === 0;
}
exports.zerop = zerop_;
exports.zerop_ = zerop_;
zerop_.lispSource = [Symbol.for('define'), [Symbol.for('zero?_'), Symbol.for('n')], [Symbol.for('='), Symbol.for('n'), 0]];
/**
 * Whether a value is the number one.
 */
function onep_(n) {
    return n === 1;
}
exports.onep_ = onep_;
onep_.lispSource = [Symbol.for('define'), [Symbol.for('one?_'), Symbol.for('n')], [Symbol.for('='), Symbol.for('n'), 1]];
/**
 * Whether a number is odd.
 *
 * Similar to [`odd?` in Racket][rkt:oddp] and
 * [`oddp` in Common Lisp][rkt:oddp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._odd~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#oddp
 */
function oddp_(n) {
    return (n % 2) !== 0;
}
exports.oddp_ = oddp_;
oddp_.lispSource = [Symbol.for('define'), [Symbol.for('odd?_'), Symbol.for('n')], [Symbol.for('not'), [Symbol.for('even?'), Symbol.for('n')]]];
/**
 * Whether a number is even.
 *
 * Similar to [`even?` in Racket][rkt:evenp] and
 * [`evenp` in Common Lisp][rkt:evenp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._even~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#evenp
 */
function evenp_(n) {
    return (n % 2) === 0;
}
exports.evenp_ = evenp_;
evenp_.lispSource = [Symbol.for('define'), [Symbol.for('even?_'), Symbol.for('n')], [Symbol.for('zero?'), [Symbol.for('modulo'), Symbol.for('n'), 2]]];
/**
 * Whether a value is truthy.
 */
function truep(x) {
    // TODO: Remove.
    return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;
}
exports.truep_ = truep;
exports.truep = truep;
truep.lispSource = [Symbol.for('define'), [Symbol.for('true?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('undefined?'), Symbol.for('x')]], Symbol.for('x'), [Symbol.for('not'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], 0]]], [Symbol.for('not'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('js-keys'), Symbol.for('x')]], 0], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('constructor'), Symbol.for('x')], Symbol.for('Object')]]], Symbol.for('#t')]];
/**
 * Whether a value is falsy.
 */
function falsep(x) {
    // TODO: Remove.
    return !truep(x);
}
exports.falsep_ = falsep;
exports.falsep = falsep;
falsep.lispSource = [Symbol.for('define'), [Symbol.for('false?'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('true?'), Symbol.for('x')]]];
/**
 * The identity function.
 *
 * Similar to [`identity` in Racket][rkt:identity] and
 * [`identity` in Common Lisp][cl:identity].
 *
 * [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
 * [cl:identity]: http://clhs.lisp.se/Body/f_identi.htm#identity
 */
function identity_(x) {
    return x;
}
exports.identity_ = identity_;
identity_.lispSource = [Symbol.for('define'), [Symbol.for('identity_'), Symbol.for('x')], Symbol.for('x')];
/**
 * Returns a procedure that accepts any arguments and returns `x`.
 *
 * Similar to [`const` in Racket][rkt:const] and
 * [`constantly` in Common Lisp][cl:constantly].
 *
 * [rkt:const]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._const%29%29
 * [cl:constantly]: http://clhs.lisp.se/Body/f_cons_1.htm#constantly
 */
function const_(x = undefined) {
    return function (...args) {
        return x;
    };
}
exports.const_ = const_;
const_.lispSource = [Symbol.for('define'), [Symbol.for('const_'), [Symbol.for('x'), Symbol.for('undefined')]], [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('x')]];
/**
 * Return a tuple of multiple values.
 *
 * Similar to [`values` in Racket][rkt:values] and
 * [`values` in Common Lisp][cl:values].
 *
 * [rkt:values]: https://docs.racket-lang.org/reference/values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29
 * [cl:values]: http://clhs.lisp.se/Body/f_values.htm
 */
function values_(...args) {
    // Implementation-wise, `values` does the same as `list`;
    // there is no separate data type for value tuples.
    return args;
}
exports.values = values_;
exports.values_ = values_;
values_.lispSource = [Symbol.for('define'), [Symbol.for('values_'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')];
/**
 * Whether something is a keyword, i.e., a symbol
 * whose first character is `:`.
 *
 * Similar to [`keyword?` in Racket][rkt:keywordp] and
 * [`keywordp` in Common Lisp][cl:keywordp].
 *
 * [rkt:keywordp]: https://docs.racket-lang.org/reference/keywords.html#%28def._%28%28quote._~23~25kernel%29._keyword~3f%29%29
 * [cl:keywordp]: http://clhs.lisp.se/Body/f_kwdp.htm#keywordp
 */
function keywordp_(obj) {
    return (typeof obj === 'symbol') && obj.description.match(new RegExp('^:'));
}
exports.keywordp = keywordp_;
exports.keywordp_ = keywordp_;
keywordp_.lispSource = [Symbol.for('define'), [Symbol.for('keyword?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('obj')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^:'], [Symbol.for('symbol->string'), Symbol.for('obj')]]]];
/**
 * Whether something is a number.
 *
 * Similar to [`number?` in Racket][rkt:numberp] and
 * [`numberp` in Common Lisp][cl:numberp].
 *
 * [rkt:numberp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._number~3f%29%29
 * [cl:numberp]: http://clhs.lisp.se/Body/f_nump.htm#numberp
 */
function numberp_(obj) {
    return Number.isFinite(obj);
}
exports.numberp = numberp_;
exports.numberp_ = numberp_;
numberp_.lispSource = [Symbol.for('define'), [Symbol.for('number?_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isFinite'), Symbol.for('obj')]];
/**
 * Whether something is a boolean value.
 *
 * Similar to [`boolean?` in Racket][rkt:booleanp] and
 * [`booleanp` in Emacs Lisp][el:booleanp].
 *
 * [rkt:booleanp]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._boolean~3f%29%29
 * [el:booleanp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html#index-booleanp
 */
function booleanp_(obj) {
    return typeof obj === 'boolean';
}
exports.booleanp_ = booleanp_;
booleanp_.lispSource = [Symbol.for('define'), [Symbol.for('boolean?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'boolean']];
/**
 * Whether something is the value `undefined`.
 */
function undefinedp_(obj) {
    return obj === undefined;
}
exports.undefinedp_ = undefinedp_;
undefinedp_.lispSource = [Symbol.for('define'), [Symbol.for('undefined?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), Symbol.for('undefined')]];
/**
 * Fold up a list left to right.
 *
 * Similar to [`foldl` in Racket][rkt:foldl] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldl]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
function foldl_(f, v, lst) {
    return lst.reduce(function (acc, x) {
        return f(x, acc);
    }, v);
}
exports.foldl = foldl_;
exports.foldl_ = foldl_;
foldl_.lispSource = [Symbol.for('define'), [Symbol.for('foldl_'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]];
/**
 * Fold up a list right to left.
 *
 * Similar to [`foldr` in Racket][rkt:foldr] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
function foldr_(f, v, lst) {
    return lst.reduceRight(function (acc, x) {
        return f(x, acc);
    }, v);
}
exports.foldr = foldr_;
exports.foldr_ = foldr_;
foldr_.lispSource = [Symbol.for('define'), [Symbol.for('foldr_'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('foldr'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]];
/**
 * Whether a list contains a value.
 * Returns a sublist if found, otherwise `#f`.
 * By default, comparison is done with `equal?`.
 *
 * Similar to [`member` in Racket][rkt:member] and
 * [`member` in Common Lisp][cl:member].
 *
 * [rkt:member]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._member%29%29
 * [cl:member]: http://clhs.lisp.se/Body/f_mem_m.htm
 */
function member_(v, lst, isEqual = undefined) {
    const idx = lst.findIndex(isEqual ? (function (x) {
        return isEqual(v, x);
    }) : (function (x) {
        return equalp(v, x);
    }));
    if (idx >= 0) {
        if (idx === 0) {
            return lst;
        }
        else {
            return lst.slice(idx);
        }
    }
    else {
        return false;
    }
}
exports.member = member_;
exports.memq = member_;
exports.member_ = member_;
member_.lispSource = [Symbol.for('define'), [Symbol.for('member_'), Symbol.for('v'), Symbol.for('lst'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]]], Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('#f')]]];
/**
 * Whether a list contains a value.
 * Like `member`, but always returns a boolean value.
 */
function memberp_(v, lst, isEqual = undefined) {
    return lst.findIndex(isEqual ? (function (x) {
        return isEqual(v, x);
    }) : (function (x) {
        return equalp(v, x);
    })) >= 0;
}
exports.memberP = memberp_;
exports.memberP_ = memberp_;
exports.memberp = memberp_;
exports.memberp_ = memberp_;
memberp_.lispSource = [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('memf?'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]]], Symbol.for('lst')]];
/**
 * Whether a list contains a value.
 * Like `member`, but comparison is done with `eq?`.
 *
 * Similar to [`meq` in Racket][rkt:memq] and
 * [`memq` in Emacs Lisp][el:memq].
 *
 * [rkt:memq]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._memq%29%29
 * [el:memq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html#index-memq
 */
function memq_(v, lst) {
    const idx = lst.findIndex(function (x) {
        return v === x;
    });
    if (idx >= 0) {
        if (idx === 0) {
            return lst;
        }
        else {
            return lst.slice(idx);
        }
    }
    else {
        return false;
    }
}
exports.memq_ = memq_;
memq_.lispSource = [Symbol.for('define'), [Symbol.for('memq_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('#f')]]];
/**
 * Whether a list contains a value,
 * using `eq?` for comparisons. Like `memq`,
 * but always returns a boolean value.
 */
function memqp_(v, lst) {
    // This construct maps neatly onto
    // [`Array.prototype.includes()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes).
    return lst.includes(v);
}
exports.memqp_ = memqp_;
memqp_.lispSource = [Symbol.for('define'), [Symbol.for('memq?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('includes'), Symbol.for('v')]];
/**
 * Whether a list contains a value matching a predicate.
 * Applies the predicate `proc` to elements in the list
 * and returns a sublist if found, otherwise `#f`.
 *
 * Similar to [`memf` in Racket][rkt:memf] and
 * [`member-if` in Common Lisp][cl:member-if].
 *
 * [rkt:memf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._memf%29%29
 * [cl:member-if]: http://clhs.lisp.se/Body/f_mem_m.htm#member-if
 */
function memf_(proc, lst, notFound = false) {
    const idx = lst.findIndex(proc);
    if (idx >= 0) {
        if (idx === 0) {
            return lst;
        }
        else {
            return lst.slice(idx);
        }
    }
    else {
        return notFound;
    }
}
exports.memf = memf_;
exports.memf_ = memf_;
memf_.lispSource = [Symbol.for('define'), [Symbol.for('memf_'), Symbol.for('proc'), Symbol.for('lst'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('not-found')]]];
/**
 * Whether a list contains a value matching a predicate.
 * Like `memf`, but always returns a boolean value.
 */
function memfp_(proc, lst) {
    return lst.findIndex(proc) >= 0;
}
exports.memfp_ = memfp_;
memfp_.lispSource = [Symbol.for('define'), [Symbol.for('memf?_'), Symbol.for('proc'), Symbol.for('lst')], [Symbol.for('>='), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')], 0]];
/**
 * Find a list element matching a predicate.
 *
 * Similar to [`findf` in Racket][rkt:findf] and
 * [`find-if` in Common Lisp][cl:find-if].
 *
 * [rkt:findf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._findf%29%29
 * [cl:find-if]: http://clhs.lisp.se/Body/f_find_.htm#find-if
 */
function findf_(proc, lst, notFound = false) {
    const idx = lst.findIndex(proc);
    if (idx >= 0) {
        return lst[idx];
    }
    else {
        return notFound;
    }
}
exports.findf = findf_;
exports.findf_ = findf_;
findf_.lispSource = [Symbol.for('define'), [Symbol.for('findf_'), Symbol.for('proc'), Symbol.for('lst'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('array-list-nth'), Symbol.for('idx'), Symbol.for('lst')], Symbol.for('not-found')]]];
/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
function findfIndex_(proc, seq, notFound = false) {
    const idx = seq.findIndex(proc);
    if (idx >= 0) {
        return idx;
    }
    else {
        return notFound;
    }
}
exports.findfIndex = findfIndex_;
exports.findfIndex_ = findfIndex_;
findfIndex_.lispSource = [Symbol.for('define'), [Symbol.for('findf-index_'), Symbol.for('proc'), Symbol.for('seq'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('seq')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], Symbol.for('idx'), Symbol.for('not-found')]]];
/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
function indexWhere_(seq, proc, notFound = false) {
    const idx = seq.findIndex(proc);
    if (idx >= 0) {
        return idx;
    }
    else {
        return notFound;
    }
}
exports.indexWhere_ = indexWhere_;
indexWhere_.lispSource = [Symbol.for('define'), [Symbol.for('index-where_'), Symbol.for('seq'), Symbol.for('proc'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('seq')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], Symbol.for('idx'), Symbol.for('not-found')]]];
/**
 * Find the index of a list element.
 *
 * Similar to [`index-of` in Racket][rkt:index-of] and
 * [`position` in Common Lisp][cl:position].
 *
 * [rkt:index-of]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
 * [cl:position]: http://clhs.lisp.se/Body/f_pos_p.htm#position
 */
function indexOf_(seq, v, isEqual = undefined) {
    const idx = seq.findIndex(isEqual ? (function (x) {
        return isEqual(x, v);
    }) : (function (x) {
        return equalp(x, v);
    }));
    if (idx >= 0) {
        return idx;
    }
    else {
        return false;
    }
}
exports.indexOf_ = indexOf_;
indexOf_.lispSource = [Symbol.for('define'), [Symbol.for('index-of_'), Symbol.for('seq'), Symbol.for('v'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('index-where'), Symbol.for('seq'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('x'), Symbol.for('v')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('x'), Symbol.for('v')]]]]];
/**
 * Return the intersection of multiple lists.
 *
 * Similar to [`intersection` in Common Lisp][cl:intersection].
 *
 * [cl:intersection]: http://clhs.lisp.se/Body/f_isec_.htm#intersection
 */
function intersection_(...args) {
    function intersection2(arr1, arr2) {
        let result = [];
        for (let element of arr1) {
            if (arr2.includes(element) && !result.includes(element)) {
                result.push(element);
            }
        }
        return result;
    }
    intersection2.lispSource = [Symbol.for('define'), [Symbol.for('intersection2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('arr2')], [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]];
    if (args.length === 0) {
        return [];
    }
    else if (args.length === 1) {
        return args[0];
    }
    else {
        return args.slice(1).reduce(function (acc, x) {
            return intersection2(acc, x);
        }, args[0]);
    }
}
exports.intersection = intersection_;
exports.intersection_ = intersection_;
intersection_.lispSource = [Symbol.for('define'), [Symbol.for('intersection_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), [Symbol.for('intersection2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('arr2')], [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('quote'), []]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('intersection2'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('first'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('args')]]]]];
/**
 * Return the union of multiple lists.
 *
 * Similar to [`union` in Common Lisp][cl:union].
 *
 * [cl:union]: http://clhs.lisp.se/Body/f_unionc.htm#union
 */
function union_(...args) {
    function union2(arr1, arr2) {
        let result = [];
        for (let element of arr1) {
            if (!result.includes(element)) {
                result.push(element);
            }
        }
        for (let element of arr2) {
            if (!result.includes(element)) {
                result.push(element);
            }
        }
        return result;
    }
    union2.lispSource = [Symbol.for('define'), [Symbol.for('union2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr2')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]];
    return args.reduce(function (acc, x) {
        return union2(acc, x);
    }, []);
}
exports.union = union_;
exports.union_ = union_;
union_.lispSource = [Symbol.for('define'), [Symbol.for('union_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), [Symbol.for('union2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr2')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('union2'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('quote'), []], Symbol.for('args')]];
/**
 * Produce a list of numbers from `start`, inclusive,
 * to `end`, exclusive.
 *
 * Similar to [`range` in Racket][rkt:range].
 *
 * [rkt:range]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._range%29%29
 */
function range_(start, end = undefined, step = undefined) {
    const startN = (end === undefined) ? 0 : start;
    const endN = (end === undefined) ? start : end;
    const stepN = step || 1;
    let result = [];
    for (let i = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
        result.push(i);
    }
    return result;
}
exports.range = range_;
exports.range_ = range_;
range_.lispSource = [Symbol.for('define'), [Symbol.for('range_'), Symbol.for('start'), [Symbol.for('end'), Symbol.for('undefined')], [Symbol.for('step'), Symbol.for('undefined')]], [Symbol.for('let*'), [[Symbol.for('start-n'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], 0, Symbol.for('start')]], [Symbol.for('end-n'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], Symbol.for('start'), Symbol.for('end')]], [Symbol.for('step-n'), [Symbol.for('or'), Symbol.for('step'), 1]], [Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), Symbol.for('start-n'), Symbol.for('end-n'), Symbol.for('step-n')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')]];
/**
 * Right-to-left function composition.
 *
 * Similar to [`compose` in Racket][rkt:compose].
 *
 * [rkt:compose]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._compose%29%29
 */
function compose_(...args) {
    const functions = args.slice(0, -1);
    const lastFunction = args[args.length - 1];
    return function (...args) {
        const val = lastFunction(...args);
        return functions.reduceRight(function (x, f) {
            return f(x);
        }, val);
    };
}
exports.compose = compose_;
exports.compose_ = compose_;
compose_.lispSource = [Symbol.for('define'), [Symbol.for('compose_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('functions'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('last-function'), [Symbol.for('array-list-last'), Symbol.for('args')]]], [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('let'), [[Symbol.for('val'), [Symbol.for('apply'), Symbol.for('last-function'), Symbol.for('args')]]], [Symbol.for('foldr'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]], Symbol.for('val'), Symbol.for('functions')]]]]];
/**
 * Left-to-right function composition.
 *
 * Like `compose`, but in the other direction.
 */
function pipe_(...args) {
    const functions = args.slice(1);
    const firstFunction = args[0];
    return function (...args) {
        const val = firstFunction(...args);
        return functions.reduce(function (x, f) {
            return f(x);
        }, val);
    };
}
exports.pipe = pipe_;
exports.pipe_ = pipe_;
pipe_.lispSource = [Symbol.for('define'), [Symbol.for('pipe_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('functions'), [Symbol.for('rest'), Symbol.for('args')]], [Symbol.for('first-function'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('let'), [[Symbol.for('val'), [Symbol.for('apply'), Symbol.for('first-function'), Symbol.for('args')]]], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]], Symbol.for('val'), Symbol.for('functions')]]]]];
/**
 * Filter a list by a predicate.
 *
 * Similar to [`filter` in Racket][rkt:filter] and
 * [`remove-if-not` in Common Lisp][cl:remove-if-not].
 *
 * [rkt:filter]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
 * [cl:remove-if-not]: http://clhs.lisp.se/Body/f_rm_rm.htm#remove-if-not
 */
function filter_(pred, lst) {
    return lst.filter(pred);
}
exports.filter_ = filter_;
filter_.lispSource = [Symbol.for('define'), [Symbol.for('filter_'), Symbol.for('pred'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('filter'), Symbol.for('pred')]];
/**
 * Whether a value is self-evaluating.
 */
function selfEvaluatingP_(x) {
    return (typeof x === 'boolean') || Number.isFinite(x) || (typeof x === 'string') || keywordp(x) || (x === null) || (x === undefined);
}
exports.selfEvaluatingP_ = selfEvaluatingP_;
selfEvaluatingP_.lispSource = [Symbol.for('define'), [Symbol.for('self-evaluating?_'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')], [Symbol.for('keyword?'), Symbol.for('x')], [Symbol.for('js/null?'), Symbol.for('x')], [Symbol.for('undefined?'), Symbol.for('x')]]];
/**
 * Write an error message to the console
 * if the assertion is false.
 *
 * Similar to [`assert` in Clojure][clj:assert]
 * and [`assert` in Racket][rkt:assert].
 *
 * [clj:assert]: https://clojuredocs.org/clojure.core/assert
 * [rkt:assert]: https://docs.racket-lang.org/ts-reference/Utilities.html#(def._((lib._typed-racket/base-env/extra-procs..rkt)._assert))
 */
function assert_(x, ...args) {
    return console.assert(x, ...args);
}
exports.assert_ = assert_;
assert_.lispSource = [Symbol.for('define'), [Symbol.for('assert_'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('console'), Symbol.for('assert'), Symbol.for('x'), Symbol.for('args')]];
/**
 * Output a message to the console.
 *
 * Similar to [`display` in Racket][rkt:display] and
 * [`print` in Common Lisp][cl:print].
 *
 * [rkt:display]: https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._display%29%29
 * [cl:print]: http://clhs.lisp.se/Body/f_wr_pr.htm#print
 */
function display_(...args) {
    return console.log(...args);
}
exports.display = display_;
exports.display_ = display_;
display_.lispSource = [Symbol.for('define'), [Symbol.for('display_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('console'), Symbol.for('log'), Symbol.for('args')]];
/**
 * Throw an error.
 *
 * Similar to [`error` in Racket][rkt:error] and
 * [`error` in Common Lisp][cl:error].
 *
 * [rkt:error]: https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._error%29%29
 * [cl:error]: http://clhs.lisp.se/Body/f_error.htm
 */
function error_(arg = undefined) {
    throw new Error(arg);
}
exports.error = error_;
exports.error_ = error_;
error_.lispSource = [Symbol.for('define'), [Symbol.for('error_'), [Symbol.for('arg'), Symbol.for('undefined')]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), Symbol.for('arg')]]];
/**
 * Get the type of a value.
 *
 * Similar to [`type-of` in Common Lisp][cl:type-of].
 *
 * [cl:type-of]: http://clhs.lisp.se/Body/f_tp_of.htm#type-of
 */
function typeOf_(x) {
    return typeof x;
}
exports.typeOf = typeOf_;
exports.typeOf_ = typeOf_;
typeOf_.lispSource = [Symbol.for('define'), [Symbol.for('type-of_'), Symbol.for('x')], [Symbol.for('js/typeof'), Symbol.for('x')]];
/**
 * Whether `obj` is an instance of `cls`.
 *
 * Similar to [`is-a?` in Racket][rkt:is-a-p].
 *
 * [rkt:is-a-p]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._is-a~3f%29%29
 */
function isAP_(obj, cls) {
    return obj instanceof cls;
}
exports.instanceOf = isAP_;
exports.instanceOfP = isAP_;
exports.instanceOfP_ = isAP_;
exports.instanceOf_ = isAP_;
exports.instanceofp = isAP_;
exports.isAP = isAP_;
exports.isAP_ = isAP_;
isAP_.lispSource = [Symbol.for('define'), [Symbol.for('is-a?_'), Symbol.for('obj'), Symbol.for('cls')], [Symbol.for('js/instanceof?'), Symbol.for('obj'), Symbol.for('cls')]];

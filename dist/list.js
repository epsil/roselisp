"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # List functions
 *
 * Functions for working with lists.
 *
 * ## Description
 *
 * Lists are implemented in terms of JavaScript arrays. There is,
 * however, no direct JavaScript equivalent to the Lisp concept of a
 * *cons cell*, such as `'(1 . 2)`. Instead, such values are
 * represented as a *dotted list*, which is a list where the
 * penultimate value is the symbol `|.|` (in JavaScript,
 * `Symbol.for('.')`).
 *
 *     > (dotted-list? '(1 . 2))
 *     #t
 *     > (dotted-list? '(1 2 . 3))
 *     #t
 *     > (dotted-list? (list 1 '|.| 2))
 *     #t
 *
 * The simplifying assumption is made that dotted lists are only used
 * to represent values that cannot be expressed without a dot. This
 * permits most list functions to be implemented as simple array
 * operations, although some checks are necessary in places since
 * dotted lists are also implemented as arrays.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.popRightX = exports.popLeftX = exports.popx_ = exports.popx = exports.pairp = exports.consp_ = exports.consp = exports.nullp = exports.nthcdr = exports.dottedListNthcdr_ = exports.dottedListNthcdr = exports.nth = exports.dottedListNth_ = exports.dottedListNth = exports.ninth = exports.nbutlast = exports.makePair = exports.makeList = exports.makeDottedList = exports.list = exports.properListP = exports.listp = exports.listStar = exports.length = exports.last = exports.lastPair = exports.lastCons_ = exports.lastCdr = exports.dottedListLastCdr_ = exports.improperListP = exports.fourth = exports.flatten = exports.head_ = exports.head = exports.first = exports.car_ = exports.car = exports.fifth = exports.eighth = exports.drop = exports.dropRight = exports.dottedListP = exports.cons = exports.circularListP = exports.tail_ = exports.tail = exports.cdr = exports.butlast = exports.buildList = exports.append = void 0;
exports.fifth_ = exports.eighth_ = exports.drop_ = exports.dropRight_ = exports.dottedProperListP_ = exports.dottedPairP_ = exports.dottedPairCdr_ = exports.dottedListP_ = exports.dottedListThird_ = exports.dottedListLink_ = exports.dottedListTenth_ = exports.dottedListTail_ = exports.dottedListSixth_ = exports.dottedListSeventh_ = exports.dottedListSet_ = exports.dottedListSetX_ = exports.dottedListSecond_ = exports.dottedListRef_ = exports.dottedListParse_ = exports.dottedListNinth_ = exports.dottedListLength_ = exports.dottedListLast_ = exports.dottedListHead_ = exports.dottedListFourth_ = exports.dottedListFirst_ = exports.dottedListFifth_ = exports.dottedListEighth_ = exports.dottedListToList_ = exports.dottedImproperListP_ = exports.cons_ = exports.circularListP_ = exports.cdr_ = exports.butlast_ = exports.buildList_ = exports.append_ = exports.third = exports.tenth = exports.take = exports.sixth = exports.seventh = exports.setCdrX = exports.setCarX = exports.second = exports.cadr_ = exports.reverse = exports.rest = exports.pushRightX = exports.appendToList = exports.pushLeftX = exports.pushx = void 0;
exports.third_ = exports.tenth_ = exports.take_ = exports.sixth_ = exports.seventh_ = exports.setCdrX_ = exports.setCarX_ = exports.second_ = exports.reverse_ = exports.reversex_ = exports.rest_ = exports.pushRightX_ = exports.pushLeftX_ = exports.properListP_ = exports.popRightX_ = exports.popLeftX_ = exports.pairp_ = exports.pairOrListP_ = exports.nullp_ = exports.nthcdr_ = exports.nth_ = exports.ninth_ = exports.nbutlast_ = exports.makePair_ = exports.makeList_ = exports.makeDottedList_ = exports.list_ = exports.listp_ = exports.listTail_ = exports.listStar_ = exports.listSet_ = exports.listSetX_ = exports.listRef_ = exports.listToDottedList_ = exports.length_ = exports.last_ = exports.lastPair_ = exports.lastCdr_ = exports.improperListP_ = exports.fourth_ = exports.flatten_ = exports.first_ = void 0;
// (require (only-in "./estree"
//                   CallExpression
//                   Identifier
//                   Literal
//                   MemberExpression))
const [lastCdr, range, dottedListSecond, dottedListThird, dottedListFourth, dottedListFifth, dottedListSixth, dottedListSeventh, dottedListEighth, dottedListNinth, dottedListTenth, dottedListLength, dottedListLast] = (() => {
    function lastCdr_(lst) {
        if (!Array.isArray(lst)) {
            return undefined;
        }
        else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
            let result = lst;
            while (Array.isArray(result) && (result.length >= 3) && (result.at(-2) === Symbol.for('.'))) {
                result = result.at(-1);
            }
            return result;
        }
        else {
            return [];
        }
    }
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
    function dottedListSecond_(lst) {
        return dottedListRef_(lst, 1);
    }
    function dottedListThird_(lst) {
        return dottedListRef_(lst, 2);
    }
    function dottedListFourth_(lst) {
        return dottedListRef_(lst, 3);
    }
    function dottedListFifth_(lst) {
        return dottedListRef_(lst, 4);
    }
    function dottedListSixth_(lst) {
        return dottedListRef_(lst, 5);
    }
    function dottedListSeventh_(lst) {
        return dottedListRef_(lst, 6);
    }
    function dottedListEighth_(lst) {
        return dottedListRef_(lst, 7);
    }
    function dottedListNinth_(lst) {
        return dottedListRef_(lst, 8);
    }
    function dottedListTenth_(lst) {
        return dottedListRef_(lst, 9);
    }
    function dottedListLength_(lst) {
        let len = 0;
        let current = lst;
        while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
            len = len + (lst.length - 2);
            current = current.at(-1);
        }
        return len;
    }
    function dottedListLast_(lst) {
        let current = lst;
        let result = undefined;
        while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x) => {
            return Array.isArray(x) && (x.length === 0);
        })(((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1))) {
            current = ((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1);
        }
        if (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
            result = current[current.length - 3];
        }
        return result;
    }
    return [lastCdr_, range_, dottedListSecond_, dottedListThird_, dottedListFourth_, dottedListFifth_, dottedListSixth_, dottedListSeventh_, dottedListEighth_, dottedListNinth_, dottedListTenth_, dottedListLength_, dottedListLast_];
})();
/**
 * Whether something is a pair, i.e., a cons cell.
 *
 * Similar to [`pair?` in Racket][rkt:pairp] and
 * [`consp` in Common Lisp][cl:consp].
 *
 * [rkt:pairp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._pair~3f%29%29
 * [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
 */
function pairp_(x) {
    // All lists except the empty list qualify as pairs.
    return Array.isArray(x) && (x.length > 0);
}
exports.consp = pairp_;
exports.consp_ = pairp_;
exports.pairp = pairp_;
exports.pairp_ = pairp_;
pairp_.fsource = [Symbol.for('define'), [Symbol.for('pair?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 0]]];
/**
 * Whether something is the empty list.
 *
 * Similar to [`null?` in Racket][rkt:nullp].
 *
 * [rkt:nullp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null~3f%29%29
 */
function nullp_(x) {
    return Array.isArray(x) && (x.length === 0);
}
exports.nullp = nullp_;
exports.nullp_ = nullp_;
nullp_.fsource = [Symbol.for('define'), [Symbol.for('null?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 0]]];
/**
 * Whether something is a list.
 *
 * This function checks whether the list is a proper list, i.e.,
 * whether the list is terminated by the empty list. In that regard,
 * this function is more similar to [`list?` in Racket][rkt:listp]
 * than to [`listp` in Common Lisp][cl:listp].
 *
 * [rkt:listp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list~3f%29%29
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 */
function listp_(x) {
    const x1 = lastCdr(x);
    return Array.isArray(x1) && (x1.length === 0);
}
exports.listp = listp_;
exports.properListP = listp_;
exports.listp_ = listp_;
listp_.fsource = [Symbol.for('define'), [Symbol.for('list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];
/**
 * Whether something is a pair or a list.
 *
 * Similar to [`listp` in Common Lisp][cl:listp] and
 * [`listp` in Emacs Lisp], which are not as
 * rigorous as `list?` in Scheme.
 *
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 * [el:listp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/List_002drelated-Predicates.html#index-listp
 */
function pairOrListP_(x) {
    return (Array.isArray(x) && (x.length > 0)) || (Array.isArray(x) && (x.length === 0));
}
exports.pairOrListP_ = pairOrListP_;
pairOrListP_.fsource = [Symbol.for('define'), [Symbol.for('pair-or-list?_'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('pair?'), Symbol.for('x')], [Symbol.for('null?'), Symbol.for('x')]]];
/**
 * Make a list.
 *
 * Similar to [`list` in Racket][rkt:list] and
 * [`list` in Common Lisp][cl:list].
 *
 * [rkt:list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%29%29
 * [cl:list]: http://clhs.lisp.se/Body/f_list_.htm
 */
function list_(...args) {
    return args;
}
exports.list = list_;
exports.list_ = list_;
list_.fsource = [Symbol.for('define'), [Symbol.for('list_'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')];
/**
 * Create a cons cell whose CAR is `x` and CDR is `y`.
 *
 * Similar to [`cons` in Racket][rkt:cons] and
 * [`cons` in Common Lisp][cl:cons].
 *
 * [rkt:cons]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cons%29%29
 * [cl:cons]: http://clhs.lisp.se/Body/f_cons.htm
 */
function cons_(x, y) {
    // Create a regular list whenever possible;
    // otherwise create a dotted list.
    return [x, ...(Array.isArray(y) ? y : [Symbol.for('.'), y])];
}
exports.cons = cons_;
exports.cons_ = cons_;
cons_.fsource = [Symbol.for('define'), [Symbol.for('cons_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote-splicing'), [Symbol.for('dotted-list-link'), Symbol.for('y')]]]]];
/**
 * Make a dotted list. Like `list`, but the final argument
 * is used as the tail, instead of as the final element.
 *
 *     > (list* 1 '())
 *     '(1)
 *     > (list* 1 2)
 *     '(1 . 2)
 *     > (list* 1 2 3)
 *     '(1 2 . 3)
 *
 * Also known as `cons*`. Similar to
 * [`list*` in Racket][rkt:list-star].
 *
 * [rkt:list-star]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%2A%29%29
 */
function listStar_(...args) {
    if (args.length === 0) {
        return undefined;
    }
    else if (args.length === 1) {
        return args[0];
    }
    else {
        const tailLst = args.at(-1);
        const headLst = args.slice(0, -1);
        if (Array.isArray(tailLst)) {
            // Make a proper list if possible.
            return [...headLst, ...tailLst];
        }
        else {
            // If not, make a dotted list.
            return [...headLst, Symbol.for('.'), tailLst];
        }
    }
}
exports.listStar = listStar_;
exports.listStar_ = listStar_;
listStar_.fsource = [Symbol.for('define'), [Symbol.for('list-star_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('args')], 0], undefined], [[Symbol.for('='), [Symbol.for('length'), Symbol.for('args')], 1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('tail-lst'), [Symbol.for('last'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('head-lst'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('tail-lst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], [Symbol.for('unquote-splicing'), Symbol.for('tail-lst')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('tail-lst')]]]]]]]];
/**
 * Make a list of `n` elements. The function `proc` is applied
 * to the integers from `0` to `n - 1`.
 *
 * Similar to [`build-list` in Racket][rkt:build-list].
 *
 * [rkt:build-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._build-list%29%29
 */
function buildList_(n, proc) {
    return range(0, n).map(function (x) {
        return proc(x);
    });
}
exports.buildList = buildList_;
exports.buildList_ = buildList_;
buildList_.fsource = [Symbol.for('define'), [Symbol.for('build-list_'), Symbol.for('n'), Symbol.for('proc')], [Symbol.for('map'), Symbol.for('proc'), [Symbol.for('range'), 0, Symbol.for('n')]]];
/**
 * Make a list of length `k`, where every element is the value `v`.
 *
 * Similar to [`make-list` in Racket][rkt:make-list].
 *
 * [rkt:make-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._make-list%29%29
 */
function makeList_(k, v) {
    let result = [];
    for (let i = 0; i < k; i++) {
        result.push(v);
    }
    return result;
}
exports.makeList = makeList_;
exports.makeList_ = makeList_;
makeList_.fsource = [Symbol.for('define'), [Symbol.for('make-list_'), Symbol.for('k'), Symbol.for('v')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('k')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('v')]], Symbol.for('result')]];
/**
 * List concatenation.
 *
 * Similar to [`append` in Racket][rkt:append] and
 * [`append` in Common Lisp][cl:append].
 *
 * [rkt:append]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29
 * [cl:append]: http://clhs.lisp.se/Body/f_append.htm#append
 */
function append_(...args) {
    return args.reduce(function (acc, x) {
        return [...acc, ...x];
    }, []);
}
exports.append = append_;
exports.append_ = append_;
append_.fsource = [Symbol.for('define'), [Symbol.for('append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('append'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('quote'), []], Symbol.for('args')]];
/**
 * Flatten an arbitrarily nested list.
 *
 * Similar to [`flatten` in Racket][rkt:flatten].
 *
 * [rkt:flatten]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._flatten%29%29
 */
function flatten_(lst) {
    return lst.reduce(function (acc, x) {
        if (Array.isArray(x)) {
            return [...acc, ...flatten_(x)];
        }
        else if (x === Symbol.for('.')) {
            return acc;
        }
        else {
            acc.push(x);
            return acc;
        }
    }, []);
}
exports.flatten = flatten_;
exports.flatten_ = flatten_;
flatten_.fsource = [Symbol.for('define'), [Symbol.for('flatten_'), Symbol.for('lst')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('x')], [Symbol.for('append'), Symbol.for('acc'), [Symbol.for('flatten_'), Symbol.for('x')]]], [[Symbol.for('eq?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], Symbol.for('acc')], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x')]]]], [Symbol.for('quote'), []], Symbol.for('lst')]];
/**
 * Return the first element of a list.
 *
 * Similar to [`first` in Racket][rkt:first].
 *
 * [rkt:first]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._first%29%29
 */
function first_(lst) {
    return lst[0];
}
exports.car = first_;
exports.car_ = first_;
exports.first = first_;
exports.head = first_;
exports.head_ = first_;
exports.first_ = first_;
first_.fsource = [Symbol.for('define'), [Symbol.for('first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];
/**
 * Return the second element of a list.
 *
 * Similar to [`second` in Racket][rkt:second].
 *
 * [rkt:second]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29
 */
function second_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListSecond(lst);
    }
    else {
        return lst[1];
    }
}
exports.cadr_ = second_;
exports.second = second_;
exports.second_ = second_;
second_.fsource = [Symbol.for('define'), [Symbol.for('second_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-second'), Symbol.for('lst')], [Symbol.for('array-second'), Symbol.for('lst')]]];
/**
 * Return the third element of a list.
 *
 * Similar to [`third` in Racket][rkt:third].
 *
 * [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
 */
function third_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListThird(lst);
    }
    else {
        return lst[2];
    }
}
exports.third = third_;
exports.third_ = third_;
third_.fsource = [Symbol.for('define'), [Symbol.for('third_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-third'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')]]];
/**
 * Return the fourth element of a list.
 *
 * Similar to [`fourth` in Racket][rkt:fourth].
 *
 * [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
 */
function fourth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListFourth(lst);
    }
    else {
        return lst[3];
    }
}
exports.fourth = fourth_;
exports.fourth_ = fourth_;
fourth_.fsource = [Symbol.for('define'), [Symbol.for('fourth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-fourth'), Symbol.for('lst')], [Symbol.for('array-fourth'), Symbol.for('lst')]]];
/**
 * Return the fifth element of a list.
 *
 * Similar to [`fifth` in Racket][rkt:fifth].
 *
 * [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
 */
function fifth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListFifth(lst);
    }
    else {
        return lst[4];
    }
}
exports.fifth = fifth_;
exports.fifth_ = fifth_;
fifth_.fsource = [Symbol.for('define'), [Symbol.for('fifth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-fifth'), Symbol.for('lst')], [Symbol.for('array-fifth'), Symbol.for('lst')]]];
/**
 * Return the sixth element of a list.
 *
 * Similar to [`sixth` in Racket][rkt:sixth].
 *
 * [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
 */
function sixth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListSixth(lst);
    }
    else {
        return lst[5];
    }
}
exports.sixth = sixth_;
exports.sixth_ = sixth_;
sixth_.fsource = [Symbol.for('define'), [Symbol.for('sixth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-sixth'), Symbol.for('lst')], [Symbol.for('array-sixth'), Symbol.for('lst')]]];
/**
 * Return the seventh element of a list.
 *
 * Similar to [`seventh` in Racket][rkt:seventh].
 *
 * [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
 */
function seventh_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListSeventh(lst);
    }
    else {
        return lst[6];
    }
}
exports.seventh = seventh_;
exports.seventh_ = seventh_;
seventh_.fsource = [Symbol.for('define'), [Symbol.for('seventh_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-seventh'), Symbol.for('lst')], [Symbol.for('array-seventh'), Symbol.for('lst')]]];
/**
 * Return the eighth element of a list.
 *
 * Similar to [`eighth` in Racket][rkt:eighth].
 *
 * [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
 */
function eighth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListEighth(lst);
    }
    else {
        return lst[7];
    }
}
exports.eighth = eighth_;
exports.eighth_ = eighth_;
eighth_.fsource = [Symbol.for('define'), [Symbol.for('eighth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-eighth'), Symbol.for('lst')], [Symbol.for('array-eighth'), Symbol.for('lst')]]];
/**
 * Return the ninth element of a list.
 *
 * Similar to [`ninth` in Racket][rkt:ninth].
 *
 * [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
 */
function ninth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListNinth(lst);
    }
    else {
        return lst[8];
    }
}
exports.ninth = ninth_;
exports.ninth_ = ninth_;
ninth_.fsource = [Symbol.for('define'), [Symbol.for('ninth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-ninth'), Symbol.for('lst')], [Symbol.for('array-ninth'), Symbol.for('lst')]]];
/**
 * Return the tenth element of a list.
 *
 * Similar to [`tenth` in Racket][rkt:tenth].
 *
 * [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
 */
function tenth_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListTenth(lst);
    }
    else {
        return lst[9];
    }
}
exports.tenth = tenth_;
exports.tenth_ = tenth_;
tenth_.fsource = [Symbol.for('define'), [Symbol.for('tenth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-tenth'), Symbol.for('lst')], [Symbol.for('array-tenth'), Symbol.for('lst')]]];
/**
 * Return the tail of a list.
 *
 * Interprets `lst` as a linked list of pairs, and returns the second
 * element of the first pair.
 *
 * Similar to [`cdr` in Racket][rkt:cdr] and
 * [`cdr` in Common Lisp][cl:cdr].
 *
 * [rkt:cdr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29
 * [cl:cdr]: http://clhs.lisp.se/Body/f_car_c.htm#cdr
 */
function cdr_(lst) {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
        return lst[2];
    }
    else {
        return lst.slice(1);
    }
}
exports.cdr = cdr_;
exports.tail = cdr_;
exports.tail_ = cdr_;
exports.cdr_ = cdr_;
cdr_.fsource = [Symbol.for('define'), [Symbol.for('cdr_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-pair?'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')], [Symbol.for('array-rest'), Symbol.for('lst')]]];
/**
 * Return the tail of a list.
 *
 * Similar to [`rest` in Racket][rkt:rest].
 *
 * [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
 */
function rest_(lst) {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
        return lst[2];
    }
    else {
        return lst.slice(1);
    }
}
exports.rest = rest_;
exports.rest_ = rest_;
rest_.fsource = [Symbol.for('define'), [Symbol.for('rest_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-pair?'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')], [Symbol.for('array-rest'), Symbol.for('lst')]]];
/**
 * Access the list element indicated by
 * one or more `indices`.
 */
function listRef_(lst, ...indices) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListRef_(lst, ...indices);
    }
    else {
        let result = lst;
        for (let i of indices) {
            result = lst[i];
        }
        return result;
    }
}
exports.listRef_ = listRef_;
listRef_.fsource = [Symbol.for('define'), [Symbol.for('list-ref_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-ref_'), Symbol.for('lst'), Symbol.for('indices')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-ref'), Symbol.for('lst'), Symbol.for('i')]]], Symbol.for('result')]]];
/**
 * Return the `n`-th element of a list.
 *
 * Similar to [`nth` in Racket][rkt:nth] and
 * [`nth` in Common Lisp][cl:nth].
 *
 * [rkt:nth]: https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._nth%29%29
 * [cl:nth]: http://clhs.lisp.se/Body/f_nth.htm#nth
 */
function nth_(n, lst) {
    return listRef_(lst, n);
}
exports.dottedListNth = nth_;
exports.dottedListNth_ = nth_;
exports.nth = nth_;
exports.nth_ = nth_;
nth_.fsource = [Symbol.for('define'), [Symbol.for('nth_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('list-ref_'), Symbol.for('lst'), Symbol.for('n')]];
/**
 * Set a list position to a given value.
 * Returns a new list.
 */
function listSet_(lst, ...indicesAndValue) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListSet_(lst, ...indicesAndValue);
    }
    else {
        let result = [...lst];
        if (indicesAndValue.length > 2) {
            const [pos, ...indicesAndValue1] = indicesAndValue;
            result[pos] = listSet_(result[pos], ...indicesAndValue1);
        }
        else {
            const [pos, val] = indicesAndValue;
            result[pos] = val;
        }
        return result;
    }
}
exports.listSet_ = listSet_;
listSet_.fsource = [Symbol.for('define'), [Symbol.for('list-set_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-set_'), Symbol.for('lst'), Symbol.for('indices-and-value')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), [Symbol.for('apply'), Symbol.for('list-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('pos')], Symbol.for('indices-and-value-1')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), Symbol.for('val')]]], Symbol.for('result')]]];
/**
 * Set a list position to a given value.
 * Modifies the original list.
 */
function listSetX_(lst, ...indicesAndValue) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListSetX_(lst, ...indicesAndValue);
    }
    else {
        const indices = indicesAndValue.slice(0, -1);
        const firstIndices = indices.slice(0, -1);
        let lastIndex = indices.at(-1);
        const value = indicesAndValue.at(-1);
        let lst1 = lst;
        for (let i of firstIndices) {
            lst1 = lst1[i];
        }
        lst1[lastIndex] = value;
        return value;
    }
}
exports.listSetX_ = listSetX_;
listSetX_.fsource = [Symbol.for('define'), [Symbol.for('list-set!_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-set!_'), Symbol.for('lst'), Symbol.for('indices-and-value')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('first-indices'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('lst1'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('first-indices')]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('list-ref'), Symbol.for('lst1'), Symbol.for('i')]]], [Symbol.for('array-set!'), Symbol.for('lst1'), Symbol.for('last-index'), Symbol.for('value')], Symbol.for('value')]]];
/**
 * Return the `n`-th CDR element of a list.
 */
function listTail_(lst, n) {
    let result = lst;
    let i = n;
    while (i > 0) {
        result = ((result.length === 3) && (result[1] === Symbol.for('.'))) ? result[2] : result.slice(1);
        i--;
    }
    return result;
}
exports.listTail_ = listTail_;
listTail_.fsource = [Symbol.for('define'), [Symbol.for('list-tail_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cdr'), Symbol.for('result')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')];
/**
 * Return the `n`-th CDR element of a list.
 *
 * Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
 *
 * [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
 */
function nthcdr_(n, lst) {
    let result = lst;
    let i = n;
    while (i > 0) {
        result = ((result.length === 3) && (result[1] === Symbol.for('.'))) ? result[2] : result.slice(1);
        i--;
    }
    return result;
}
exports.dottedListNthcdr = nthcdr_;
exports.dottedListNthcdr_ = nthcdr_;
exports.nthcdr = nthcdr_;
exports.nthcdr_ = nthcdr_;
nthcdr_.fsource = [Symbol.for('define'), [Symbol.for('nthcdr_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cdr'), Symbol.for('result')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')];
/**
 * Take the `n` first elements from `lst`.
 *
 * Similar to [`take` in Racket][rkt:take].
 *
 * [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
 */
function take_(lst, n) {
    return lst.slice(0, -(lst.length - n) || undefined);
}
exports.take = take_;
exports.take_ = take_;
take_.fsource = [Symbol.for('define'), [Symbol.for('take_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('drop-right'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('lst')], Symbol.for('n')]]];
/**
 * Return the list obtained by dropping
 * the first `n` elements from `lst`.
 *
 * Similar to [`drop` in Racket][rkt:drop].
 *
 * [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
 */
function drop_(lst, n) {
    return lst.slice(n);
}
exports.drop = drop_;
exports.drop_ = drop_;
drop_.fsource = [Symbol.for('define'), [Symbol.for('drop_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop'), Symbol.for('lst'), Symbol.for('n')]];
/**
 * Return the list obtained by dropping
 * the last `n` elements from `lst`.
 *
 * Similar to [`drop-right` in Racket][rkt:drop-right].
 *
 * [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
 */
function dropRight_(lst, n) {
    return lst.slice(0, -n || undefined);
}
exports.dropRight = dropRight_;
exports.dropRight_ = dropRight_;
dropRight_.fsource = [Symbol.for('define'), [Symbol.for('drop-right_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('lst'), Symbol.for('n')]];
/**
 * Reverse the order of a list.
 * Returns a new list.
 *
 * Similar to [`reverse` in Racket][rkt:reverse].
 *
 * [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
 */
function reverse_(lst) {
    return [...lst].reverse();
}
exports.reverse = reverse_;
exports.reverse_ = reverse_;
reverse_.fsource = [Symbol.for('define'), [Symbol.for('reverse_'), Symbol.for('lst')], [Symbol.for('array-reverse'), Symbol.for('lst')]];
/**
 * Reverse the order of a list.
 */
function reversex_(lst) {
    return lst.reverse();
}
exports.reversex_ = reversex_;
reversex_.fsource = [Symbol.for('define'), [Symbol.for('reverse!_'), Symbol.for('lst')], [Symbol.for('array-reverse!'), Symbol.for('lst')]];
/**
 * Return a list where the last `n` conses have been omitted.
 *
 * Similar to [`butlast` in Common Lisp][cl:butlast].
 *
 * [cl:butlast]: http://clhs.lisp.se/Body/f_butlas.htm#butlast
 */
function butlast_(x, n = 1) {
    let result = [...x];
    let i = n;
    while ((i > 0) && (result.length > 0)) {
        result.pop();
        i--;
    }
    return result;
}
exports.butlast = butlast_;
exports.butlast_ = butlast_;
butlast_.fsource = [Symbol.for('define'), [Symbol.for('butlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('x')]]]], [Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('length'), Symbol.for('result')], 0]], [Symbol.for('pop-right!'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')]];
/**
 * Return a list where the last `n` conses have been omitted.
 * Changes the original list.
 *
 * Similar to [`nbutlast` in Common Lisp][cl:nbutlast].
 *
 * [cl:nbutlast]: http://clhs.lisp.se/Body/f_butlas.htm#nbutlast
 */
function nbutlast_(x, n = 1) {
    let i = n;
    while ((i > 0) && (x.length > 0)) {
        x.pop();
        i--;
    }
    return x;
}
exports.nbutlast = nbutlast_;
exports.nbutlast_ = nbutlast_;
nbutlast_.fsource = [Symbol.for('define'), [Symbol.for('nbutlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('length'), Symbol.for('x')], 0]], [Symbol.for('pop-right!'), Symbol.for('x')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('x')]];
/**
 * Pop an element off the beginning of a list.
 *
 * Similar to [`pop` in Common Lisp][cl:pop].
 *
 * [cl:pop]: http://clhs.lisp.se/Body/m_pop.htm#pop
 */
function popLeftX_(lst) {
    return lst.shift();
}
exports.popx = popLeftX_;
exports.popx_ = popLeftX_;
exports.popLeftX = popLeftX_;
exports.popLeftX_ = popLeftX_;
popLeftX_.fsource = [Symbol.for('define'), [Symbol.for('pop-left!_'), Symbol.for('lst')], [Symbol.for('array-pop-left!'), Symbol.for('lst')]];
/**
 * Pop an element off the end of a list.
 *
 * Similar to [`Array.prototype.pop()` in JavaScript][js:pop].
 *
 * [js:pop]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
 */
function popRightX_(lst) {
    return lst.pop();
}
exports.popRightX = popRightX_;
exports.popRightX_ = popRightX_;
popRightX_.fsource = [Symbol.for('define'), [Symbol.for('pop-right!_'), Symbol.for('lst')], [Symbol.for('array-pop-right!'), Symbol.for('lst')]];
/**
 * Push an element onto the beginning of a list.
 *
 * Similar to [`push` in Common Lisp][cl:push].
 *
 * [cl:push]: http://clhs.lisp.se/Body/m_push.htm#push
 */
function pushLeftX_(lst, x) {
    lst.unshift(x);
    return lst;
}
exports.pushx = pushLeftX_;
exports.pushLeftX = pushLeftX_;
exports.pushLeftX_ = pushLeftX_;
pushLeftX_.fsource = [Symbol.for('define'), [Symbol.for('push-left!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('array-push-left!'), Symbol.for('lst'), Symbol.for('x')]];
/**
 * Push an element onto the end of a list.
 *
 * Similar to [`Array.prototype.push()` in JavaScript][js:push].
 *
 * [js:push]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
 */
function pushRightX_(lst, x) {
    lst.push(x);
    return lst;
}
exports.appendToList = pushRightX_;
exports.pushRightX = pushRightX_;
exports.pushRightX_ = pushRightX_;
pushRightX_.fsource = [Symbol.for('define'), [Symbol.for('push-right!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('array-push-right!'), Symbol.for('lst'), Symbol.for('x')]];
/**
 * Return the length of a list.
 *
 * Similar to [`length` in Racket][rkt:length] and
 * [`length` in Common Lisp][cl:length].
 *
 * [rkt:length]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
 * [cl:length]: http://clhs.lisp.se/Body/f_length.htm#length
 */
function length_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListLength(lst);
    }
    else {
        return lst.length;
    }
}
exports.length = length_;
exports.length_ = length_;
length_.fsource = [Symbol.for('define'), [Symbol.for('length_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-length'), Symbol.for('lst')], [Symbol.for('array-length'), Symbol.for('lst')]]];
/**
 * Return the last element of a list.
 *
 * Similar to [`last` in Racket][rkt:last].
 *
 * [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
 */
function last_(lst) {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        return dottedListLast(lst);
    }
    else {
        return lst.at(-1);
    }
}
exports.last = last_;
exports.last_ = last_;
last_.fsource = [Symbol.for('define'), [Symbol.for('last_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-last'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]]];
/**
 * Return the last pair of a list.
 *
 * Similar to [`last-pair` in Racket][rkt:last-pair] and
 * [`last` in Common Lisp][cl:last].
 *
 * [rkt:last-pair]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last-pair%29%29
 * [cl:last]: http://clhs.lisp.se/Body/f_last.htm#last
 */
function lastPair_(lst) {
    if (!Array.isArray(lst)) {
        return undefined;
    }
    else if (Array.isArray(lst) && (lst.length === 0)) {
        return lst;
    }
    else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        let current = lst;
        let result = undefined;
        while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x) => {
            return Array.isArray(x) && (x.length === 0);
        })(current.at(-1))) {
            current = current.at(-1);
        }
        return result;
    }
    else {
        return lst.slice(lst.length - 1);
    }
}
exports.lastCons_ = lastPair_;
exports.lastPair = lastPair_;
exports.lastPair_ = lastPair_;
lastPair_.fsource = [Symbol.for('define'), [Symbol.for('last-pair_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('lst')]], undefined], [[Symbol.for('null?'), Symbol.for('lst')], Symbol.for('lst')], [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('array-drop'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 1]]]]];
/**
 * Return the last cdr of a list, i.e., the terminating empty list.
 */
function lastCdr_(lst) {
    if (!Array.isArray(lst)) {
        return undefined;
    }
    else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
        let result = lst;
        while (Array.isArray(result) && (result.length >= 3) && (result.at(-2) === Symbol.for('.'))) {
            result = result.at(-1);
        }
        return result;
    }
    else {
        return [];
    }
}
exports.dottedListLastCdr_ = lastCdr_;
exports.lastCdr = lastCdr_;
exports.lastCdr_ = lastCdr_;
lastCdr_.fsource = [Symbol.for('define'), [Symbol.for('last-cdr_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('lst')]], undefined], [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('dotted-list?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('dotted-list-tail'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('quote'), []]]]];
/**
 * Set the CAR of a list.
 *
 * Similar to [`set-car!` in Racket][rkt:set-car].
 *
 * [rkt:set-car]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_416
 */
function setCarX_(x, y) {
    if (x.length > 0) {
        x[0] = y;
    }
    return undefined;
}
exports.setCarX = setCarX_;
exports.setCarX_ = setCarX_;
setCarX_.fsource = [Symbol.for('define'), [Symbol.for('set-car!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('x')], 0], [Symbol.for('list-set!'), Symbol.for('x'), 0, Symbol.for('y')]], undefined];
/**
 * Set the CDR of a list.
 *
 * Similar to [`set-cdr!` in Racket][rkt:set-cdr].
 *
 * [rkt:set-cdr]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_418
 */
function setCdrX_(x, y) {
    if (Array.isArray(x) && (x.length === 0)) {
    }
    else if (x === y) {
        if (Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.'))) {
            x[x.length - 1] = y;
        }
        else {
            x.push(Symbol.for('.'));
            x.push(y);
        }
    }
    else {
        while (x.length > 1) {
            x.pop();
        }
        if (Array.isArray(y)) {
            for (let z of y) {
                x.push(z);
            }
        }
        else {
            x.push(Symbol.for('.'));
            x.push(y);
        }
    }
    return undefined;
}
exports.setCdrX = setCdrX_;
exports.setCdrX_ = setCdrX_;
setCdrX_.fsource = [Symbol.for('define'), [Symbol.for('set-cdr!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('x')]], [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('x')], [Symbol.for('array-set!'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 1], Symbol.for('y')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]], [Symbol.for('else'), [Symbol.for('while'), [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 1], [Symbol.for('pop-right!'), Symbol.for('x')]], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('y')], [Symbol.for('for'), [[Symbol.for('z'), Symbol.for('y')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('z')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]]], undefined];
/**
 * Whether something is a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function dottedListP_(x) {
    return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.'));
}
exports.dottedListP = dottedListP_;
exports.dottedListP_ = dottedListP_;
dottedListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]]]];
/**
 * Whether something is a dotted pair.
 */
function dottedPairP_(x) {
    return Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'));
}
exports.dottedPairP_ = dottedPairP_;
dottedPairP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-pair?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-ref'), Symbol.for('x'), 1], [Symbol.for('quote'), Symbol.for('.')]]]];
/**
 * Whether something is a proper dotted list.
 */
function dottedProperListP_(x) {
    return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && ((x) => {
        return Array.isArray(x) && (x.length === 0);
    })(lastCdr(x));
}
exports.dottedProperListP_ = dottedProperListP_;
dottedProperListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-proper-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];
/**
 * Whether something is an improper dotted list.
 */
function dottedImproperListP_(x) {
    return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && !((x) => {
        return Array.isArray(x) && (x.length === 0);
    })(lastCdr(x));
}
exports.dottedImproperListP_ = dottedImproperListP_;
dottedImproperListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-improper-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]]];
/**
 * Return the head of a dotted list.
 */
function dottedListHead_(lst) {
    return lst.slice(0, -2);
}
exports.dottedListHead_ = dottedListHead_;
dottedListHead_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-head_'), Symbol.for('lst')], [Symbol.for('array-drop-right'), Symbol.for('lst'), 2]];
/**
 * Return the tail of a dotted list.
 */
function dottedListTail_(lst) {
    return lst.at(-1);
}
exports.dottedListTail_ = dottedListTail_;
dottedListTail_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-tail_'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]];
/**
 * Create a dotted list link.
 */
function dottedListLink_(x) {
    if (Array.isArray(x)) {
        return x;
    }
    else {
        return [Symbol.for('.'), x];
    }
}
exports.dottedListLink_ = dottedListLink_;
dottedListLink_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-link_'), Symbol.for('x')], [Symbol.for('if'), [Symbol.for('pair-or-list?'), Symbol.for('x')], Symbol.for('x'), [Symbol.for('list'), [Symbol.for('quote'), Symbol.for('.')], Symbol.for('x')]]];
/**
 * Return the CDR of a dotted pair.
 */
function dottedPairCdr_(x) {
    return x[2];
}
exports.dottedPairCdr_ = dottedPairCdr_;
dottedPairCdr_.fsource = [Symbol.for('define'), [Symbol.for('dotted-pair-cdr_'), Symbol.for('x')], [Symbol.for('array-third'), Symbol.for('x')]];
/**
 * Parse a dotted list.
 */
function dottedListParse_(lst) {
    return [dottedListHead_(lst), dottedListTail_(lst)];
}
exports.dottedListParse_ = dottedListParse_;
dottedListParse_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-parse_'), Symbol.for('lst')], [Symbol.for('values'), [Symbol.for('dotted-list-head_'), Symbol.for('lst')], [Symbol.for('dotted-list-tail_'), Symbol.for('lst')]]];
/**
 * Return the length of a dotted list.
 */
function dottedListLength_(lst) {
    let len = 0;
    let current = lst;
    while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
        len = len + (lst.length - 2);
        current = current.at(-1);
    }
    return len;
}
exports.dottedListLength_ = dottedListLength_;
dottedListLength_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-length_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('len'), 0], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('len'), [Symbol.for('+'), Symbol.for('len'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]], Symbol.for('len')];
/**
 * Access the dotted list element indicated by
 * one or more `indices`.
 */
function dottedListRef_(lst, ...indices) {
    let result = lst;
    for (let i of indices) {
        while (i > 0) {
            if (i < (result.length - 2)) {
                break;
            }
            else {
                i = i - (result.length - 2);
                result = result.at(-1);
            }
        }
        if (Array.isArray(result)) {
            result = result[i];
        }
    }
    return result;
}
exports.dottedListRef_ = dottedListRef_;
dottedListRef_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('result')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('result')], 2]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('dotted-list-tail'), Symbol.for('result')]]]]], [Symbol.for('when'), [Symbol.for('pair-or-list?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('i')]]]], Symbol.for('result')];
/**
 * Set a dotted list position to a given value.
 * Returns a new list.
 */
function dottedListSet_(lst, ...indicesAndValue) {
    if (indicesAndValue.length > 2) {
        const [pos, ...indicesAndValue1] = indicesAndValue;
        if (pos < (lst.length - 2)) {
            let result = [...lst];
            result[pos] = dottedListSet_(result[pos], ...indicesAndValue1);
            return result;
        }
        else {
            return [...lst.slice(0, -1), dottedListSet_(lst.at(-1), [pos - (lst.length - 2), ...indicesAndValue1])];
        }
    }
    else {
        const [pos, val] = indicesAndValue;
        if (pos < (lst.length - 2)) {
            let result = [...lst];
            result[pos] = val;
            return result;
        }
        else {
            return [...lst.slice(0, -1), dottedListSet_(lst.at(-1), pos - (lst.length - 2), val)];
        }
    }
}
exports.dottedListSet_ = dottedListSet_;
dottedListSet_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-set_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), [Symbol.for('apply'), Symbol.for('dotted-list-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('pos')], Symbol.for('indices-and-value-1')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('append'), [Symbol.for('array-drop-right'), Symbol.for('lst'), 1], [Symbol.for('list'), [Symbol.for('dotted-list-set_'), [Symbol.for('array-last'), Symbol.for('lst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('-'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]]], [Symbol.for('unquote-splicing'), Symbol.for('indices-and-value-1')]]]]]]]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), Symbol.for('val')], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('append'), [Symbol.for('array-drop-right'), Symbol.for('lst'), 1], [Symbol.for('list'), [Symbol.for('dotted-list-set_'), [Symbol.for('array-last'), Symbol.for('lst')], [Symbol.for('-'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], Symbol.for('val')]]]]]]]];
/**
 * Set a dotted list position to a given value.
 * Modifies the original list.
 */
function dottedListSetX_(lst, ...indicesAndValue) {
    const indices = indicesAndValue.slice(0, -1);
    const indices1 = indices.slice(0, -1);
    let lastIndex = indices.at(-1);
    const value = indicesAndValue.at(-1);
    let lst1 = lst;
    for (let i of indices1) {
        while (i > 0) {
            if (i < (lst1.length - 2)) {
                break;
            }
            else {
                i = i - (lst1.length - 2);
                lst1 = lst1.at(-1);
            }
        }
        if (Array.isArray(lst1)) {
            lst1 = lst1[i];
        }
    }
    while (lastIndex > 0) {
        if (lastIndex < (lst1.length - 2)) {
            break;
        }
        else {
            lastIndex = lastIndex - (lst1.length - 2);
            lst1 = lst1.at(-1);
        }
    }
    lst1[lastIndex] = value;
    return value;
}
exports.dottedListSetX_ = dottedListSetX_;
dottedListSetX_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-set!_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('indices1'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('lst1'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices1')]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('dotted-list-tail'), Symbol.for('lst1')]]]]], [Symbol.for('when'), [Symbol.for('pair-or-list?'), Symbol.for('lst1')], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('list-ref'), Symbol.for('lst1'), Symbol.for('i')]]]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('last-index'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('last-index'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('last-index'), [Symbol.for('-'), Symbol.for('last-index'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('dotted-list-tail'), Symbol.for('lst1')]]]]], [Symbol.for('array-set!'), Symbol.for('lst1'), Symbol.for('last-index'), Symbol.for('value')], Symbol.for('value')];
/**
 * Return the first element of a dotted list.
 */
function dottedListFirst_(lst) {
    return lst[0];
}
exports.dottedListFirst_ = dottedListFirst_;
dottedListFirst_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];
/**
 * Return the second element of a dotted list.
 */
function dottedListSecond_(lst) {
    return dottedListRef_(lst, 1);
}
exports.dottedListSecond_ = dottedListSecond_;
dottedListSecond_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-second_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 1]];
/**
 * Return the third element of a dotted list.
 */
function dottedListThird_(lst) {
    return dottedListRef_(lst, 2);
}
exports.dottedListThird_ = dottedListThird_;
dottedListThird_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-third_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 2]];
/**
 * Return the fourth element of a dotted list.
 */
function dottedListFourth_(lst) {
    return dottedListRef_(lst, 3);
}
exports.dottedListFourth_ = dottedListFourth_;
dottedListFourth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-fourth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 3]];
/**
 * Return the fifth element of a dotted list.
 */
function dottedListFifth_(lst) {
    return dottedListRef_(lst, 4);
}
exports.dottedListFifth_ = dottedListFifth_;
dottedListFifth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-fifth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 4]];
/**
 * Return the sixth element of a dotted list.
 */
function dottedListSixth_(lst) {
    return dottedListRef_(lst, 5);
}
exports.dottedListSixth_ = dottedListSixth_;
dottedListSixth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-sixth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 5]];
/**
 * Return the seventh element of a dotted list.
 */
function dottedListSeventh_(lst) {
    return dottedListRef_(lst, 6);
}
exports.dottedListSeventh_ = dottedListSeventh_;
dottedListSeventh_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-seventh_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 6]];
/**
 * Return the eighth element of a dotted list.
 */
function dottedListEighth_(lst) {
    return dottedListRef_(lst, 7);
}
exports.dottedListEighth_ = dottedListEighth_;
dottedListEighth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-eighth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 7]];
/**
 * Return the ninth element of a dotted list.
 */
function dottedListNinth_(lst) {
    return dottedListRef_(lst, 8);
}
exports.dottedListNinth_ = dottedListNinth_;
dottedListNinth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-ninth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 8]];
/**
 * Return the tenth element of a dotted list.
 */
function dottedListTenth_(lst) {
    return dottedListRef_(lst, 9);
}
exports.dottedListTenth_ = dottedListTenth_;
dottedListTenth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-tenth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 9]];
/**
 * Return the last element of a dotted list.
 */
function dottedListLast_(lst) {
    let current = lst;
    let result = undefined;
    while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x) => {
        return Array.isArray(x) && (x.length === 0);
    })(((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1))) {
        current = ((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1);
    }
    if (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
        result = current[current.length - 3];
    }
    return result;
}
exports.dottedListLast_ = dottedListLast_;
dottedListLast_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-last_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('dotted-list-cdr'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-cdr'), Symbol.for('current')]]], [Symbol.for('when'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list-ref'), Symbol.for('current'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('current')], 3]]]], Symbol.for('result')];
/**
 * Make a dotted list.
 */
function makeDottedList_(car, cdr) {
    return listStar_(car, cdr);
}
exports.makeDottedList = makeDottedList_;
exports.makeDottedList_ = makeDottedList_;
makeDottedList_.fsource = [Symbol.for('define'), [Symbol.for('make-dotted-list_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('list-star_'), Symbol.for('car'), Symbol.for('cdr')]];
/**
 * Make a dotted pair.
 */
function makePair_(car, cdr) {
    return [car, Symbol.for('.'), cdr];
}
exports.makePair = makePair_;
exports.makePair_ = makePair_;
makePair_.fsource = [Symbol.for('define'), [Symbol.for('make-pair_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('car')], Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('cdr')]]]];
/**
 * Whether something is a proper list,
 * i.e., a list that is terminated by
 * the empty list.
 *
 * Similar to [`proper-list?` in Racket][rkt:proper-list-p].
 *
 * [rkt:proper-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#proper-list-p
 */
function properListP_(x) {
    const x1 = lastCdr(x);
    return Array.isArray(x1) && (x1.length === 0);
}
exports.properListP_ = properListP_;
properListP_.fsource = [Symbol.for('define'), [Symbol.for('proper-list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];
/**
 * Whether something is an improper list,
 * i.e., a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function improperListP_(x) {
    return !((x) => {
        return Array.isArray(x) && (x.length === 0);
    })(lastCdr(x));
}
exports.improperListP = improperListP_;
exports.improperListP_ = improperListP_;
improperListP_.fsource = [Symbol.for('define'), [Symbol.for('improper-list?_'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];
/**
 * Whether something is a circular list.
 *
 * Similar to [`circular-list?` in Racket][rkt:circular-list-p].
 *
 * [rkt:circular-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#circular-list-p
 */
function circularListP_(x) {
    return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && (x.at(-1) === x);
}
exports.circularListP = circularListP_;
exports.circularListP_ = circularListP_;
circularListP_.fsource = [Symbol.for('define'), [Symbol.for('circular-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('dotted-list-tail'), Symbol.for('x')], Symbol.for('x')]]];
/**
 * Convert an array list to a linked list.
 */
function listToDottedList_(x) {
    return [...x.slice(0, -1), Symbol.for('.'), x.at(-1)];
}
exports.listToDottedList_ = listToDottedList_;
listToDottedList_.fsource = [Symbol.for('define'), [Symbol.for('list->dotted-list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('array-drop-right'), Symbol.for('x'), 1]], Symbol.for('.'), [Symbol.for('unquote'), [Symbol.for('array-last'), Symbol.for('x')]]]]];
/**
 * Convert a linked list to an array list.
 */
function dottedListToList_(x) {
    return [...x.slice(0, -2), x.at(-1)];
}
exports.dottedListToList_ = dottedListToList_;
dottedListToList_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list->list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('dotted-list-head'), Symbol.for('x')]], [Symbol.for('unquote'), [Symbol.for('dotted-list-tail'), Symbol.for('x')]]]]];

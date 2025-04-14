"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Property lists
 *
 * Property lists, also known as plists.
 *
 * ## Description
 *
 * Various functions for working with property lists.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.plistp_ = exports.plistSet_ = exports.plistSetX_ = exports.plistHasP_ = exports.plistGet_ = exports.plistCopy_ = exports.plistToAlist_ = exports.plistHas_ = exports.plistRef_ = void 0;
const [cons] = (() => {
    function cons_(x, y) {
        if (Array.isArray(y)) {
            return [x, ...y];
        }
        else {
            return [x, Symbol.for('.'), y];
        }
    }
    return [cons_];
})();
/**
 * Whether something is a property list.
 *
 * Similar to [`plistp` in Emacs Lisp][el:plistp].
 *
 * [el:plistp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html#index-plistp
 */
function plistp_(obj) {
    // Since we permit properties to be any kind of value, it suffices
    // to verify that the input is an array of even length.
    return Array.isArray(obj) && ((obj.length % 2) === 0);
}
exports.plistp_ = plistp_;
plistp_.lispSource = [Symbol.for('define'), [Symbol.for('plist?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('obj')], [Symbol.for('even?'), [Symbol.for('array-length'), Symbol.for('obj')]]]];
/**
 * Copy a property list.
 */
function plistCopy_(plist) {
    return [...plist];
}
exports.plistCopy_ = plistCopy_;
plistCopy_.lispSource = [Symbol.for('define'), [Symbol.for('plist-copy_'), Symbol.for('plist')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('plist')]]]];
/**
 * Return the value of a property in a property list.
 * Returns `undefined` if not found.
 *
 * Similar to [`plist-get` in Emacs Lisp][el:plist-get].
 *
 * [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
 */
function plistGet_(plist, prop) {
    let val = undefined;
    const _end = plist.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plist[i] === prop) {
            val = plist[i + 1];
            break;
        }
    }
    return val;
}
exports.plistRef_ = plistGet_;
exports.plistGet_ = plistGet_;
plistGet_.lispSource = [Symbol.for('define'), [Symbol.for('plist-get_'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('val'), Symbol.for('undefined')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('plist'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('break')]]], Symbol.for('val')];
/**
 * Whether a property list contains a given property.
 */
function plistHasP_(plist, prop) {
    let found = false;
    const _end = plist.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plist[i] === prop) {
            found = true;
            break;
        }
    }
    return found;
}
exports.plistHas_ = plistHasP_;
exports.plistHasP_ = plistHasP_;
plistHasP_.lispSource = [Symbol.for('define'), [Symbol.for('plist-has?_'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('found'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('found'), Symbol.for('#t')], [Symbol.for('break')]]], Symbol.for('found')];
/**
 * Set the value of a property in a property list.
 *
 * Similar to [`plist-put` in Emacs Lisp][el:plist-put].
 *
 * [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
 */
function plistSetX_(plist, prop, val) {
    let found = false;
    const _end = plist.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plist[i] === prop) {
            plist[i + 1] = val;
            found = true;
            break;
        }
        if (!found) {
            plist.push(prop);
            plist.push(val);
        }
    }
    return undefined;
}
exports.plistSetX_ = plistSetX_;
plistSetX_.lispSource = [Symbol.for('define'), [Symbol.for('plist-set!_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('found'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('aset!'), Symbol.for('plist'), [Symbol.for('+'), Symbol.for('i'), 1], Symbol.for('val')], [Symbol.for('set!'), Symbol.for('found'), Symbol.for('#t')], [Symbol.for('break')]], [Symbol.for('unless'), Symbol.for('found'), [Symbol.for('push-right!'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('push-right!'), Symbol.for('plist'), Symbol.for('val')]]], Symbol.for('undefined')];
/**
 * Set the value of a property in a property list,
 * returning a new property list.
 */
function plistSet_(plist, prop, val) {
    const result = [...plist];
    plistSetX_(plist, prop, val);
    return result;
}
exports.plistSet_ = plistSet_;
plistSet_.lispSource = [Symbol.for('define'), [Symbol.for('plist-set_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('plist-copy'), Symbol.for('plist')]]], [Symbol.for('plist-set!_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], Symbol.for('result')]];
/**
 * Convert a plist to an association list.
 */
function plistToAlist_(plst) {
    const alst = [];
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        alst.push(cons(plst[i], plst[i + 1]));
    }
    return alst;
}
exports.plistToAlist_ = plistToAlist_;
plistToAlist_.lispSource = [Symbol.for('define'), [Symbol.for('plist->alist_'), Symbol.for('plst')], [Symbol.for('define'), Symbol.for('alst'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('plst')], 2]]], [Symbol.for('push-right!'), Symbol.for('alst'), [Symbol.for('cons'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], Symbol.for('alst')];

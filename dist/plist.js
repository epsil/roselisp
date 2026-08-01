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
exports.plistp_ = exports.plistSet_ = exports.plistSetX_ = exports.plistHasP_ = exports.plistGet_ = exports.plistCopy_ = exports.plistToObject_ = exports.plistMap_ = exports.plistToAlist_ = exports.plistp = exports.plistSet = exports.plistSetX = exports.plistHas_ = exports.plistHasP = exports.plistRef_ = exports.plistGet = exports.plistCopy = exports.plistToObject = exports.plistMap = exports.plistToAlist = void 0;
const util_1 = require("./util");
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
exports.plistp = plistp_;
exports.plistp_ = plistp_;
plistp_.fsource = [Symbol.for('define'), [Symbol.for('plist?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('obj')], [Symbol.for('even?'), [Symbol.for('array-length'), Symbol.for('obj')]]]];
/**
 * Copy a property list.
 */
function plistCopy_(plst) {
    return [...plst];
}
exports.plistCopy = plistCopy_;
exports.plistCopy_ = plistCopy_;
plistCopy_.fsource = [Symbol.for('define'), [Symbol.for('plist-copy_'), Symbol.for('plst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('plst')]]]];
/**
 * Return the value of a property in a property list.
 * Returns `#u` if not found.
 *
 * Similar to [`plist-get` in Emacs Lisp][el:plist-get].
 *
 * [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
 */
function plistGet_(plst, prop) {
    let val = undefined;
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plst[i] === prop) {
            val = plst[i + 1];
            break;
        }
    }
    return val;
}
exports.plistGet = plistGet_;
exports.plistRef_ = plistGet_;
exports.plistGet_ = plistGet_;
plistGet_.fsource = [Symbol.for('define'), [Symbol.for('plist-get_'), Symbol.for('plst'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('val'), undefined], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plst')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('break')]]], Symbol.for('val')];
/**
 * Whether a property list contains a given property.
 */
function plistHasP_(plst, prop) {
    let found = false;
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plst[i] === prop) {
            found = true;
            break;
        }
    }
    return found;
}
exports.plistHasP = plistHasP_;
exports.plistHas_ = plistHasP_;
exports.plistHasP_ = plistHasP_;
plistHasP_.fsource = [Symbol.for('define'), [Symbol.for('plist-has?_'), Symbol.for('plst'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('found'), false], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plst')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('found'), true], [Symbol.for('break')]]], Symbol.for('found')];
/**
 * Set the value of a property in a property list.
 *
 * Similar to [`plist-put` in Emacs Lisp][el:plist-put].
 *
 * [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
 */
function plistSetX_(plst, prop, val) {
    let found = false;
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        if (plst[i] === prop) {
            plst[i + 1] = val;
            found = true;
            break;
        }
        if (!found) {
            plst.push(prop);
            plst.push(val);
        }
    }
    return undefined;
}
exports.plistSetX = plistSetX_;
exports.plistSetX_ = plistSetX_;
plistSetX_.fsource = [Symbol.for('define'), [Symbol.for('plist-set!_'), Symbol.for('plst'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('found'), false], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plst')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('aset!'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1], Symbol.for('val')], [Symbol.for('set!'), Symbol.for('found'), true], [Symbol.for('break')]], [Symbol.for('unless'), Symbol.for('found'), [Symbol.for('push-right!'), Symbol.for('plst'), Symbol.for('prop')], [Symbol.for('push-right!'), Symbol.for('plst'), Symbol.for('val')]]], undefined];
/**
 * Set the value of a property in a property list,
 * returning a new property list.
 */
function plistSet_(plst, prop, val) {
    const result = [...plst];
    plistSetX_(plst, prop, val);
    return result;
}
exports.plistSet = plistSet_;
exports.plistSet_ = plistSet_;
plistSet_.fsource = [Symbol.for('define'), [Symbol.for('plist-set_'), Symbol.for('plst'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('plist-copy'), Symbol.for('plst')]]], [Symbol.for('plist-set!_'), Symbol.for('plst'), Symbol.for('prop'), Symbol.for('val')], Symbol.for('result')]];
/**
 * Iterate over a property list.
 */
function plistIterate_(f, plst) {
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        const prop = plst[i];
        let val = plst[i + 1];
        const entry = [prop, val];
        f(entry);
    }
}
plistIterate_.fsource = [Symbol.for('define'), [Symbol.for('plist-iterate_'), Symbol.for('f'), Symbol.for('plst')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('plst')], 2]]], [Symbol.for('define'), Symbol.for('prop'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('list'), Symbol.for('prop'), Symbol.for('val')]], [Symbol.for('f'), Symbol.for('entry')]]];
/**
 * Map a function over a property list.
 */
function plistMap_(f, plst) {
    const result = [];
    plistIterate_(function (entry) {
        let [prop, val] = f(entry);
        result.push(prop);
        result.push(val);
        return result;
    }, plst);
    return result;
}
exports.plistMap = plistMap_;
exports.plistMap_ = plistMap_;
plistMap_.fsource = [Symbol.for('define'), [Symbol.for('plist-map_'), Symbol.for('f'), Symbol.for('plst')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('plist-iterate_'), [Symbol.for('lambda'), [Symbol.for('entry')], [Symbol.for('define-values'), [Symbol.for('prop'), Symbol.for('val')], [Symbol.for('f'), Symbol.for('entry')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('prop')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('val')]], Symbol.for('plst')], Symbol.for('result')];
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
exports.plistToAlist = plistToAlist_;
exports.plistToAlist_ = plistToAlist_;
plistToAlist_.fsource = [Symbol.for('define'), [Symbol.for('plist->alist_'), Symbol.for('plst')], [Symbol.for('define'), Symbol.for('alst'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('plst')], 2]]], [Symbol.for('push-right!'), Symbol.for('alst'), [Symbol.for('cons'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], Symbol.for('alst')];
/**
 * Convert a property list to a JavaScript object.
 */
function plistToObject_(plst, options = {}) {
    const result = {};
    const _end = plst.length;
    for (let i = 0; i < _end; i = i + 2) {
        const prop = plst[i];
        let val = plst[i + 1];
        const key = (0, util_1.makeIdentifierString)(prop.description.replace(new RegExp('^:'), ''), options);
        result[key] = val;
    }
    return result;
}
exports.plistToObject = plistToObject_;
exports.plistToObject_ = plistToObject_;
plistToObject_.fsource = [Symbol.for('define'), [Symbol.for('plist->object_'), Symbol.for('plst'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('js/obj')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('plst')], 2]]], [Symbol.for('define'), Symbol.for('prop'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('~>'), Symbol.for('prop'), [Symbol.for('symbol->string'), Symbol.for('_')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^:'], Symbol.for('_'), ''], [Symbol.for('make-identifier-string'), Symbol.for('_'), Symbol.for('options')]]], [Symbol.for('oset!'), Symbol.for('result'), Symbol.for('key'), Symbol.for('val')]], Symbol.for('result')];

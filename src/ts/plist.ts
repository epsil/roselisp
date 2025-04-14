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

const [cons]: any[] = ((): any => {
  function cons_(x: any, y: any): any {
    if (Array.isArray(y)) {
      return [x, ...y];
    } else {
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
function plistp_(obj: any): any {
  // Since we permit properties to be any kind of value, it suffices
  // to verify that the input is an array of even length.
  return Array.isArray(obj) && ((obj.length % 2) === 0);
}

plistp_.lispSource = [Symbol.for('define'), [Symbol.for('plist?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('obj')], [Symbol.for('even?'), [Symbol.for('array-length'), Symbol.for('obj')]]]];

/**
 * Copy a property list.
 */
function plistCopy_(plist: any): any {
  return [...plist];
}

plistCopy_.lispSource = [Symbol.for('define'), [Symbol.for('plist-copy_'), Symbol.for('plist')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('plist')]]]];

/**
 * Return the value of a property in a property list.
 * Returns `undefined` if not found.
 *
 * Similar to [`plist-get` in Emacs Lisp][el:plist-get].
 *
 * [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
 */
function plistGet_(plist: any, prop: any): any {
  let val: any = undefined;
  const _end: any = plist.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    if ((plist as any)[i] === prop) {
      val = plist[i + 1];
      break;
    }
  }
  return val;
}

plistGet_.lispSource = [Symbol.for('define'), [Symbol.for('plist-get_'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('val'), Symbol.for('undefined')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('plist'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('break')]]], Symbol.for('val')];

/**
 * Whether a property list contains a given property.
 */
function plistHasP_(plist: any, prop: any): any {
  let found: any = false;
  const _end: any = plist.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    if ((plist as any)[i] === prop) {
      found = true;
      break;
    }
  }
  return found;
}

plistHasP_.lispSource = [Symbol.for('define'), [Symbol.for('plist-has?_'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('found'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('found'), Symbol.for('#t')], [Symbol.for('break')]]], Symbol.for('found')];

/**
 * Set the value of a property in a property list.
 *
 * Similar to [`plist-put` in Emacs Lisp][el:plist-put].
 *
 * [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
 */
function plistSetX_(plist: any, prop: any, val: any): any {
  let found: any = false;
  const _end: any = plist.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    if ((plist as any)[i] === prop) {
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

plistSetX_.lispSource = [Symbol.for('define'), [Symbol.for('plist-set!_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('found'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-length'), Symbol.for('plist')], 2]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('plist'), Symbol.for('i')], Symbol.for('prop')], [Symbol.for('aset!'), Symbol.for('plist'), [Symbol.for('+'), Symbol.for('i'), 1], Symbol.for('val')], [Symbol.for('set!'), Symbol.for('found'), Symbol.for('#t')], [Symbol.for('break')]], [Symbol.for('unless'), Symbol.for('found'), [Symbol.for('push-right!'), Symbol.for('plist'), Symbol.for('prop')], [Symbol.for('push-right!'), Symbol.for('plist'), Symbol.for('val')]]], Symbol.for('undefined')];

/**
 * Set the value of a property in a property list,
 * returning a new property list.
 */
function plistSet_(plist: any, prop: any, val: any): any {
  const result: any = [...plist];
  plistSetX_(plist, prop, val);
  return result;
}

plistSet_.lispSource = [Symbol.for('define'), [Symbol.for('plist-set_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('plist-copy'), Symbol.for('plist')]]], [Symbol.for('plist-set!_'), Symbol.for('plist'), Symbol.for('prop'), Symbol.for('val')], Symbol.for('result')]];

/**
 * Convert a plist to an association list.
 */
function plistToAlist_(plst: any): any {
  const alst: any = [];
  const _end: any = plst.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    alst.push(cons((plst as any)[i], plst[i + 1]));
  }
  return alst;
}

plistToAlist_.lispSource = [Symbol.for('define'), [Symbol.for('plist->alist_'), Symbol.for('plst')], [Symbol.for('define'), Symbol.for('alst'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('plst')], 2]]], [Symbol.for('push-right!'), Symbol.for('alst'), [Symbol.for('cons'), [Symbol.for('aget'), Symbol.for('plst'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('plst'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], Symbol.for('alst')];

export {
  plistGet_ as plistRef_,
  plistHasP_ as plistHas_,
  plistToAlist_,
  plistCopy_,
  plistGet_,
  plistHasP_,
  plistSetX_,
  plistSet_,
  plistp_
};
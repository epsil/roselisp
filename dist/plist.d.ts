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
/**
 * Whether something is a property list.
 *
 * Similar to [`plistp` in Emacs Lisp][el:plistp].
 *
 * [el:plistp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html#index-plistp
 */
declare function plistp_(obj: any): any;
declare namespace plistp_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Copy a property list.
 */
declare function plistCopy_(plist: any): any;
declare namespace plistCopy_ {
    var lispSource: (symbol | (symbol | symbol[][])[])[];
}
/**
 * Return the value of a property in a property list.
 * Returns `undefined` if not found.
 *
 * Similar to [`plist-get` in Emacs Lisp][el:plist-get].
 *
 * [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
 */
declare function plistGet_(plist: any, prop: any): any;
declare namespace plistGet_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Whether a property list contains a given property.
 */
declare function plistHasP_(plist: any, prop: any): any;
declare namespace plistHasP_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Set the value of a property in a property list.
 *
 * Similar to [`plist-put` in Emacs Lisp][el:plist-put].
 *
 * [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
 */
declare function plistSetX_(plist: any, prop: any, val: any): any;
declare namespace plistSetX_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Set the value of a property in a property list,
 * returning a new property list.
 */
declare function plistSet_(plist: any, prop: any, val: any): any;
declare namespace plistSet_ {
    var lispSource: (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[];
}
/**
 * Convert a plist to an association list.
 */
declare function plistToAlist_(plst: any): any;
declare namespace plistToAlist_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
export { plistGet_ as plistRef_, plistHasP_ as plistHas_, plistToAlist_, plistCopy_, plistGet_, plistHasP_, plistSetX_, plistSet_, plistp_ };

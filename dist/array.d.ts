/**
 * # Arrays
 *
 * Array functions.
 *
 * ## Description
 *
 * Functions for working with arrays.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Whether something is an array.
 */
declare function arrayp_(obj: any): any;
declare namespace arrayp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the last element of an array.
 */
declare function arrayLast_(arr: any): any;
declare namespace arrayLast_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Return the length of an array.
 */
declare function arrayLength_(arr: any): any;
declare namespace arrayLength_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the first element of an array.
 */
declare function arrayFirst_(lst: any): any;
declare namespace arrayFirst_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the second element of an array.
 */
declare function arraySecond_(lst: any): any;
declare namespace arraySecond_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the third element of an array.
 */
declare function arrayThird_(lst: any): any;
declare namespace arrayThird_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fourth element of an array.
 */
declare function arrayFourth_(lst: any): any;
declare namespace arrayFourth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fifth element of an array.
 */
declare function arrayFifth_(lst: any): any;
declare namespace arrayFifth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the sixth element of an array.
 */
declare function arraySixth_(lst: any): any;
declare namespace arraySixth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the seventh element of an array.
 */
declare function arraySeventh_(lst: any): any;
declare namespace arraySeventh_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the eight element of an array.
 */
declare function arrayEighth_(lst: any): any;
declare namespace arrayEighth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the ninth element of an array.
 */
declare function arrayNinth_(lst: any): any;
declare namespace arrayNinth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tenth element of an array.
 */
declare function arrayTenth_(lst: any): any;
declare namespace arrayTenth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Access the array element indicated by
 * one or more `indices`.
 *
 * Similar to [`array-ref` in Racket][rkt:array-ref],
 * [`aref` in Common Lisp][cl:aref] and
 * [`aget` in ClojureScript][cljs:aget].
 *
 * [rkt:array-ref]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-ref%29%29
 * [cl:aref]: http://clhs.lisp.se/Body/f_aref.htm#aref
 * [cljs:aget]: https://cljs.github.io/api/cljs.core/#aget
 */
declare function arrayRef_(arr: any, ...indices: any[]): any;
declare namespace arrayRef_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Set the array position indiciated by one or more indices
 * to a given value.
 *
 * Similar to [`array-set!` in Racket][rkt:array-set] and
 * [`aset` in ClojureScript][cljs:aset].
 *
 * [rkt:array-set]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-set%21%29%29
 * [cljs:aset]: https://cljs.github.io/api/cljs.core/#aset
 */
declare function arraySet_(arr: any, ...indicesAndValue: any[]): any;
declare namespace arraySet_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (number | symbol)[])[])[];
}
/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
declare function arrayDrop_(arr: any, n: any): any;
declare namespace arrayDrop_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[])[])[];
}
/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
declare function arrayDropRight_(arr: any, n: any): any;
declare namespace arrayDropRight_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[];
}
/**
 * Return the tail of an array.
 */
declare function arrayRest_(arr: any): any;
declare namespace arrayRest_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Reverse the order of an array.
 * Returns a new array list.
 */
declare function arrayReverse_(arr: any): any;
declare namespace arrayReverse_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Take the `n` first elements from `arr`.
 */
declare function arrayTake_(arr: any, n: any): any;
declare namespace arrayTake_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
export { arrayRef_ as aget, arrayRef_ as aget_, arrayRef_ as aref, arraySet_ as arraySet, arraySet_ as aset, arraySet_ as aset_, arrayDropRight_, arrayDrop_, arrayEighth_, arrayFifth_, arrayFirst_, arrayFourth_, arrayLast_, arrayLength_, arrayNinth_, arrayRef_, arrayRest_, arrayReverse_, arraySecond_, arraySet_, arraySeventh_, arraySixth_, arrayTenth_, arrayTake_, arrayThird_, arrayp_ };

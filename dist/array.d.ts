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
declare function arrayp_(x: any): any;
declare namespace arrayp_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the length of an array.
 */
declare function arrayLength_(arr: any): any;
declare namespace arrayLength_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Copy an array.
 */
declare function arrayCopy_(arr: any): any;
declare namespace arrayCopy_ {
    var fsource: (symbol | (symbol | symbol[][])[])[];
}
/**
 * Return the first element of an array.
 */
declare function arrayFirst_(arr: any): any;
declare namespace arrayFirst_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the second element of an array.
 */
declare function arraySecond_(arr: any): any;
declare namespace arraySecond_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the third element of an array.
 */
declare function arrayThird_(arr: any): any;
declare namespace arrayThird_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fourth element of an array.
 */
declare function arrayFourth_(arr: any): any;
declare namespace arrayFourth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fifth element of an array.
 */
declare function arrayFifth_(arr: any): any;
declare namespace arrayFifth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the sixth element of an array.
 */
declare function arraySixth_(arr: any): any;
declare namespace arraySixth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the seventh element of an array.
 */
declare function arraySeventh_(arr: any): any;
declare namespace arraySeventh_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the eight element of an array.
 */
declare function arrayEighth_(arr: any): any;
declare namespace arrayEighth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the ninth element of an array.
 */
declare function arrayNinth_(arr: any): any;
declare namespace arrayNinth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tenth element of an array.
 */
declare function arrayTenth_(arr: any): any;
declare namespace arrayTenth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the last element of an array.
 */
declare function arrayLast_(arr: any): any;
declare namespace arrayLast_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the `n`-th element counting from
 * the end of the array.
 */
declare function arrayNlast_(arr: any, n: any): any;
declare namespace arrayNlast_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
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
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Return the `i`-th element of the array.
 * Accepts negative values, counting back
 * from the end of the array.
 */
declare function arrayAt_(arr: any, i: any): any;
declare namespace arrayAt_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Set an array position to a given value.
 * Returns a new array.
 */
declare function arraySet_(arr: any, ...indicesAndValue: any[]): any;
declare namespace arraySet_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | ((symbol | (symbol | symbol[])[])[] | (number | symbol | symbol[])[])[])[])[];
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
declare function arraySetX_(arr: any, ...indicesAndValue: any[]): any;
declare namespace arraySetX_ {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Take the `n` first elements from `arr`.
 */
declare function arrayTake_(arr: any, n: any): any;
declare namespace arrayTake_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Return the tail of an array.
 */
declare function arrayRest_(arr: any): any;
declare namespace arrayRest_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Slice a JavaScript array.
 */
declare function arraySlice_(arr: any, ...args: any[]): any;
declare namespace arraySlice_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
declare function arrayDrop_(arr: any, n: any): any;
declare namespace arrayDrop_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
declare function arrayDropRight_(arr: any, n: any): any;
declare namespace arrayDropRight_ {
    var fsource: (symbol | (number | symbol | (symbol | symbol[] | undefined)[])[])[];
}
/**
 * Concatenate arrays.
 */
declare function arrayConcat_(...args: any[]): any;
declare namespace arrayConcat_ {
    var fsource: (symbol | (symbol | (symbol | never[])[])[])[];
}
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
declare function arrayReverse_(arr: any): any;
declare namespace arrayReverse_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
declare function arrayReverseX_(arr: any): any;
declare namespace arrayReverseX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Pop an element off the beginning of an array.
 */
declare function arrayPopLeftX_(arr: any): any;
declare namespace arrayPopLeftX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Pop an element off the end of an array.
 */
declare function arrayPopRightX_(arr: any): any;
declare namespace arrayPopRightX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Push an element onto the beginning of an array.
 */
declare function arrayPushLeftX_(arr: any, x: any): any;
declare namespace arrayPushLeftX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Push an element onto the end of an array.
 */
declare function arrayPushRightX_(arr: any, x: any): any;
declare namespace arrayPushRightX_ {
    var fsource: (symbol | symbol[])[];
}
export { arrayRef_ as aget, arrayRef_ as aget_, arrayRef_ as aref, arraySet_ as arraySet, arraySet_ as aset, arraySet_ as aset_, arrayAt_, arrayConcat_, arrayCopy_, arrayDropRight_, arrayDrop_, arrayEighth_, arrayFifth_, arrayFirst_, arrayFourth_, arrayLast_, arrayLength_, arrayNinth_, arrayPopLeftX_, arrayPopRightX_, arrayPushLeftX_, arrayPushRightX_, arrayRef_, arrayRest_, arrayReverseX_, arrayReverse_, arraySecond_, arraySetX_, arraySet_, arraySeventh_, arraySixth_, arraySlice_, arrayTake_, arrayTenth_, arrayNlast_, arrayThird_, arrayp_ };

"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.arrayp_ = exports.arrayThird_ = exports.arrayTenth_ = exports.arrayTake_ = exports.arraySort_ = exports.arraySlice_ = exports.arraySixth_ = exports.arraySeventh_ = exports.arraySet_ = exports.arraySetX_ = exports.arraySecond_ = exports.arrayReverse_ = exports.arrayReverseX_ = exports.arrayRest_ = exports.arrayRef_ = exports.arrayPushRightX_ = exports.arrayPushLeftX_ = exports.arrayPopRightX_ = exports.arrayPopLeftX_ = exports.arrayNlast_ = exports.arrayNinth_ = exports.arrayLength_ = exports.arrayLast_ = exports.arrayFourth_ = exports.arrayFirst_ = exports.arrayFifth_ = exports.arrayEighth_ = exports.arrayDrop_ = exports.arrayDropRight_ = exports.arrayCopy_ = exports.arrayConcat_ = exports.arrayAt_ = exports.aset_ = exports.aset = exports.arraySet = exports.aref = exports.aget_ = exports.aget = void 0;
/**
 * Whether something is an array.
 */
function arrayp_(x) {
    return Array.isArray(x);
}
exports.arrayp_ = arrayp_;
arrayp_.fsource = [Symbol.for('define'), [Symbol.for('array?_'), Symbol.for('x')], [Symbol.for('js/array?'), Symbol.for('x')]];
arrayp_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [x] = exp.slice(1);
        return [Symbol.for('js/array?'), x];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the length of an array.
 */
function arrayLength_(arr) {
    return arr.length;
}
exports.arrayLength_ = arrayLength_;
arrayLength_.fsource = [Symbol.for('define'), [Symbol.for('array-length_'), Symbol.for('arr')], [Symbol.for('js/length'), Symbol.for('arr')]];
arrayLength_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('js/length'), arr];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Copy an array.
 */
function arrayCopy_(arr) {
    return [...arr];
}
exports.arrayCopy_ = arrayCopy_;
arrayCopy_.fsource = [Symbol.for('define'), [Symbol.for('array-copy_'), Symbol.for('arr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('arr')]]]];
arrayCopy_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), arr]]];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the first element of an array.
 */
function arrayFirst_(arr) {
    return arr[0];
}
exports.arrayFirst_ = arrayFirst_;
arrayFirst_.fsource = [Symbol.for('define'), [Symbol.for('array-first_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 0]];
arrayFirst_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 0];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the second element of an array.
 */
function arraySecond_(arr) {
    return arr[1];
}
exports.arraySecond_ = arraySecond_;
arraySecond_.fsource = [Symbol.for('define'), [Symbol.for('array-second_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 1]];
arraySecond_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 1];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the third element of an array.
 */
function arrayThird_(arr) {
    return arr[2];
}
exports.arrayThird_ = arrayThird_;
arrayThird_.fsource = [Symbol.for('define'), [Symbol.for('array-third_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 2]];
arrayThird_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 2];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the fourth element of an array.
 */
function arrayFourth_(arr) {
    return arr[3];
}
exports.arrayFourth_ = arrayFourth_;
arrayFourth_.fsource = [Symbol.for('define'), [Symbol.for('array-fourth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 3]];
arrayFourth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 3];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the fifth element of an array.
 */
function arrayFifth_(arr) {
    return arr[4];
}
exports.arrayFifth_ = arrayFifth_;
arrayFifth_.fsource = [Symbol.for('define'), [Symbol.for('array-fifth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 4]];
arrayFifth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 4];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the sixth element of an array.
 */
function arraySixth_(arr) {
    return arr[5];
}
exports.arraySixth_ = arraySixth_;
arraySixth_.fsource = [Symbol.for('define'), [Symbol.for('array-sixth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 5]];
arraySixth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 5];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the seventh element of an array.
 */
function arraySeventh_(arr) {
    return arr[6];
}
exports.arraySeventh_ = arraySeventh_;
arraySeventh_.fsource = [Symbol.for('define'), [Symbol.for('array-seventh_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 6]];
arraySeventh_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 6];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the eight element of an array.
 */
function arrayEighth_(arr) {
    return arr[7];
}
exports.arrayEighth_ = arrayEighth_;
arrayEighth_.fsource = [Symbol.for('define'), [Symbol.for('array-eighth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 7]];
arrayEighth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 7];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the ninth element of an array.
 */
function arrayNinth_(arr) {
    return arr[8];
}
exports.arrayNinth_ = arrayNinth_;
arrayNinth_.fsource = [Symbol.for('define'), [Symbol.for('array-ninth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 8]];
arrayNinth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 8];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the tenth element of an array.
 */
function arrayTenth_(arr) {
    return arr[9];
}
exports.arrayTenth_ = arrayTenth_;
arrayTenth_.fsource = [Symbol.for('define'), [Symbol.for('array-tenth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 9]];
arrayTenth_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-ref'), arr, 9];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the last element of an array.
 */
function arrayLast_(arr) {
    return arr[arr.length - 1];
}
exports.arrayLast_ = arrayLast_;
arrayLast_.fsource = [Symbol.for('define'), [Symbol.for('array-last_'), Symbol.for('arr')], [Symbol.for('array-nlast'), Symbol.for('arr'), 1]];
arrayLast_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-nlast'), arr, 1];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the `n`-th element counting from
 * the end of the array.
 */
function arrayNlast_(arr, n) {
    // We could have called `array-at` with a negative index,
    // but this has better backwards compatibility.
    return arr[arr.length - n];
}
exports.arrayNlast_ = arrayNlast_;
arrayNlast_.fsource = [Symbol.for('define'), [Symbol.for('array-nlast_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-ref'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];
arrayNlast_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, n] = exp.slice(1);
        if (!(Array.isArray(arr) && (arr.length > 0))) {
            return [Symbol.for('array-ref'), arr, [Symbol.for('-'), [Symbol.for('array-length'), arr], n]];
        }
        else {
            const arr1 = Symbol('arr');
            return [Symbol.for('let'), [[arr1, arr]], ((arr) => {
                    return [Symbol.for('array-ref'), arr, [Symbol.for('-'), [Symbol.for('array-length'), arr], n]];
                })(arr1)];
        }
    };
    f.ftype = 'macro';
    return f;
})();
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
function arrayRef_(arr, ...indices) {
    let result = arr;
    for (let i of indices) {
        result = arr[i];
    }
    return result;
}
exports.aget = arrayRef_;
exports.aget_ = arrayRef_;
exports.aref = arrayRef_;
exports.arrayRef_ = arrayRef_;
arrayRef_.fsource = [Symbol.for('define'), [Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/get'), Symbol.for('arr'), Symbol.for('i')]]], Symbol.for('result')];
/**
 * Compiler macro for `(array-ref ...)` expressions.
 */
arrayRef_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, ...indices] = exp.slice(1);
        return [Symbol.for('js/get'), arr, ...indices];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the `i`-th element of the array.
 * Accepts negative values, counting back
 * from the end of the array.
 */
function arrayAt_(arr, i) {
    return arr.at(i);
}
exports.arrayAt_ = arrayAt_;
arrayAt_.fsource = [Symbol.for('define'), [Symbol.for('array-at_'), Symbol.for('arr'), Symbol.for('i')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('at'), Symbol.for('i')]];
arrayAt_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, i] = exp.slice(1);
        return [Symbol.for('send'), arr, Symbol.for('at'), i];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Set an array position to a given value.
 * Returns a new array.
 */
function arraySet_(arr, ...indicesAndValue) {
    let result = [...arr];
    if (indicesAndValue.length > 2) {
        const [i, ...indicesAndValue1] = indicesAndValue;
        result[i] = arraySet_(result[i], ...indicesAndValue1);
    }
    else {
        const [i, val] = indicesAndValue;
        result[i] = val;
    }
    return result;
}
exports.arraySet = arraySet_;
exports.aset = arraySet_;
exports.aset_ = arraySet_;
exports.arraySet_ = arraySet_;
arraySet_.fsource = [Symbol.for('define'), [Symbol.for('array-set_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('array-copy'), Symbol.for('arr')]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('i'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('i'), [Symbol.for('apply'), Symbol.for('array-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('i')], Symbol.for('indices-and-value-1')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('i'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('i'), Symbol.for('val')]]], Symbol.for('result')];
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
function arraySetX_(arr, ...indicesAndValue) {
    const indices = indicesAndValue.slice(0, -1);
    const firstIndices = indices.slice(0, -1);
    const lastIndex = indices[indices.length - 1];
    const value = indicesAndValue[indicesAndValue.length - 1];
    let arr1 = arr;
    for (let i of firstIndices) {
        arr1 = arr1[i];
    }
    arr1[lastIndex] = value;
    return value;
}
exports.arraySetX_ = arraySetX_;
arraySetX_.fsource = [Symbol.for('define'), [Symbol.for('array-set!_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('first-indices'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('arr1'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('first-indices')]], [Symbol.for('set!'), Symbol.for('arr1'), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('i')]]], [Symbol.for('js/='), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('last-index')], Symbol.for('value')], Symbol.for('value')];
/**
 * Compiler macro for `(array-set! ...)` expressions.
 */
arraySetX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, ...indicesAndValue] = exp.slice(1);
        const indices = indicesAndValue.slice(0, -1);
        const value = indicesAndValue[indicesAndValue.length - 1];
        return [Symbol.for('js/='), [Symbol.for('js/get'), arr, ...indices], value];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Take the `n` first elements from `arr`.
 */
function arrayTake_(arr, n) {
    return arr.slice(0, -(arr.length - n) || undefined);
}
exports.arrayTake_ = arrayTake_;
arrayTake_.fsource = [Symbol.for('define'), [Symbol.for('array-take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];
arrayTake_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, n] = exp.slice(1);
        if (!(Array.isArray(arr) && (arr.length > 0))) {
            return [Symbol.for('array-drop-right'), arr, [Symbol.for('-'), [Symbol.for('array-length'), arr], n]];
        }
        else {
            const arr1 = Symbol('arr');
            return [Symbol.for('let'), [[arr1, arr]], ((arr) => {
                    return [Symbol.for('array-drop-right'), arr, [Symbol.for('-'), [Symbol.for('array-length'), arr], n]];
                })(arr1)];
        }
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the tail of an array.
 */
function arrayRest_(arr) {
    return arr.slice(1);
}
exports.arrayRest_ = arrayRest_;
arrayRest_.fsource = [Symbol.for('define'), [Symbol.for('array-rest_'), Symbol.for('arr')], [Symbol.for('array-drop'), Symbol.for('arr'), 1]];
arrayRest_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-drop'), arr, 1];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Slice a JavaScript array.
 */
function arraySlice_(arr, ...args) {
    return arr.slice(...args);
}
exports.arraySlice_ = arraySlice_;
arraySlice_.fsource = [Symbol.for('define'), [Symbol.for('array-slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];
arraySlice_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, ...args] = exp.slice(1);
        return [Symbol.for('send/apply'), arr, Symbol.for('slice'), [Symbol.for('list'), ...args]];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
function arrayDrop_(arr, n) {
    return arr.slice(n);
}
exports.arrayDrop_ = arrayDrop_;
arrayDrop_.fsource = [Symbol.for('define'), [Symbol.for('array-drop_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), Symbol.for('n')]];
/**
 * Compiler macro for `(array-drop ...)` expressions.
 */
arrayDrop_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, n] = exp.slice(1);
        if (n === 0) {
            return arr;
        }
        else {
            return [Symbol.for('array-slice'), arr, n];
        }
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
function arrayDropRight_(arr, n) {
    // Edge case: `(array-slice arr 0 (- n))` works well most
    // of the time, but not when `n` is zero, in which case
    // `(array-slice arr 0 0)` returns an empty array (and
    // not the full array, as expected). Hence the `or`
    // expression.
    return (
    // Edge case: `(array-slice arr 0 (- n))` works well most
    // of the time, but not when `n` is zero, in which case
    // `(array-slice arr 0 0)` returns an empty array (and
    // not the full array, as expected). Hence the `or`
    // expression.
    arr.slice(0, -n || undefined));
}
exports.arrayDropRight_ = arrayDropRight_;
arrayDropRight_.fsource = [Symbol.for('define'), [Symbol.for('array-drop-right_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), 0, [Symbol.for('or'), [Symbol.for('-'), Symbol.for('n')], undefined]]];
/**
 * Compiler macro for `(array-drop-right ...)` expressions.
 */
arrayDropRight_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, n] = exp.slice(1);
        if (Number.isFinite(n)) {
            if (n === 0) {
                return arr;
            }
            else {
                return [Symbol.for('array-slice'), arr, 0, [Symbol.for('-'), n]];
            }
        }
        else {
            return [Symbol.for('array-slice'), arr, 0, [Symbol.for('or'), [Symbol.for('-'), n], undefined]];
        }
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Concatenate arrays.
 */
function arrayConcat_(...args) {
    return [].concat(...args);
}
exports.arrayConcat_ = arrayConcat_;
arrayConcat_.fsource = [Symbol.for('define'), [Symbol.for('array-concat_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), [Symbol.for('quote'), []], Symbol.for('concat'), Symbol.for('args')]];
/**
 * Compiler macro for `(array-concat ...)` expressions.
 */
arrayConcat_.compilerMacro = (() => {
    const f = function (exp, env) {
        const args = exp.slice(1);
        if (args.length === 0) {
            return [];
        }
        else if (args.length === 1) {
            return args[0];
        }
        else {
            return [Symbol.for('send'), args[0], Symbol.for('concat'), ...args.slice(1)];
        }
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverse_(arr) {
    return [...arr].reverse();
}
exports.arrayReverse_ = arrayReverse_;
arrayReverse_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse_'), Symbol.for('arr')], [Symbol.for('array-reverse!'), [Symbol.for('array-copy'), Symbol.for('arr')]]];
arrayReverse_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('array-reverse!'), [Symbol.for('array-copy'), arr]];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverseX_(arr) {
    return arr.reverse();
}
exports.arrayReverseX_ = arrayReverseX_;
arrayReverseX_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];
arrayReverseX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('send'), arr, Symbol.for('reverse')];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Pop an element off the beginning of an array.
 */
function arrayPopLeftX_(arr) {
    return arr.shift();
}
exports.arrayPopLeftX_ = arrayPopLeftX_;
arrayPopLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-left!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('shift')]];
arrayPopLeftX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('send'), arr, Symbol.for('shift')];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Pop an element off the end of an array.
 */
function arrayPopRightX_(arr) {
    return arr.pop();
}
exports.arrayPopRightX_ = arrayPopRightX_;
arrayPopRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-right!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('pop')]];
arrayPopRightX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr] = exp.slice(1);
        return [Symbol.for('send'), arr, Symbol.for('pop')];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Push an element onto the beginning of an array.
 */
function arrayPushLeftX_(arr, x) {
    arr.unshift(x);
    return arr;
}
exports.arrayPushLeftX_ = arrayPushLeftX_;
arrayPushLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-left!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('arr')];
/**
 * Compiler macro for `(array-push-left! ...)` expressions.
 */
arrayPushLeftX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, x] = exp.slice(1);
        return [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), [Symbol.for('send'), arr, Symbol.for('unshift'), x], Symbol.for(':expression'), !(Array.isArray(arr) && (arr.length > 0)) ? [Symbol.for('begin'), [Symbol.for('send'), arr, Symbol.for('unshift'), x], arr] : ((arr1) => {
                return [Symbol.for('let'), [[arr1, arr]], ((arr) => {
                        return [Symbol.for('begin'), [Symbol.for('send'), arr, Symbol.for('unshift'), x], arr];
                    })(arr1)];
            })(Symbol('arr'))];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Push an element onto the end of an array.
 */
function arrayPushRightX_(arr, x) {
    arr.push(x);
    return arr;
}
exports.arrayPushRightX_ = arrayPushRightX_;
arrayPushRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-right!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('push'), Symbol.for('x')], Symbol.for('arr')];
/**
 * Compiler macro for `(array-push-right! ...)` expressions.
 */
arrayPushRightX_.compilerMacro = (() => {
    const f = function (exp, env) {
        const [arr, x] = exp.slice(1);
        return [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), [Symbol.for('send'), arr, Symbol.for('push'), x], Symbol.for(':expression'), !(Array.isArray(arr) && (arr.length > 0)) ? [Symbol.for('begin'), [Symbol.for('send'), arr, Symbol.for('push'), x], arr] : ((arr1) => {
                return [Symbol.for('let'), [[arr1, arr]], ((arr) => {
                        return [Symbol.for('begin'), [Symbol.for('send'), arr, Symbol.for('push'), x], arr];
                    })(arr1)];
            })(Symbol('arr'))];
    };
    f.ftype = 'macro';
    return f;
})();
/**
 * Sort an array with a comparator.
 */
function arraySort_(arr, comp = undefined) {
    return arr.sort(comp);
}
exports.arraySort_ = arraySort_;
arraySort_.fsource = [Symbol.for('define'), [Symbol.for('array-sort_'), Symbol.for('arr'), [Symbol.for('comp'), undefined]], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('sort'), Symbol.for('comp')]];
/**
 * Compiler macro for `(array-sort ...)` expressions.
 */
arraySort_.compilerMacro = (() => {
    const f = function (exp, env) {
        let [arr, comp] = exp.slice(1);
        if (comp === undefined) {
            comp = undefined;
        }
        if (comp) {
            return [Symbol.for('send'), arr, Symbol.for('sort'), comp];
        }
        else {
            return [Symbol.for('send'), arr, Symbol.for('sort')];
        }
    };
    f.ftype = 'macro';
    return f;
})();

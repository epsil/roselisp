/**
 * # Macros
 *
 * Macro definitions.
 *
 * ## Description
 *
 * This file provides macro implementations of some special forms.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Expand a `(define/private ...)` expression.
 */
declare function definePrivate_(exp: any, env: any): any;
declare namespace definePrivate_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define/public ...)` expression.
 */
declare function definePublic_(exp: any, env: any): any;
declare namespace definePublic_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(defclass ...)` expression.
 */
declare function defclass_(exp: any, env: any): any;
declare namespace defclass_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(defmacro ...)` expression.
 */
declare function defmacro_(exp: any, env: any): any;
declare namespace defmacro_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(defun ...)` expression.
 */
declare function defun_(exp: any, env: any): any;
declare namespace defun_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[] | symbol[][])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(begin0 ...)` or `(prog1 ...)` expression.
 */
declare function begin0_(exp: any, env: any): any;
declare namespace begin0_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][][])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(multiple-values-bind ...)` expression.
 */
declare function multipleValueBind_(exp: any, env: any): any;
declare namespace multipleValueBind_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[] | symbol[][][])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(rkt/new ...)' expression.
 */
declare function rktNew_(exp: any, env: any): any;
declare namespace rktNew_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(if ...)` expression.
 */
declare function if_(exp: any, env: any): any;
declare namespace if_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | never[])[])[] | ((number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[][])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(when ...)` expression.
 */
declare function when_(exp: any, env: any): any;
declare namespace when_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(unless ...)` expression.
 */
declare function unless_(exp: any, env: any): any;
declare namespace unless_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(as~> ...)` expression.
 *
 * Similar to the [`as->` macro][clj:thread-as] in Clojure.
 *
 * [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
 */
declare function threadAs_(exp: any, env: any): any;
declare namespace threadAs_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[] | symbol[][][])[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Evaluate a `(~> ...)` expression. Based on the
 * [`->` macro][clj:thread-first] in Clojure (also known as
 * the "`thread-first` macro").
 *
 * [clj:thread-first]: https://clojuredocs.org/clojure.core/-%3E
 */
declare function threadFirst_(exp: any, env: any): any;
declare namespace threadFirst_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | ((number | symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (symbol[] | (symbol | symbol[])[][])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Evaluate a `(~>> ...)` expression. Based on the
 * [`->>` macro][clj:thread-last] in Clojure (also known as
 * the "`thread-last` macro").
 *
 * [clj:thread-last]: https://clojuredocs.org/clojure.core/-%3E%3E
 */
declare function threadLast_(exp: any, env: any): any;
declare namespace threadLast_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | (number | symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(unwind-protect ...)` expression.
 */
declare function unwindProtect_(exp: any, env: any): any;
declare namespace unwindProtect_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(do ...)` expression.
 */
declare function do_(exp: any, env: any): any;
declare namespace do_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(while ...)` expression.
 */
declare function while_(exp: any, env: any): any;
declare namespace while_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(js/for ...)` expression.
 */
declare function jsFor_(exp: any, env: any): any;
declare namespace jsFor_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | symbol[][])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(js/for-in ...)` expression.
 */
declare function jsForIn_(exp: any, env: any): any;
declare namespace jsForIn_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(js/for-of ...)` expression.
 */
declare function jsForOf_(exp: any, env: any): any;
declare namespace jsForOf_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(field-bound? ...)` expression.
 */
declare function fieldBoundP_(exp: any, env: any): any;
declare namespace fieldBoundP_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (string | symbol)[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(case ...)` expression.
 */
declare function case_(exp: any, env: any): any;
declare namespace case_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(case/eq ...)` expression.
 */
declare function caseEq_(exp: any, env: any): any;
declare namespace caseEq_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(let-env ...)` expression.
 */
declare function letEnv_(exp: any, env: any): any;
declare namespace letEnv_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(set ...)` expression.
 */
declare function set_(exp: any, env: any): any;
declare namespace set_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(new/apply ...)` expression.
 */
declare function newApply_(exp: any, env: any): any;
declare namespace newApply_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(clj/try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
declare function cljTry_(exp: any, env: any): any;
declare namespace cljTry_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | ((symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | ((symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[])[];
    var lispMacro: boolean;
}
export { fieldBoundP_ as fieldBoundp_, begin0_, caseEq_, case_, cljTry_, defclass_, definePrivate_, definePublic_, defmacro_, defun_, do_, fieldBoundP_, if_, jsForIn_, jsForOf_, jsFor_, letEnv_, multipleValueBind_, newApply_, rktNew_, set_, threadAs_, threadFirst_, threadLast_, unless_, unwindProtect_, when_, while_ };

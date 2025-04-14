/**
 * # Evaluator
 *
 * S-expression evaluator and ESTree evaluator.
 *
 * ## Description
 *
 * Implements two evaluators: one for S-expressions (raw or wrapped
 * in rose trees) and one for ESTree nodes.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
import { jsEval_ } from './javascript';
/**
 * The default evaluator.
 */
declare const defaultEvaluator: any;
/**
 * Evaluate a S-expression `exp` in the Lisp environment `env`.
 *
 * `env` may be an {@link Environment}, or it may be an
 * {@link Evaluator}. In the latter case, evaluation takes place
 * by calling its `.eval()` method.
 *
 * This function is more fundamental and basic than `interpret`, which
 * is probably what you want. `interpret` stacks the given environment
 * on top of a Lisp environment, while `eval_` is a low-level function
 * that performs no such stacking.
 */
declare const eval_: any;
/**
 * Call an evaluator on an expression.
 */
declare function callEvaluator(evaluator: any, exp: any, env?: any, options?: any): any;
declare namespace callEvaluator {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something is an evaluator.
 */
declare function evaluatorp(obj: any): any;
declare namespace evaluatorp {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluator class.
 */
declare class Evaluator {
    /**
     * The simplest possible evaluator is the
     * identity function.
     */
    eval(exp: any, env?: any, options?: any): any;
}
/**
 * Lisp-1 evaluator function.
 */
declare function eval1(exp: any, env: any, options?: any): any;
declare namespace eval1 {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate an S-expression.
 *
 * This is a case-by-case function.
 */
declare function evalSexp(exp: any, env: any, options?: any): any;
declare namespace evalSexp {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | (symbol | (symbol | (string | symbol | symbol[])[])[])[])[][] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | ((symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[][])[])[])[] | (symbol | (symbol | symbol[])[] | (symbol | ((symbol | symbol[])[] | (string | symbol)[])[] | ((symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | string[])[])[])[])[])[])[])[][] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[][])[][])[])[])[])[];
}
/**
 * Evaluate an S-expression wrapped in a rose tree.
 */
declare function evalRose(node: any, env: any, options?: any): any;
declare namespace evalRose {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate an [ESTree][github:estree] node
 * (i.e., a JavaScript [AST][w:Abstract syntax tree]).
 *
 * [github:estree]: https://github.com/estree/estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
declare function evalEstree(node: any, env: any, options?: any): any;
declare namespace evalEstree {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
export { eval_ as seval, Evaluator, callEvaluator, defaultEvaluator, evalEstree, evalRose, evalSexp, eval1, eval_, evaluatorp, jsEval_ };

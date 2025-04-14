// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Special forms
 *
 * Interpreter procedures for special forms.
 *
 * ## Description
 *
 * The default approach for the interpreter is to compile the Lisp
 * code to JavaScript code and then evaluate the JavaScript code.
 * That is to say that the S-expression is compiled to an ESTree
 * tree, which is then evaluated.
 *
 * However, it is possible to make the intepretation more efficient
 * by skipping the compilation step and interpreting the S-expression
 * directly. This file defines interpreter procedures which do just
 * that.
 *
 * The functions defined in this file are *special forms* and must be
 * typed as such in the language environment. A special form receives
 * its arguments unevaluated and returns a value that is used
 * directly. It is similar to a macro, except that the value returned
 * by a macro is re-evaluated, while the value returned by a special
 * form is used as-is.
 *
 * Care must be taken to implement these functions correctly, as
 * their behavior should be identical to the standard behavior of
 * compiling the code to an ESTree tree and then evaluating. In other
 * words, the only purpose of the code in this file is to make
 * interpretation faster.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  aget_,
  aset_
} from './array';

import {
  EnvironmentStack,
  LispEnvironment,
  currentEnvironment
} from './env';

import {
  eval_ as evalT,
  eval_
} from './eval';

import {
  BreakException,
  ContinueException,
  ReturnException
} from './exception';

import {
  funcall as tcall
} from './procedures';

import {
  Rose,
  makeRose,
  rosep,
  transferComments
} from './rose';

// (require (only-in "./trampoline"
//                   tcall))
import {
  beginWrap,
  formp,
  lambdaToLet,
  quasiquotep,
  quotep,
  taggedListP,
  textOfQuotation,
  unquoteSplicingP,
  unquotep
} from './util';

const [lastCdr, flatten, nthcdr, cdr]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      let result: any = lst;
      while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  function flatten_(lst: any): any {
    return lst.reduce(function (acc: any, x: any): any {
      if (Array.isArray(x)) {
        return [...acc, ...flatten_(x)];
      } else if (x === Symbol.for('.')) {
        return acc;
      } else {
        acc.push(x);
        return acc;
      }
    }, []);
  }
  function nthcdr_(n: any, lst: any): any {
    if ((lst.length === (n + 2)) && ((lst as any)[n] === Symbol.for('.'))) {
      return lst[lst.length - 1];
    } else {
      if (n === 0) {
        return lst;
      } else {
        return lst.slice(n);
      }
    }
  }
  function cdr_(lst: any): any {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  return [lastCdr_, flatten_, nthcdr_, cdr_];
})();

/**
 * Evaluate a `(quote ...)` form.
 */
function quoteSpecial_(exp: any, env: any): any {
  return textOfQuotation(exp);
}

quoteSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('quote-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('text-of-quotation'), Symbol.for('exp')]];

/**
 * Evaluate a `(quasiquote ...)` form.
 */
function quasiquoteSpecial_(exp: any, env: any): any {
  return quasiquoteHelper((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
}

quasiquoteSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('quasiquote-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('quasiquote-helper'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]];

/**
 * Helper function for `quasiquote-special_`.
 */
function quasiquoteHelper(exp: any, env: any): any {
  if (!((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    return exp;
  } else {
    let result: any = [];
    for (let x of exp) {
      if (unquotep(x)) {
        const val: any = eval_((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
          const x1: any = lastCdr(x);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = x;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = x[x.length - 1];
            } else {
              result = x.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : x[1], env);
        result.push(val);
      } else if (unquoteSplicingP(x)) {
        const val: any = eval_((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
          const x1: any = lastCdr(x);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = x;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = x[x.length - 1];
            } else {
              result = x.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : x[1], env);
        if (!((): any => {
          const x1: any = lastCdr(val);
          return Array.isArray(x1) && (x1.length === 0);
        })()) {
          throw new Error('Wrong type of argument: expected list');
        }
        result = [...result, ...val];
      } else if (quasiquotep(x)) {
        result.push(x);
      } else if (((): any => {
        const x1: any = lastCdr(x);
        return Array.isArray(x1) && (x1.length === 0);
      })()) {
        const val: any = quasiquoteHelper(x, env);
        result.push(val);
      } else {
        result.push(x);
      }
    }
    return result;
  }
}

quasiquoteHelper.lispSource = [Symbol.for('define'), [Symbol.for('quasiquote-helper'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('list?'), Symbol.for('exp')]], Symbol.for('exp')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('unquote?'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('x')], Symbol.for('env')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('val')]], [[Symbol.for('unquote-splicing?'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('x')], Symbol.for('env')]], [Symbol.for('unless'), [Symbol.for('list?'), Symbol.for('val')], [Symbol.for('error'), 'Wrong type of argument: expected list']], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), Symbol.for('val')]]], [[Symbol.for('quasiquote?'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('x')]], [[Symbol.for('list?'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('quasiquote-helper'), Symbol.for('x'), Symbol.for('env')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('val')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('x')]]]], Symbol.for('result')]]];

/**
 * Evaluate a `(setq ...)` form.
 */
function setqSpecial_(exp: any, env: any): any {
  const assignments: any = [];
  const _end: any = exp.length - 1;
  for (let i: any = 1; i < _end; i = i + 2) {
    let sym: any = (exp as any)[i];
    const val: any = exp[i + 1];
    const assignment: any = [Symbol.for('set'), [Symbol.for('quote'), sym], val];
    assignments.push(assignment);
  }
  let setExp: any = [];
  if (assignments.length > 1) {
    setExp = [Symbol.for('begin'), ...assignments];
  } else {
    setExp = assignments[0];
  }
  return eval_(setExp, env);
}

setqSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('setq-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('assignments'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], 2]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('assignment'), [Symbol.for('quasiquote'), [Symbol.for('set'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('sym')]], [Symbol.for('unquote'), Symbol.for('val')]]]], [Symbol.for('push-right!'), Symbol.for('assignments'), Symbol.for('assignment')]], [Symbol.for('define'), Symbol.for('set-exp'), [Symbol.for('quote'), []]], [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('assignments')], 1], [Symbol.for('set!'), Symbol.for('set-exp'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('assignments')]]]], [Symbol.for('set!'), Symbol.for('set-exp'), [Symbol.for('first'), Symbol.for('assignments')]]], [Symbol.for('eval_'), Symbol.for('set-exp'), Symbol.for('env')]];

/**
 * Evaluate a `(set ...)` form.
 */
function setSpecial_(exp: any, env: any): any {
  const params: any = exp.slice(1);
  let sym: any = params[0];
  sym = eval_(sym, env);
  if (formp(sym, env, aget_)) {
    return eval_([aset_, ...sym.slice(1), (Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(params);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = params;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = params[params.length - 1];
        } else {
          result = params.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : params[1]], env);
  } else {
    const val: any = eval_((Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(params);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = params;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = params[params.length - 1];
        } else {
          result = params.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : params[1], env);
    if (((): any => {
      const x: any = lastCdr(sym);
      return Array.isArray(x) && (x.length === 0);
    })() && (sym.length === 2)) {
      let prop: any = sym[0];
      let obj: any = eval_((Array.isArray(sym) && (sym.length >= 3) && (sym[sym.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(sym);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = sym;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = sym[sym.length - 1];
          } else {
            result = sym.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : sym[1], env);
      if (typeof prop === 'symbol') {
        let match: any;
        if ((match = (prop.description as string).match(new RegExp('^\\.-(.*)$')))) {
          prop = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(match);
            return Array.isArray(x) && (x.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = match;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = match[match.length - 1];
              } else {
                result = match.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })() : match[1];
          (obj as any)[prop] = val;
        }
      }
    } else {
      env.set(sym, val, 'variable');
    }
    return val;
  }
}

setSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('set-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('params')]], [Symbol.for('set!'), Symbol.for('sym'), [Symbol.for('eval_'), Symbol.for('sym'), Symbol.for('env')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('sym'), Symbol.for('env'), Symbol.for('aget_')], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('aset_')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('sym')]], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('params')]]]], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('params')], Symbol.for('env')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('list?'), Symbol.for('sym')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('sym')], 2]], [Symbol.for('define'), Symbol.for('prop'), [Symbol.for('first'), Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('sym')], Symbol.for('env')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('prop')], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('when'), [Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.-(.*)$'], [Symbol.for('symbol->string'), Symbol.for('prop')]]], [Symbol.for('set!'), Symbol.for('prop'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('set!'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('prop')], Symbol.for('val')]]]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('val'), 'variable']]], Symbol.for('val')]]];

/**
 * Evaluate a `(fset ...)` form.
 */
function fsetSpecial_(exp: any, env: any): any {
  const params: any = exp.slice(1);
  let sym: any = eval_(params[0], env);
  const val: any = eval_((Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(params);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = params;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = params[params.length - 1];
      } else {
        result = params.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : params[1], env);
  env.set(sym, val, 'function');
  return val;
}

fsetSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('fset-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('eval_'), [Symbol.for('first'), Symbol.for('params')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('params')], Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('val'), 'function'], Symbol.for('val')];

/**
 * Evaluate a `(module ...)` form.
 */
function moduleSpecial_(exp: any, env: any): any {
  return evalT([Symbol.for('begin'), ...exp.slice(3)], env);
}

moduleSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('module-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('tcall'), Symbol.for('eval-t'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('exp'), 3]]]], Symbol.for('env')]];

/**
 * Evaluate a `(begin ...)` form.
 */
function beginSpecial_(exp: any, env: any): any {
  return beginHelper(exp.slice(1), env, undefined);
}

beginSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('begin-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('begin-helper'), [Symbol.for('rest'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('undefined')]];

/**
 * Helper function for `begin-special_`.
 */
function beginHelper(expressions: any, env: any, val: any): any {
  if (expressions.length === 0) {
    return val;
  } else {
    return beginHelper(expressions.slice(1), env, evalT(expressions[0], env));
  }
}

beginHelper.lispSource = [Symbol.for('define'), [Symbol.for('begin-helper'), Symbol.for('expressions'), Symbol.for('env'), Symbol.for('val')], [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('expressions')], 0], Symbol.for('val'), [Symbol.for('tcall'), Symbol.for('begin-helper'), [Symbol.for('rest'), Symbol.for('expressions')], Symbol.for('env'), [Symbol.for('tcall'), Symbol.for('eval-t'), [Symbol.for('first'), Symbol.for('expressions')], Symbol.for('env')]]]];

/**
 * Evaluate a `(let* ...)` form.
 */
function letStarSpecial_(exp: any, env: any): any {
  const params: any = exp.slice(1);
  const varExps: any = params[0];
  let body: any = params.slice(1);
  const bindings: any = [];
  const initExps: any = [];
  for (let varExp of varExps) {
    if (typeof varExp === 'symbol') {
      bindings.push([varExp, undefined, 'variable']);
    } else {
      bindings.push([varExp[0], undefined, 'variable']);
      const initExp: any = [Symbol.for('setq'), ...varExp];
      initExps.push(initExp);
    }
  }
  const letEnv: any = new LispEnvironment(bindings);
  const combinedEnv: any = new EnvironmentStack(letEnv, env);
  const beginExp: any = [Symbol.for('begin'), ...initExps, ...body];
  return evalT(beginExp, combinedEnv);
}

letStarSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('let-star-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('var-exps'), [Symbol.for('first'), Symbol.for('params')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('params')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('init-exps'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('var-exp'), Symbol.for('var-exps')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('var-exp')], [Symbol.for('push-right!'), Symbol.for('bindings'), [Symbol.for('list'), Symbol.for('var-exp'), Symbol.for('undefined'), 'variable']]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('bindings'), [Symbol.for('list'), [Symbol.for('first'), Symbol.for('var-exp')], Symbol.for('undefined'), 'variable']], [Symbol.for('define'), Symbol.for('init-exp'), [Symbol.for('quasiquote'), [Symbol.for('setq'), [Symbol.for('unquote-splicing'), Symbol.for('var-exp')]]]], [Symbol.for('push-right!'), Symbol.for('init-exps'), Symbol.for('init-exp')]]]], [Symbol.for('define'), Symbol.for('let-env'), [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('combined-env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('let-env'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('begin-exp'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('init-exps')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]], [Symbol.for('tcall'), Symbol.for('eval-t'), Symbol.for('begin-exp'), Symbol.for('combined-env')]];

/**
 * Evaluate a `(let-values ...)` form.
 */
function letValuesSpecial_(exp: any, env: any): any {
  const bindings: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let body: any = exp.slice(2);
  const letBindings: any = [];
  const _end: any = bindings.length;
  for (let i: any = 0; i < _end; i++) {
    let result: any = Symbol('let-values-result-' + i + 1 + '');
    const binding: any = (bindings as any)[i];
    const bindingVars: any = binding[0];
    const bindingExp: any = (Array.isArray(binding) && (binding.length >= 3) && (binding[binding.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(binding);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = binding;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = binding[binding.length - 1];
        } else {
          result = binding.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : binding[1];
    let regularBindings: any = [];
    let restBinding: any = undefined;
    if (typeof bindingVars === 'symbol') {
      restBinding = bindingVars;
    } else if (Array.isArray(bindingVars) && (bindingVars.length >= 3) && (bindingVars[bindingVars.length - 2] === Symbol.for('.')) && !((): any => {
      const x: any = lastCdr(bindingVars);
      return Array.isArray(x) && (x.length === 0);
    })()) {
      const bindingList: any = flatten(bindingVars);
      regularBindings = bindingList.slice(0, -1);
      restBinding = bindingList[bindingList.length - 1];
    } else {
      regularBindings = bindingVars;
    }
    letBindings.push([result, bindingExp]);
    const _end1: any = regularBindings.length;
    for (let j: any = 0; j < _end1; j++) {
      letBindings.push([(regularBindings as any)[j], [Symbol.for('aget'), result, j]]);
    }
    if (restBinding) {
      letBindings.push([restBinding, [Symbol.for('nthcdr'), regularBindings.length, result]]);
    }
  }
  return eval_([Symbol.for('let*'), letBindings, ...body], env);
}

letValuesSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('let-values-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('bindings')]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('gensym'), [Symbol.for('string-append'), 'let-values-result-', [Symbol.for('number->string'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], [Symbol.for('define'), Symbol.for('binding'), [Symbol.for('aget'), Symbol.for('bindings'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('binding-vars'), [Symbol.for('first'), Symbol.for('binding')]], [Symbol.for('define'), Symbol.for('binding-exp'), [Symbol.for('second'), Symbol.for('binding')]], [Symbol.for('define'), Symbol.for('regular-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-binding'), Symbol.for('undefined')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('binding-vars')], [Symbol.for('set!'), Symbol.for('rest-binding'), Symbol.for('binding-vars')]], [[Symbol.for('dotted-list?'), Symbol.for('binding-vars')], [Symbol.for('define'), Symbol.for('binding-list'), [Symbol.for('flatten'), Symbol.for('binding-vars')]], [Symbol.for('set!'), Symbol.for('regular-bindings'), [Symbol.for('drop-right'), Symbol.for('binding-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-binding'), [Symbol.for('array-list-last'), Symbol.for('binding-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-bindings'), Symbol.for('binding-vars')]]], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('list'), Symbol.for('result'), Symbol.for('binding-exp')]], [Symbol.for('for'), [[Symbol.for('j'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('regular-bindings')]]]], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('list'), [Symbol.for('aget'), Symbol.for('regular-bindings'), Symbol.for('j')], [Symbol.for('quasiquote'), [Symbol.for('aget'), [Symbol.for('unquote'), Symbol.for('result')], [Symbol.for('unquote'), Symbol.for('j')]]]]]], [Symbol.for('when'), Symbol.for('rest-binding'), [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('list'), Symbol.for('rest-binding'), [Symbol.for('quasiquote'), [Symbol.for('nthcdr'), [Symbol.for('unquote'), [Symbol.for('array-list-length'), Symbol.for('regular-bindings')]], [Symbol.for('unquote'), Symbol.for('result')]]]]]]], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('let*'), [Symbol.for('unquote'), Symbol.for('let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]], Symbol.for('env')]];

/**
 * Evaluate a `(define-values ...)` form.
 */
function defineValuesSpecial_(exp: any, env: any): any {
  const ids: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const val: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  let regularBindings: any = [];
  let restBinding: any = undefined;
  let result: any;
  if (typeof ids === 'symbol') {
    restBinding = ids;
  } else if (Array.isArray(ids) && (ids.length >= 3) && (ids[ids.length - 2] === Symbol.for('.')) && !((): any => {
    const x: any = lastCdr(ids);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    const bindingList: any = flatten(ids);
    regularBindings = bindingList.slice(0, -1);
    restBinding = bindingList[bindingList.length - 1];
  } else {
    regularBindings = ids;
  }
  result = eval_(val, env);
  const _end: any = regularBindings.length;
  for (let i: any = 0; i < _end; i++) {
    eval_([Symbol.for('define'), (regularBindings as any)[i], [Symbol.for('quote'), (result as any)[i]]], env);
  }
  if (restBinding) {
    eval_([Symbol.for('define'), restBinding, [Symbol.for('quote'), nthcdr(regularBindings.length, result)]], env);
  }
  return undefined;
}

defineValuesSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-values-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('regular-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-binding'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids')], [Symbol.for('set!'), Symbol.for('rest-binding'), Symbol.for('ids')]], [[Symbol.for('dotted-list?'), Symbol.for('ids')], [Symbol.for('define'), Symbol.for('binding-list'), [Symbol.for('flatten'), Symbol.for('ids')]], [Symbol.for('set!'), Symbol.for('regular-bindings'), [Symbol.for('drop-right'), Symbol.for('binding-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-binding'), [Symbol.for('array-list-last'), Symbol.for('binding-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-bindings'), Symbol.for('ids')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('val'), Symbol.for('env')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('regular-bindings')]]]], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('regular-bindings'), Symbol.for('i')]], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('result'), Symbol.for('i')]]]]], Symbol.for('env')]], [Symbol.for('when'), Symbol.for('rest-binding'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('rest-binding')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('nthcdr'), [Symbol.for('array-list-length'), Symbol.for('regular-bindings')], Symbol.for('result')]]]]], Symbol.for('env')]], Symbol.for('undefined')];

/**
 * Evaluate a `(set!-values ...)` form.
 */
function setValuesSpecial_(exp: any, env: any): any {
  const ids: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const val: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  let regularBindings: any = [];
  let restBinding: any = undefined;
  let result: any;
  if (typeof ids === 'symbol') {
    restBinding = ids;
  } else if (Array.isArray(ids) && (ids.length >= 3) && (ids[ids.length - 2] === Symbol.for('.')) && !((): any => {
    const x: any = lastCdr(ids);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    const bindingList: any = flatten(ids);
    regularBindings = bindingList.slice(0, -1);
    restBinding = bindingList[bindingList.length - 1];
  } else {
    regularBindings = ids;
  }
  result = eval_(val, env);
  const _end: any = regularBindings.length;
  for (let i: any = 0; i < _end; i++) {
    eval_([Symbol.for('set!'), (regularBindings as any)[i], [Symbol.for('quote'), (result as any)[i]]], env);
  }
  if (restBinding) {
    eval_([Symbol.for('set!'), restBinding, [Symbol.for('quote'), nthcdr(regularBindings.length, result)]], env);
  }
  return undefined;
}

setValuesSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('set-values-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('regular-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-binding'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids')], [Symbol.for('set!'), Symbol.for('rest-binding'), Symbol.for('ids')]], [[Symbol.for('dotted-list?'), Symbol.for('ids')], [Symbol.for('define'), Symbol.for('binding-list'), [Symbol.for('flatten'), Symbol.for('ids')]], [Symbol.for('set!'), Symbol.for('regular-bindings'), [Symbol.for('drop-right'), Symbol.for('binding-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-binding'), [Symbol.for('array-list-last'), Symbol.for('binding-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-bindings'), Symbol.for('ids')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('val'), Symbol.for('env')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('regular-bindings')]]]], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('regular-bindings'), Symbol.for('i')]], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('result'), Symbol.for('i')]]]]], Symbol.for('env')]], [Symbol.for('when'), Symbol.for('rest-binding'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('rest-binding')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('nthcdr'), [Symbol.for('array-list-length'), Symbol.for('regular-bindings')], Symbol.for('result')]]]]], Symbol.for('env')]], Symbol.for('undefined')];

/**
 * Evaluate a `(define ...)` form.
 */
function defineSpecial_(exp: any, env: any): any {
  let name: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let body: any = exp.slice(2);
  if (Array.isArray(name)) {
    // Function definition.
    const nameAndParams: any = name;
    const fName: any = nameAndParams[0];
    const params: any = nameAndParams.slice(1);
    if (Array.isArray(fName)) {
      // Curried function definition.
      // Curry the function. (The following is more similar to
      // currying in Haskell than currying in Racket because the
      // whole function is curried, not just some arguments.)
      const curriedNameAndParams: any = nameAndParams.flat(Infinity);
      const curriedName: any = curriedNameAndParams[0];
      const curriedParams: any = curriedNameAndParams.slice(1);
      const curriedArity: any = curriedParams.length;
      const curriedFunctionExp: any = [Symbol.for('curry'), [Symbol.for('lambda'), curriedParams, ...body], curriedArity];
      const val: any = eval_(curriedFunctionExp, env);
      env.setLocal(curriedName, val);
      return val;
    } else {
      // Uncurried function definition.
      const lambdaExp: any = [Symbol.for('lambda'), params, ...body];
      const val: any = eval_(lambdaExp, env);
      env.setLocal(fName, val);
      return val;
    }
  } else if ((
   (exp.length === 3) &&
   // (form? (third exp) env define-class_)
   taggedListP((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
     const x: any = lastCdr(exp);
     return Array.isArray(x) && (x.length === 0);
   })()) ? ((): any => {
     let i: any = 2;
     let result: any = exp;
     while (i > 0) {
       if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
         result = exp[exp.length - 1];
       } else {
         result = exp.slice(1);
       }
       i--;
     }
     if (Array.isArray(result)) {
       result = result[0];
     }
     return result;
   })() : exp[2], Symbol.for('define-class'))
  )) {
    // Class definition.
    return eval_(defineToDefineClass(exp), env);
  } else {
    // Variable definition.
    const valExp: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[2];
    const val: any = eval_(valExp, env);
    env.setLocal(name, val);
    return val;
  }
}

defineSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('name-and-params'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('f-name'), [Symbol.for('first'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('name-and-params')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('f-name')], [Symbol.for('define'), Symbol.for('curried-name-and-params'), [Symbol.for('send'), Symbol.for('name-and-params'), Symbol.for('flat'), Infinity]], [Symbol.for('define'), Symbol.for('curried-name'), [Symbol.for('first'), Symbol.for('curried-name-and-params')]], [Symbol.for('define'), Symbol.for('curried-params'), [Symbol.for('rest'), Symbol.for('curried-name-and-params')]], [Symbol.for('define'), Symbol.for('curried-arity'), [Symbol.for('array-list-length'), Symbol.for('curried-params')]], [Symbol.for('define'), Symbol.for('curried-function-exp'), [Symbol.for('quasiquote'), [Symbol.for('curry'), [Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('curried-params')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote'), Symbol.for('curried-arity')]]]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), Symbol.for('curried-function-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('curried-name'), Symbol.for('val')], Symbol.for('val')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), Symbol.for('lambda-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('f-name'), Symbol.for('val')], Symbol.for('val')]]], [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('tagged-list?'), [Symbol.for('third'), Symbol.for('exp')], [Symbol.for('quote'), Symbol.for('define-class')]]], [Symbol.for('eval_'), [Symbol.for('define->define-class'), Symbol.for('exp')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('val-exp'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), Symbol.for('val-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('name'), Symbol.for('val')], Symbol.for('val')]]];

/**
 * Convert a `(define ... (class ...))` form to
 * a `(define-class ...)` form.
 */
function defineToDefineClass(node: any): any {
  if (node instanceof Rose) {
    const superclass: any = node.get(2).get(1);
    const superclassExp: any = superclass.getValue();
    const superclassList: any = ((superclassExp === Symbol.for('object%')) || (superclassExp === Symbol.for('object')) || (superclassExp === Symbol.for('Object'))) ? [] : [superclass];
    return transferComments(node, makeRose([Symbol.for('define-class'), node.get(1), makeRose(superclassList), ...node.get(2).drop(2)]));
  } else {
    return defineToDefineClass(makeRose(node)).getValue();
  }
}

defineToDefineClass.lispSource = [Symbol.for('define'), [Symbol.for('define->define-class'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('define'), Symbol.for('superclass'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('superclass-exp'), [Symbol.for('send'), Symbol.for('superclass'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('superclass-list'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), Symbol.for('object%')]], [Symbol.for('eq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), Symbol.for('object')]], [Symbol.for('eq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), Symbol.for('Object')]]], [Symbol.for('quote'), []], [Symbol.for('list'), Symbol.for('superclass')]]], [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('make-rose'), Symbol.for('superclass-list')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('drop'), 2]]]]]]], [Symbol.for('else'), [Symbol.for('~>'), [Symbol.for('define->define-class'), [Symbol.for('make-rose'), Symbol.for('node')]], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]]]];

/**
 * Evaluate a `(define/public ...)` form.
 */
function definePublicSpecial_(exp: any, env: any): any {
  return defineSpecial_(exp, env);
}

definePublicSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-public-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-special_'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Evaluate a `(define/generator ...)` form.
 */
function defineGeneratorSpecial_(exp: any, env: any): any {
  return defineSpecial_(exp, env);
}

defineGeneratorSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-generator-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-special_'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Evaluate a `(define/async ...)` form.
 */
function defineAsyncSpecial_(exp: any, env: any): any {
  return defineSpecial_(exp, env);
}

defineAsyncSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-async-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-special_'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Evaluate a `(defmacro ...)` form.
 */
function defmacroSpecial_(exp: any, env: any): any {
  let name: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  if (((): any => {
    const x: any = lastCdr(name);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    name = name[0];
  }
  const macroFn: any = defmacroToFn(exp, env);
  env.set(name, macroFn, 'macro');
  // name
  return macroFn;
}

defmacroSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('defmacro-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('list?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('first'), Symbol.for('name')]]], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('defmacro->fn'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('name'), Symbol.for('macro-fn'), 'macro'], Symbol.for('macro-fn')];

/**
 * Create a macro function on the basis of a
 * `(defmacro ...)` form.
 */
function defmacroToFn(exp: any, env: any): any {
  const macroFn: any = defmacroToLambdaForm(exp);
  return eval_(macroFn, env);
}

defmacroToFn.lispSource = [Symbol.for('define'), [Symbol.for('defmacro->fn'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('defmacro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];

/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(defmacro ...)` form.
 */
function defmacroToLambdaForm(exp: any): any {
  let name: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let args: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  let body: any = exp.slice(3);
  let env: any = Symbol.for('env');
  let macroArgs: any = [];
  if (((): any => {
    const x: any = lastCdr(name);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    args = name.slice(1);
    name = name[0];
    body = exp.slice(2);
  }
  if (((): any => {
    const x: any = lastCdr(args);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    const _end: any = args.length;
    for (let i: any = 0; i < _end; i++) {
      const arg: any = (args as any)[i];
      if (arg === Symbol.for('&environment')) {
        env = args[i + 1];
        i = i + 2;
      } else {
        macroArgs.push(arg);
      }
    }
  } else {
    macroArgs = args;
  }
  if (Array.isArray(macroArgs) && (macroArgs.length === 0)) {
    return [Symbol.for('lambda'), [Symbol.for('exp'), env], ...body];
  } else {
    return [Symbol.for('lambda'), [Symbol.for('exp'), env], [Symbol.for('let-values'), [[macroArgs, [Symbol.for('rest'), Symbol.for('exp')]]], ...body]];
  }
}

defmacroToLambdaForm.lispSource = [Symbol.for('define'), [Symbol.for('defmacro->lambda-form'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 3]], [Symbol.for('define'), Symbol.for('env'), [Symbol.for('quote'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('when'), [Symbol.for('list?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('name')]], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('first'), Symbol.for('name')]], [Symbol.for('set!'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]]], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('exp'), [Symbol.for('unquote'), Symbol.for('env')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('exp'), [Symbol.for('unquote'), Symbol.for('env')]], [Symbol.for('let-values'), [[[Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]]];

/**
 * Evaluate a `(define-macro ...)` form.
 */
function defineMacroSpecial_(exp: any, env: any): any {
  let name: any = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1])[0];
  const macroFn: any = defineMacroToFn(exp, env);
  env.set(name, macroFn, 'macro');
  return name;
}

defineMacroSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-macro-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->fn'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('name'), Symbol.for('macro-fn'), 'macro'], Symbol.for('name')];

/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` form.
 */
function defineMacroToFn(exp: any, env: any): any {
  const macroFn: any = defineMacroToLambdaForm(exp);
  return eval_(macroFn, env);
}

defineMacroToFn.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->fn'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];

/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` form.
 */
function defineMacroToLambdaForm(exp: any): any {
  const nameAndArgs: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let name: any = nameAndArgs[0];
  let args: any = cdr(nameAndArgs);
  let body: any = exp.slice(2);
  let env: any = Symbol.for('env');
  let macroArgs: any = [];
  if (((): any => {
    const x: any = lastCdr(args);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    const _end: any = args.length;
    for (let i: any = 0; i < _end; i++) {
      const arg: any = (args as any)[i];
      if (arg === Symbol.for('&environment')) {
        env = args[i + 1];
        i = i + 2;
      } else {
        macroArgs.push(arg);
      }
    }
  } else {
    macroArgs = args;
  }
  if (Array.isArray(macroArgs) && (macroArgs.length === 0)) {
    return [Symbol.for('lambda'), [Symbol.for('exp'), env], ...body];
  } else {
    return [Symbol.for('lambda'), [Symbol.for('exp'), env], [Symbol.for('let-values'), [[macroArgs, [Symbol.for('rest'), Symbol.for('exp')]]], ...body]];
  }
}

defineMacroToLambdaForm.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('cdr'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('env'), [Symbol.for('quote'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('exp'), [Symbol.for('unquote'), Symbol.for('env')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('exp'), [Symbol.for('unquote'), Symbol.for('env')]], [Symbol.for('let-values'), [[[Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]]];

/**
 * Evaluate a `(for ...)` form.
 */
function forSpecial_(exp: any, env: any): any {
  const decls: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let body: any = exp.slice(2);
  const [decl1]: any[] = decls;
  let [sym, valuesExpr]: any[] = decl1;
  const values: any = eval_(valuesExpr, env);
  let result: any = undefined;
  try {
    for (let value of values) {
      try {
        result = eval_([Symbol.for('let'), [[sym, value]], ...body], env);
      } catch (e) {
        if (e instanceof ContinueException) {
        } else {
          throw e;
        }
      }
    }
  } catch (e) {
    if (e instanceof BreakException) {
      result = undefined;
    } else {
      throw e;
    }
  }
  return result;
}

forSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('for-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('decls'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define-values'), [Symbol.for('decl1')], Symbol.for('decls')], [Symbol.for('define-values'), [Symbol.for('sym'), Symbol.for('values-expr')], Symbol.for('decl1')], [Symbol.for('define'), Symbol.for('values'), [Symbol.for('eval_'), Symbol.for('values-expr'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('try'), [Symbol.for('for'), [[Symbol.for('value'), Symbol.for('values')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('value')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]], Symbol.for('env')]], [Symbol.for('catch'), Symbol.for('ContinueException'), Symbol.for('e')]]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('undefined')]]], Symbol.for('result')];

/**
 * Evaluate a `(js/while ...)` form.
 */
function jsWhileSpecial_(exp: any, env: any): any {
  const test: any = [Symbol.for('truep'), (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1]];
  let body: any = beginWrap(exp.slice(2));
  let result: any = undefined;
  try {
    while (eval_(test, env)) {
      try {
        result = eval_(body, env);
      } catch (e) {
        if (e instanceof ContinueException) {
        } else {
          throw e;
        }
      }
    }
  } catch (e) {
    if (e instanceof BreakException) {
      result = undefined;
    } else {
      throw e;
    }
  }
  return result;
}

jsWhileSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('js-while-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('quasiquote'), [Symbol.for('truep'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('begin-wrap'), [Symbol.for('drop'), Symbol.for('exp'), 2]]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('try'), [Symbol.for('while'), [Symbol.for('eval_'), Symbol.for('test'), Symbol.for('env')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('body'), Symbol.for('env')]], [Symbol.for('catch'), Symbol.for('ContinueException'), Symbol.for('e')]]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('undefined')]]], Symbol.for('result')];

/**
 * Evaluate a `(js/do-while ...)` form.
 */
function jsDoWhileSpecial_(exp: any, env: any): any {
  let body: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const test: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  const beginExp: any = [Symbol.for('begin'), body, [Symbol.for('while'), test, body]];
  return eval_(beginExp, env);
}

jsDoWhileSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('js-do-while-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('begin-exp'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('body')], [Symbol.for('while'), [Symbol.for('unquote'), Symbol.for('test')], [Symbol.for('unquote'), Symbol.for('body')]]]]], [Symbol.for('eval_'), Symbol.for('begin-exp'), Symbol.for('env')]];

/**
 * Evaluate a `(break)` form.
 */
function breakSpecial_(exp: any, env: any): any {
  throw new BreakException();
}

breakSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('break-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('BreakException')]]];

/**
 * Evaluate a `(continue)` form.
 */
function continueSpecial_(exp: any, env: any): any {
  throw new ContinueException();
}

continueSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('continue-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('ContinueException')]]];

/**
 * Evaluate a `(yield ...)` form.
 */
function yieldSpecial_(exp: any, env: any): any {
  const val: any = eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
  return val;
}

yieldSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('yield-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]], Symbol.for('val')];

/**
 * Evaluate a `(return ...)` form.
 */
function returnSpecial_(exp: any, env: any): any {
  const val: any = eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
  throw new ReturnException(val);
}

returnSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('return-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('ReturnException'), Symbol.for('val')]]];

/**
 * Evaluate a `(throw ...)` form.
 */
function throwSpecial_(exp: any, env: any): any {
  const val: any = eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
  throw val;
}

throwSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('throw-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]], [Symbol.for('throw'), Symbol.for('val')]];

/**
 * Evaluate an `(async ...)` form.
 */
function asyncSpecial_(exp: any, env: any): any {
  return async function (...args: any[]): Promise<any> {
    return eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1], env)(...args);
  };
}

asyncSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('async-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('async'), [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('apply'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')], Symbol.for('args')]]]];

/**
 * Evaluate an `(await ...)` form.
 */
async function awaitSpecial_(exp: any, env: any): Promise<any> {
  return await eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
}

awaitSpecial_.lispSource = [Symbol.for('define/async'), [Symbol.for('await-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('await'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]]];

/**
 * Evaluate a `(lambda ...)` form.
 */
function lambdaSpecial_(exp: any, env: any): any {
  return jsFunctionSpecial_(exp, env);
}

lambdaSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('lambda-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('js-function-special_'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Evaluate a `(js/function ...)` form.
 */
function jsFunctionSpecial_(exp: any, env: any): any {
  const f: any = function (...args: any[]): any {
    const letExp: any = lambdaToLet(exp, args);
    let result: any;
    try {
      result = eval_(letExp, env);
    } catch (e) {
      if (e instanceof ReturnException) {
        result = e.value;
      } else {
        throw e;
      }
    }
    return result;
  };
  f.lispInfo = [exp, env];
  return f;
}

jsFunctionSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('js-function-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('js/function'), Symbol.for('args'), [Symbol.for('define'), Symbol.for('let-exp'), [Symbol.for('lambda->let'), Symbol.for('exp'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('let-exp'), Symbol.for('env')]], [Symbol.for('catch'), Symbol.for('ReturnException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]]], Symbol.for('result')]], [Symbol.for('set-field!'), Symbol.for('lisp-info'), Symbol.for('f'), [Symbol.for('list'), Symbol.for('exp'), Symbol.for('env')]], Symbol.for('f')];

/**
 * Evaluate a `(js/arrow ...)` form.
 */
function jsArrowSpecial_(exp: any, env: any): any {
  const f: any = (...args: any[]): any => {
    const letExp: any = lambdaToLet(exp, args);
    let result: any;
    try {
      result = eval_(letExp, env);
    } catch (e) {
      if (e instanceof ReturnException) {
        result = e.value;
      } else {
        throw e;
      }
    }
    return result;
  };
  f.lispInfo = [exp, env];
  return f;
}

jsArrowSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('js-arrow-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('js/arrow'), Symbol.for('args'), [Symbol.for('define'), Symbol.for('let-exp'), [Symbol.for('lambda->let'), Symbol.for('exp'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('let-exp'), Symbol.for('env')]], [Symbol.for('catch'), Symbol.for('ReturnException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]]], Symbol.for('result')]], [Symbol.for('set-field!'), Symbol.for('lisp-info'), Symbol.for('f'), [Symbol.for('list'), Symbol.for('exp'), Symbol.for('env')]], Symbol.for('f')];

/**
 * Evaluate a `(cond ...)` form.
 */
function condSpecial_(exp: any, env: any): any {
  if (exp.length <= 1) {
    return false;
  } else {
    const clause: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1];
    const clauses: any = exp.slice(2);
    const condition: any = clause[0];
    const thenExpr: any = beginWrap(clause.slice(1));
    return condHelper((condition === Symbol.for('else')) || evalT([Symbol.for('truep'), condition], env), thenExpr, clauses, env);
  }
}

condSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('cond-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], Symbol.for('#f')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('clause'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('first'), Symbol.for('clause')]], [Symbol.for('define'), Symbol.for('then-expr'), [Symbol.for('begin-wrap'), [Symbol.for('rest'), Symbol.for('clause')]]], [Symbol.for('tcall'), Symbol.for('cond-helper'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('condition'), [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('tcall'), Symbol.for('eval-t'), [Symbol.for('quasiquote'), [Symbol.for('truep'), [Symbol.for('unquote'), Symbol.for('condition')]]], Symbol.for('env')]], Symbol.for('then-expr'), Symbol.for('clauses'), Symbol.for('env')]]]];

/**
 * Helper function for `cond-special_`.
 */
function condHelper(condition: any, thenExpr: any, clauses: any, env: any): any {
  if (condition) {
    return evalT(thenExpr, env);
  } else if (clauses.length === 0) {
    return undefined;
  } else {
    const clause1: any = clauses[0];
    const clauses1: any = clauses.slice(1);
    const condition1: any = clause1[0];
    const thenExpr1: any = beginWrap(clause1.slice(1));
    return condHelper((condition1 === Symbol.for('else')) || evalT([Symbol.for('truep'), condition1], env), thenExpr1, clauses1, env);
  }
}

condHelper.lispSource = [Symbol.for('define'), [Symbol.for('cond-helper'), Symbol.for('condition'), Symbol.for('then-expr'), Symbol.for('clauses'), Symbol.for('env')], [Symbol.for('cond'), [Symbol.for('condition'), [Symbol.for('tcall'), Symbol.for('eval-t'), Symbol.for('then-expr'), Symbol.for('env')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('clauses')], 0], Symbol.for('undefined')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('clause1'), [Symbol.for('first'), Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('clauses1'), [Symbol.for('rest'), Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('condition1'), [Symbol.for('first'), Symbol.for('clause1')]], [Symbol.for('define'), Symbol.for('then-expr-1'), [Symbol.for('begin-wrap'), [Symbol.for('rest'), Symbol.for('clause1')]]], [Symbol.for('tcall'), Symbol.for('cond-helper'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('condition1'), [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('tcall'), Symbol.for('eval-t'), [Symbol.for('quasiquote'), [Symbol.for('truep'), [Symbol.for('unquote'), Symbol.for('condition1')]]], Symbol.for('env')]], Symbol.for('then-expr-1'), Symbol.for('clauses1'), Symbol.for('env')]]]];

/**
 * Evaluate an `(and ...)` form.
 */
function andSpecial_(exp: any, env: any): any {
  const params: any = exp.slice(1);
  let result: any = true;
  for (let operand of params) {
    result = eval_(operand, env);
    if (!eval_([Symbol.for('truep'), [Symbol.for('quote'), result]], env)) {
      return false;
    }
  }
  return result;
}

andSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('and-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('#t')], [Symbol.for('for'), [[Symbol.for('operand'), Symbol.for('params')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('operand'), Symbol.for('env')]], [Symbol.for('unless'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('truep'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('result')]]]], Symbol.for('env')], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('result')];

/**
 * Evaluate an `(or ...)` form.
 */
function orSpecial_(exp: any, env: any): any {
  const params: any = exp.slice(1);
  let result: any = false;
  for (let operand of params) {
    result = eval_(operand, env);
    if (eval_([Symbol.for('truep'), [Symbol.for('quote'), result]], env)) {
      return result;
    }
  }
  return result;
}

orSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('or-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('operand'), Symbol.for('params')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('operand'), Symbol.for('env')]], [Symbol.for('when'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('truep'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('result')]]]], Symbol.for('env')], [Symbol.for('return'), Symbol.for('result')]]], Symbol.for('result')];

/**
 * Evaluate a `(send ...)` form.
 */
function sendSpecial_(exp: any, env: any): any {
  let obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  obj = eval_(obj, env);
  let method: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  if (quotep(method)) {
    method = eval_(method, env);
  }
  let args: any = exp.slice(3);
  args = args.map(function (x: any): any {
    return eval_(x, env);
  });
  return sendMethod(obj, method, ...args);
}

sendSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('send-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('obj'), [Symbol.for('eval_'), Symbol.for('obj'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('quote?'), Symbol.for('method')], [Symbol.for('set!'), Symbol.for('method'), [Symbol.for('eval_'), Symbol.for('method'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('drop'), Symbol.for('exp'), 3]], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eval_'), Symbol.for('x'), Symbol.for('env')]], Symbol.for('args')]], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), Symbol.for('method'), Symbol.for('args')]];

/**
 * Evaluate a `(send/apply ...)` form.
 */
function sendApplySpecial_(exp: any, env: any): any {
  let obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  obj = eval_(obj, env);
  let method: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  if (quotep(method)) {
    method = eval_(method, env);
  }
  let args: any = exp.slice(3);
  args = args.map(function (x: any): any {
    return eval_(x, env);
  });
  if (args.length > 0) {
    args = [...args.slice(0, -1), ...args[args.length - 1]];
  }
  return sendMethod(obj, method, ...args);
}

sendApplySpecial_.lispSource = [Symbol.for('define'), [Symbol.for('send-apply-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('obj'), [Symbol.for('eval_'), Symbol.for('obj'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('quote?'), Symbol.for('method')], [Symbol.for('set!'), Symbol.for('method'), [Symbol.for('eval_'), Symbol.for('method'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('drop'), Symbol.for('exp'), 3]], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eval_'), Symbol.for('x'), Symbol.for('env')]], Symbol.for('args')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('append'), [Symbol.for('drop-right'), Symbol.for('args'), 1], [Symbol.for('array-list-last'), Symbol.for('args')]]]], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), Symbol.for('method'), Symbol.for('args')]];

/**
 * Call a method on an object.
 *
 * Helper function for `send-special_` and `send-apply-special_`.
 */
function sendMethod(...args: any[]): any {
  let [obj, method, ...restArgs]: any[] = args;
  if (typeof method === 'symbol') {
    return sendMethod(obj, method.description as string, ...restArgs);
  } else if (typeof method === 'string') {
    return sendMethod(obj, (obj as any)[method], ...restArgs);
  } else if (method instanceof Function) {
    return method.call(obj, ...restArgs);
  } else {
    throw new Error('Not a method: ' + method);
  }
}

sendMethod.lispSource = [Symbol.for('define'), [Symbol.for('send-method'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define-values'), [Symbol.for('obj'), Symbol.for('method'), Symbol.for('.'), Symbol.for('rest-args')], Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'symbol'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('symbol->string'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'string'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('is-a?'), Symbol.for('method'), Symbol.for('Function')], [Symbol.for('send/apply'), Symbol.for('method'), Symbol.for('call'), Symbol.for('obj'), Symbol.for('rest-args')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), [Symbol.for('string-append'), 'Not a method: ', Symbol.for('method')]]]]]];

/**
 * Evaluate a `(. ...)` form.
 */
function dotSpecial_(exp: any, env: any): any {
  let obj: any = eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
  let method: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  let field: any;
  let match: any;
  if ((typeof method === 'symbol') && (match = (method.description as string).match(new RegExp('^-(.*)$')))) {
    field = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(match);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = match;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = match[match.length - 1];
        } else {
          result = match.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : match[1];
    return eval_([Symbol.for('get-field'), Symbol.for(field), obj], env);
  } else {
    return sendSpecial_(exp, env);
  }
}

dotSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('dot-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('field')], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('method')], [Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], [Symbol.for('symbol->string'), Symbol.for('method')]]]], [Symbol.for('set!'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('send-special_'), Symbol.for('exp'), Symbol.for('env')]]]];

/**
 * Evaluate a `(get-field ...)` form.
 */
function getFieldSpecial_(exp: any, env: any): any {
  let field: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const fieldName: any = field.description as string;
  let obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  return (eval_(obj, env) as any)[fieldName];
}

getFieldSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('get-field-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('field-name'), [Symbol.for('symbol->string'), Symbol.for('field')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('~>'), [Symbol.for('eval_'), Symbol.for('obj'), Symbol.for('env')], [Symbol.for('oget'), Symbol.for('_'), Symbol.for('field-name')]]];

/**
 * Evaluate a `(js/optional-chaining ...)` form.
 */
function jsOptionalChainingSpecial_(exp: any, env: any): any {
  let obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let field: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  const fieldName: any = field.description as string;
  return (eval_(obj, env) as any)[fieldName];
}

jsOptionalChainingSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('js-optional-chaining-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('field-name'), [Symbol.for('symbol->string'), Symbol.for('field')]], [Symbol.for('~>'), [Symbol.for('eval_'), Symbol.for('obj'), Symbol.for('env')], [Symbol.for('oget'), Symbol.for('_'), Symbol.for('field-name')]]];

/**
 * Evaluate a `(set-field! ...)` form.
 */
function setFieldSpecial_(exp: any, env: any): any {
  let field: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2];
  const val: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 3;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[3];
  return (eval_(obj, env) as any)[field.description as string] = eval_(val, env);
}

setFieldSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('set-field-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('third'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('fourth'), Symbol.for('exp')]], [Symbol.for('oset!'), [Symbol.for('eval_'), Symbol.for('obj'), Symbol.for('env')], [Symbol.for('symbol->string'), Symbol.for('field')], [Symbol.for('eval_'), Symbol.for('val'), Symbol.for('env')]]];

/**
 * Evaluate a `(define-class ...)` form.
 */
function defineClassSpecial_(exp: any, env: any): any {
  const fields: any = [];
  const methods: any = [];
  const constructors: any = new Map();
  const constructor: any = function (this: any, ...args: any[]): any {
    // Initialize fields.
    for (let field of fields) {
      let name: any = ((Array.isArray(field) && (field.length >= 3) && (field[field.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(field);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = field;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = field[field.length - 1];
          } else {
            result = field.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : field[1]).description as string;
      const exp: any = (Array.isArray(field) && (field.length >= 3) && (field[field.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(field);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 2;
        let result: any = field;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = field[field.length - 1];
          } else {
            result = field.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : field[2];
      (this as any)[name] = eval_([Symbol.for('let'), [[Symbol.for('this'), [Symbol.for('quote'), this]]], exp], env);
    }
    const arity: any = args.length;
    const constructorFn: any = constructors.get(arity);
    if (constructorFn instanceof Function) {
      return constructorFn.call(this, ...args);
    }
  };
  const params: any = exp.slice(1);
  let definitions: any = params;
  // Get name of the class and, if specified, the base class it extends.
  // If no class name is specified, an anonymous class is created.
  const classNameSymbol: any = params[0];
  const className: any = (typeof classNameSymbol === 'symbol') ? (classNameSymbol.description as string) : '';
  let baseClass: any = undefined;
  if (className !== '') {
    definitions = definitions.slice(1);
  }
  const superClasses: any = definitions[0];
  if (((): any => {
    const x: any = lastCdr(superClasses);
    return Array.isArray(x) && (x.length === 0);
  })() && !taggedListP(superClasses, Symbol.for('define'))) {
    // The first form is a list of superclasses.
    definitions = definitions.slice(1);
    // JavaScript supports single inheritance only,
    // so only the first class is used.
    if (superClasses.length > 0) {
      baseClass = eval_(superClasses[0], env);
    }
  }
  if (baseClass) {
    // <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
    constructor['prototype'] = Object.create(baseClass['prototype']);
  }
  // Sort field definitions from method definitions.
  for (let definition of definitions) {
    if (typeof ((Array.isArray(definition) && (definition.length >= 3) && (definition[definition.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(definition);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = definition;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = definition[definition.length - 1];
        } else {
          result = definition.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : definition[1]) === 'symbol') {
      fields.push(definition);
    } else {
      methods.push(definition);
    }
  }
  for (let method of methods) {
    const defParams: any = method.slice(1);
    const defNameAndArgs: any = defParams[0];
    const defName: any = defNameAndArgs[0].description as string;
    const defArgs: any = defNameAndArgs.slice(1);
    const defBody: any = defParams.slice(1);
    const arity: any = defArgs.length;
    // Create a method function that binds JavaScript's
    // `this` value to the Lisp symbol `this`.
    const methodFn: any = function (this: any, ...args: any[]): any {
      const varExps: any = [];
      for (let i: any = 0; i < arity; i++) {
        const argExp: any = (defArgs as any)[i];
        let name: any = ((): any => {
          const x: any = lastCdr(argExp);
          return Array.isArray(x) && (x.length === 0);
        })() ? argExp[0] : argExp;
        const value: any = (i >= args.length) ? undefined : (args as any)[i];
        const varExp: any = [name, [Symbol.for('quote'), value]];
        varExps.push(varExp);
      }
      const thisExp: any = [Symbol.for('this'), [Symbol.for('quote'), this]];
      varExps.push(thisExp);
      const letExp: any = [Symbol.for('let*'), varExps, ...defBody];
      return eval_(letExp, env);
    };
    // A form on the form `(define (constructor ...) ...)`
    // or `(define/public (constructor ...) ...)` is
    // understood to define a constructor.
    // Cf. the constructor syntax of TypeScript:
    // <https://www.typescriptlang.org/docs/handbook/2/classes.html#constructors>
    if ((defName === 'constructor') || (defName === className)) {
      constructors.set(arity, methodFn);
    } else {
      (constructor.prototype as any)[defName] = methodFn;
    }
  }
  if (className !== '') {
    env.set(classNameSymbol, constructor, 'variable');
  }
  return constructor;
}

defineClassSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-class-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('methods'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('constructors'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('constructor'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('for'), [[Symbol.for('field'), Symbol.for('fields')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('symbol->string'), [Symbol.for('second'), Symbol.for('field')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('third'), Symbol.for('field')]], [Symbol.for('set!'), [Symbol.for('oget'), Symbol.for('this'), Symbol.for('name')], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('this'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('this')]]]], [Symbol.for('unquote'), Symbol.for('exp')]]], Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('arity'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('constructor-fn'), [Symbol.for('send'), Symbol.for('constructors'), Symbol.for('get'), Symbol.for('arity')]], [Symbol.for('when'), [Symbol.for('procedure?'), Symbol.for('constructor-fn')], [Symbol.for('send/apply'), Symbol.for('constructor-fn'), Symbol.for('call'), Symbol.for('this'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('definitions'), Symbol.for('params')], [Symbol.for('define'), Symbol.for('class-name-symbol'), [Symbol.for('first'), Symbol.for('params')]], [Symbol.for('define'), Symbol.for('class-name'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('class-name-symbol')], [Symbol.for('symbol->string'), Symbol.for('class-name-symbol')], '']], [Symbol.for('define'), Symbol.for('base-class'), Symbol.for('undefined')], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('class-name'), ''], [Symbol.for('set!'), Symbol.for('definitions'), [Symbol.for('rest'), Symbol.for('definitions')]]], [Symbol.for('define'), Symbol.for('super-classes'), [Symbol.for('first'), Symbol.for('definitions')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('list?'), Symbol.for('super-classes')], [Symbol.for('not'), [Symbol.for('tagged-list?'), Symbol.for('super-classes'), [Symbol.for('quote'), Symbol.for('define')]]]], [Symbol.for('set!'), Symbol.for('definitions'), [Symbol.for('rest'), Symbol.for('definitions')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('super-classes')], 0], [Symbol.for('set!'), Symbol.for('base-class'), [Symbol.for('eval_'), [Symbol.for('first'), Symbol.for('super-classes')], Symbol.for('env')]]]], [Symbol.for('when'), Symbol.for('base-class'), [Symbol.for('set!'), [Symbol.for('oget'), Symbol.for('constructor'), 'prototype'], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('create'), [Symbol.for('oget'), Symbol.for('base-class'), 'prototype']]]], [Symbol.for('for'), [[Symbol.for('definition'), Symbol.for('definitions')]], [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('definition')]], [Symbol.for('push-right!'), Symbol.for('fields'), Symbol.for('definition')], [Symbol.for('push-right!'), Symbol.for('methods'), Symbol.for('definition')]]], [Symbol.for('for'), [[Symbol.for('method'), Symbol.for('methods')]], [Symbol.for('define'), Symbol.for('def-params'), [Symbol.for('rest'), Symbol.for('method')]], [Symbol.for('define'), Symbol.for('def-name-and-args'), [Symbol.for('first'), Symbol.for('def-params')]], [Symbol.for('define'), Symbol.for('def-name'), [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('def-name-and-args')]]], [Symbol.for('define'), Symbol.for('def-args'), [Symbol.for('rest'), Symbol.for('def-name-and-args')]], [Symbol.for('define'), Symbol.for('def-body'), [Symbol.for('rest'), Symbol.for('def-params')]], [Symbol.for('define'), Symbol.for('arity'), [Symbol.for('array-list-length'), Symbol.for('def-args')]], [Symbol.for('define'), Symbol.for('method-fn'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('var-exps'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('arity')]]], [Symbol.for('define'), Symbol.for('arg-exp'), [Symbol.for('aget'), Symbol.for('def-args'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('if'), [Symbol.for('list?'), Symbol.for('arg-exp')], [Symbol.for('first'), Symbol.for('arg-exp')], Symbol.for('arg-exp')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('if'), [Symbol.for('>='), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], Symbol.for('undefined'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]]], [Symbol.for('define'), Symbol.for('var-exp'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('value')]]]]], [Symbol.for('push-right!'), Symbol.for('var-exps'), Symbol.for('var-exp')]], [Symbol.for('define'), Symbol.for('this-exp'), [Symbol.for('quasiquote'), [Symbol.for('this'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('this')]]]]], [Symbol.for('push-right!'), Symbol.for('var-exps'), Symbol.for('this-exp')], [Symbol.for('define'), Symbol.for('let-exp'), [Symbol.for('quasiquote'), [Symbol.for('let*'), [Symbol.for('unquote'), Symbol.for('var-exps')], [Symbol.for('unquote-splicing'), Symbol.for('def-body')]]]], [Symbol.for('eval_'), Symbol.for('let-exp'), Symbol.for('env')]]], [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('def-name'), 'constructor'], [Symbol.for('eq?'), Symbol.for('def-name'), Symbol.for('class-name')]], [Symbol.for('send'), Symbol.for('constructors'), Symbol.for('set'), Symbol.for('arity'), Symbol.for('method-fn')], [Symbol.for('set!'), [Symbol.for('oget'), [Symbol.for('get-field'), Symbol.for('prototype'), Symbol.for('constructor')], Symbol.for('def-name')], Symbol.for('method-fn')]]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('class-name'), ''], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('class-name-symbol'), Symbol.for('constructor'), 'variable']], Symbol.for('constructor')];

/**
 * Evaluate a `(try ...)` form.
 */
function trySpecial_(exp: any, env: any): any {
  const bodyClauses: any = [];
  const catchClauses: any = [];
  const finallyClauses: any = [];
  let result: any = undefined;
  let body: any;
  for (let x of exp.slice(1)) {
    if (taggedListP(x, Symbol.for('catch'))) {
      catchClauses.push(x);
    } else if (taggedListP(x, Symbol.for('finally'))) {
      finallyClauses.push(x);
    } else {
      bodyClauses.push(x);
    }
  }
  body = (bodyClauses.length === 1) ? bodyClauses[0] : [Symbol.for('begin'), ...bodyClauses];
  try {
    result = eval_(body, env);
  } catch (err) {
    for (let clause of catchClauses) {
      if (err instanceof eval_((Array.isArray(clause) && (clause.length >= 3) && (clause[clause.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(clause);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = clause;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = clause[clause.length - 1];
          } else {
            result = clause.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : clause[1], env)) {
        result = eval_([Symbol.for('let'), [[(Array.isArray(clause) && (clause.length >= 3) && (clause[clause.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(clause);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 2;
          let result: any = clause;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = clause[clause.length - 1];
            } else {
              result = clause.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : clause[2], [Symbol.for('quote'), err]]], ...clause.slice(3)], env);
        break;
      }
    }
  } finally {
    if (finallyClauses.length > 0) {
      eval_([Symbol.for('begin'), ...finallyClauses[0].slice(1)], env);
    }
  }
  return result;
}

trySpecial_.lispSource = [Symbol.for('define'), [Symbol.for('try-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('body-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('finally-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('body')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('drop'), Symbol.for('exp'), 1]]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('push-right!'), Symbol.for('catch-clauses'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('push-right!'), Symbol.for('finally-clauses'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-clauses'), Symbol.for('x')]]]], [Symbol.for('set!'), Symbol.for('body'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('body-clauses')], 1], [Symbol.for('first'), Symbol.for('body-clauses')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body-clauses')]]]]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), Symbol.for('body'), Symbol.for('env')]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('err'), [Symbol.for('for'), [[Symbol.for('clause'), Symbol.for('catch-clauses')]], [Symbol.for('when'), [Symbol.for('is-a?'), Symbol.for('err'), [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('clause')], Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), [Symbol.for('third'), Symbol.for('clause')]], [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('err')]]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('clause'), 3]]]], Symbol.for('env')]], [Symbol.for('break')]]]], [Symbol.for('finally'), [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('finally-clauses')], 0], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('drop'), [Symbol.for('first'), Symbol.for('finally-clauses')], 1]]]], Symbol.for('env')]]]], Symbol.for('result')];

/**
 * Evaluate a `(provide ...)` form.
 */
function provideSpecial_(exp: any, env: any): any {
  return undefined;
}

provideSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('provide-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(require ...)` form.
 */
function requireSpecial_(exp: any, env: any): any {
  return undefined;
}

requireSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('require-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate an `(ann ...)` form.
 */
function annSpecial_(exp: any, env: any): any {
  return eval_((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1], env);
}

annSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('ann-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('eval_'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env')]];

/**
 * Evaluate a `(colon ...)` form.
 */
function colonSpecial_(exp: any, env: any): any {
  return undefined;
}

colonSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('colon-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(define-type ...)` form.
 */
function defineTypeSpecial_(exp: any, env: any): any {
  return undefined;
}

defineTypeSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-type-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(let-js-obj ...)` form.
 */
function letJsObjSpecial_(exp: any, env: any): any {
  return undefined;
}

letJsObjSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('let-js-obj-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(define-js-obj ...)` form.
 */
function defineJsObjSpecial_(exp: any, env: any): any {
  return undefined;
}

defineJsObjSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('define-js-obj-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(set!-js-obj ...)` form.
 */
function setJsObjSpecial_(exp: any, env: any): any {
  return undefined;
}

setJsObjSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('set-js-obj-special_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Evaluate a `(let-env ...)` form.
 */
function letEnvSpecial_(exp: any, env: any): any {
  // Legacy function.
  const params: any = exp.slice(1);
  let body: any = params.slice(1);
  const letEnv: any = eval_(params[0], env);
  const combinedEnv: any = new EnvironmentStack(letEnv, env);
  return eval_([Symbol.for('begin'), ...body], combinedEnv);
}

letEnvSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('let-env-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('params')]], [Symbol.for('define'), Symbol.for('let-env'), [Symbol.for('eval_'), [Symbol.for('first'), Symbol.for('params')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('combined-env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('let-env'), Symbol.for('env')]], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], Symbol.for('combined-env')]];

/**
 * Evaluate a `(macrop ...)` form.
 *
 * Whether `macro` is a macro in the environment `env`.
 * `macro` may be a symbol, a string, or a macro function.
 */
function macropSpecial_(val: any): any {
  return eval_([Symbol.for('macrop'), val], currentEnvironment());
}

macropSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('macrop-special_'), Symbol.for('val')], [Symbol.for('eval_'), [Symbol.for('quasiquote'), [Symbol.for('macrop'), [Symbol.for('unquote'), Symbol.for('val')]]], [Symbol.for('current-environment')]]];

/**
 * Evaluate an `(nlambda ...)` form.
 */
function nlambdaSpecial_(exp: any, env: any): any {
  const f: any = lambdaSpecial_(exp, env);
  f.fexpr = true;
  return f;
}

nlambdaSpecial_.lispSource = [Symbol.for('define'), [Symbol.for('nlambda-special_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda-special_'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('set-field!'), Symbol.for('fexpr'), Symbol.for('f'), Symbol.for('#t')], Symbol.for('f')];

export {
  andSpecial_,
  annSpecial_,
  asyncSpecial_,
  awaitSpecial_,
  beginSpecial_,
  breakSpecial_,
  colonSpecial_,
  condSpecial_,
  continueSpecial_,
  defineAsyncSpecial_,
  defineClassSpecial_,
  defineGeneratorSpecial_,
  defineJsObjSpecial_,
  defineMacroSpecial_,
  definePublicSpecial_,
  defineSpecial_,
  defineTypeSpecial_,
  defineValuesSpecial_,
  defmacroSpecial_,
  dotSpecial_,
  forSpecial_,
  fsetSpecial_,
  getFieldSpecial_,
  jsArrowSpecial_,
  jsDoWhileSpecial_,
  jsFunctionSpecial_,
  jsOptionalChainingSpecial_,
  jsWhileSpecial_,
  lambdaSpecial_,
  letEnvSpecial_,
  letJsObjSpecial_,
  letStarSpecial_,
  letValuesSpecial_,
  macropSpecial_,
  moduleSpecial_,
  nlambdaSpecial_,
  orSpecial_,
  provideSpecial_,
  quasiquoteSpecial_,
  quoteSpecial_,
  requireSpecial_,
  returnSpecial_,
  sendApplySpecial_,
  sendSpecial_,
  setFieldSpecial_,
  setJsObjSpecial_,
  setSpecial_,
  setValuesSpecial_,
  setqSpecial_,
  throwSpecial_,
  trySpecial_,
  yieldSpecial_
};
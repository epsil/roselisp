/**
 * # TypeScript
 *
 * Tests of TypeScript constructs.
 */

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('ts/as', (): any => {
  it('(ts/as #t Any)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('ts/as'), true, Symbol.for('Any')], true]));
  it('(ts/as #f Any)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('ts/as'), false, Symbol.for('Any')], false]));
  it('(ts/as #u Any)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('ts/as'), undefined, Symbol.for('Any')], undefined]));
  it('(compile \'(ts/as #t Any))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), true, Symbol.for('Any')]]], 'true;']));
  it('(compile \'(ts/as #t Any) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), true, Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], 'true as any;']));
  it('(compile \'(ts/as 1 Number) :to "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), 1, Symbol.for('Number')]], Symbol.for(':to'), 'javascript'], '1;']));
  it('(compile \'(ts/as 1 Number) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), 1, Symbol.for('Number')]], Symbol.for(':to'), 'typescript'], '1 as number;']));
  it('(compile \'(ts/as (list) Any) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), [Symbol.for('list')], Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], '[] as any;']));
  it('(compile \'(ts/as \'() Any) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), [Symbol.for('quote'), []], Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], '[] as any;']));
  it('(compile \'(ts/as x (List Any)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), Symbol.for('x'), [Symbol.for('List'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as [any];']));
  it('(compile \'(ts/as x (List Number Any)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), Symbol.for('x'), [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as [number, any];']));
  it('(compile \'(ts/as x NN) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), Symbol.for('x'), Symbol.for('NN')]], Symbol.for(':to'), 'typescript'], 'x as NN;']));
  it('(compile \'(ts/as x (NN Any)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), Symbol.for('x'), [Symbol.for('NN'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as NN<any>;']));
  it('(compile \'(ts/as x (NN Any Any)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/as'), Symbol.for('x'), [Symbol.for('NN'), Symbol.for('Any'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as NN<any,any>;']));
  it('(compile \'((ts/as (lambda (x) x) Any) 1) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [[Symbol.for('ts/as'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], Symbol.for('Any')], 1]], Symbol.for(':to'), 'typescript'], '((x: any): any => x as any)(1);']));
  return it('(compile \'(lambda (x) (ts/as (send x foo) Any)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('ts/as'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('foo')], Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], '(x: any): any => x.foo() as any;']));
});

describe('ts/raw', (): any => {
  it('(compile \'(ts/raw "x as any;") :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/raw'), 'x as any;']], Symbol.for(':to'), 'typescript'], 'x as any;']));
  return it('(compile \'(ts/raw "function I(x: any): any { return x; }") :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ts/raw'), 'function I(x: any): any { return x; }']], Symbol.for(':to'), 'typescript'], 'function I(x: any): any { return x; }']));
});
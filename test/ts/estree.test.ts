import {
  parse
} from '@typescript-eslint/typescript-estree';

import {
  BinaryExpression
} from '../../src/ts/estree';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('ESTree', (): any => {
  xit('const hello: string = \'world\';', (): any => assertEqual(((): any => {
    const code: any = 'const hello: string = \'world\';';
    const ast: any = parse(code, {
      loc: true,
      range: true
    });
    return ast;
  })(), {}));
  xit('foo();', (): any => assertEqual(((): any => {
    const code: any = 'foo();';
    const ast: any = parse(code, {
      loc: true,
      range: true
    });
    return ast;
  })(), {}));
  xit('foo.bar();', (): any => assertEqual(((): any => {
    const code: any = 'foo.bar();';
    const ast: any = parse(code, {
      loc: true,
      range: true
    });
    return ast;
  })(), {}));
  return xit('let foo = function () { return undefined };', (): any => assertEqual(((): any => {
    const code: any = 'let foo = function () { return undefined };';
    const ast: any = parse(code, {
      loc: true,
      range: true
    });
    return ast;
  })(), {}));
});
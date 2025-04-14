import * as chai from 'chai';

import { parse } from '@typescript-eslint/typescript-estree';

import { BinaryExpression } from '../../src/ts/estree';

import { assertEqual } from './test-util';

describe('ESTree', function (): any {
  return describe('parsing', function (): any {
    xit("const hello: string = 'world';", function (): any {
      const code: any = "const hello: string = 'world';";
      const ast: any = parse(code, {
        loc: true,
        range: true,
      });
      return assertEqual(ast, {});
    });
    xit('foo();', function (): any {
      const code: any = 'foo();';
      const ast: any = parse(code, {
        loc: true,
        range: true,
      });
      return assertEqual(ast, {});
    });
    xit('foo.bar();', function (): any {
      const code: any = 'foo.bar();';
      const ast: any = parse(code, {
        loc: true,
        range: true,
      });
      return assertEqual(ast, {});
    });
    return xit('let foo = function () { return undefined };', function (): any {
      const code: any = 'let foo = function () { return undefined };';
      const ast: any = parse(code, {
        loc: true,
        range: true,
      });
      return assertEqual(ast, {});
    });
  });
});

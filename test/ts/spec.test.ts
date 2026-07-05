/**
 * # Test specification
 *
 * Tests expressed as a Roselisp REPL session.
 */

import { testRepl } from './test-util';

function printSexp(exp: any): any {
  if (exp === undefined) {
    return '#u';
  } else if (exp === null) {
    return '#n';
  } else if (typeof exp === 'boolean') {
    if (exp) {
      return '#t';
    } else {
      return '#f';
    }
  } else if (Array.isArray(exp) && !(Array.isArray(exp) && exp.length === 0)) {
    return (
      '(' +
      exp
        .map(function (x: any): any {
          return printSexp(x);
        })
        .join(' ') +
      ')'
    );
  } else if (typeof exp === 'string') {
    return (
      '"' +
      exp
        .replace(new RegExp('\\\\', 'g'), '\\\\')
        .replace(new RegExp('"', 'g'), '\\"') +
      '"'
    );
  } else if (typeof exp === 'symbol') {
    return exp.description as string;
  } else {
    return exp + '';
  }
}

function testMacro(exp: any, env: any): any {
  const body: any = exp.slice(1);
  let group: any = [];
  const groups: any = [];
  const _end: any = body.length;
  for (let i: any = 0; i < _end; i = i + 3) {
    const prompt: any = (body as any)[i];
    const expression: any = body[i + 1];
    const value: any = body[i + 2];
    if (
      Array.isArray(expression) &&
      expression.length === 2 &&
      expression[0] === Symbol.for('describe')
    ) {
      if (group.length > 0) {
        groups.push(group);
        group = [];
      }
      const description: any = expression[1];
      group.push(description);
    } else {
      const itDescription: any = printSexp(expression);
      const itExpression: any = [
        Symbol.for('it'),
        itDescription,
        [
          Symbol.for('fn'),
          [],
          [
            Symbol.for('test-repl'),
            [
              Symbol.for('quote'),
              [Symbol.for('roselisp'), prompt, expression, value],
            ],
          ],
        ],
      ];
      group.push(itExpression);
    }
  }
  if (group.length > 0) {
    groups.push(group);
  }
  const tests: any = groups.map(function (group: any): any {
    return [
      Symbol.for('describe'),
      group[0],
      [Symbol.for('fn'), [], ...group.slice(1)],
    ];
  });
  return [Symbol.for('begin'), ...tests];
}

testMacro.lispMacro = true;

describe('eq?', function (): any {
  it('(eq? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), true, true],
      true,
    ]);
  });
  it('(eq? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), false, false],
      true,
    ]);
  });
  return it('(eq? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), true, false],
      false,
    ]);
  });
});

describe('equal?', function (): any {
  it('(equal? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), true, true],
      true,
    ]);
  });
  it('(equal? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), false, false],
      true,
    ]);
  });
  return it('(equal? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), true, false],
      false,
    ]);
  });
});

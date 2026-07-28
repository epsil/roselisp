/**
 * # Test utilities tests
 *
 * Tests for the test utilities.
 */

import {
  assertEqual,
  compileReplForm,
  simplifyReplForm,
  testRepl,
  testMacro,
} from './test-util';

testMacro.ftype = 'macro';

describe('test-macro', function (): any {
  it('(funcall test-macro \'(test-macro > (describe "foo") _) #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [Symbol.for('describe'), 'foo', [Symbol.for('fn'), []]],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ > (foo) "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo')],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('it'),
              '(foo)',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo')], 'foo'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ > (define (foo x) x) _ > (foo "foo") "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('>'),
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo'), 'foo'],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('define'),
              [Symbol.for('foo'), Symbol.for('x')],
              Symbol.for('x'),
            ],
            [
              Symbol.for('it'),
              '(foo "foo")',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo'), 'foo'], 'foo'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ only> (foo) "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('only>'),
          [Symbol.for('foo')],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('send'),
              Symbol.for('it'),
              Symbol.for('only'),
              '(foo)',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo')], 'foo'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ > (it "Call foo with no arguments" (foo)) "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('it'), 'Call foo with no arguments', [Symbol.for('foo')]],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('it'),
              'Call foo with no arguments',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo')], 'foo'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ > (it "Call foo twice" (assert-equal (foo "foo") "foo") (assert-equal (foo "bar") "bar")) _) #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('>'),
          [
            Symbol.for('it'),
            'Call foo twice',
            [Symbol.for('assert-equal'), [Symbol.for('foo'), 'foo'], 'foo'],
            [Symbol.for('assert-equal'), [Symbol.for('foo'), 'bar'], 'bar'],
          ],
          Symbol.for('_'),
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('it'),
              'Call foo twice',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo'), 'foo'], 'foo'],
                [Symbol.for('assert-equal'), [Symbol.for('foo'), 'bar'], 'bar'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  it('(funcall test-macro \'(test-macro > (describe "foo") _ xit> (foo) "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('xit>'),
          [Symbol.for('foo')],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('xit'),
              '(foo)',
              [
                Symbol.for('fn'),
                [],
                [Symbol.for('assert-equal'), [Symbol.for('foo')], 'foo'],
              ],
            ],
          ],
        ],
      ]
    );
  });
  return it('(funcall test-macro \'(test-macro :repl #t > (describe "foo") _ > (foo) "foo") #u)', function (): any {
    return assertEqual(
      testMacro(
        [
          Symbol.for('test-macro'),
          Symbol.for(':repl'),
          true,
          Symbol.for('>'),
          [Symbol.for('describe'), 'foo'],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo')],
          'foo',
        ],
        undefined
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('describe'),
          'foo',
          [
            Symbol.for('fn'),
            [],
            [
              Symbol.for('it'),
              '(foo)',
              [
                Symbol.for('fn'),
                [],
                [
                  Symbol.for('test-repl'),
                  [
                    Symbol.for('quote'),
                    [
                      Symbol.for('roselisp'),
                      Symbol.for('>'),
                      [Symbol.for('foo')],
                      'foo',
                    ],
                  ],
                ],
              ],
            ],
          ],
        ],
      ]
    );
  });
});

describe('test-repl', function (): any {
  it('(> ...)', function (): any {
    return testRepl([Symbol.for('>'), [Symbol.for('+'), 1, 1], 2]);
  });
  it('(_ > ...)', function (): any {
    return testRepl([
      Symbol.for('_'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 1],
      2,
    ]);
  });
  it('(repl > ...)', function (): any {
    return testRepl([
      Symbol.for('repl'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 1],
      2,
    ]);
  });
  it('(shell > ...)', function (): any {
    return testRepl([
      Symbol.for('shell'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 1],
      2,
    ]);
  });
  it('(roselisp > ...)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 1],
      2,
    ]);
  });
  it('($ roselisp > ...)', function (): any {
    return testRepl([
      Symbol.for('$'),
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 1],
      2,
    ]);
  });
  it('(+ 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 2, 2],
      4,
    ]);
  });
  it('(list 1 2 3 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list'), 1, 2, 3, 4],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  it('1 + 1', function (): any {
    return testRepl([Symbol.for('node'), Symbol.for('>'), '1 + 1', '2']);
  });
  return xit('const n = 1', function (): any {
    return testRepl([
      Symbol.for('node'),
      Symbol.for('>'),
      'const n = 1',
      'undefined',
      Symbol.for('>'),
      'n + 1',
      '2',
    ]);
  });
});

describe('compile-repl-form', function (): any {
  it('(roselisp > (+ 1 1) 2)', function (): any {
    return assertEqual(
      compileReplForm(
        [Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 1], 2],
        {
          from: 'roselisp',
          to: 'node',
        }
      ),
      [
        Symbol.for('node'),
        Symbol.for('>'),
        '(function () {\n' + '  return 1 + 1;\n' + '})()',
        '(function () {\n' + '  return 2;\n' + '})()',
      ]
    );
  });
  it('(roselisp > (+ 1 1) _)', function (): any {
    return assertEqual(
      compileReplForm(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('+'), 1, 1],
          Symbol.for('_'),
        ],
        {
          from: 'roselisp',
          to: 'node',
        }
      ),
      [
        Symbol.for('node'),
        Symbol.for('>'),
        '(function () {\n' + '  return 1 + 1;\n' + '})()',
        '_',
      ]
    );
  });
  return xit('(roselisp > (+ 1 1) 2), plist', function (): any {
    return assertEqual(
      compileReplForm(
        [Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 1], 2],
        Symbol.for(':from'),
        'roselisp',
        Symbol.for(':to'),
        'node'
      ),
      [
        Symbol.for('node'),
        Symbol.for('>'),
        '(function () {\n' + '  return 1 + 1;\n' + '})()',
        '(function () {\n' + '  return 2;\n' + '})()',
      ]
    );
  });
});

describe('simplify-repl-form', function (): any {
  return it('(roselisp > (+ 1 1) 2)', function (): any {
    return assertEqual(
      simplifyReplForm([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
        Symbol.for('>'),
        [Symbol.for('+'), 2, 2],
        4,
      ]),
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('begin'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]],
        4,
      ]
    );
  });
});

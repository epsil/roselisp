import * as chai from 'chai';

import {
  ArrowFunctionExpression,
  AssignmentExpression,
  AssignmentPattern,
  BinaryExpression,
  BlockStatement,
  CallExpression,
  ExportAllDeclaration,
  ExpressionStatement,
  ForStatement,
  FunctionExpression,
  Identifier,
  IfStatement,
  LeadingComment,
  Literal,
  LogicalExpression,
  MemberExpression,
  ObjectExpression,
  ReturnStatement,
  TSAnyKeyword,
  TSAsExpression,
  TSFunctionType,
  TSNumberKeyword,
  TSTypeAliasDeclaration,
  TSTypeAnnotation,
  TSTypeParameterInstantiation,
  TSTypeReference,
  TaggedTemplateExpression,
  TemplateElement,
  TemplateLiteral,
  TrailingComment,
  VariableDeclaration,
  VariableDeclarator,
  WhileStatement,
} from '../../src/ts/estree';

import { printEstree, writeToString } from '../../src/ts/printer';

import { s, sexp } from '../../src/ts/sexp';

import { assertEqual } from './test-util';

describe('print-estree', function (): any {
  describe('ESTree', function (): any {
    describe('Identifier', function (): any {
      it('foo', function (): any {
        return assertEqual(printEstree(new Identifier('foo')), 'foo');
      });
      it('foo, leading comment', function (): any {
        return assertEqual(
          printEstree(
            new Identifier('foo').addComment(new LeadingComment('comment')),
            {
              comments: true,
            }
          ),
          '// comment\n' + 'foo'
        );
      });
      return it('foo, multi-line comment', function (): any {
        return assertEqual(
          printEstree(
            new Identifier('foo').addComment(
              new LeadingComment('multi-line\n' + 'comment')
            ),
            {
              comments: true,
            }
          ),
          '// multi-line\n' + '// comment\n' + 'foo'
        );
      });
    });
    describe('FunctionExpression', function (): any {
      return it('function (x) { return x; }', function (): any {
        return assertEqual(
          printEstree(
            new FunctionExpression(
              [new Identifier('x')],
              new BlockStatement([new ReturnStatement(new Identifier('x'))])
            )
          ),
          'function (x) {\n' + '  return x;\n' + '}'
        );
      });
    });
    describe('CallExpression', function (): any {
      it('foo()', function (): any {
        return assertEqual(
          printEstree(new CallExpression(new Identifier('foo'))),
          'foo()'
        );
      });
      it('foo(1)', function (): any {
        return assertEqual(
          printEstree(
            new CallExpression(new Identifier('foo'), [new Literal(1)])
          ),
          'foo(1)'
        );
      });
      it('foo(1, 2)', function (): any {
        return assertEqual(
          printEstree(
            new CallExpression(new Identifier('foo'), [
              new Literal(1),
              new Literal(2),
            ])
          ),
          'foo(1, 2)'
        );
      });
      it('foo.bar()', function (): any {
        return assertEqual(
          printEstree(
            new CallExpression(
              new MemberExpression(
                new Identifier('foo'),
                new Identifier('bar')
              ),
              []
            )
          ),
          'foo.bar()'
        );
      });
      return it('({}).foo()', function (): any {
        return assertEqual(
          printEstree(
            new CallExpression(
              new MemberExpression(
                new ObjectExpression(),
                new Identifier('foo')
              ),
              []
            )
          ),
          '({}).foo()'
        );
      });
    });
    describe('BinaryExpression', function (): any {
      it('a + b', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression('+', new Identifier('a'), new Identifier('b'))
          ),
          'a + b'
        );
      });
      it('a + b, leading comment', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '+',
              new Identifier('a').addComment(new LeadingComment('comment')),
              new Identifier('b')
            ),
            {
              comments: true,
            }
          ),
          '(\n' + ' // comment\n' + ' a +\n' + ' b\n' + ')'
        );
      });
      it('(a + b) + c, leading comment', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '+',
              new BinaryExpression(
                '+',
                new Identifier('a'),
                new Identifier('b')
              ).addComment(new LeadingComment('comment')),
              new Identifier('c')
            ),
            {
              comments: true,
            }
          ),
          '(\n' + ' // comment\n' + ' a + b +\n' + ' c\n' + ')'
        );
      });
      it('(a + b) + c, trailing comment', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '+',
              new BinaryExpression(
                '+',
                new Identifier('a'),
                new Identifier('b')
              ).addComment(new TrailingComment('comment')),
              new Identifier('c')
            ),
            {
              comments: true,
            }
          ),
          '(\n' + ' a + b // comment\n' + ' +\n' + ' c\n' + ')'
        );
      });
      it('a + b + c', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '+',
              new BinaryExpression(
                '+',
                new Identifier('a'),
                new Identifier('b')
              ),
              new Identifier('c')
            )
          ),
          'a + b + c'
        );
      });
      xit('a + b + c + d, leading comments', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '+',
              new BinaryExpression(
                '+',
                new BinaryExpression(
                  '+',
                  new Identifier('a'),
                  new Identifier('b')
                ),
                new Identifier('c')
              ),
              new Identifier('d')
            )
          ),
          'a + b + c'
        );
      });
      it('a < b', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression('<', new Identifier('a'), new Identifier('b'))
          ),
          'a < b'
        );
      });
      return it('a < b < c', function (): any {
        return assertEqual(
          printEstree(
            new BinaryExpression(
              '<',
              new BinaryExpression(
                '<',
                new Identifier('a'),
                new Identifier('b')
              ),
              new Identifier('c')
            )
          ),
          'a < b < c'
        );
      });
    });
    describe('LogicalExpression', function (): any {
      it('a && b', function (): any {
        return assertEqual(
          printEstree(
            new LogicalExpression(
              '&&',
              new Identifier('a'),
              new Identifier('b')
            )
          ),
          'a && b'
        );
      });
      return it('a || b', function (): any {
        return assertEqual(
          printEstree(
            new LogicalExpression(
              '||',
              new Identifier('a'),
              new Identifier('b')
            )
          ),
          'a || b'
        );
      });
    });
    describe('IfStatement', function (): any {
      it('if (x) { x = 1; }', function (): any {
        return assertEqual(
          printEstree(
            new IfStatement(
              new Identifier('x'),
              new BlockStatement([
                new ExpressionStatement(
                  new AssignmentExpression(
                    '=',
                    new Identifier('x'),
                    new Literal(1)
                  )
                ),
              ])
            )
          ),
          'if (x) {\n' + '  x = 1;\n' + '}'
        );
      });
      return it('if ((x = 1)) { x = 1; }', function (): any {
        return assertEqual(
          printEstree(
            new IfStatement(
              new AssignmentExpression(
                '=',
                new Identifier('x'),
                new Literal(1)
              ),
              new BlockStatement([
                new ExpressionStatement(
                  new AssignmentExpression(
                    '=',
                    new Identifier('x'),
                    new Literal(1)
                  )
                ),
              ])
            )
          ),
          'if ((x = 1)) {\n' + '  x = 1;\n' + '}'
        );
      });
    });
    describe('WhileStatement', function (): any {
      it('while (x) { x = 1; }', function (): any {
        return assertEqual(
          printEstree(
            new WhileStatement(
              new Identifier('x'),
              new BlockStatement([
                new ExpressionStatement(
                  new AssignmentExpression(
                    '=',
                    new Identifier('x'),
                    new Literal(1)
                  )
                ),
              ])
            )
          ),
          'while (x) {\n' + '  x = 1;\n' + '}'
        );
      });
      return it('while ((x = 1)) { x = 1; }', function (): any {
        return assertEqual(
          printEstree(
            new WhileStatement(
              new AssignmentExpression(
                '=',
                new Identifier('x'),
                new Literal(1)
              ),
              new BlockStatement([
                new ExpressionStatement(
                  new AssignmentExpression(
                    '=',
                    new Identifier('x'),
                    new Literal(1)
                  )
                ),
              ])
            )
          ),
          'while ((x = 1)) {\n' + '  x = 1;\n' + '}'
        );
      });
    });
    describe('ForStatement', function (): any {
      return it('for (i = 0; i < 10; i = i + 1) { x = 1; }', function (): any {
        return assertEqual(
          printEstree(
            new ForStatement(
              new AssignmentExpression(
                '=',
                new Identifier('i'),
                new Literal(0)
              ),
              new BinaryExpression('<', new Identifier('i'), new Literal(10)),
              new AssignmentExpression(
                '=',
                new Identifier('i'),
                new BinaryExpression('+', new Identifier('i'), new Literal(1))
              ),
              new BlockStatement([
                new ExpressionStatement(
                  new AssignmentExpression(
                    '=',
                    new Identifier('x'),
                    new Literal(1)
                  )
                ),
              ])
            )
          ),
          'for (i = 0; i < 10; i = i + 1) {\n' + '  x = 1;\n' + '}'
        );
      });
    });
    return describe('ReturnStatement', function (): any {
      it('return 0;', function (): any {
        return assertEqual(
          printEstree(new ReturnStatement(new Literal(0))),
          'return 0;'
        );
      });
      return it('return ( ... );', function (): any {
        return assertEqual(
          printEstree(
            new ReturnStatement(
              new Literal(0).addComment(new LeadingComment('comment'))
            ),
            {
              comments: true,
            }
          ),
          'return (\n' + '  // comment\n' + '  0\n' + ');'
        );
      });
    });
  });
  describe('TSESTree', function (): any {
    describe('TSTypeAliasDeclaration', function (): any {
      return it('const x: number = 1;', function (): any {
        return assertEqual(
          printEstree(
            new TSTypeAliasDeclaration(
              new Identifier('X'),
              new TSNumberKeyword()
            ),
            {
              language: 'TypeScript',
            }
          ),
          'type X = number;'
        );
      });
    });
    return describe('TSAsExpression', function (): any {
      it('1 as number', function (): any {
        return assertEqual(
          printEstree(
            new TSAsExpression(new Literal(1), new TSNumberKeyword()),
            {
              language: 'TypeScript',
            }
          ),
          '1 as number'
        );
      });
      it('x as any', function (): any {
        return assertEqual(
          printEstree(
            new TSAsExpression(new Identifier('x'), new TSAnyKeyword()),
            {
              language: 'TypeScript',
            }
          ),
          'x as any'
        );
      });
      it('x as Foo', function (): any {
        return assertEqual(
          printEstree(
            new TSAsExpression(
              new Identifier('x'),
              new TSTypeReference(new Identifier('Foo'))
            ),
            {
              language: 'TypeScript',
            }
          ),
          'x as Foo'
        );
      });
      return it('x as Promise<any>', function (): any {
        return assertEqual(
          printEstree(
            new TSAsExpression(
              new Identifier('x'),
              new TSTypeReference(
                new Identifier('Promise'),
                new TSTypeParameterInstantiation([new TSAnyKeyword()])
              )
            ),
            {
              language: 'TypeScript',
            }
          ),
          'x as Promise<any>'
        );
      });
    });
  });
  describe('VariableDeclaration', function (): any {
    return it('const x: number = 1;', function (): any {
      return assertEqual(
        printEstree(
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('x').setType(new TSNumberKeyword()),
                new Literal(1)
              ),
            ],
            'const'
          ),
          {
            language: 'TypeScript',
          }
        ),
        'const x: number = 1;'
      );
    });
  });
  describe('FunctionExpression', function (): any {
    it('function (x: number): number { return x; }', function (): any {
      return assertEqual(
        printEstree(
          new FunctionExpression(
            [new Identifier('x').setType(new TSNumberKeyword())],
            new BlockStatement([new ReturnStatement(new Identifier('x'))])
          ).setType(new TSNumberKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        'function (x: number): number {\n' + '  return x;\n' + '}'
      );
    });
    it('function (x: number = 1): number { return x; }', function (): any {
      return assertEqual(
        printEstree(
          new FunctionExpression(
            [
              new AssignmentPattern(
                new Identifier('x').setType(new TSNumberKeyword()),
                new Literal(1)
              ),
            ],
            new BlockStatement([new ReturnStatement(new Identifier('x'))])
          ).setType(new TSNumberKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        'function (x: number = 1): number {\n' + '  return x;\n' + '}'
      );
    });
    return it('function (x: number = y): number { return x; }', function (): any {
      return assertEqual(
        printEstree(
          new FunctionExpression(
            [
              new AssignmentPattern(
                new Identifier('x').setType(new TSNumberKeyword()),
                new Identifier('y')
              ),
            ],
            new BlockStatement([new ReturnStatement(new Identifier('x'))])
          ).setType(new TSNumberKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        'function (x: number = y): number {\n' + '  return x;\n' + '}'
      );
    });
  });
  describe('ArrowFunctionExpression', function (): any {
    it('function (x: number): number { return x; }', function (): any {
      return assertEqual(
        printEstree(
          new ArrowFunctionExpression(
            [new Identifier('x').setType(new TSNumberKeyword())],
            new BlockStatement([new ReturnStatement(new Identifier('x'))])
          ).setType(new TSNumberKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        '(x: number): number => {\n' + '  return x;\n' + '}'
      );
    });
    return it('const f: (a: any) => any = (x: any): any => { return x; };', function (): any {
      return assertEqual(
        printEstree(
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('f').setType(
                  new TSFunctionType(
                    [new Identifier('a').setType(new TSAnyKeyword())],
                    new TSTypeAnnotation(new TSAnyKeyword())
                  )
                ),
                new ArrowFunctionExpression(
                  [new Identifier('x')],
                  new BlockStatement([new ReturnStatement(new Identifier('x'))])
                )
              ),
            ],
            'const'
          ),
          {
            language: 'TypeScript',
          }
        ),
        'const f: (a: any) => any = (x: any): any => {\n' +
          '  return x;\n' +
          '};'
      );
    });
  });
  describe('TemplateLiteral', function (): any {
    it('`foo`', function (): any {
      return assertEqual(
        printEstree(new TemplateLiteral([new TemplateElement(true, 'foo')]), {
          language: 'TypeScript',
        }),
        '`foo`'
      );
    });
    it('`foo\n' + 'bar`', function (): any {
      return assertEqual(
        printEstree(
          new TemplateLiteral([new TemplateElement(true, 'foo\n' + 'bar')]),
          {
            language: 'TypeScript',
          }
        ),
        '`foo\n' + 'bar`'
      );
    });
    it('`foo\n' + '\\`bar`', function (): any {
      return assertEqual(
        printEstree(
          new TemplateLiteral([new TemplateElement(true, 'foo\n' + '`bar')]),
          {
            language: 'TypeScript',
          }
        ),
        '`foo\n' + '\\`bar`'
      );
    });
    return it('function (): any { return `foo\n' + 'bar`; }', function (): any {
      return assertEqual(
        printEstree(
          new FunctionExpression(
            [],
            new BlockStatement([
              new ReturnStatement(
                new TemplateLiteral([
                  new TemplateElement(true, 'foo\n' + 'bar'),
                ])
              ),
            ])
          ).setType(new TSAnyKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        'function (): any {\n' + '  return `foo\n' + 'bar`;\n' + '}'
      );
    });
  });
  describe('TaggedTemplateExpression', function (): any {
    it('foo`bar`', function (): any {
      return assertEqual(
        printEstree(
          new TaggedTemplateExpression(
            new Identifier('foo'),
            new TemplateLiteral([new TemplateElement(true, 'bar')])
          ),
          {
            language: 'TypeScript',
          }
        ),
        'foo`bar`'
      );
    });
    it('foo`bar\n' + 'baz`', function (): any {
      return assertEqual(
        printEstree(
          new TaggedTemplateExpression(
            new Identifier('foo'),
            new TemplateLiteral([new TemplateElement(true, 'bar\n' + 'baz')])
          ),
          {
            language: 'TypeScript',
          }
        ),
        'foo`bar\n' + 'baz`'
      );
    });
    it('function (): any { return foo`bar\n' + 'baz`; }', function (): any {
      return assertEqual(
        printEstree(
          new FunctionExpression(
            [],
            new BlockStatement([
              new ReturnStatement(
                new TaggedTemplateExpression(
                  new Identifier('foo'),
                  new TemplateLiteral([
                    new TemplateElement(true, 'bar\n' + 'baz'),
                  ])
                )
              ),
            ])
          ).setType(new TSAnyKeyword()),
          {
            language: 'TypeScript',
          }
        ),
        'function (): any {\n' + '  return foo`bar\n' + 'baz`;\n' + '}'
      );
    });
    return it(
      'function (): any { return function (): any { return foo`bar\n' +
        'baz`; }; }',
      function (): any {
        return assertEqual(
          printEstree(
            new FunctionExpression(
              [],
              new BlockStatement([
                new ReturnStatement(
                  new FunctionExpression(
                    [],
                    new BlockStatement([
                      new ReturnStatement(
                        new TaggedTemplateExpression(
                          new Identifier('foo'),
                          new TemplateLiteral([
                            new TemplateElement(true, 'bar\n' + 'baz'),
                          ])
                        )
                      ),
                    ])
                  ).setType(new TSAnyKeyword())
                ),
              ])
            ).setType(new TSAnyKeyword()),
            {
              language: 'TypeScript',
            }
          ),
          'function (): any {\n' +
            '  return function (): any {\n' +
            '    return foo`bar\n' +
            'baz`;\n' +
            '  };\n' +
            '}'
        );
      }
    );
  });
  return describe('ExportAllDeclaration', function (): any {
    return it('export * from "foo";', function (): any {
      return assertEqual(
        printEstree(new ExportAllDeclaration(new Literal('foo')), {
          language: 'JavaScript',
        }),
        "export * from 'foo';"
      );
    });
  });
});

describe('write-to-string', function (): any {
  describe('symbol', function (): any {
    return it('foo', function (): any {
      return assertEqual(writeToString(Symbol.for('foo')), 'foo');
    });
  });
  describe('string', function (): any {
    it('"foo"', function (): any {
      return assertEqual(writeToString('foo'), '"foo"');
    });
    it('"foo\n' + 'bar"', function (): any {
      return assertEqual(
        writeToString([Symbol.for('begin'), 'foo\n' + 'bar'], {
          pretty: true,
        }),
        '(begin\n' + '  "foo\n' + 'bar")'
      );
    });
    return it('"\\"foo bar\\""', function (): any {
      return assertEqual(
        writeToString([Symbol.for('begin'), '"foo bar"'], {
          pretty: true,
        }),
        '(begin\n' + '  "\\"foo bar\\"")'
      );
    });
  });
  describe('number', function (): any {
    return it('1', function (): any {
      return assertEqual(writeToString(1), '1');
    });
  });
  describe('function call', function (): any {
    it('(foo)', function (): any {
      return assertEqual(writeToString([Symbol.for('foo')]), '(foo)');
    });
    it('(foo bar)', function (): any {
      return assertEqual(
        writeToString([Symbol.for('foo'), Symbol.for('bar')]),
        '(foo bar)'
      );
    });
    it('(foo "bar")', function (): any {
      return assertEqual(
        writeToString([Symbol.for('foo'), 'bar']),
        '(foo "bar")'
      );
    });
    it('("foo" "bar")', function (): any {
      return assertEqual(writeToString(['foo', 'bar']), '("foo" "bar")');
    });
    it('(foo (bar))', function (): any {
      return assertEqual(
        writeToString([Symbol.for('foo'), [Symbol.for('bar')]]),
        '(foo (bar))'
      );
    });
    return it('(foo (bar (baz)))', function (): any {
      return assertEqual(
        writeToString([
          Symbol.for('foo'),
          [Symbol.for('bar'), [Symbol.for('baz')]],
        ]),
        '(foo (bar (baz)))'
      );
    });
  });
  describe('begin', function (): any {
    it('(begin (foo) (bar)), pretty', function (): any {
      return assertEqual(
        writeToString(
          [Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]],
          {
            pretty: true,
          }
        ),
        '(begin\n' + '  (foo)\n' + '  (bar))'
      );
    });
    return it('(begin (foo (bar)) (bar (baz))), pretty', function (): any {
      return assertEqual(
        writeToString(
          [
            Symbol.for('begin'),
            [Symbol.for('foo'), [Symbol.for('bar')]],
            [Symbol.for('bar'), [Symbol.for('baz')]],
          ],
          {
            pretty: true,
          }
        ),
        '(begin\n' + '  (foo (bar))\n' + '  (bar (baz)))'
      );
    });
  });
  describe('cond', function (): any {
    return it('(cond (foo (bar)) (bar (baz))), pretty', function (): any {
      return assertEqual(
        writeToString(
          [
            Symbol.for('cond'),
            [Symbol.for('foo'), [Symbol.for('bar')]],
            [Symbol.for('bar'), [Symbol.for('baz')]],
          ],
          {
            pretty: true,
          }
        ),
        '(cond\n' + ' (foo\n' + '  (bar))\n' + ' (bar\n' + '  (baz)))'
      );
    });
  });
  describe('if', function (): any {
    return it('(if foo bar baz), pretty', function (): any {
      return assertEqual(
        writeToString(
          [
            Symbol.for('if'),
            Symbol.for('foo'),
            Symbol.for('bar'),
            Symbol.for('baz'),
          ],
          {
            pretty: true,
          }
        ),
        '(if foo\n' + '    bar\n' + '    baz)'
      );
    });
  });
  describe('when', function (): any {
    return it('(when foo bar), pretty', function (): any {
      return assertEqual(
        writeToString(
          [Symbol.for('when'), Symbol.for('foo'), Symbol.for('bar')],
          {
            pretty: true,
          }
        ),
        '(when foo\n' + '  bar)'
      );
    });
  });
  describe('unless', function (): any {
    return it('(unless foo bar), pretty', function (): any {
      return assertEqual(
        writeToString(
          [Symbol.for('unless'), Symbol.for('foo'), Symbol.for('bar')],
          {
            pretty: true,
          }
        ),
        '(unless foo\n' + '  bar)'
      );
    });
  });
  describe('define', function (): any {
    return it('(define (foo x) x), pretty', function (): any {
      return assertEqual(
        writeToString(
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          {
            pretty: true,
          }
        ),
        '(define (foo x)\n' + '  x)'
      );
    });
  });
  describe(
    'module',
    function (): any {
      return it('(module m scheme ... (define ...) ...), pretty', function (): any {
        return assertEqual(
          writeToString(
            [
              Symbol.for('module'),
              Symbol.for('m'),
              Symbol.for('scheme'),
              [
                Symbol.for('define'),
                [Symbol.for('foo'), Symbol.for('x')],
                Symbol.for('x'),
              ],
              [
                Symbol.for('define'),
                [Symbol.for('bar'), Symbol.for('y')],
                Symbol.for('y'),
              ],
            ],
            {
              pretty: true,
            }
          ),
          '(module m scheme\n' +
            '  (define (foo x)\n' +
            '    x)\n' +
            '\n' +
            '  (define (bar y)\n' +
            '    y))'
        );
      });
    },
    it('(module m scheme ... (define ...) ...), pretty', function (): any {
      return assertEqual(
        writeToString(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('scheme'),
            [
              Symbol.for('define'),
              [Symbol.for('foo'), Symbol.for('x')],
              Symbol.for('x'),
            ],
            [
              Symbol.for('define'),
              [Symbol.for('bar'), Symbol.for('y')],
              Symbol.for('y'),
            ],
          ],
          {
            noModuleForm: true,
            pretty: true,
          }
        ),
        '(define (foo x)\n' + '  x)\n' + '\n' + '(define (bar y)\n' + '  y)'
      );
    })
  );
  return describe('unsorted', function (): any {
    it('number 1', function (): any {
      return assertEqual(writeToString(1), '1');
    });
    it('symbol foo', function (): any {
      return assertEqual(writeToString(Symbol.for('foo')), 'foo');
    });
    it('string "foo"', function (): any {
      return assertEqual(writeToString('foo'), '"foo"');
    });
    it('string "foo\\bar"', function (): any {
      return assertEqual(writeToString('foo\\bar'), '"foo\\\\bar"');
    });
    it('string "\\"', function (): any {
      return assertEqual(writeToString('\\'), '"\\\\"');
    });
    it('string "foo\\"bar"', function (): any {
      return assertEqual(writeToString('foo"bar'), '"foo\\"bar"');
    });
    it('list ()', function (): any {
      return assertEqual(writeToString([]), '()');
    });
    return it('cons cell (1 . 2)', function (): any {
      return assertEqual(writeToString([1, Symbol.for('.'), 2]), '(1 . 2)');
    });
  });
});

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

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('print-estree', function (): any {
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
  it('foo, multi-line comment', function (): any {
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
  it('function (x) { return x; }', function (): any {
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
  it('foo()', function (): any {
    return assertEqual(
      printEstree(new CallExpression(new Identifier('foo'))),
      'foo()'
    );
  });
  it('foo(1)', function (): any {
    return assertEqual(
      printEstree(new CallExpression(new Identifier('foo'), [new Literal(1)])),
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
          new MemberExpression(new Identifier('foo'), new Identifier('bar')),
          []
        )
      ),
      'foo.bar()'
    );
  });
  it('({}).foo()', function (): any {
    return assertEqual(
      printEstree(
        new CallExpression(
          new MemberExpression(new ObjectExpression(), new Identifier('foo')),
          []
        )
      ),
      '({}).foo()'
    );
  });
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
          new BinaryExpression('+', new Identifier('a'), new Identifier('b')),
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
            new BinaryExpression('+', new Identifier('a'), new Identifier('b')),
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
  it('a < b < c', function (): any {
    return assertEqual(
      printEstree(
        new BinaryExpression(
          '<',
          new BinaryExpression('<', new Identifier('a'), new Identifier('b')),
          new Identifier('c')
        )
      ),
      'a < b < c'
    );
  });
  it('a && b', function (): any {
    return assertEqual(
      printEstree(
        new LogicalExpression('&&', new Identifier('a'), new Identifier('b'))
      ),
      'a && b'
    );
  });
  it('a || b', function (): any {
    return assertEqual(
      printEstree(
        new LogicalExpression('||', new Identifier('a'), new Identifier('b'))
      ),
      'a || b'
    );
  });
  it('if (x) { x = 1; }', function (): any {
    return assertEqual(
      printEstree(
        new IfStatement(
          new Identifier('x'),
          new BlockStatement([
            new ExpressionStatement(
              new AssignmentExpression('=', new Identifier('x'), new Literal(1))
            ),
          ])
        )
      ),
      'if (x) {\n' + '  x = 1;\n' + '}'
    );
  });
  it('if ((x = 1)) { x = 1; }', function (): any {
    return assertEqual(
      printEstree(
        new IfStatement(
          new AssignmentExpression('=', new Identifier('x'), new Literal(1)),
          new BlockStatement([
            new ExpressionStatement(
              new AssignmentExpression('=', new Identifier('x'), new Literal(1))
            ),
          ])
        )
      ),
      'if ((x = 1)) {\n' + '  x = 1;\n' + '}'
    );
  });
  it('while (x) { x = 1; }', function (): any {
    return assertEqual(
      printEstree(
        new WhileStatement(
          new Identifier('x'),
          new BlockStatement([
            new ExpressionStatement(
              new AssignmentExpression('=', new Identifier('x'), new Literal(1))
            ),
          ])
        )
      ),
      'while (x) {\n' + '  x = 1;\n' + '}'
    );
  });
  it('while ((x = 1)) { x = 1; }', function (): any {
    return assertEqual(
      printEstree(
        new WhileStatement(
          new AssignmentExpression('=', new Identifier('x'), new Literal(1)),
          new BlockStatement([
            new ExpressionStatement(
              new AssignmentExpression('=', new Identifier('x'), new Literal(1))
            ),
          ])
        )
      ),
      'while ((x = 1)) {\n' + '  x = 1;\n' + '}'
    );
  });
  it('for (i = 0; i < 10; i = i + 1) { x = 1; }', function (): any {
    return assertEqual(
      printEstree(
        new ForStatement(
          new AssignmentExpression('=', new Identifier('i'), new Literal(0)),
          new BinaryExpression('<', new Identifier('i'), new Literal(10)),
          new AssignmentExpression(
            '=',
            new Identifier('i'),
            new BinaryExpression('+', new Identifier('i'), new Literal(1))
          ),
          new BlockStatement([
            new ExpressionStatement(
              new AssignmentExpression('=', new Identifier('x'), new Literal(1))
            ),
          ])
        )
      ),
      'for (i = 0; i < 10; i = i + 1) {\n' + '  x = 1;\n' + '}'
    );
  });
  it('(print-estree (new ReturnStatement (new Literal 0)))', function (): any {
    return assertEqual(
      printEstree(new ReturnStatement(new Literal(0))),
      'return 0;'
    );
  });
  it('return ( ... );', function (): any {
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
  it('const x: number = 1;', function (): any {
    return assertEqual(
      printEstree(
        new TSTypeAliasDeclaration(new Identifier('X'), new TSNumberKeyword()),
        {
          language: 'typescript',
        }
      ),
      'type X = number;'
    );
  });
  it('1 as number', function (): any {
    return assertEqual(
      printEstree(new TSAsExpression(new Literal(1), new TSNumberKeyword()), {
        language: 'typescript',
      }),
      '1 as number'
    );
  });
  it('x as any', function (): any {
    return assertEqual(
      printEstree(new TSAsExpression(new Identifier('x'), new TSAnyKeyword()), {
        language: 'typescript',
      }),
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
          language: 'typescript',
        }
      ),
      'x as Foo'
    );
  });
  it('x as Promise<any>', function (): any {
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
          language: 'typescript',
        }
      ),
      'x as Promise<any>'
    );
  });
  it('const x: number = 1;', function (): any {
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
          language: 'typescript',
        }
      ),
      'const x: number = 1;'
    );
  });
  it('function (x: number): number { return x; }', function (): any {
    return assertEqual(
      printEstree(
        new FunctionExpression(
          [new Identifier('x').setType(new TSNumberKeyword())],
          new BlockStatement([new ReturnStatement(new Identifier('x'))])
        ).setType(new TSNumberKeyword()),
        {
          language: 'typescript',
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
          language: 'typescript',
        }
      ),
      'function (x: number = 1): number {\n' + '  return x;\n' + '}'
    );
  });
  it('function (x: number = y): number { return x; }', function (): any {
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
          language: 'typescript',
        }
      ),
      'function (x: number = y): number {\n' + '  return x;\n' + '}'
    );
  });
  it('function (x: number): number { return x; }', function (): any {
    return assertEqual(
      printEstree(
        new ArrowFunctionExpression(
          [new Identifier('x').setType(new TSNumberKeyword())],
          new BlockStatement([new ReturnStatement(new Identifier('x'))])
        ).setType(new TSNumberKeyword()),
        {
          language: 'typescript',
        }
      ),
      '(x: number): number => {\n' + '  return x;\n' + '}'
    );
  });
  it('const f: (a: any) => any = (x: any): any => { return x; };', function (): any {
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
          language: 'typescript',
        }
      ),
      'const f: (a: any) => any = (x: any): any => {\n' + '  return x;\n' + '};'
    );
  });
  it('`foo`', function (): any {
    return assertEqual(
      printEstree(new TemplateLiteral([new TemplateElement(true, 'foo')]), {
        language: 'typescript',
      }),
      '`foo`'
    );
  });
  it('`foo\n' + 'bar`', function (): any {
    return assertEqual(
      printEstree(
        new TemplateLiteral([new TemplateElement(true, 'foo\n' + 'bar')]),
        {
          language: 'typescript',
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
          language: 'typescript',
        }
      ),
      '`foo\n' + '\\`bar`'
    );
  });
  it('function (): any { return `foo\n' + 'bar`; }', function (): any {
    return assertEqual(
      printEstree(
        new FunctionExpression(
          [],
          new BlockStatement([
            new ReturnStatement(
              new TemplateLiteral([new TemplateElement(true, 'foo\n' + 'bar')])
            ),
          ])
        ).setType(new TSAnyKeyword()),
        {
          language: 'typescript',
        }
      ),
      'function (): any {\n' + '  return `foo\n' + 'bar`;\n' + '}'
    );
  });
  it('foo`bar`', function (): any {
    return assertEqual(
      printEstree(
        new TaggedTemplateExpression(
          new Identifier('foo'),
          new TemplateLiteral([new TemplateElement(true, 'bar')])
        ),
        {
          language: 'typescript',
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
          language: 'typescript',
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
          language: 'typescript',
        }
      ),
      'function (): any {\n' + '  return foo`bar\n' + 'baz`;\n' + '}'
    );
  });
  it(
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
            language: 'typescript',
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
  return it('export * from "foo";', function (): any {
    return assertEqual(
      printEstree(new ExportAllDeclaration(new Literal('foo')), {
        language: 'javascript',
      }),
      "export * from 'foo';"
    );
  });
});

describe('write-to-string', function (): any {
  it('(write-to-string 1)', function (): any {
    return assertEqual(writeToString(1), '1');
  });
  it("(write-to-string 'foo)", function (): any {
    return assertEqual(writeToString(Symbol.for('foo')), 'foo');
  });
  it('(write-to-string "foo")', function (): any {
    return assertEqual(writeToString('foo'), '"foo"');
  });
  it('(write-to-string "foo")', function (): any {
    return assertEqual(writeToString('foo'), '"foo"');
  });
  it('(write-to-string "foo\\\\bar")', function (): any {
    return assertEqual(writeToString('foo\\bar'), '"foo\\\\bar"');
  });
  it('(write-to-string "\\\\")', function (): any {
    return assertEqual(writeToString('\\'), '"\\\\"');
  });
  it('(write-to-string "foo\\"bar")', function (): any {
    return assertEqual(writeToString('foo"bar'), '"foo\\"bar"');
  });
  it("(write-to-string '())", function (): any {
    return assertEqual(writeToString([]), '()');
  });
  it("(write-to-string '(1 . 2))", function (): any {
    return assertEqual(writeToString([1, Symbol.for('.'), 2]), '(1 . 2)');
  });
  it(
    '(write-to-string \'(begin "foo\n' + 'bar") (js/obj :pretty #t))',
    function (): any {
      return assertEqual(
        writeToString([Symbol.for('begin'), 'foo\n' + 'bar'], {
          pretty: true,
        }),
        '(begin\n' + '  "foo\n' + 'bar")'
      );
    }
  );
  it('(write-to-string \'(begin "\\"foo bar\\"") (js/obj :pretty #t))', function (): any {
    return assertEqual(
      writeToString([Symbol.for('begin'), '"foo bar"'], {
        pretty: true,
      }),
      '(begin\n' + '  "\\"foo bar\\"")'
    );
  });
  it('(write-to-string 1)', function (): any {
    return assertEqual(writeToString(1), '1');
  });
  it("(write-to-string '(foo))", function (): any {
    return assertEqual(writeToString([Symbol.for('foo')]), '(foo)');
  });
  it("(write-to-string '(foo bar))", function (): any {
    return assertEqual(
      writeToString([Symbol.for('foo'), Symbol.for('bar')]),
      '(foo bar)'
    );
  });
  it('(write-to-string \'(foo "bar"))', function (): any {
    return assertEqual(
      writeToString([Symbol.for('foo'), 'bar']),
      '(foo "bar")'
    );
  });
  it('(write-to-string \'("foo" "bar"))', function (): any {
    return assertEqual(writeToString(['foo', 'bar']), '("foo" "bar")');
  });
  it("(write-to-string '(foo (bar)))", function (): any {
    return assertEqual(
      writeToString([Symbol.for('foo'), [Symbol.for('bar')]]),
      '(foo (bar))'
    );
  });
  it("(write-to-string '(foo (bar (baz))))", function (): any {
    return assertEqual(
      writeToString([
        Symbol.for('foo'),
        [Symbol.for('bar'), [Symbol.for('baz')]],
      ]),
      '(foo (bar (baz)))'
    );
  });
  it("(write-to-string '(begin (foo) (bar)) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(begin (foo (bar)) (bar (baz))) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(cond (foo (bar)) (bar (baz))) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(if foo bar baz) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(when foo bar) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(unless foo bar) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(define (foo x) x) (js/obj :pretty #t))", function (): any {
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
  it("(write-to-string '(module m scheme (define (foo x) x) (define (bar y) y)) (js/obj :pretty #t))", function (): any {
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
  return it("(write-to-string '(module m scheme (define (foo x) x) (define (bar y) y)) (js/obj :no-module-form #t :pretty #t))", function (): any {
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
  });
});

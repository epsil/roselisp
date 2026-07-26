import {
  ArrayExpression,
  ArrayPattern,
  AssignmentExpression,
  ExpressionStatement,
  Identifier,
  Literal,
  Program,
  UpdateExpression,
  VariableDeclaration,
  VariableDeclarator,
} from '../../src/ts/estree';

import {
  letVarsToConstVars,
  lispEnvironment,
  optimizeSexp,
  sexp,
} from '../../src/ts/language';

import { makeRose } from '../../src/ts/rose';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('optimize-sexp', function (): any {
  it("(optimize-sexp '() lisp-environment)", function (): any {
    return assertEqual(optimizeSexp([], lispEnvironment), []);
  });
  xit("(optimize-sexp '(let ((x 1)) x) lisp-environment)", function (): any {
    return assertEqual(
      optimizeSexp(
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
        lispEnvironment
      ),
      [
        Symbol.for('begin'),
        [Symbol.for('define'), Symbol.for('x'), 1],
        Symbol.for('x'),
      ]
    );
  });
  xit("(optimize-sexp '(let-values (((y) (foo))) y) lisp-environment)", function (): any {
    return assertEqual(
      optimizeSexp(
        [
          Symbol.for('let-values'),
          [[[Symbol.for('y')], [Symbol.for('foo')]]],
          Symbol.for('y'),
        ],
        lispEnvironment
      ),
      [
        Symbol.for('begin'),
        [Symbol.for('define-values'), [Symbol.for('y')], [Symbol.for('foo')]],
        Symbol.for('y'),
      ]
    );
  });
  xit("(optimize-sexp '(define (f x) (let-values (((y) (foo))) y)) lisp-environment)", function (): any {
    return assertEqual(
      optimizeSexp(
        [
          Symbol.for('define'),
          [Symbol.for('f'), Symbol.for('x')],
          [
            Symbol.for('let-values'),
            [[[Symbol.for('y')], [Symbol.for('foo')]]],
            Symbol.for('y'),
          ],
        ],
        lispEnvironment
      ),
      [
        Symbol.for('define'),
        [Symbol.for('f'), Symbol.for('x')],
        [
          Symbol.for('begin'),
          [Symbol.for('define-values'), [Symbol.for('y')], [Symbol.for('foo')]],
          Symbol.for('y'),
        ],
      ]
    );
  });
  xit("(optimize-sexp '(define (f x) `(let-values (((y) (foo))) y)) lisp-environment)", function (): any {
    return assertEqual(
      optimizeSexp(
        [
          Symbol.for('define'),
          [Symbol.for('f'), Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('let-values'),
              [[[Symbol.for('y')], [Symbol.for('foo')]]],
              Symbol.for('y'),
            ],
          ],
        ],
        lispEnvironment
      ),
      [
        Symbol.for('define'),
        [Symbol.for('f'), Symbol.for('x')],
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('let-values'),
            [[[Symbol.for('y')], [Symbol.for('foo')]]],
            Symbol.for('y'),
          ],
        ],
      ]
    );
  });
  return xit("(optimize-sexp '(define (make-macro-function-form exp) (let* ((name (second exp)) (args (third exp)) (body (drop exp 3))) (when (list? name) (set! args (rest name)) (set! name (first name)) (set! body (drop exp 2))) `(lambda (exp env) (let-values ((,args (rest exp))) ,@body)))) lisp-environment)", function (): any {
    return assertEqual(
      optimizeSexp(
        [
          Symbol.for('define'),
          [Symbol.for('make-macro-function-form'), Symbol.for('exp')],
          [
            Symbol.for('let*'),
            [
              [Symbol.for('name'), [Symbol.for('second'), Symbol.for('exp')]],
              [Symbol.for('args'), [Symbol.for('third'), Symbol.for('exp')]],
              [Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 3]],
            ],
            [
              Symbol.for('when'),
              [Symbol.for('list?'), Symbol.for('name')],
              [
                Symbol.for('set!'),
                Symbol.for('args'),
                [Symbol.for('rest'), Symbol.for('name')],
              ],
              [
                Symbol.for('set!'),
                Symbol.for('name'),
                [Symbol.for('first'), Symbol.for('name')],
              ],
              [
                Symbol.for('set!'),
                Symbol.for('body'),
                [Symbol.for('drop'), Symbol.for('exp'), 2],
              ],
            ],
            [
              Symbol.for('quasiquote'),
              [
                Symbol.for('lambda'),
                [Symbol.for('exp'), Symbol.for('env')],
                [
                  Symbol.for('let-values'),
                  [
                    [
                      [Symbol.for('unquote'), Symbol.for('args')],
                      [Symbol.for('rest'), Symbol.for('exp')],
                    ],
                  ],
                  [Symbol.for('unquote-splicing'), Symbol.for('body')],
                ],
              ],
            ],
          ],
        ],
        lispEnvironment
      ),
      [
        Symbol.for('define'),
        [Symbol.for('make-macro-function-form'), Symbol.for('exp')],
        [
          Symbol.for('begin'),
          [
            Symbol.for('define'),
            Symbol.for('name'),
            [Symbol.for('second'), Symbol.for('exp')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('args'),
            [Symbol.for('third'), Symbol.for('exp')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('body'),
            [Symbol.for('drop'), Symbol.for('exp'), 3],
          ],
          [
            Symbol.for('when'),
            [Symbol.for('list?'), Symbol.for('name')],
            [
              Symbol.for('set!'),
              Symbol.for('args'),
              [Symbol.for('rest'), Symbol.for('name')],
            ],
            [
              Symbol.for('set!'),
              Symbol.for('name'),
              [Symbol.for('first'), Symbol.for('name')],
            ],
            [
              Symbol.for('set!'),
              Symbol.for('body'),
              [Symbol.for('drop'), Symbol.for('exp'), 2],
            ],
          ],
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('lambda'),
              [Symbol.for('exp'), Symbol.for('env')],
              [
                Symbol.for('let-values'),
                [
                  [
                    [Symbol.for('unquote'), Symbol.for('args')],
                    [Symbol.for('rest'), Symbol.for('exp')],
                  ],
                ],
                [Symbol.for('unquote-splicing'), Symbol.for('body')],
              ],
            ],
          ],
        ],
      ]
    );
  });
});

describe('let-vars-to-const-vars', function (): any {
  it('empty program', function (): any {
    return assertEqual(letVarsToConstVars(new Program()), new Program());
  });
  it('single uninitialized let variable', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('foo'))],
            'let'
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [new VariableDeclarator(new Identifier('foo'))],
          'let'
        ),
      ])
    );
  });
  it('single let variable', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
            'let'
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
          'const'
        ),
      ])
    );
  });
  it('single let variable with assignment', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
            'let'
          ),
          new ExpressionStatement(
            new AssignmentExpression('=', new Identifier('foo'), new Literal(2))
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
          'let'
        ),
        new ExpressionStatement(
          new AssignmentExpression('=', new Identifier('foo'), new Literal(2))
        ),
      ])
    );
  });
  it('single let variable with destructuring initialization', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new ArrayPattern([new Identifier('foo')]),
                new ArrayExpression([new Literal(1)])
              ),
            ],
            'let'
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [
            new VariableDeclarator(
              new ArrayPattern([new Identifier('foo')]),
              new ArrayExpression([new Literal(1)])
            ),
          ],
          'const'
        ),
      ])
    );
  });
  it('single let variable with destructuring initialization and subsequent assignment', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new ArrayPattern([new Identifier('foo')]),
                new ArrayExpression([new Literal(1)])
              ),
            ],
            'let'
          ),
          new ExpressionStatement(
            new AssignmentExpression('=', new Identifier('foo'), new Literal(2))
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [
            new VariableDeclarator(
              new ArrayPattern([new Identifier('foo')]),
              new ArrayExpression([new Literal(1)])
            ),
          ],
          'let'
        ),
        new ExpressionStatement(
          new AssignmentExpression('=', new Identifier('foo'), new Literal(2))
        ),
      ])
    );
  });
  it('single let variable with assignment by update expression', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
            'let'
          ),
          new ExpressionStatement(
            new UpdateExpression('++', new Identifier('foo'), false)
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
          'let'
        ),
        new ExpressionStatement(
          new UpdateExpression('++', new Identifier('foo'), false)
        ),
      ])
    );
  });
  return it('single let variable with destructuring assignment', function (): any {
    return assertEqual(
      letVarsToConstVars(
        new Program([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
            'let'
          ),
          new ExpressionStatement(
            new AssignmentExpression(
              '=',
              new ArrayPattern([new Identifier('foo')]),
              new ArrayExpression([new Literal(2)])
            )
          ),
        ])
      ),
      new Program([
        new VariableDeclaration(
          [new VariableDeclarator(new Identifier('foo'), new Literal(1))],
          'let'
        ),
        new ExpressionStatement(
          new AssignmentExpression(
            '=',
            new ArrayPattern([new Identifier('foo')]),
            new ArrayExpression([new Literal(2)])
          )
        ),
      ])
    );
  });
});

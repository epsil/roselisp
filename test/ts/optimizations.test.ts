import * as chai from 'chai';

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

import { assertEqual } from './test-util';

describe('optimize-sexp', function (): any {
  it('()', function (): any {
    return assertEqual(optimizeSexp([], lispEnvironment), []);
  });
  xit('(let ((x 1)) x)', function (): any {
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
  xit('(let-values (((y) (foo))) y)', function (): any {
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
  xit('(define (f x) (let-values (((y) (foo))) y))', function (): any {
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
  xit('(define (f x) `(let-values (((y) (foo))) y))', function (): any {
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
  return xit('(define (make-macro-function-form exp) ...)', function (): any {
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

// (describe "rewrite-define-to-define-class"
//   (fn ()
//     (xit "()"
//          (fn ()
//            (assert-equal
//             (rewrite-define-to-define-class
//              (make-rose
//               '())
//              lisp-environment)
//             (make-rose
//              '()))))
//     (xit "(define Foo (class ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-define-to-define-class
//              (make-rose
//               '(define Foo
//                  (class object%
//                    (define/public (foo)
//                      this))))
//              lisp-environment)
//             (make-rose
//              '(define-class Foo ()
//                 (define/public (foo)
//                   this))))))))

// (describe "rewrite-let-to-begin-define"
//   (fn ()
//     (xit "()"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose '())
//              lisp-environment)
//             (make-rose '()))))
//     (xit "(let (x) x)"
//         (fn ()
//           (assert-equal
//            (rewrite-let-to-begin-define
//             (make-rose
//              '(let (x)
//                 x))
//             lisp-environment)
//            (make-rose
//             '(begin
//                (define x)
//                x)))))
//     (xit "(let ((x 1)) x)"
//         (fn ()
//           (assert-equal
//            (rewrite-let-to-begin-define
//             (make-rose
//              '(let ((x 1))
//                 x))
//             lisp-environment)
//            (make-rose
//             '(begin
//                (define x 1)
//                x)))))
//     (xit "(lambda (x) (let ((y 1)) y))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(lambda (x)
//                  (let ((y 1))
//                    y)))
//              lisp-environment)
//             (make-rose
//              '(lambda (x)
//                 (begin
//                   (define y 1)
//                   y))))))
//     (xit "(lambda (x) (let ((x 1)) x))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(lambda (x)
//                  (let ((x 1))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(lambda (x)
//                 (let ((x 1))
//                   x))))))
//     (xit "(define (f x) (let ((x 1)) x))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(define (f x)
//                  (let ((x 1))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(define (f x)
//                 (let ((x 1))
//                   x))))))
//     (xit "(let ((x 1)) (let ((x 2)) x))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(let ((x 1))
//                  (let ((x 2))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(begin
//                 (define x 1)
//                 (let ((x 2))
//                   x))))))
//     (xit "(for ((x xs)) (let ((x 2)) x))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(for ((x xs))
//                  (let ((x 1))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(for ((x xs))
//                 (let ((x 1))
//                   x))))))
//     (xit "(for ((x xs)) (let ((x 2)) x))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-to-begin-define
//              (make-rose
//               '(for ((x xs))
//                  (let ((y 1))
//                    y)))
//              lisp-environment)
//             (make-rose
//              '(for ((x xs))
//                 (begin
//                   (define y 1)
//                   y))))))))
//
// (describe "rewrite-let-values-to-begin-define-values"
//   (fn ()
//     (xit "()"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '())
//              lisp-environment)
//             (make-rose
//              '()))))
//     (xit "(let-values (((x) (foo))) x)"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(let-values (((x) (foo)))
//                  x))
//              lisp-environment)
//             (make-rose
//              '(begin
//                 (define-values (x) (foo))
//                 x)))))
//     (xit "(lambda (x) (let-values ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(lambda (x)
//                  (let-values (((x) (foo)))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(lambda (x)
//                 (let-values (((x) (foo)))
//                   x))))))
//     (xit "(define (f x) (let-values ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(define (f x)
//                  (let-values (((x) (foo)))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(define (f x)
//                 (let-values (((x) (foo)))
//                   x))))))
//     (xit "(let ((x 1)) (let-values (((x) ...)) ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(let ((x 1))
//                  (let-values (((x) (foo)))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(let ((x 1))
//                 (let-values (((x) (foo)))
//                   x))))))
//     (xit "(let ((x 1)) (let-values (((y) ...)) ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(let ((x 1))
//                  (let-values (((y) (foo)))
//                    y)))
//              lisp-environment)
//             (make-rose
//              '(let ((x 1))
//                 (begin
//                   (define-values (y) (foo))
//                   y))))))
//     (xit "(for ((x xs)) (let-values (((x) ...)) ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(for ((x xs))
//                  (let-values (((x) (foo)))
//                    x)))
//              lisp-environment)
//             (make-rose
//              '(for ((x xs))
//                 (let-values (((x) (foo)))
//                   x))))))
//     (xit "(for ((x xs)) (let-values (((y) ...)) ...))"
//          (fn ()
//            (assert-equal
//             (rewrite-let-values-to-begin-define-values
//              (make-rose
//               '(for ((x xs))
//                  (let-values (((y) (foo)))
//                    y)))
//              lisp-environment)
//             (make-rose
//              '(for ((x xs))
//                 (begin
//                   (define-values (y) (foo))
//                   y))))))))

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

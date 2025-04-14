;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # ESTree implementation
;;;
;;; A collection of ESTree node classes.
;;;
;;; ## Description
;;;
;;; This is a simple implementation of a subset of the [ESTree
;;; specification][github:estree]. It defines a class for each ESTree
;;; node type, permitting ESTree nodes to be instantiated with `new`.
;;; For example,
;;;
;;;     (new BlockStatement
;;;          (list ...))
;;;
;;; amounts to the same as
;;;
;;;     (js-obj "type" "BlockStatement"
;;;             "body" (list ...))
;;;
;;; Objects instantiated with `new` are compatible with objects
;;; created with `js-obj`.
;;;
;;; There are no private fields. For maximum compatibility with
;;; existing implementations, every field and method is public.
;;;
;;; This implementation also provides support for
;;; [TSESTree][npm:typescript-estree]-compatible
;;; [type annotations][github:estree-types].
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [github:estree]: https://github.com/estree/estree
;;; [github:estree-types]: https://github.com/estree/estree/blob/master/extensions/type-annotations.md
;;; [npm:typescript-estree] https://www.npmjs.com/package/@typescript-eslint/typescript-estree

;;; Node
;;;
;;; Creates an ESTree [`Node`][estree:node] node.
;;;
;;; This is a base class.
;;;
;;; [estree:node]: https://github.com/estree/estree/blob/master/es5.md#node-objects
(define-class Node ()
  (define/public type "Node")
  (define/public comments '())
  (define/public loc js/null)

  (define/public (add-comment comment)
    (push-right! (get-field comments this) comment)
    this))

;;; TSNode
;;;
;;; Creates an TSESTree `TSNode` node.
;;; Like `Node`, but with type annotations.
;;;
;;; This is a base class.
;;;
;;; Cf. the [`@typescript-eslint/typescript-estree`
;;; documentation][doc:typescript-estree].
;;;
;;; [doc:typescript-estree]: https://typescript-eslint.io/packages/typescript-estree/
(define-class TSNode (Node)
  (define/public type "TSNode")
  (define/public typeAnnotation)

  (define/public (get-type)
    (send this get-type-annotation))

  (define/public (get-type-annotation)
    (get-field typeAnnotation this))

  (define/public (has-type)
    (and (send this get-type)
         #t))

  (define/public (set-type type_)
    (send this set-type-annotation type_))

  (define/public (set-type-annotation typeAnnotation)
    (set-field! typeAnnotation this typeAnnotation)
    this))

;;; Expression
;;;
;;; Creates an ESTree [`Expression`][estree:expression] node.
;;;
;;; This is a base class.
;;;
;;; [estree:expression]: https://github.com/estree/estree/blob/master/es5.md#expressions
(define-class Expression (TSNode)
  (define/public type "Expression"))

;;; Program
;;;
;;; Creates an ESTree [`Program`][estree:program] node.
;;;
;;; [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
(define-class Program (TSNode)
  (define/public type "Program")
  (define/public body)

  (define/public (constructor body)
    (super)
    (set-field! body this body)))

;;; Statement
;;;
;;; Creates an ESTree [`Statement`][estree:statement] node.
;;;
;;; [estree:statement]: https://github.com/estree/estree/blob/master/es5.md#statements
(define-class Statement (TSNode)
  (define/public type "Statement"))

;;; IfStatement
;;;
;;; Creates an ESTree [`IfStatement`][estree:ifstatement] node.
;;;
;;; [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
(define-class IfStatement (Statement)
  (define/public type "IfStatement")
  (define/public test)
  (define/public consequent)
  (define/public alternate)

  (define/public (constructor test consequent alternate)
    (super)
    (set-field! test this test)
    (set-field! consequent this consequent)
    (set-field! alternate this alternate)))

;;; BlockStatement
;;;
;;; Creates an ESTree [`BlockStatement`][estree:blockstatement] node.
;;;
;;; [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
(define-class BlockStatement (Statement)
  (define/public type "BlockStatement")
  (define/public body)

  (define/public (constructor (body '()))
    (super)
    (set-field! body this body)))

;;; ExpressionStatement
;;;
;;; Creates an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
;;;
;;; [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
(define-class ExpressionStatement (Statement)
  (define/public type "ExpressionStatement")
  (define/public expression)

  (define/public (constructor expression)
    (super)
    (set-field! expression this expression)))

;;; ReturnStatement
;;;
;;; Creates an ESTree [`ReturnStatement`][estree:returnstatement] node.
;;;
;;; [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
(define-class ReturnStatement (Statement)
  (define/public type "ReturnStatement")
  (define/public argument)

  (define/public (constructor (argument js/null))
    (super)
    (set-field! argument this argument)))

;;; ThrowStatement
;;;
;;; Creates an ESTree [`ThrowStatement`][estree:throwstatement] node.
;;;
;;; [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
(define-class ThrowStatement (Statement)
  (define/public type "ThrowStatement")
  (define/public argument)

  (define/public (constructor argument)
    (super)
    (set-field! argument this argument)))

;;; BreakStatement
;;;
;;; Creates an ESTree [`BreakStatement`][estree:breakstatement] node.
;;;
;;; [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
(define-class BreakStatement (Statement)
  (define/public type "BreakStatement")
  (define/public label)

  (define/public (constructor (label js/null))
    (super)
    (set-field! label this label)))

;;; ContinueStatement
;;;
;;; Creates an ESTree [`ContinueStatement`][estree:continuestatement] node.
;;;
;;; [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
(define-class ContinueStatement (Statement)
  (define/public type "ContinueStatement")
  (define/public label)

  (define/public (constructor (label js/null))
    (super)
    (set-field! label this label)))

;;; WhileStatement
;;;
;;; Creates an ESTree [`WhileStatement`][estree:whilestatement] node.
;;;
;;; [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
(define-class WhileStatement (Statement)
  (define/public type "WhileStatement")
  (define/public test)
  (define/public body)

  (define/public (constructor test body)
    (super)
    (set-field! test this test)
    (set-field! body this body)))

;;; DoWhileStatement
;;;
;;; Creates an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
;;;
;;; [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
(define-class DoWhileStatement (Statement)
  (define/public type "DoWhileStatement")
  (define/public test)
  (define/public body)

  (define/public (constructor test body)
    (super)
    (set-field! test this test)
    (set-field! body this body)))

;;; ForStatement
;;;
;;; Creates an ESTree [`ForStatement`][estree:forstatement] node.
;;;
;;; [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
(define-class ForStatement (Statement)
  (define/public type "ForStatement")
  (define/public init)
  (define/public test)
  (define/public update)
  (define/public body)

  (define/public (constructor init test update body)
    (super)
    (set-field! init this init)
    (set-field! test this test)
    (set-field! update this update)
    (set-field! body this body)))

;;; ForInStatement
;;;
;;; Creates an ESTree [`ForInStatement`][estree:forinstatement] node.
;;;
;;; [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
(define-class ForInStatement (Statement)
  (define/public type "ForInStatement")
  (define/public left)
  (define/public right)
  (define/public body)

  (define/public (constructor left right body)
    (super)
    (set-field! left this left)
    (set-field! right this right)
    (set-field! body this body)))

;;; ForOfStatement
;;;
;;; Creates an ESTree [`ForOfStatement`][estree:forofstatement] node.
;;;
;;; [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
(define-class ForOfStatement (ForInStatement)
  (define/public type "ForOfStatement")
  (define/public left)
  (define/public right)
  (define/public body)

  (define/public (constructor left right body)
    (super left right body)
    (set-field! left this left)
    (set-field! right this right)
    (set-field! body this body)))

;;; TryStatement
;;;
;;; Creates an ESTree [`TryStatement`][estree:trystatement] node.
;;;
;;; [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
(define-class TryStatement (Statement)
  (define/public type "TryStatement")
  (define/public block)
  (define/public handler)
  (define/public finalizer)

  (define/public (constructor block (handler js/null) (finalizer js/null))
    (super)
    (set-field! block this block)
    (set-field! handler this handler)
    (set-field! finalizer this finalizer)))

;;; CatchClause
;;;
;;; Creates an ESTree [`CatchClause`][estree:catchclause] node.
;;;
;;; [estree:catchclause]: https://github.com/estree/estree/blob/master/es5.md#catchclause
(define-class CatchClause (TSNode)
  (define/public type "CatchClause")
  (define/public param)
  (define/public body)

  (define/public (constructor param body)
    (super)
    (set-field! param this param)
    (set-field! body this body)))

;;; Declaration
;;;
;;; Creates an ESTree [`Declaration`][estree:declaration] node.
;;;
;;; [estree:declaration]: https://github.com/estree/estree/blob/master/es5.md#declarations
(define-class Declaration (Statement)
  (define/public type "Declaration"))

;;; VariableDeclaration
;;;
;;; Creates an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
;;;
;;; [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
(define-class VariableDeclaration (Declaration)
  (define/public type "VariableDeclaration")
  (define/public declarations)
  (define/public kind)

  (define/public (constructor declarations (kind "var"))
    (super)
    (set-field! declarations this declarations)
    (set-field! kind this kind)))

;;; VariableDeclarator
;;;
;;; Creates an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
;;;
;;; [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
(define-class VariableDeclarator (TSNode)
  (define/public type "VariableDeclarator")
  (define/public id)
  (define/public init)

  (define/public (constructor id (init js/null))
    (super)
    (set-field! id this id)
    (set-field! init this init)))

;;; ClassDeclaration
;;;
;;; Creates an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
;;;
;;; [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
(define-class ClassDeclaration (Declaration)
  (define/public type "ClassDeclaration")
  (define/public id)
  (define/public body)
  (define/public superClass)

  (define/public (constructor id (body (new ClassBody)) (super-class undefined))
    (super)
    (set-field! id this id)
    (set-field! body this body)
    (set-field! superClass this super-class)))

;;; ImportOrExportDeclaration
;;;
;;; Creates an ESTree [`ImportOrExportDeclaration`][estree:importorexportdeclaration] node.
;;;
;;; [estree:importorexportdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importorexportdeclaration
(define-class ImportOrExportDeclaration (Declaration)
  (define/public type "ImportOrExportDeclaration"))

;;; ModuleSpecifier
;;;
;;; Creates an ESTree [`ModuleSpecifier`][estree:modulespecifier] node.
;;;
;;; [estree:modulespecifier]: https://github.com/estree/estree/blob/master/es2015.md#modulespecifier
(define-class ModuleSpecifier (TSNode)
  (define/public type "ModuleSpecifier")
  (define/public local)

  (define/public (constructor local)
    (super)
    (set-field! local this local)))

;;; ImportDeclaration
;;;
;;; Creates an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
;;;
;;; [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
(define-class ImportDeclaration (ImportOrExportDeclaration)
  (define/public type "ImportDeclaration")
  (define/public specifiers)
  (define/public source)

  (define/public (constructor specifiers source)
    (super)
    (set-field! specifiers this specifiers)
    (set-field! source this source)))

;;; ImportSpecifier
;;;
;;; Creates an ESTree [`ImportSpecifier`][estree:importspecifier] node.
;;;
;;; [estree:importspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importspecifier
(define-class ImportSpecifier (ModuleSpecifier)
  (define/public type "ImportSpecifier")
  (define/public local)
  (define/public imported)

  (define/public (constructor local (imported undefined))
    (super local)
    (set-field! local this local)
    (set-field! imported this (or imported local))))

;;; ImportDefaultSpecifier
;;;
;;; Creates an ESTree [`ImportDefaultSpecifier`][estree:importdefaultspecifier] node.
;;;
;;; [estree:importdefaultspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importdefaultspecifier
(define-class ImportDefaultSpecifier (ModuleSpecifier)
  (define/public type "ImportDefaultSpecifier")
  (define/public local)

  (define/public (constructor local)
    (super local)
    (set-field! local this local)))

;;; ImportNamespaceSpecifier
;;;
;;; Creates an ESTree [`ImportNamespaceSpecifier`][estree:importnamespacespecifier] node.
;;;
;;; [estree:importnamespacespecifier]: https://github.com/estree/estree/blob/master/es2015.md#importnamespacespecifier
(define-class ImportNamespaceSpecifier (ModuleSpecifier)
  (define/public type "ImportNamespaceSpecifier")
  (define/public local)

  (define/public (constructor local)
    (super local)
    (set-field! local this local)))

;;; ExportNamedDeclaration
;;;
;;; Creates an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
;;;
;;; [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
(define-class ExportNamedDeclaration (ImportOrExportDeclaration)
  (define/public type "ExportNamedDeclaration")
  (define/public declaration)
  (define/public specifiers)
  (define/public source)

  (define/public (constructor (declaration js/null)
                              (specifiers '())
                              (source js/null))
    (super)
    (set-field! declaration this declaration)
    (set-field! specifiers this specifiers)
    (set-field! source this source)))

;;; ExportSpecifier
;;;
;;; Creates an ESTree [`ExportSpecifier`][estree:exportspecifier] node.
;;;
;;; [estree:exportspecifier]: https://github.com/estree/estree/blob/master/es2015.md#exportspecifier
(define-class ExportSpecifier (ModuleSpecifier)
  (define/public type "ExportSpecifier")
  (define/public local)
  (define/public exported)

  (define/public (constructor local (exported undefined))
    (super local)
    (set-field! local this local)
    (set-field! exported this (or exported local))))

;;; ExportAllDeclaration
;;;
;;; Creates an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
;;;
;;; [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
(define-class ExportAllDeclaration (ImportOrExportDeclaration)
  (define/public type "ExportAllDeclaration")
  (define/public source)

  (define/public (constructor source)
    (super)
    (set-field! source this source)))

;;; ClassExpression
;;;
;;; Creates an ESTree [`ClassExpression`][estree:classexpression] node.
;;;
;;; [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
(define-class ClassExpression (Expression)
  (define/public type "ClassExpression")
  (define/public body)
  (define/public superClass)

  (define/public (constructor (body (new ClassBody)) (super-class undefined))
    (super)
    (set-field! body this body)
    (set-field! superClass this super-class)))

;;; ClassBody
;;;
;;; Creates an ESTree [`ClassBody`][estree:classbody] node.
;;;
;;; [estree:classbody]: https://github.com/estree/estree/blob/master/es2015.md#classbody
(define-class ClassBody (TSNode)
  (define/public type "ClassBody")
  (define/public body)

  (define/public (constructor (body '()))
    (super)
    (set-field! body this body)))

;;; PropertyDefinition
;;;
;;; Creates an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
;;;
;;; [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
(define-class PropertyDefinition (TSNode)
  (define/public type "PropertyDefinition")
  (define/public key)
  (define/public value)
  (define/public static)

  ;;; Accessibility (public or private).
  ;;;
  ;;; Not a standard ESTree property, but part of TypeScript.
  (define/public accessibility)

  (define/public (constructor key
                              value
                              (static-flag #f)
                              (accessibility "public"))
    (super)
    (set-field! key this key)
    (set-field! value this value)
    (set-field! static this static-flag)
    (set-field! accessibility this accessibility)))

;;; MethodDefinition
;;;
;;; Creates an ESTree [`MethodDefinition`][estree:methoddefinition] node.
;;;
;;; [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
(define-class MethodDefinition (TSNode)
  (define/public type "MethodDefinition")
  (define/public key)
  (define/public value)
  (define/public kind)
  (define/public static)
  (define/public override)
  (define/public computed)

  ;;; Accessibility (public or private).
  ;;;
  ;;; Not a standard ESTree property, but part of TypeScript.
  (define/public accessibility)

  (define/public (constructor key
                              value
                              (kind "method")
                              (static-flag #f)
                              (override #f)
                              (computed #f)
                              (accessibility "public"))
    (super)
    (set-field! key this key)
    (set-field! value this value)
    (set-field! kind this kind)
    (set-field! static this static-flag)
    (set-field! override this override)
    (set-field! computed this computed)
    (set-field! accessibility this accessibility)))

;;; Identifier
;;;
;;; Creates an ESTree [`Identifier`][estree:identifier] node.
;;;
;;; [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
(define-class Identifier (Expression)
  (define/public type "Identifier")
  (define/public name)
  (define/public optional)

  (define/public (constructor name (optional #f))
    (super)
    (set-field! name this name)
    (set-field! optional this optional)))

;;; Literal
;;;
;;; Creates an ESTree [`Literal`][estree:literal] node.
;;;
;;; [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
(define-class Literal (Expression)
  (define/public type "Literal")
  (define/public value)
  (define/public raw)

  (define/public (constructor value (raw value))
    (super)
    (set-field! value this value)
    (set-field! raw this raw)))

;;; RegExpLiteral
;;;
;;; Creates an ESTree [`RegExpLiteral`][estree:regexpliteral] node.
;;;
;;; [estree:regexpliteral]: https://github.com/estree/estree/blob/master/es5.md#regexpliteral
(define-class RegExpLiteral (Literal)
  (define/public type "RegExpLiteral")
  (define/public pattern)
  (define/public flags)

  (define/public (constructor pattern (flags ""))
    (super pattern)
    (set-field! pattern this pattern)
    (set-field! flags this flags)))

;;; CallExpression
;;;
;;; Creates an ESTree [`CallExpression`][estree:callexpression] node.
;;;
;;; [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
(define-class CallExpression (Expression)
  (define/public type "CallExpression")
  (define/public callee)
  (define/public arguments)
  (define/public optional)

  (define/public (constructor callee (args '()) (optional #f))
    (super)
    (set-field! callee this callee)
    (set-field! arguments this args)
    (set-field! optional this optional)))

;;; MemberExpression
;;;
;;; Creates an ESTree [`MemberExpression`][estree:memberexpression] node.
;;;
;;; [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
(define-class MemberExpression (Expression)
  (define/public type "MemberExpression")
  (define/public object)
  (define/public property)
  (define/public computed)

  (define/public (constructor object property (computed #f))
    (super)
    (set-field! object this object)
    (set-field! property this property)
    (set-field! computed this computed)))

;;; ChainExpression
;;;
;;; Creates an ESTree [`ChainExpression`][estree:chainexpression] node.
;;;
;;; [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
(define-class ChainExpression (Expression)
  (define/public type "ChainExpression")
  (define/public expression)

  (define/public (constructor expression)
    (super)
    (set-field! expression this expression)))

;;; ChainElement
;;;
;;; Creates an ESTree [`ChainElement`][estree:chainelement] node.
;;;
;;; [estree:chainelement]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
(define-class ChainElement (Node)
  (define/public type "ChainElement")
  (define/public optional)

  (define/public (constructor (optional #f))
    (super)
    (set-field! optional this optional)))

;;; UnaryExpression
;;;
;;; Creates an ESTree [`UnaryExpression`][estree:unaryexpression] node.
;;;
;;; [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
(define-class UnaryExpression (Expression)
  (define/public type "UnaryExpression")
  (define/public operator)
  (define/public prefix)
  (define/public argument)

  (define/public (constructor operator prefix argument)
    (super)
    (set-field! operator this operator)
    (set-field! prefix this prefix)
    (set-field! argument this argument)))

;;; UpdateExpression
;;;
;;; Creates an ESTree [`UpdateExpression`][estree:updateexpression] node.
;;;
;;; [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
(define-class UpdateExpression (Expression)
  (define/public type "UpdateExpression")
  (define/public operator)
  (define/public argument)
  (define/public prefix)

  (define/public (constructor operator argument prefix)
    (super)
    (set-field! operator this operator)
    (set-field! argument this argument)
    (set-field! prefix this prefix)))

;;; BinaryExpression
;;;
;;; Creates an ESTree [`BinaryExpression`][estree:binaryexpression] node.
;;;
;;; [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
(define-class BinaryExpression (Expression)
  (define/public type "BinaryExpression")
  (define/public operator)
  (define/public left)
  (define/public right)

  (define/public (constructor operator left right)
    (super)
    (set-field! operator this operator)
    (set-field! left this left)
    (set-field! right this right)))

;;; LogicalExpression
;;;
;;; Creates an ESTree [`LogicalExpression`][estree:logicalexpression] node.
;;;
;;; [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
(define-class LogicalExpression (Expression)
  (define/public type "LogicalExpression")
  (define/public operator)
  (define/public left)
  (define/public right)

  (define/public (constructor operator left right)
    (super)
    (set-field! operator this operator)
    (set-field! left this left)
    (set-field! right this right)))

;;; AssignmentExpression
;;;
;;; Creates an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
;;;
;;; [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
(define-class AssignmentExpression (Expression)
  (define/public type "AssignmentExpression")
  (define/public operator)
  (define/public left)
  (define/public right)

  (define/public (constructor operator left right)
    (super)
    (set-field! operator this operator)
    (set-field! left this left)
    (set-field! right this right)))

;;; SequenceExpression
;;;
;;; Creates an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
;;;
;;; [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
(define-class SequenceExpression (Expression)
  (define/public type "SequenceExpression")
  (define/public expressions)

  (define/public (constructor expressions)
    (super)
    (set-field! expressions this expressions)))

;;; ObjectExpression
;;;
;;; Creates an ESTree [`ObjectExpression`][estree:objectexpression] node.
;;;
;;; [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
(define-class ObjectExpression (Expression)
  (define/public type "ObjectExpression")
  (define/public properties)

  (define/public (constructor (properties '()))
    (super)
    (set-field! properties this properties)))

;;; Property
;;;
;;; Creates an ESTree [`Property`][estree:property] node.
;;;
;;; [estree:property]: https://github.com/estree/estree/blob/master/es5.md#property
(define-class Property (TSNode)
  (define/public type "Property")
  (define/public key)
  (define/public value)
  (define/public computed)
  (define/public kind)

  (define/public (constructor key value (computed #f) (kind "init"))
    (super)
    (set-field! key this key)
    (set-field! value this value)
    (set-field! computed this computed)
    (set-field! kind this kind)))

;;; Function
;;;
;;; Creates an ESTree [`Function`][estree:function] node.
;;;
;;; [estree:function]: https://github.com/estree/estree/blob/master/es5.md#functions
(define-class Function (TSNode)
  (define/public type "Function")
  (define/public id)
  (define/public params)
  (define/public body)

  (define/public (constructor id params body)
    (super)
    (set-field! id this id)
    (set-field! params this params)
    (set-field! body this body)))

;;; FunctionDeclaration
;;;
;;; Creates an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
;;;
;;; [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
(define-class FunctionDeclaration (Declaration)
  (define/public type "FunctionDeclaration")
  (define/public id)
  (define/public params)
  (define/public body)
  (define/public async)
  (define/public generator)
  (define/public returnType)

  (define/public (constructor id params body (async #f) (returnType undefined) (generator #f))
    (super)
    (set-field! id this id)
    (set-field! params this params)
    (set-field! body this body)
    (set-field! async this async)
    (set-field! returnType this returnType)
    (set-field! generator this generator)))

;;; FunctionExpression
;;;
;;; Creates an ESTree [`FunctionExpression`][estree:functionexpression] node.
;;;
;;; <https://docs.esprima.org/en/latest/syntax-tree-format.html#function-expression>
;;;
;;; [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
(define-class FunctionExpression (Expression)
  (define/public type "FunctionExpression")
  (define/public id)
  (define/public params)
  (define/public body)
  (define/public async)
  (define/public generator)
  (define/public returnType)

  (define/public (constructor params
                              body
                              (async #f)
                              (generator #f))
    (super)
    (set-field! id this js/null)
    (set-field! params this params)
    (set-field! body this body)
    (set-field! async this async)
    (set-field! generator this generator))

  (define/public (set-type type_)
    (set-field! returnType this type_)
    this))

;;; ArrowFunctionExpression
;;;
;;; Creates an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
;;;
;;; [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
(define-class ArrowFunctionExpression (FunctionExpression)
  (define/public type "ArrowFunctionExpression")

  (define/public (constructor params body (async #f))
    (super params body async)))

;;; ConditionalExpression
;;;
;;; Creates an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
;;;
;;; [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
(define-class ConditionalExpression (Expression)
  (define/public type "ConditionalExpression")
  (define/public test)
  (define/public consequent)
  (define/public alternate)

  (define/public (constructor test consequent alternate)
    (super)
    (set-field! test this test)
    (set-field! consequent this consequent)
    (set-field! alternate this alternate)))

;;; Pattern
;;;
;;; Creates an ESTree [`Pattern`][estree:pattern] node.
;;;
;;; [estree:pattern]: https://github.com/estree/estree/blob/master/es5.md#patterns
(define-class Pattern (TSNode)
  (define/public type "Pattern"))

;;; AssignmentPattern
;;;
;;; Creates an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
;;;
;;; [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
(define-class AssignmentPattern (Pattern)
  (define/public type "AssignmentPattern")
  (define/public left)
  (define/public right)

  (define/public (constructor left right)
    (super)
    (set-field! left this left)
    (set-field! right this right))

  (define/public (get-type)
    (send (get-field left this) get-type))

  (define/public (set-type type_)
    (send (get-field left this) set-type type_)
    this))

;;; ArrayPattern
;;;
;;; Creates an ESTree [`ArrayPattern`][estree:arraypattern] node.
;;;
;;; [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
(define-class ArrayPattern (Pattern)
  (define/public type "ArrayPattern")
  (define/public elements)

  (define/public (constructor elements)
    (super)
    (set-field! elements this elements)))

;;; ObjectPattern
;;;
;;; Creates an ESTree [`ObjectPattern`][estree:objectpattern] node.
;;;
;;; [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
(define-class ObjectPattern (Pattern)
  (define/public type "ObjectPattern")
  (define/public properties)

  (define/public (constructor (properties '()))
    (super)
    (set-field! properties this properties)))

;;; ThisExpression
;;;
;;; Creates an ESTree [`ThisExpression`][estree:thisexpression] node.
;;;
;;; [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
(define-class ThisExpression (Expression)
  (define/public type "ThisExpression"))

;;; ArrayExpression
;;;
;;; Creates an ESTree [`ArrayExpression`][estree:arrayexpression] node.
;;;
;;; [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
(define-class ArrayExpression (Expression)
  (define/public type "ArrayExpression")
  (define/public elements)

  (define/public (constructor (elements '()))
    (super)
    (set-field! elements this elements)))

;;; RestElement
;;;
;;; Creates an ESTree [`RestElement`][estree:restelement] node.
;;;
;;; [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
(define-class RestElement (Pattern)
  (define/public type "RestElement")
  (define/public argument)

  (define/public (constructor argument)
    (super)
    (set-field! argument this argument)))

;;; SpreadElement
;;;
;;; Creates an ESTree [`SpreadElement`][estree:spreadelement] node.
;;;
;;; [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define-class SpreadElement (Expression)
  (define/public type "SpreadElement")
  (define/public argument)

  (define/public (constructor argument)
    (super)
    (set-field! argument this argument)))

;;; NewExpression
;;;
;;; Creates an ESTree [`NewExpression`][estree:newexpression] node.
;;;
;;; [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define-class NewExpression (Expression)
  (define/public type "NewExpression")
  (define/public callee)
  (define/public arguments)

  (define/public (constructor callee (args '()))
    (super)
    (set-field! callee this callee)
    (set-field! arguments this args)))

;;; YieldExpression
;;;
;;; Creates an ESTree [`YieldExpression`][estree:yieldexpression] node.
;;;
;;; [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
(define-class YieldExpression (Expression)
  (define/public type "YieldExpression")
  (define/public argument)
  (define/public delegate)

  (define/public (constructor (argument js/null) (delegate #f))
    (super)
    (set-field! argument this argument)
    (set-field! delegate this delegate)))

;;; AwaitExpression
;;;
;;; Creates an ESTree [`AwaitExpression`][estree:awaitexpression] node.
;;;
;;; [estree:awaitexpression]: https://github.com/estree/estree/blob/master/es2017.md#awaitexpression
(define-class AwaitExpression (Expression)
  (define/public type "AwaitExpression")
  (define/public argument)

  (define/public (constructor (argument js/null))
    (super)
    (set-field! argument this argument)))

;;; TemplateLiteral
;;;
;;; Creates an ESTree [`TemplateLiteral`][estree:templateliteral] node.
;;;
;;; [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
(define-class TemplateLiteral (Expression)
  (define/public type "TemplateLiteral")
  (define/public quasis)
  (define/public expressions)

  (define/public (constructor (quasis '()) (expressions '()))
    (super)
    (set-field! quasis this quasis)
    (set-field! expressions this expressions)))

;;; TaggedTemplateExpression
;;;
;;; Creates an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
;;;
;;; [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
(define-class TaggedTemplateExpression (Expression)
  (define/public type "TaggedTemplateExpression")
  (define/public tag)
  (define/public quasi)

  (define/public (constructor tag quasi)
    (super)
    (set-field! tag this tag)
    (set-field! quasi this quasi)))

;;; TemplateElement
;;;
;;; Creates an ESTree [`TemplateElement`][estree:templateelement] node.
;;;
;;; [estree:templateelement]: https://github.com/estree/estree/blob/master/es2015.md#templateelement
(define-class TemplateElement (TSNode)
  (define/public type "TemplateElement")
  (define/public tail)
  (define/public value)

  (define/public (constructor tail cooked (raw undefined))
    (super)
    (set-field! tail this tail)
    (set-field! value
                this
                (js-obj
                 "cooked"
                 cooked
                 "raw"
                 (or raw
                     (~> cooked
                         (regexp-replace
                          (regexp "\\\\" "g") _ "\\\\")
                         (regexp-replace
                          (regexp "`" "g") _ "\\`")))))))

;;; SwitchStatement
;;;
;;; Creates an ESTree [`SwitchStatement`][estree:switchstatement] node.
;;;
;;; [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
(define-class SwitchStatement (Statement)
  (define/public type "SwitchStatement")
  (define/public discriminant)
  (define/public cases)

  (define/public (constructor discriminant (cases '()))
    (super)
    (set-field! discriminant this discriminant)
    (set-field! cases this cases)))

;;; SwitchCase
;;;
;;; Creates an ESTree [`SwitchCase`][estree:switchcase] node.
;;;
;;; [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
(define-class SwitchCase (TSNode)
  (define/public type "SwitchCase")
  (define/public test)
  (define/public consequent)

  (define/public (constructor test (consequent '()))
    (super)
    (set-field! test this test)
    (set-field! consequent this consequent)))

;;; ESTree extensions

;;; TSIdentifier
;;;
;;; TSTree version of `Identifier`.
(define-class TSIdentifier (Identifier)
  (define/public type "Identifier")
  (define/public name)
  (define/public typeAnnotation)

  (define/public (constructor name type-annotation)
    (super name)
    (set-field! typeAnnotation this type-annotation)))

;;; TSESTree extensions.
;;;
;;; <https://typescript-eslint.io/packages/parser/>

;;; TSTypeAnnotation
;;;
;;; TypeScript type annotation.
(define-class TSTypeAnnotation (TSNode)
  (define/public type "TSTypeAnnotation")
  (define/public typeAnnotation)

  (define/public (constructor typeAnnotation)
    (super)
    (set-field! typeAnnotation this typeAnnotation)))

;;; TSKeywordTypeNode
;;;
;;; Base class for TSESTree keywords.
(define-class TSKeywordTypeNode (Node)
  (define/public type "TSKeywordTypeNode"))

;;; TSAnyKeyword
;;;
;;; TypeScript `any` type.
(define-class TSAnyKeyword (TSKeywordTypeNode)
  (define/public type "TSAnyKeyword"))

;;; TSVoidKeyword
;;;
;;; TypeScript `void` type.
(define-class TSVoidKeyword (TSKeywordTypeNode)
  (define/public type "TSVoidKeyword"))

;;; TSUndefinedKeyword
;;;
;;; TypeScript `undefined` type.
(define-class TSUndefinedKeyword (TSKeywordTypeNode)
  (define/public type "TSUndefinedKeyword"))

;;; TSBooleanKeyword
;;;
;;; TypeScript `boolean` type.
(define-class TSBooleanKeyword (TSKeywordTypeNode)
  (define/public type "TSBooleanKeyword"))

;;; TSNumberKeyword
;;;
;;; TypeScript `number` type.
(define-class TSNumberKeyword (TSKeywordTypeNode)
  (define/public type "TSNumberKeyword"))

;;; TSStringKeyword
;;;
;;; TypeScript `string` type.
(define-class TSStringKeyword (TSKeywordTypeNode)
  (define/public type "TSStringKeyword"))

;;; TSArrayType
;;;
;;; TypeScript array type.
(define-class TSArrayType (Node)
  (define/public type "TSArrayType")
  (define/public elementType)

  (define/public (constructor elementType)
    (super)
    (set-field! elementType this elementType)))

;;; TSTupleType
;;;
;;; TypeScript tuple type.
(define-class TSTupleType (Node)
  (define/public type "TSTupleType")
  (define/public elementTypes)

  (define/public (constructor elementTypes)
    (super)
    (set-field! elementTypes this elementTypes)))

;;; TSLiteralType
;;;
;;; TypeScript literal type.
(define-class TSLiteralType (Node)
  (define/public type "TSLiteralType")
  (define/public literal)

  (define/public (constructor literal)
    (super)
    (set-field! literal this literal)))

;;; TSUnionType
;;;
;;; TypeScript union type.
(define-class TSUnionType (Node)
  (define/public type "TSUnionType")
  (define/public types)

  (define/public (constructor types)
    (super)
    (set-field! types this types)))

;;; TSFunctionType
;;;
;;; TypeScript function type.
(define-class TSFunctionType (Node)
  (define/public type "TSFunctionType")
  (define/public params)
  (define/public returnType)

  (define/public (constructor params returnType)
    (super)
    (set-field! params this params)
    (set-field! returnType this returnType)))

;;; TSTypeAliasDeclaration
;;;
;;; TypeScript `type` alias declaration.
(define-class TSTypeAliasDeclaration (Node)
  (define/public type "TSTypeAliasDeclaration")
  (define/public id)
  (define/public typeAnnotation)

  (define/public (constructor id typeAnnotation)
    (super)
    (set-field! id this id)
    (set-field! typeAnnotation this typeAnnotation)))

;;; TSTypeReference
;;;
;;; TypeScript `boolean` type.
(define-class TSTypeReference (Node)
  (define/public type "TSTypeReference")
  (define/public typeName)
  (define/public typeParameters)

  (define/public (constructor typeName (typeParameters undefined))
    (super)
    (set-field! typeName this typeName)
    (set-field! typeParameters this typeParameters)))

;;; TSTypeParameterInstantiation
;;;
;;; TypeScript type parameters.
(define-class TSTypeParameterInstantiation (Node)
  (define/public type "TSTypeParameterInstantiation")
  (define/public params)

  (define/public (constructor (params '()))
    (super)
    (set-field! params this params)))

;;; TSAsExpression
;;;
;;; TypeScript `as` expression.
(define-class TSAsExpression (Expression)
  (define/public type "TSAsExpression")
  (define/public expression)

  (define/public (constructor expression typeAnnotation)
    (super)
    (set-field! expression this expression)
    (set-field! typeAnnotation this typeAnnotation)))

;;; Comment
;;;
;;; Base class for comments.
(define-class Comment ()
  (define/public original-text "")
  (define/public leading #t)
  (define/public trailing #f)
  (define/public printed #t)

  (define/public (constructor original-text (comment-type "leading"))
    (set-field! original-text this original-text)
    (cond
     ((eq? comment-type "trailing")
      (set-field! leading this #f)
      (set-field! trailing this #t))
     (else
      (set-field! leading this #t)
      (set-field! trailing this #f))))

  (define/public (get-text)
    (get-field original-text this)))

;;; LeadingComment
;;;
;;; Leading comment.
(define-class LeadingComment (Comment)
  (define/public (constructor original-text)
    (super original-text "leading")))

;;; BlockComment
;;;
;;; Block comment.
(define-class BlockComment (LeadingComment)
  (define/public (constructor original-text)
    (super original-text)))

;;; TrailingComment
;;;
;;; Trailing comment.
(define-class TrailingComment (Comment)
  (define/public (constructor original-text)
    (super original-text "trailing")))

;;; XRawJavaScript
;;;
;;; A raw JavaScript string that is to be spliced directly
;;; into the output.
;;;
;;; Nonstandard ESTree extension.
(define-class XRawJavaScript (TSNode)
  (define/public type "XRawJavaScript")
  (define/public js)

  (define/public (constructor js)
    (super)
    (set-field! js this js)))

;;; Whether `obj` is an ESTree.
;;;
;;; This function only works on instantiated objects.
(define (estree? obj)
  (is-a? obj Node))

;;; Get the type of an ESTree node.
(define (estree-type node)
  (get-field type node))

;;; Whether the type of the ESTree node `node` is `typ`.
(define (estree-type? node typ)
  (eq? (estree-type node) typ))

;;; Wrap a value in an ESTree node.
;;;
;;; Distinguishes between list values and atomic values.
(define (wrap-in-estree x)
  (cond
   ((array? x)
    (new ArrayExpression
         (map wrap-in-estree x)))
   (else
    (new Literal x))))

(provide
  (rename-out (Expression ESTreeExpression))
  (rename-out (Node ESTreeNode))
  (rename-out (Statement ESTreeStatement))
  (rename-out (TSNode TSESTreeNode))
  (rename-out (estree-type? estree-is?))
  ArrayExpression
  ArrayPattern
  ArrowFunctionExpression
  AssignmentExpression
  AssignmentPattern
  AwaitExpression
  BinaryExpression
  BlockComment
  BlockStatement
  BreakStatement
  CallExpression
  CatchClause
  ChainElement
  ChainExpression
  ClassBody
  ClassDeclaration
  ClassExpression
  Comment
  ConditionalExpression
  ContinueStatement
  DoWhileStatement
  ExportAllDeclaration
  ExportNamedDeclaration
  ExportSpecifier
  Expression
  ExpressionStatement
  ForInStatement
  ForOfStatement
  ForStatement
  Function
  FunctionDeclaration
  FunctionExpression
  Identifier
  IfStatement
  ImportDeclaration
  ImportDefaultSpecifier
  ImportNamespaceSpecifier
  ImportSpecifier
  LeadingComment
  Literal
  LogicalExpression
  MemberExpression
  MethodDefinition
  NewExpression
  Node
  ObjectExpression
  ObjectPattern
  Program
  Property
  PropertyDefinition
  RegExpLiteral
  RestElement
  ReturnStatement
  SequenceExpression
  SpreadElement
  Statement
  SwitchCase
  SwitchStatement
  TSAnyKeyword
  TSArrayType
  TSAsExpression
  TSBooleanKeyword
  TSFunctionType
  TSIdentifier
  TSLiteralType
  TSNode
  TSNumberKeyword
  TSStringKeyword
  TSTupleType
  TSTypeAliasDeclaration
  TSTypeAnnotation
  TSTypeParameterInstantiation
  TSTypeReference
  TSUndefinedKeyword
  TSUnionType
  TSVoidKeyword
  TaggedTemplateExpression
  TemplateElement
  TemplateLiteral
  ThisExpression
  ThrowStatement
  TrailingComment
  TryStatement
  UnaryExpression
  UpdateExpression
  VariableDeclaration
  VariableDeclarator
  WhileStatement
  XRawJavaScript
  YieldExpression
  estree-type
  estree-type?
  estree?
  wrap-in-estree)

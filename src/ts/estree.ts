// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # ESTree implementation
 *
 * A collection of ESTree node classes.
 *
 * ## Description
 *
 * This is a simple implementation of a subset of the [ESTree
 * specification][github:estree]. It defines a class for each ESTree
 * node type, permitting ESTree nodes to be instantiated with `new`.
 * For example,
 *
 *     (new BlockStatement
 *          (list ...))
 *
 * amounts to the same as
 *
 *     (js-obj "type" "BlockStatement"
 *             "body" (list ...))
 *
 * Objects instantiated with `new` are compatible with objects
 * created with `js-obj`.
 *
 * There are no private fields. For maximum compatibility with
 * existing implementations, every field and method is public.
 *
 * This implementation also provides support for
 * [TSESTree][npm:typescript-estree]-compatible
 * [type annotations][github:estree-types].
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [github:estree]: https://github.com/estree/estree
 * [github:estree-types]: https://github.com/estree/estree/blob/master/extensions/type-annotations.md
 * [npm:typescript-estree] https://www.npmjs.com/package/@typescript-eslint/typescript-estree
 */

/**
 * Node
 *
 * Creates an ESTree [`Node`][estree:node] node.
 *
 * This is a base class.
 *
 * [estree:node]: https://github.com/estree/estree/blob/master/es5.md#node-objects
 */
class Node {
  type: any = 'Node';

  comments: any = [];

  loc: any = null;

  addComment(comment: any): any {
    this.comments.push(comment);
    return this;
  }
}

/**
 * TSNode
 *
 * Creates an TSESTree `TSNode` node.
 * Like `Node`, but with type annotations.
 *
 * This is a base class.
 *
 * Cf. the [`@typescript-eslint/typescript-estree`
 * documentation][doc:typescript-estree].
 *
 * [doc:typescript-estree]: https://typescript-eslint.io/packages/typescript-estree/
 */
class TSNode extends Node {
  type: any = 'TSNode';

  typeAnnotation: any;

  getType(): any {
    return this.getTypeAnnotation();
  }

  getTypeAnnotation(): any {
    return this.typeAnnotation;
  }

  hasType(): any {
    return this.getType() && true;
  }

  setType(type_: any): any {
    return this.setTypeAnnotation(type_);
  }

  setTypeAnnotation(typeAnnotation: any): any {
    this.typeAnnotation = typeAnnotation;
    return this;
  }
}

/**
 * Expression
 *
 * Creates an ESTree [`Expression`][estree:expression] node.
 *
 * This is a base class.
 *
 * [estree:expression]: https://github.com/estree/estree/blob/master/es5.md#expressions
 */
class Expression extends TSNode {
  type: any = 'Expression';
}

/**
 * Program
 *
 * Creates an ESTree [`Program`][estree:program] node.
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
class Program extends TSNode {
  type: any = 'Program';

  body: any;

  constructor(body: any) {
    super();
    this.body = body;
  }
}

/**
 * Statement
 *
 * Creates an ESTree [`Statement`][estree:statement] node.
 *
 * [estree:statement]: https://github.com/estree/estree/blob/master/es5.md#statements
 */
class Statement extends TSNode {
  type: any = 'Statement';
}

/**
 * IfStatement
 *
 * Creates an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
class IfStatement extends Statement {
  type: any = 'IfStatement';

  test: any;

  consequent: any;

  alternate: any;

  constructor(test: any, consequent: any, alternate: any) {
    super();
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
  }
}

/**
 * BlockStatement
 *
 * Creates an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
class BlockStatement extends Statement {
  type: any = 'BlockStatement';

  body: any;

  constructor(body: any = []) {
    super();
    this.body = body;
  }
}

/**
 * ExpressionStatement
 *
 * Creates an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
 *
 * [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
 */
class ExpressionStatement extends Statement {
  type: any = 'ExpressionStatement';

  expression: any;

  constructor(expression: any) {
    super();
    this.expression = expression;
  }
}

/**
 * ReturnStatement
 *
 * Creates an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
class ReturnStatement extends Statement {
  type: any = 'ReturnStatement';

  argument: any;

  constructor(argument: any = null) {
    super();
    this.argument = argument;
  }
}

/**
 * ThrowStatement
 *
 * Creates an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
class ThrowStatement extends Statement {
  type: any = 'ThrowStatement';

  argument: any;

  constructor(argument: any) {
    super();
    this.argument = argument;
  }
}

/**
 * BreakStatement
 *
 * Creates an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
class BreakStatement extends Statement {
  type: any = 'BreakStatement';

  label: any;

  constructor(label: any = null) {
    super();
    this.label = label;
  }
}

/**
 * ContinueStatement
 *
 * Creates an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
class ContinueStatement extends Statement {
  type: any = 'ContinueStatement';

  label: any;

  constructor(label: any = null) {
    super();
    this.label = label;
  }
}

/**
 * WhileStatement
 *
 * Creates an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
class WhileStatement extends Statement {
  type: any = 'WhileStatement';

  test: any;

  body: any;

  constructor(test: any, body: any) {
    super();
    this.test = test;
    this.body = body;
  }
}

/**
 * DoWhileStatement
 *
 * Creates an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
 *
 * [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
 */
class DoWhileStatement extends Statement {
  type: any = 'DoWhileStatement';

  test: any;

  body: any;

  constructor(test: any, body: any) {
    super();
    this.test = test;
    this.body = body;
  }
}

/**
 * ForStatement
 *
 * Creates an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
class ForStatement extends Statement {
  type: any = 'ForStatement';

  init: any;

  test: any;

  update: any;

  body: any;

  constructor(init: any, test: any, update: any, body: any) {
    super();
    this.init = init;
    this.test = test;
    this.update = update;
    this.body = body;
  }
}

/**
 * ForInStatement
 *
 * Creates an ESTree [`ForInStatement`][estree:forinstatement] node.
 *
 * [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
 */
class ForInStatement extends Statement {
  type: any = 'ForInStatement';

  left: any;

  right: any;

  body: any;

  constructor(left: any, right: any, body: any) {
    super();
    this.left = left;
    this.right = right;
    this.body = body;
  }
}

/**
 * ForOfStatement
 *
 * Creates an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
class ForOfStatement extends ForInStatement {
  type: any = 'ForOfStatement';

  left: any;

  right: any;

  body: any;

  constructor(left: any, right: any, body: any) {
    super(left, right, body);
    this.left = left;
    this.right = right;
    this.body = body;
  }
}

/**
 * TryStatement
 *
 * Creates an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
class TryStatement extends Statement {
  type: any = 'TryStatement';

  block: any;

  handler: any;

  finalizer: any;

  constructor(block: any, handler: any = null, finalizer: any = null) {
    super();
    this.block = block;
    this.handler = handler;
    this.finalizer = finalizer;
  }
}

/**
 * CatchClause
 *
 * Creates an ESTree [`CatchClause`][estree:catchclause] node.
 *
 * [estree:catchclause]: https://github.com/estree/estree/blob/master/es5.md#catchclause
 */
class CatchClause extends TSNode {
  type: any = 'CatchClause';

  param: any;

  body: any;

  constructor(param: any, body: any) {
    super();
    this.param = param;
    this.body = body;
  }
}

/**
 * Declaration
 *
 * Creates an ESTree [`Declaration`][estree:declaration] node.
 *
 * [estree:declaration]: https://github.com/estree/estree/blob/master/es5.md#declarations
 */
class Declaration extends Statement {
  type: any = 'Declaration';
}

/**
 * VariableDeclaration
 *
 * Creates an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
class VariableDeclaration extends Declaration {
  type: any = 'VariableDeclaration';

  declarations: any;

  kind: any;

  constructor(declarations: any, kind: any = 'var') {
    super();
    this.declarations = declarations;
    this.kind = kind;
  }
}

/**
 * VariableDeclarator
 *
 * Creates an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
 *
 * [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
 */
class VariableDeclarator extends TSNode {
  type: any = 'VariableDeclarator';

  id: any;

  init: any;

  constructor(id: any, init: any = null) {
    super();
    this.id = id;
    this.init = init;
  }
}

/**
 * ClassDeclaration
 *
 * Creates an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
class ClassDeclaration extends Declaration {
  type: any = 'ClassDeclaration';

  id: any;

  body: any;

  superClass: any;

  constructor(id: any, body: any = new ClassBody(), superClass: any = undefined) {
    super();
    this.id = id;
    this.body = body;
    this.superClass = superClass;
  }
}

/**
 * ImportOrExportDeclaration
 *
 * Creates an ESTree [`ImportOrExportDeclaration`][estree:importorexportdeclaration] node.
 *
 * [estree:importorexportdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importorexportdeclaration
 */
class ImportOrExportDeclaration extends Declaration {
  type: any = 'ImportOrExportDeclaration';
}

/**
 * ModuleSpecifier
 *
 * Creates an ESTree [`ModuleSpecifier`][estree:modulespecifier] node.
 *
 * [estree:modulespecifier]: https://github.com/estree/estree/blob/master/es2015.md#modulespecifier
 */
class ModuleSpecifier extends TSNode {
  type: any = 'ModuleSpecifier';

  local: any;

  constructor(local: any) {
    super();
    this.local = local;
  }
}

/**
 * ImportDeclaration
 *
 * Creates an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
 *
 * [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
 */
class ImportDeclaration extends ImportOrExportDeclaration {
  type: any = 'ImportDeclaration';

  specifiers: any;

  source: any;

  constructor(specifiers: any, source: any) {
    super();
    this.specifiers = specifiers;
    this.source = source;
  }
}

/**
 * ImportSpecifier
 *
 * Creates an ESTree [`ImportSpecifier`][estree:importspecifier] node.
 *
 * [estree:importspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importspecifier
 */
class ImportSpecifier extends ModuleSpecifier {
  type: any = 'ImportSpecifier';

  local: any;

  imported: any;

  constructor(local: any, imported: any = undefined) {
    super(local);
    this.local = local;
    this.imported = imported || local;
  }
}

/**
 * ImportDefaultSpecifier
 *
 * Creates an ESTree [`ImportDefaultSpecifier`][estree:importdefaultspecifier] node.
 *
 * [estree:importdefaultspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importdefaultspecifier
 */
class ImportDefaultSpecifier extends ModuleSpecifier {
  type: any = 'ImportDefaultSpecifier';

  local: any;

  constructor(local: any) {
    super(local);
    this.local = local;
  }
}

/**
 * ImportNamespaceSpecifier
 *
 * Creates an ESTree [`ImportNamespaceSpecifier`][estree:importnamespacespecifier] node.
 *
 * [estree:importnamespacespecifier]: https://github.com/estree/estree/blob/master/es2015.md#importnamespacespecifier
 */
class ImportNamespaceSpecifier extends ModuleSpecifier {
  type: any = 'ImportNamespaceSpecifier';

  local: any;

  constructor(local: any) {
    super(local);
    this.local = local;
  }
}

/**
 * ExportNamedDeclaration
 *
 * Creates an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
 *
 * [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
 */
class ExportNamedDeclaration extends ImportOrExportDeclaration {
  type: any = 'ExportNamedDeclaration';

  declaration: any;

  specifiers: any;

  source: any;

  constructor(declaration: any = null, specifiers: any = [], source: any = null) {
    super();
    this.declaration = declaration;
    this.specifiers = specifiers;
    this.source = source;
  }
}

/**
 * ExportSpecifier
 *
 * Creates an ESTree [`ExportSpecifier`][estree:exportspecifier] node.
 *
 * [estree:exportspecifier]: https://github.com/estree/estree/blob/master/es2015.md#exportspecifier
 */
class ExportSpecifier extends ModuleSpecifier {
  type: any = 'ExportSpecifier';

  local: any;

  exported: any;

  constructor(local: any, exported: any = undefined) {
    super(local);
    this.local = local;
    this.exported = exported || local;
  }
}

/**
 * ExportAllDeclaration
 *
 * Creates an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
 *
 * [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
 */
class ExportAllDeclaration extends ImportOrExportDeclaration {
  type: any = 'ExportAllDeclaration';

  source: any;

  constructor(source: any) {
    super();
    this.source = source;
  }
}

/**
 * ClassExpression
 *
 * Creates an ESTree [`ClassExpression`][estree:classexpression] node.
 *
 * [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
 */
class ClassExpression extends Expression {
  type: any = 'ClassExpression';

  body: any;

  superClass: any;

  constructor(body: any = new ClassBody(), superClass: any = undefined) {
    super();
    this.body = body;
    this.superClass = superClass;
  }
}

/**
 * ClassBody
 *
 * Creates an ESTree [`ClassBody`][estree:classbody] node.
 *
 * [estree:classbody]: https://github.com/estree/estree/blob/master/es2015.md#classbody
 */
class ClassBody extends TSNode {
  type: any = 'ClassBody';

  body: any;

  constructor(body: any = []) {
    super();
    this.body = body;
  }
}

/**
 * PropertyDefinition
 *
 * Creates an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
 *
 * [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
 */
class PropertyDefinition extends TSNode {
  type: any = 'PropertyDefinition';

  key: any;

  value: any;

  static: any;

  /**
   * Accessibility (public or private).
   *
   * Not a standard ESTree property, but part of TypeScript.
   */
  accessibility: any;

  constructor(key: any, value: any, staticFlag: any = false, accessibility: any = 'public') {
    super();
    this.key = key;
    this.value = value;
    this.static = staticFlag;
    this.accessibility = accessibility;
  }
}

/**
 * MethodDefinition
 *
 * Creates an ESTree [`MethodDefinition`][estree:methoddefinition] node.
 *
 * [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
 */
class MethodDefinition extends TSNode {
  type: any = 'MethodDefinition';

  key: any;

  value: any;

  kind: any;

  static: any;

  override: any;

  computed: any;

  /**
   * Accessibility (public or private).
   *
   * Not a standard ESTree property, but part of TypeScript.
   */
  accessibility: any;

  constructor(key: any, value: any, kind: any = 'method', staticFlag: any = false, override: any = false, computed: any = false, accessibility: any = 'public') {
    super();
    this.key = key;
    this.value = value;
    this.kind = kind;
    this.static = staticFlag;
    this.override = override;
    this.computed = computed;
    this.accessibility = accessibility;
  }
}

/**
 * Identifier
 *
 * Creates an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
class Identifier extends Expression {
  type: any = 'Identifier';

  name: any;

  optional: any;

  constructor(name: any, optional: any = false) {
    super();
    this.name = name;
    this.optional = optional;
  }
}

/**
 * Literal
 *
 * Creates an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
class Literal extends Expression {
  type: any = 'Literal';

  value: any;

  raw: any;

  constructor(value: any, raw: any = value) {
    super();
    this.value = value;
    this.raw = raw;
  }
}

/**
 * RegExpLiteral
 *
 * Creates an ESTree [`RegExpLiteral`][estree:regexpliteral] node.
 *
 * [estree:regexpliteral]: https://github.com/estree/estree/blob/master/es5.md#regexpliteral
 */
class RegExpLiteral extends Literal {
  type: any = 'RegExpLiteral';

  pattern: any;

  flags: any;

  constructor(pattern: any, flags: any = '') {
    super(pattern);
    this.pattern = pattern;
    this.flags = flags;
  }
}

/**
 * CallExpression
 *
 * Creates an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
class CallExpression extends Expression {
  type: any = 'CallExpression';

  callee: any;

  arguments: any;

  optional: any;

  constructor(callee: any, args: any = [], optional: any = false) {
    super();
    this.callee = callee;
    this.arguments = args;
    this.optional = optional;
  }
}

/**
 * MemberExpression
 *
 * Creates an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
class MemberExpression extends Expression {
  type: any = 'MemberExpression';

  object: any;

  property: any;

  computed: any;

  constructor(object: any, property: any, computed: any = false) {
    super();
    this.object = object;
    this.property = property;
    this.computed = computed;
  }
}

/**
 * ChainExpression
 *
 * Creates an ESTree [`ChainExpression`][estree:chainexpression] node.
 *
 * [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
class ChainExpression extends Expression {
  type: any = 'ChainExpression';

  expression: any;

  constructor(expression: any) {
    super();
    this.expression = expression;
  }
}

/**
 * ChainElement
 *
 * Creates an ESTree [`ChainElement`][estree:chainelement] node.
 *
 * [estree:chainelement]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
class ChainElement extends Node {
  type: any = 'ChainElement';

  optional: any;

  constructor(optional: any = false) {
    super();
    this.optional = optional;
  }
}

/**
 * UnaryExpression
 *
 * Creates an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
class UnaryExpression extends Expression {
  type: any = 'UnaryExpression';

  operator: any;

  prefix: any;

  argument: any;

  constructor(operator: any, prefix: any, argument: any) {
    super();
    this.operator = operator;
    this.prefix = prefix;
    this.argument = argument;
  }
}

/**
 * UpdateExpression
 *
 * Creates an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
class UpdateExpression extends Expression {
  type: any = 'UpdateExpression';

  operator: any;

  argument: any;

  prefix: any;

  constructor(operator: any, argument: any, prefix: any) {
    super();
    this.operator = operator;
    this.argument = argument;
    this.prefix = prefix;
  }
}

/**
 * BinaryExpression
 *
 * Creates an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
class BinaryExpression extends Expression {
  type: any = 'BinaryExpression';

  operator: any;

  left: any;

  right: any;

  constructor(operator: any, left: any, right: any) {
    super();
    this.operator = operator;
    this.left = left;
    this.right = right;
  }
}

/**
 * LogicalExpression
 *
 * Creates an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
class LogicalExpression extends Expression {
  type: any = 'LogicalExpression';

  operator: any;

  left: any;

  right: any;

  constructor(operator: any, left: any, right: any) {
    super();
    this.operator = operator;
    this.left = left;
    this.right = right;
  }
}

/**
 * AssignmentExpression
 *
 * Creates an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
class AssignmentExpression extends Expression {
  type: any = 'AssignmentExpression';

  operator: any;

  left: any;

  right: any;

  constructor(operator: any, left: any, right: any) {
    super();
    this.operator = operator;
    this.left = left;
    this.right = right;
  }
}

/**
 * SequenceExpression
 *
 * Creates an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
class SequenceExpression extends Expression {
  type: any = 'SequenceExpression';

  expressions: any;

  constructor(expressions: any) {
    super();
    this.expressions = expressions;
  }
}

/**
 * ObjectExpression
 *
 * Creates an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
class ObjectExpression extends Expression {
  type: any = 'ObjectExpression';

  properties: any;

  constructor(properties: any = []) {
    super();
    this.properties = properties;
  }
}

/**
 * Property
 *
 * Creates an ESTree [`Property`][estree:property] node.
 *
 * [estree:property]: https://github.com/estree/estree/blob/master/es5.md#property
 */
class Property extends TSNode {
  type: any = 'Property';

  key: any;

  value: any;

  computed: any;

  kind: any;

  constructor(key: any, value: any, computed: any = false, kind: any = 'init') {
    super();
    this.key = key;
    this.value = value;
    this.computed = computed;
    this.kind = kind;
  }
}

/**
 * Function
 *
 * Creates an ESTree [`Function`][estree:function] node.
 *
 * [estree:function]: https://github.com/estree/estree/blob/master/es5.md#functions
 */
class Function extends TSNode {
  type: any = 'Function';

  id: any;

  params: any;

  body: any;

  constructor(id: any, params: any, body: any) {
    super();
    this.id = id;
    this.params = params;
    this.body = body;
  }
}

/**
 * FunctionDeclaration
 *
 * Creates an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
class FunctionDeclaration extends Declaration {
  type: any = 'FunctionDeclaration';

  id: any;

  params: any;

  body: any;

  async: any;

  generator: any;

  returnType: any;

  constructor(id: any, params: any, body: any, async: any = false, returnType: any = undefined, generator: any = false) {
    super();
    this.id = id;
    this.params = params;
    this.body = body;
    this.async = async;
    this.returnType = returnType;
    this.generator = generator;
  }
}

/**
 * FunctionExpression
 *
 * Creates an ESTree [`FunctionExpression`][estree:functionexpression] node.
 *
 * <https://docs.esprima.org/en/latest/syntax-tree-format.html#function-expression>
 *
 * [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
 */
class FunctionExpression extends Expression {
  type: any = 'FunctionExpression';

  id: any;

  params: any;

  body: any;

  async: any;

  generator: any;

  returnType: any;

  constructor(params: any, body: any, async: any = false, generator: any = false) {
    super();
    this.id = null;
    this.params = params;
    this.body = body;
    this.async = async;
    this.generator = generator;
  }

  setType(type_: any): any {
    this.returnType = type_;
    return this;
  }
}

/**
 * ArrowFunctionExpression
 *
 * Creates an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
class ArrowFunctionExpression extends FunctionExpression {
  type: any = 'ArrowFunctionExpression';

  constructor(params: any, body: any, async: any = false) {
    super(params, body, async);
  }
}

/**
 * ConditionalExpression
 *
 * Creates an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
class ConditionalExpression extends Expression {
  type: any = 'ConditionalExpression';

  test: any;

  consequent: any;

  alternate: any;

  constructor(test: any, consequent: any, alternate: any) {
    super();
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
  }
}

/**
 * Pattern
 *
 * Creates an ESTree [`Pattern`][estree:pattern] node.
 *
 * [estree:pattern]: https://github.com/estree/estree/blob/master/es5.md#patterns
 */
class Pattern extends TSNode {
  type: any = 'Pattern';
}

/**
 * AssignmentPattern
 *
 * Creates an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
 *
 * [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
 */
class AssignmentPattern extends Pattern {
  type: any = 'AssignmentPattern';

  left: any;

  right: any;

  constructor(left: any, right: any) {
    super();
    this.left = left;
    this.right = right;
  }

  getType(): any {
    return this.left.getType();
  }

  setType(type_: any): any {
    this.left.setType(type_);
    return this;
  }
}

/**
 * ArrayPattern
 *
 * Creates an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
class ArrayPattern extends Pattern {
  type: any = 'ArrayPattern';

  elements: any;

  constructor(elements: any) {
    super();
    this.elements = elements;
  }
}

/**
 * ObjectPattern
 *
 * Creates an ESTree [`ObjectPattern`][estree:objectpattern] node.
 *
 * [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
 */
class ObjectPattern extends Pattern {
  type: any = 'ObjectPattern';

  properties: any;

  constructor(properties: any = []) {
    super();
    this.properties = properties;
  }
}

/**
 * ThisExpression
 *
 * Creates an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
class ThisExpression extends Expression {
  type: any = 'ThisExpression';
}

/**
 * ArrayExpression
 *
 * Creates an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
class ArrayExpression extends Expression {
  type: any = 'ArrayExpression';

  elements: any;

  constructor(elements: any = []) {
    super();
    this.elements = elements;
  }
}

/**
 * RestElement
 *
 * Creates an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
class RestElement extends Pattern {
  type: any = 'RestElement';

  argument: any;

  constructor(argument: any) {
    super();
    this.argument = argument;
  }
}

/**
 * SpreadElement
 *
 * Creates an ESTree [`SpreadElement`][estree:spreadelement] node.
 *
 * [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
class SpreadElement extends Expression {
  type: any = 'SpreadElement';

  argument: any;

  constructor(argument: any) {
    super();
    this.argument = argument;
  }
}

/**
 * NewExpression
 *
 * Creates an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
class NewExpression extends Expression {
  type: any = 'NewExpression';

  callee: any;

  arguments: any;

  constructor(callee: any, args: any = []) {
    super();
    this.callee = callee;
    this.arguments = args;
  }
}

/**
 * YieldExpression
 *
 * Creates an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
class YieldExpression extends Expression {
  type: any = 'YieldExpression';

  argument: any;

  delegate: any;

  constructor(argument: any = null, delegate: any = false) {
    super();
    this.argument = argument;
    this.delegate = delegate;
  }
}

/**
 * AwaitExpression
 *
 * Creates an ESTree [`AwaitExpression`][estree:awaitexpression] node.
 *
 * [estree:awaitexpression]: https://github.com/estree/estree/blob/master/es2017.md#awaitexpression
 */
class AwaitExpression extends Expression {
  type: any = 'AwaitExpression';

  argument: any;

  constructor(argument: any = null) {
    super();
    this.argument = argument;
  }
}

/**
 * TemplateLiteral
 *
 * Creates an ESTree [`TemplateLiteral`][estree:templateliteral] node.
 *
 * [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
 */
class TemplateLiteral extends Expression {
  type: any = 'TemplateLiteral';

  quasis: any;

  expressions: any;

  constructor(quasis: any = [], expressions: any = []) {
    super();
    this.quasis = quasis;
    this.expressions = expressions;
  }
}

/**
 * TaggedTemplateExpression
 *
 * Creates an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
 *
 * [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
 */
class TaggedTemplateExpression extends Expression {
  type: any = 'TaggedTemplateExpression';

  tag: any;

  quasi: any;

  constructor(tag: any, quasi: any) {
    super();
    this.tag = tag;
    this.quasi = quasi;
  }
}

/**
 * TemplateElement
 *
 * Creates an ESTree [`TemplateElement`][estree:templateelement] node.
 *
 * [estree:templateelement]: https://github.com/estree/estree/blob/master/es2015.md#templateelement
 */
class TemplateElement extends TSNode {
  type: any = 'TemplateElement';

  tail: any;

  value: any;

  constructor(tail: any, cooked: any, raw: any = undefined) {
    super();
    this.tail = tail;
    this.value = {
      cooked: cooked,
      raw: raw || cooked.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('`', 'g'), '\\`')
    };
  }
}

/**
 * SwitchStatement
 *
 * Creates an ESTree [`SwitchStatement`][estree:switchstatement] node.
 *
 * [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
 */
class SwitchStatement extends Statement {
  type: any = 'SwitchStatement';

  discriminant: any;

  cases: any;

  constructor(discriminant: any, cases: any = []) {
    super();
    this.discriminant = discriminant;
    this.cases = cases;
  }
}

/**
 * SwitchCase
 *
 * Creates an ESTree [`SwitchCase`][estree:switchcase] node.
 *
 * [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
 */
class SwitchCase extends TSNode {
  type: any = 'SwitchCase';

  test: any;

  consequent: any;

  constructor(test: any, consequent: any = []) {
    super();
    this.test = test;
    this.consequent = consequent;
  }
}

/**
 * ESTree extensions
 */

/**
 * TSIdentifier
 *
 * TSTree version of `Identifier`.
 */
class TSIdentifier extends Identifier {
  type: any = 'Identifier';

  name: any;

  typeAnnotation: any;

  constructor(name: any, typeAnnotation: any) {
    super(name);
    this.typeAnnotation = typeAnnotation;
  }
}

/**
 * TSESTree extensions.
 *
 * <https://typescript-eslint.io/packages/parser/>
 */

/**
 * TSTypeAnnotation
 *
 * TypeScript type annotation.
 */
class TSTypeAnnotation extends TSNode {
  type: any = 'TSTypeAnnotation';

  typeAnnotation: any;

  constructor(typeAnnotation: any) {
    super();
    this.typeAnnotation = typeAnnotation;
  }
}

/**
 * TSKeywordTypeNode
 *
 * Base class for TSESTree keywords.
 */
class TSKeywordTypeNode extends Node {
  type: any = 'TSKeywordTypeNode';
}

/**
 * TSAnyKeyword
 *
 * TypeScript `any` type.
 */
class TSAnyKeyword extends TSKeywordTypeNode {
  type: any = 'TSAnyKeyword';
}

/**
 * TSVoidKeyword
 *
 * TypeScript `void` type.
 */
class TSVoidKeyword extends TSKeywordTypeNode {
  type: any = 'TSVoidKeyword';
}

/**
 * TSUndefinedKeyword
 *
 * TypeScript `undefined` type.
 */
class TSUndefinedKeyword extends TSKeywordTypeNode {
  type: any = 'TSUndefinedKeyword';
}

/**
 * TSBooleanKeyword
 *
 * TypeScript `boolean` type.
 */
class TSBooleanKeyword extends TSKeywordTypeNode {
  type: any = 'TSBooleanKeyword';
}

/**
 * TSNumberKeyword
 *
 * TypeScript `number` type.
 */
class TSNumberKeyword extends TSKeywordTypeNode {
  type: any = 'TSNumberKeyword';
}

/**
 * TSStringKeyword
 *
 * TypeScript `string` type.
 */
class TSStringKeyword extends TSKeywordTypeNode {
  type: any = 'TSStringKeyword';
}

/**
 * TSArrayType
 *
 * TypeScript array type.
 */
class TSArrayType extends Node {
  type: any = 'TSArrayType';

  elementType: any;

  constructor(elementType: any) {
    super();
    this.elementType = elementType;
  }
}

/**
 * TSTupleType
 *
 * TypeScript tuple type.
 */
class TSTupleType extends Node {
  type: any = 'TSTupleType';

  elementTypes: any;

  constructor(elementTypes: any) {
    super();
    this.elementTypes = elementTypes;
  }
}

/**
 * TSLiteralType
 *
 * TypeScript literal type.
 */
class TSLiteralType extends Node {
  type: any = 'TSLiteralType';

  literal: any;

  constructor(literal: any) {
    super();
    this.literal = literal;
  }
}

/**
 * TSUnionType
 *
 * TypeScript union type.
 */
class TSUnionType extends Node {
  type: any = 'TSUnionType';

  types: any;

  constructor(types: any) {
    super();
    this.types = types;
  }
}

/**
 * TSFunctionType
 *
 * TypeScript function type.
 */
class TSFunctionType extends Node {
  type: any = 'TSFunctionType';

  params: any;

  returnType: any;

  constructor(params: any, returnType: any) {
    super();
    this.params = params;
    this.returnType = returnType;
  }
}

/**
 * TSTypeAliasDeclaration
 *
 * TypeScript `type` alias declaration.
 */
class TSTypeAliasDeclaration extends Node {
  type: any = 'TSTypeAliasDeclaration';

  id: any;

  typeAnnotation: any;

  constructor(id: any, typeAnnotation: any) {
    super();
    this.id = id;
    this.typeAnnotation = typeAnnotation;
  }
}

/**
 * TSTypeReference
 *
 * TypeScript `boolean` type.
 */
class TSTypeReference extends Node {
  type: any = 'TSTypeReference';

  typeName: any;

  typeParameters: any;

  constructor(typeName: any, typeParameters: any = undefined) {
    super();
    this.typeName = typeName;
    this.typeParameters = typeParameters;
  }
}

/**
 * TSTypeParameterInstantiation
 *
 * TypeScript type parameters.
 */
class TSTypeParameterInstantiation extends Node {
  type: any = 'TSTypeParameterInstantiation';

  params: any;

  constructor(params: any = []) {
    super();
    this.params = params;
  }
}

/**
 * TSAsExpression
 *
 * TypeScript `as` expression.
 */
class TSAsExpression extends Expression {
  type: any = 'TSAsExpression';

  expression: any;

  constructor(expression: any, typeAnnotation: any) {
    super();
    this.expression = expression;
    this.typeAnnotation = typeAnnotation;
  }
}

/**
 * Comment
 *
 * Base class for comments.
 */
class Comment {
  originalText: any = '';

  leading: any = true;

  trailing: any = false;

  printed: any = true;

  constructor(originalText: any, commentType: any = 'leading') {
    this.originalText = originalText;
    if (commentType === 'trailing') {
      this.leading = false;
      this.trailing = true;
    } else {
      this.leading = true;
      this.trailing = false;
    }
  }

  getText(): any {
    return this.originalText;
  }
}

/**
 * LeadingComment
 *
 * Leading comment.
 */
class LeadingComment extends Comment {
  constructor(originalText: any) {
    super(originalText, 'leading');
  }
}

/**
 * BlockComment
 *
 * Block comment.
 */
class BlockComment extends LeadingComment {
  constructor(originalText: any) {
    super(originalText);
  }
}

/**
 * TrailingComment
 *
 * Trailing comment.
 */
class TrailingComment extends Comment {
  constructor(originalText: any) {
    super(originalText, 'trailing');
  }
}

/**
 * XRawJavaScript
 *
 * A raw JavaScript string that is to be spliced directly
 * into the output.
 *
 * Nonstandard ESTree extension.
 */
class XRawJavaScript extends TSNode {
  type: any = 'XRawJavaScript';

  js: any;

  constructor(js: any) {
    super();
    this.js = js;
  }
}

/**
 * Whether `obj` is an ESTree.
 *
 * This function only works on instantiated objects.
 */
function estreep(obj: any): any {
  return obj instanceof Node;
}

estreep.lispSource = [Symbol.for('define'), [Symbol.for('estree?'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Node')]];

/**
 * Get the type of an ESTree node.
 */
function estreeType(node: any): any {
  return node.type;
}

estreeType.lispSource = [Symbol.for('define'), [Symbol.for('estree-type'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('type'), Symbol.for('node')]];

/**
 * Whether the type of the ESTree node `node` is `typ`.
 */
function estreeTypeP(node: any, typ: any): any {
  return estreeType(node) === typ;
}

estreeTypeP.lispSource = [Symbol.for('define'), [Symbol.for('estree-type?'), Symbol.for('node'), Symbol.for('typ')], [Symbol.for('eq?'), [Symbol.for('estree-type'), Symbol.for('node')], Symbol.for('typ')]];

/**
 * Wrap a value in an ESTree node.
 *
 * Distinguishes between list values and atomic values.
 */
function wrapInEstree(x: any): any {
  if (Array.isArray(x)) {
    return new ArrayExpression(x.map(function (x: any): any {
      return wrapInEstree(x);
    }));
  } else {
    return new Literal(x);
  }
}

wrapInEstree.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-estree'), Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('x')], [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), Symbol.for('wrap-in-estree'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('x')]]]];

export {
  Expression as ESTreeExpression,
  Node as ESTreeNode,
  Statement as ESTreeStatement,
  TSNode as TSESTreeNode,
  estreeTypeP as estreeIsP,
  ArrayExpression,
  ArrayPattern,
  ArrowFunctionExpression,
  AssignmentExpression,
  AssignmentPattern,
  AwaitExpression,
  BinaryExpression,
  BlockComment,
  BlockStatement,
  BreakStatement,
  CallExpression,
  CatchClause,
  ChainElement,
  ChainExpression,
  ClassBody,
  ClassDeclaration,
  ClassExpression,
  Comment,
  ConditionalExpression,
  ContinueStatement,
  DoWhileStatement,
  ExportAllDeclaration,
  ExportNamedDeclaration,
  ExportSpecifier,
  Expression,
  ExpressionStatement,
  ForInStatement,
  ForOfStatement,
  ForStatement,
  Function,
  FunctionDeclaration,
  FunctionExpression,
  Identifier,
  IfStatement,
  ImportDeclaration,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportSpecifier,
  LeadingComment,
  Literal,
  LogicalExpression,
  MemberExpression,
  MethodDefinition,
  NewExpression,
  Node,
  ObjectExpression,
  ObjectPattern,
  Program,
  Property,
  PropertyDefinition,
  RegExpLiteral,
  RestElement,
  ReturnStatement,
  SequenceExpression,
  SpreadElement,
  Statement,
  SwitchCase,
  SwitchStatement,
  TSAnyKeyword,
  TSArrayType,
  TSAsExpression,
  TSBooleanKeyword,
  TSFunctionType,
  TSIdentifier,
  TSLiteralType,
  TSNode,
  TSNumberKeyword,
  TSStringKeyword,
  TSTupleType,
  TSTypeAliasDeclaration,
  TSTypeAnnotation,
  TSTypeParameterInstantiation,
  TSTypeReference,
  TSUndefinedKeyword,
  TSUnionType,
  TSVoidKeyword,
  TaggedTemplateExpression,
  TemplateElement,
  TemplateLiteral,
  ThisExpression,
  ThrowStatement,
  TrailingComment,
  TryStatement,
  UnaryExpression,
  UpdateExpression,
  VariableDeclaration,
  VariableDeclarator,
  WhileStatement,
  XRawJavaScript,
  YieldExpression,
  estreeType,
  estreeTypeP,
  estreep,
  wrapInEstree
};
"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.Node = exports.NewExpression = exports.MethodDefinition = exports.MemberExpression = exports.LogicalExpression = exports.Literal = exports.LeadingComment = exports.ImportSpecifier = exports.ImportNamespaceSpecifier = exports.ImportDefaultSpecifier = exports.ImportDeclaration = exports.IfStatement = exports.Identifier = exports.FunctionExpression = exports.FunctionDeclaration = exports.Function = exports.ForStatement = exports.ForOfStatement = exports.ForInStatement = exports.ExpressionStatement = exports.Expression = exports.ExportSpecifier = exports.ExportNamedDeclaration = exports.ExportAllDeclaration = exports.DoWhileStatement = exports.ContinueStatement = exports.ConditionalExpression = exports.Comment = exports.ClassExpression = exports.ClassDeclaration = exports.ClassBody = exports.ChainExpression = exports.ChainElement = exports.CatchClause = exports.CallExpression = exports.BreakStatement = exports.BlockStatement = exports.BlockComment = exports.BinaryExpression = exports.AwaitExpression = exports.AssignmentPattern = exports.AssignmentExpression = exports.ArrowFunctionExpression = exports.ArrayPattern = exports.ArrayExpression = exports.estreeIsP = exports.TSESTreeNode = exports.ESTreeStatement = exports.ESTreeNode = exports.ESTreeExpression = void 0;
exports.wrapInEstree = exports.estreep = exports.estreeTypeP = exports.estreeType = exports.YieldExpression = exports.XRawJavaScript = exports.WhileStatement = exports.VariableDeclarator = exports.VariableDeclaration = exports.UpdateExpression = exports.UnaryExpression = exports.TryStatement = exports.TrailingComment = exports.ThrowStatement = exports.ThisExpression = exports.TemplateLiteral = exports.TemplateElement = exports.TaggedTemplateExpression = exports.TSVoidKeyword = exports.TSUnionType = exports.TSUndefinedKeyword = exports.TSTypeReference = exports.TSTypeParameterInstantiation = exports.TSTypeAnnotation = exports.TSTypeAliasDeclaration = exports.TSTupleType = exports.TSStringKeyword = exports.TSNumberKeyword = exports.TSNode = exports.TSLiteralType = exports.TSIdentifier = exports.TSFunctionType = exports.TSBooleanKeyword = exports.TSAsExpression = exports.TSArrayType = exports.TSAnyKeyword = exports.SwitchStatement = exports.SwitchCase = exports.Statement = exports.SpreadElement = exports.SequenceExpression = exports.ReturnStatement = exports.RestElement = exports.RegExpLiteral = exports.PropertyDefinition = exports.Property = exports.Program = exports.ObjectPattern = exports.ObjectExpression = void 0;
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
    constructor() {
        this.type = 'Node';
        this.comments = [];
        this.loc = null;
    }
    addComment(comment) {
        this.comments.push(comment);
        return this;
    }
}
exports.ESTreeNode = Node;
exports.Node = Node;
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
    constructor() {
        super(...arguments);
        this.type = 'TSNode';
    }
    getType() {
        return this.getTypeAnnotation();
    }
    getTypeAnnotation() {
        return this.typeAnnotation;
    }
    hasType() {
        return this.getType() && true;
    }
    setType(type_) {
        return this.setTypeAnnotation(type_);
    }
    setTypeAnnotation(typeAnnotation) {
        this.typeAnnotation = typeAnnotation;
        return this;
    }
}
exports.TSESTreeNode = TSNode;
exports.TSNode = TSNode;
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
    constructor() {
        super(...arguments);
        this.type = 'Expression';
    }
}
exports.ESTreeExpression = Expression;
exports.Expression = Expression;
/**
 * Program
 *
 * Creates an ESTree [`Program`][estree:program] node.
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
class Program extends TSNode {
    constructor(body) {
        super();
        this.type = 'Program';
        this.body = body;
    }
}
exports.Program = Program;
/**
 * Statement
 *
 * Creates an ESTree [`Statement`][estree:statement] node.
 *
 * [estree:statement]: https://github.com/estree/estree/blob/master/es5.md#statements
 */
class Statement extends TSNode {
    constructor() {
        super(...arguments);
        this.type = 'Statement';
    }
}
exports.ESTreeStatement = Statement;
exports.Statement = Statement;
/**
 * IfStatement
 *
 * Creates an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
class IfStatement extends Statement {
    constructor(test, consequent, alternate) {
        super();
        this.type = 'IfStatement';
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}
exports.IfStatement = IfStatement;
/**
 * BlockStatement
 *
 * Creates an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
class BlockStatement extends Statement {
    constructor(body = []) {
        super();
        this.type = 'BlockStatement';
        this.body = body;
    }
}
exports.BlockStatement = BlockStatement;
/**
 * ExpressionStatement
 *
 * Creates an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
 *
 * [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
 */
class ExpressionStatement extends Statement {
    constructor(expression) {
        super();
        this.type = 'ExpressionStatement';
        this.expression = expression;
    }
}
exports.ExpressionStatement = ExpressionStatement;
/**
 * ReturnStatement
 *
 * Creates an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
class ReturnStatement extends Statement {
    constructor(argument = null) {
        super();
        this.type = 'ReturnStatement';
        this.argument = argument;
    }
}
exports.ReturnStatement = ReturnStatement;
/**
 * ThrowStatement
 *
 * Creates an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
class ThrowStatement extends Statement {
    constructor(argument) {
        super();
        this.type = 'ThrowStatement';
        this.argument = argument;
    }
}
exports.ThrowStatement = ThrowStatement;
/**
 * BreakStatement
 *
 * Creates an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
class BreakStatement extends Statement {
    constructor(label = null) {
        super();
        this.type = 'BreakStatement';
        this.label = label;
    }
}
exports.BreakStatement = BreakStatement;
/**
 * ContinueStatement
 *
 * Creates an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
class ContinueStatement extends Statement {
    constructor(label = null) {
        super();
        this.type = 'ContinueStatement';
        this.label = label;
    }
}
exports.ContinueStatement = ContinueStatement;
/**
 * WhileStatement
 *
 * Creates an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
class WhileStatement extends Statement {
    constructor(test, body) {
        super();
        this.type = 'WhileStatement';
        this.test = test;
        this.body = body;
    }
}
exports.WhileStatement = WhileStatement;
/**
 * DoWhileStatement
 *
 * Creates an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
 *
 * [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
 */
class DoWhileStatement extends Statement {
    constructor(test, body) {
        super();
        this.type = 'DoWhileStatement';
        this.test = test;
        this.body = body;
    }
}
exports.DoWhileStatement = DoWhileStatement;
/**
 * ForStatement
 *
 * Creates an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
class ForStatement extends Statement {
    constructor(init, test, update, body) {
        super();
        this.type = 'ForStatement';
        this.init = init;
        this.test = test;
        this.update = update;
        this.body = body;
    }
}
exports.ForStatement = ForStatement;
/**
 * ForInStatement
 *
 * Creates an ESTree [`ForInStatement`][estree:forinstatement] node.
 *
 * [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
 */
class ForInStatement extends Statement {
    constructor(left, right, body) {
        super();
        this.type = 'ForInStatement';
        this.left = left;
        this.right = right;
        this.body = body;
    }
}
exports.ForInStatement = ForInStatement;
/**
 * ForOfStatement
 *
 * Creates an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
class ForOfStatement extends ForInStatement {
    constructor(left, right, body) {
        super(left, right, body);
        this.type = 'ForOfStatement';
        this.left = left;
        this.right = right;
        this.body = body;
    }
}
exports.ForOfStatement = ForOfStatement;
/**
 * TryStatement
 *
 * Creates an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
class TryStatement extends Statement {
    constructor(block, handler = null, finalizer = null) {
        super();
        this.type = 'TryStatement';
        this.block = block;
        this.handler = handler;
        this.finalizer = finalizer;
    }
}
exports.TryStatement = TryStatement;
/**
 * CatchClause
 *
 * Creates an ESTree [`CatchClause`][estree:catchclause] node.
 *
 * [estree:catchclause]: https://github.com/estree/estree/blob/master/es5.md#catchclause
 */
class CatchClause extends TSNode {
    constructor(param, body) {
        super();
        this.type = 'CatchClause';
        this.param = param;
        this.body = body;
    }
}
exports.CatchClause = CatchClause;
/**
 * Declaration
 *
 * Creates an ESTree [`Declaration`][estree:declaration] node.
 *
 * [estree:declaration]: https://github.com/estree/estree/blob/master/es5.md#declarations
 */
class Declaration extends Statement {
    constructor() {
        super(...arguments);
        this.type = 'Declaration';
    }
}
/**
 * VariableDeclaration
 *
 * Creates an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
class VariableDeclaration extends Declaration {
    constructor(declarations, kind = 'var') {
        super();
        this.type = 'VariableDeclaration';
        this.declarations = declarations;
        this.kind = kind;
    }
}
exports.VariableDeclaration = VariableDeclaration;
/**
 * VariableDeclarator
 *
 * Creates an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
 *
 * [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
 */
class VariableDeclarator extends TSNode {
    constructor(id, init = null) {
        super();
        this.type = 'VariableDeclarator';
        this.id = id;
        this.init = init;
    }
}
exports.VariableDeclarator = VariableDeclarator;
/**
 * ClassDeclaration
 *
 * Creates an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
class ClassDeclaration extends Declaration {
    constructor(id, body = new ClassBody(), superClass = undefined) {
        super();
        this.type = 'ClassDeclaration';
        this.id = id;
        this.body = body;
        this.superClass = superClass;
    }
}
exports.ClassDeclaration = ClassDeclaration;
/**
 * ImportOrExportDeclaration
 *
 * Creates an ESTree [`ImportOrExportDeclaration`][estree:importorexportdeclaration] node.
 *
 * [estree:importorexportdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importorexportdeclaration
 */
class ImportOrExportDeclaration extends Declaration {
    constructor() {
        super(...arguments);
        this.type = 'ImportOrExportDeclaration';
    }
}
/**
 * ModuleSpecifier
 *
 * Creates an ESTree [`ModuleSpecifier`][estree:modulespecifier] node.
 *
 * [estree:modulespecifier]: https://github.com/estree/estree/blob/master/es2015.md#modulespecifier
 */
class ModuleSpecifier extends TSNode {
    constructor(local) {
        super();
        this.type = 'ModuleSpecifier';
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
    constructor(specifiers, source) {
        super();
        this.type = 'ImportDeclaration';
        this.specifiers = specifiers;
        this.source = source;
    }
}
exports.ImportDeclaration = ImportDeclaration;
/**
 * ImportSpecifier
 *
 * Creates an ESTree [`ImportSpecifier`][estree:importspecifier] node.
 *
 * [estree:importspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importspecifier
 */
class ImportSpecifier extends ModuleSpecifier {
    constructor(local, imported = undefined) {
        super(local);
        this.type = 'ImportSpecifier';
        this.local = local;
        this.imported = imported || local;
    }
}
exports.ImportSpecifier = ImportSpecifier;
/**
 * ImportDefaultSpecifier
 *
 * Creates an ESTree [`ImportDefaultSpecifier`][estree:importdefaultspecifier] node.
 *
 * [estree:importdefaultspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importdefaultspecifier
 */
class ImportDefaultSpecifier extends ModuleSpecifier {
    constructor(local) {
        super(local);
        this.type = 'ImportDefaultSpecifier';
        this.local = local;
    }
}
exports.ImportDefaultSpecifier = ImportDefaultSpecifier;
/**
 * ImportNamespaceSpecifier
 *
 * Creates an ESTree [`ImportNamespaceSpecifier`][estree:importnamespacespecifier] node.
 *
 * [estree:importnamespacespecifier]: https://github.com/estree/estree/blob/master/es2015.md#importnamespacespecifier
 */
class ImportNamespaceSpecifier extends ModuleSpecifier {
    constructor(local) {
        super(local);
        this.type = 'ImportNamespaceSpecifier';
        this.local = local;
    }
}
exports.ImportNamespaceSpecifier = ImportNamespaceSpecifier;
/**
 * ExportNamedDeclaration
 *
 * Creates an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
 *
 * [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
 */
class ExportNamedDeclaration extends ImportOrExportDeclaration {
    constructor(declaration = null, specifiers = [], source = null) {
        super();
        this.type = 'ExportNamedDeclaration';
        this.declaration = declaration;
        this.specifiers = specifiers;
        this.source = source;
    }
}
exports.ExportNamedDeclaration = ExportNamedDeclaration;
/**
 * ExportSpecifier
 *
 * Creates an ESTree [`ExportSpecifier`][estree:exportspecifier] node.
 *
 * [estree:exportspecifier]: https://github.com/estree/estree/blob/master/es2015.md#exportspecifier
 */
class ExportSpecifier extends ModuleSpecifier {
    constructor(local, exported = undefined) {
        super(local);
        this.type = 'ExportSpecifier';
        this.local = local;
        this.exported = exported || local;
    }
}
exports.ExportSpecifier = ExportSpecifier;
/**
 * ExportAllDeclaration
 *
 * Creates an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
 *
 * [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
 */
class ExportAllDeclaration extends ImportOrExportDeclaration {
    constructor(source) {
        super();
        this.type = 'ExportAllDeclaration';
        this.source = source;
    }
}
exports.ExportAllDeclaration = ExportAllDeclaration;
/**
 * ClassExpression
 *
 * Creates an ESTree [`ClassExpression`][estree:classexpression] node.
 *
 * [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
 */
class ClassExpression extends Expression {
    constructor(body = new ClassBody(), superClass = undefined) {
        super();
        this.type = 'ClassExpression';
        this.body = body;
        this.superClass = superClass;
    }
}
exports.ClassExpression = ClassExpression;
/**
 * ClassBody
 *
 * Creates an ESTree [`ClassBody`][estree:classbody] node.
 *
 * [estree:classbody]: https://github.com/estree/estree/blob/master/es2015.md#classbody
 */
class ClassBody extends TSNode {
    constructor(body = []) {
        super();
        this.type = 'ClassBody';
        this.body = body;
    }
}
exports.ClassBody = ClassBody;
/**
 * PropertyDefinition
 *
 * Creates an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
 *
 * [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
 */
class PropertyDefinition extends TSNode {
    constructor(key, value, staticFlag = false, accessibility = 'public') {
        super();
        this.type = 'PropertyDefinition';
        this.key = key;
        this.value = value;
        this.static = staticFlag;
        this.accessibility = accessibility;
    }
}
exports.PropertyDefinition = PropertyDefinition;
/**
 * MethodDefinition
 *
 * Creates an ESTree [`MethodDefinition`][estree:methoddefinition] node.
 *
 * [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
 */
class MethodDefinition extends TSNode {
    constructor(key, value, kind = 'method', staticFlag = false, override = false, computed = false, accessibility = 'public') {
        super();
        this.type = 'MethodDefinition';
        this.key = key;
        this.value = value;
        this.kind = kind;
        this.static = staticFlag;
        this.override = override;
        this.computed = computed;
        this.accessibility = accessibility;
    }
}
exports.MethodDefinition = MethodDefinition;
/**
 * Identifier
 *
 * Creates an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
class Identifier extends Expression {
    constructor(name, optional = false) {
        super();
        this.type = 'Identifier';
        this.name = name;
        this.optional = optional;
    }
}
exports.Identifier = Identifier;
/**
 * Literal
 *
 * Creates an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
class Literal extends Expression {
    constructor(value, raw = value) {
        super();
        this.type = 'Literal';
        this.value = value;
        this.raw = raw;
    }
}
exports.Literal = Literal;
/**
 * RegExpLiteral
 *
 * Creates an ESTree [`RegExpLiteral`][estree:regexpliteral] node.
 *
 * [estree:regexpliteral]: https://github.com/estree/estree/blob/master/es5.md#regexpliteral
 */
class RegExpLiteral extends Literal {
    constructor(pattern, flags = '') {
        super(pattern);
        this.type = 'RegExpLiteral';
        this.pattern = pattern;
        this.flags = flags;
    }
}
exports.RegExpLiteral = RegExpLiteral;
/**
 * CallExpression
 *
 * Creates an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
class CallExpression extends Expression {
    constructor(callee, args = [], optional = false) {
        super();
        this.type = 'CallExpression';
        this.callee = callee;
        this.arguments = args;
        this.optional = optional;
    }
}
exports.CallExpression = CallExpression;
/**
 * MemberExpression
 *
 * Creates an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
class MemberExpression extends Expression {
    constructor(object, property, computed = false) {
        super();
        this.type = 'MemberExpression';
        this.object = object;
        this.property = property;
        this.computed = computed;
    }
}
exports.MemberExpression = MemberExpression;
/**
 * ChainExpression
 *
 * Creates an ESTree [`ChainExpression`][estree:chainexpression] node.
 *
 * [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
class ChainExpression extends Expression {
    constructor(expression) {
        super();
        this.type = 'ChainExpression';
        this.expression = expression;
    }
}
exports.ChainExpression = ChainExpression;
/**
 * ChainElement
 *
 * Creates an ESTree [`ChainElement`][estree:chainelement] node.
 *
 * [estree:chainelement]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
class ChainElement extends Node {
    constructor(optional = false) {
        super();
        this.type = 'ChainElement';
        this.optional = optional;
    }
}
exports.ChainElement = ChainElement;
/**
 * UnaryExpression
 *
 * Creates an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
class UnaryExpression extends Expression {
    constructor(operator, prefix, argument) {
        super();
        this.type = 'UnaryExpression';
        this.operator = operator;
        this.prefix = prefix;
        this.argument = argument;
    }
}
exports.UnaryExpression = UnaryExpression;
/**
 * UpdateExpression
 *
 * Creates an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
class UpdateExpression extends Expression {
    constructor(operator, argument, prefix) {
        super();
        this.type = 'UpdateExpression';
        this.operator = operator;
        this.argument = argument;
        this.prefix = prefix;
    }
}
exports.UpdateExpression = UpdateExpression;
/**
 * BinaryExpression
 *
 * Creates an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
class BinaryExpression extends Expression {
    constructor(operator, left, right) {
        super();
        this.type = 'BinaryExpression';
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
exports.BinaryExpression = BinaryExpression;
/**
 * LogicalExpression
 *
 * Creates an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
class LogicalExpression extends Expression {
    constructor(operator, left, right) {
        super();
        this.type = 'LogicalExpression';
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
exports.LogicalExpression = LogicalExpression;
/**
 * AssignmentExpression
 *
 * Creates an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
class AssignmentExpression extends Expression {
    constructor(operator, left, right) {
        super();
        this.type = 'AssignmentExpression';
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
exports.AssignmentExpression = AssignmentExpression;
/**
 * SequenceExpression
 *
 * Creates an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
class SequenceExpression extends Expression {
    constructor(expressions) {
        super();
        this.type = 'SequenceExpression';
        this.expressions = expressions;
    }
}
exports.SequenceExpression = SequenceExpression;
/**
 * ObjectExpression
 *
 * Creates an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
class ObjectExpression extends Expression {
    constructor(properties = []) {
        super();
        this.type = 'ObjectExpression';
        this.properties = properties;
    }
}
exports.ObjectExpression = ObjectExpression;
/**
 * Property
 *
 * Creates an ESTree [`Property`][estree:property] node.
 *
 * [estree:property]: https://github.com/estree/estree/blob/master/es5.md#property
 */
class Property extends TSNode {
    constructor(key, value, computed = false, kind = 'init') {
        super();
        this.type = 'Property';
        this.key = key;
        this.value = value;
        this.computed = computed;
        this.kind = kind;
    }
}
exports.Property = Property;
/**
 * Function
 *
 * Creates an ESTree [`Function`][estree:function] node.
 *
 * [estree:function]: https://github.com/estree/estree/blob/master/es5.md#functions
 */
class Function extends TSNode {
    constructor(id, params, body) {
        super();
        this.type = 'Function';
        this.id = id;
        this.params = params;
        this.body = body;
    }
}
exports.Function = Function;
/**
 * FunctionDeclaration
 *
 * Creates an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
class FunctionDeclaration extends Declaration {
    constructor(id, params, body, async = false, returnType = undefined, generator = false) {
        super();
        this.type = 'FunctionDeclaration';
        this.id = id;
        this.params = params;
        this.body = body;
        this.async = async;
        this.returnType = returnType;
        this.generator = generator;
    }
}
exports.FunctionDeclaration = FunctionDeclaration;
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
    constructor(params, body, async = false, generator = false) {
        super();
        this.type = 'FunctionExpression';
        this.id = null;
        this.params = params;
        this.body = body;
        this.async = async;
        this.generator = generator;
    }
    setType(type_) {
        this.returnType = type_;
        return this;
    }
}
exports.FunctionExpression = FunctionExpression;
/**
 * ArrowFunctionExpression
 *
 * Creates an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
class ArrowFunctionExpression extends FunctionExpression {
    constructor(params, body, async = false) {
        super(params, body, async);
        this.type = 'ArrowFunctionExpression';
    }
}
exports.ArrowFunctionExpression = ArrowFunctionExpression;
/**
 * ConditionalExpression
 *
 * Creates an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
class ConditionalExpression extends Expression {
    constructor(test, consequent, alternate) {
        super();
        this.type = 'ConditionalExpression';
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}
exports.ConditionalExpression = ConditionalExpression;
/**
 * Pattern
 *
 * Creates an ESTree [`Pattern`][estree:pattern] node.
 *
 * [estree:pattern]: https://github.com/estree/estree/blob/master/es5.md#patterns
 */
class Pattern extends TSNode {
    constructor() {
        super(...arguments);
        this.type = 'Pattern';
    }
}
/**
 * AssignmentPattern
 *
 * Creates an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
 *
 * [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
 */
class AssignmentPattern extends Pattern {
    constructor(left, right) {
        super();
        this.type = 'AssignmentPattern';
        this.left = left;
        this.right = right;
    }
    getType() {
        return this.left.getType();
    }
    setType(type_) {
        this.left.setType(type_);
        return this;
    }
}
exports.AssignmentPattern = AssignmentPattern;
/**
 * ArrayPattern
 *
 * Creates an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
class ArrayPattern extends Pattern {
    constructor(elements) {
        super();
        this.type = 'ArrayPattern';
        this.elements = elements;
    }
}
exports.ArrayPattern = ArrayPattern;
/**
 * ObjectPattern
 *
 * Creates an ESTree [`ObjectPattern`][estree:objectpattern] node.
 *
 * [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
 */
class ObjectPattern extends Pattern {
    constructor(properties = []) {
        super();
        this.type = 'ObjectPattern';
        this.properties = properties;
    }
}
exports.ObjectPattern = ObjectPattern;
/**
 * ThisExpression
 *
 * Creates an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
class ThisExpression extends Expression {
    constructor() {
        super(...arguments);
        this.type = 'ThisExpression';
    }
}
exports.ThisExpression = ThisExpression;
/**
 * ArrayExpression
 *
 * Creates an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
class ArrayExpression extends Expression {
    constructor(elements = []) {
        super();
        this.type = 'ArrayExpression';
        this.elements = elements;
    }
}
exports.ArrayExpression = ArrayExpression;
/**
 * RestElement
 *
 * Creates an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
class RestElement extends Pattern {
    constructor(argument) {
        super();
        this.type = 'RestElement';
        this.argument = argument;
    }
}
exports.RestElement = RestElement;
/**
 * SpreadElement
 *
 * Creates an ESTree [`SpreadElement`][estree:spreadelement] node.
 *
 * [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
class SpreadElement extends Expression {
    constructor(argument) {
        super();
        this.type = 'SpreadElement';
        this.argument = argument;
    }
}
exports.SpreadElement = SpreadElement;
/**
 * NewExpression
 *
 * Creates an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
class NewExpression extends Expression {
    constructor(callee, args = []) {
        super();
        this.type = 'NewExpression';
        this.callee = callee;
        this.arguments = args;
    }
}
exports.NewExpression = NewExpression;
/**
 * YieldExpression
 *
 * Creates an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
class YieldExpression extends Expression {
    constructor(argument = null, delegate = false) {
        super();
        this.type = 'YieldExpression';
        this.argument = argument;
        this.delegate = delegate;
    }
}
exports.YieldExpression = YieldExpression;
/**
 * AwaitExpression
 *
 * Creates an ESTree [`AwaitExpression`][estree:awaitexpression] node.
 *
 * [estree:awaitexpression]: https://github.com/estree/estree/blob/master/es2017.md#awaitexpression
 */
class AwaitExpression extends Expression {
    constructor(argument = null) {
        super();
        this.type = 'AwaitExpression';
        this.argument = argument;
    }
}
exports.AwaitExpression = AwaitExpression;
/**
 * TemplateLiteral
 *
 * Creates an ESTree [`TemplateLiteral`][estree:templateliteral] node.
 *
 * [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
 */
class TemplateLiteral extends Expression {
    constructor(quasis = [], expressions = []) {
        super();
        this.type = 'TemplateLiteral';
        this.quasis = quasis;
        this.expressions = expressions;
    }
}
exports.TemplateLiteral = TemplateLiteral;
/**
 * TaggedTemplateExpression
 *
 * Creates an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
 *
 * [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
 */
class TaggedTemplateExpression extends Expression {
    constructor(tag, quasi) {
        super();
        this.type = 'TaggedTemplateExpression';
        this.tag = tag;
        this.quasi = quasi;
    }
}
exports.TaggedTemplateExpression = TaggedTemplateExpression;
/**
 * TemplateElement
 *
 * Creates an ESTree [`TemplateElement`][estree:templateelement] node.
 *
 * [estree:templateelement]: https://github.com/estree/estree/blob/master/es2015.md#templateelement
 */
class TemplateElement extends TSNode {
    constructor(tail, cooked, raw = undefined) {
        super();
        this.type = 'TemplateElement';
        this.tail = tail;
        this.value = {
            cooked: cooked,
            raw: raw || cooked.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('`', 'g'), '\\`')
        };
    }
}
exports.TemplateElement = TemplateElement;
/**
 * SwitchStatement
 *
 * Creates an ESTree [`SwitchStatement`][estree:switchstatement] node.
 *
 * [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
 */
class SwitchStatement extends Statement {
    constructor(discriminant, cases = []) {
        super();
        this.type = 'SwitchStatement';
        this.discriminant = discriminant;
        this.cases = cases;
    }
}
exports.SwitchStatement = SwitchStatement;
/**
 * SwitchCase
 *
 * Creates an ESTree [`SwitchCase`][estree:switchcase] node.
 *
 * [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
 */
class SwitchCase extends TSNode {
    constructor(test, consequent = []) {
        super();
        this.type = 'SwitchCase';
        this.test = test;
        this.consequent = consequent;
    }
}
exports.SwitchCase = SwitchCase;
/**
 * ESTree extensions
 */
/**
 * TSIdentifier
 *
 * TSTree version of `Identifier`.
 */
class TSIdentifier extends Identifier {
    constructor(name, typeAnnotation) {
        super(name);
        this.type = 'Identifier';
        this.typeAnnotation = typeAnnotation;
    }
}
exports.TSIdentifier = TSIdentifier;
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
    constructor(typeAnnotation) {
        super();
        this.type = 'TSTypeAnnotation';
        this.typeAnnotation = typeAnnotation;
    }
}
exports.TSTypeAnnotation = TSTypeAnnotation;
/**
 * TSKeywordTypeNode
 *
 * Base class for TSESTree keywords.
 */
class TSKeywordTypeNode extends Node {
    constructor() {
        super(...arguments);
        this.type = 'TSKeywordTypeNode';
    }
}
/**
 * TSAnyKeyword
 *
 * TypeScript `any` type.
 */
class TSAnyKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSAnyKeyword';
    }
}
exports.TSAnyKeyword = TSAnyKeyword;
/**
 * TSVoidKeyword
 *
 * TypeScript `void` type.
 */
class TSVoidKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSVoidKeyword';
    }
}
exports.TSVoidKeyword = TSVoidKeyword;
/**
 * TSUndefinedKeyword
 *
 * TypeScript `undefined` type.
 */
class TSUndefinedKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSUndefinedKeyword';
    }
}
exports.TSUndefinedKeyword = TSUndefinedKeyword;
/**
 * TSBooleanKeyword
 *
 * TypeScript `boolean` type.
 */
class TSBooleanKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSBooleanKeyword';
    }
}
exports.TSBooleanKeyword = TSBooleanKeyword;
/**
 * TSNumberKeyword
 *
 * TypeScript `number` type.
 */
class TSNumberKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSNumberKeyword';
    }
}
exports.TSNumberKeyword = TSNumberKeyword;
/**
 * TSStringKeyword
 *
 * TypeScript `string` type.
 */
class TSStringKeyword extends TSKeywordTypeNode {
    constructor() {
        super(...arguments);
        this.type = 'TSStringKeyword';
    }
}
exports.TSStringKeyword = TSStringKeyword;
/**
 * TSArrayType
 *
 * TypeScript array type.
 */
class TSArrayType extends Node {
    constructor(elementType) {
        super();
        this.type = 'TSArrayType';
        this.elementType = elementType;
    }
}
exports.TSArrayType = TSArrayType;
/**
 * TSTupleType
 *
 * TypeScript tuple type.
 */
class TSTupleType extends Node {
    constructor(elementTypes) {
        super();
        this.type = 'TSTupleType';
        this.elementTypes = elementTypes;
    }
}
exports.TSTupleType = TSTupleType;
/**
 * TSLiteralType
 *
 * TypeScript literal type.
 */
class TSLiteralType extends Node {
    constructor(literal) {
        super();
        this.type = 'TSLiteralType';
        this.literal = literal;
    }
}
exports.TSLiteralType = TSLiteralType;
/**
 * TSUnionType
 *
 * TypeScript union type.
 */
class TSUnionType extends Node {
    constructor(types) {
        super();
        this.type = 'TSUnionType';
        this.types = types;
    }
}
exports.TSUnionType = TSUnionType;
/**
 * TSFunctionType
 *
 * TypeScript function type.
 */
class TSFunctionType extends Node {
    constructor(params, returnType) {
        super();
        this.type = 'TSFunctionType';
        this.params = params;
        this.returnType = returnType;
    }
}
exports.TSFunctionType = TSFunctionType;
/**
 * TSTypeAliasDeclaration
 *
 * TypeScript `type` alias declaration.
 */
class TSTypeAliasDeclaration extends Node {
    constructor(id, typeAnnotation) {
        super();
        this.type = 'TSTypeAliasDeclaration';
        this.id = id;
        this.typeAnnotation = typeAnnotation;
    }
}
exports.TSTypeAliasDeclaration = TSTypeAliasDeclaration;
/**
 * TSTypeReference
 *
 * TypeScript `boolean` type.
 */
class TSTypeReference extends Node {
    constructor(typeName, typeParameters = undefined) {
        super();
        this.type = 'TSTypeReference';
        this.typeName = typeName;
        this.typeParameters = typeParameters;
    }
}
exports.TSTypeReference = TSTypeReference;
/**
 * TSTypeParameterInstantiation
 *
 * TypeScript type parameters.
 */
class TSTypeParameterInstantiation extends Node {
    constructor(params = []) {
        super();
        this.type = 'TSTypeParameterInstantiation';
        this.params = params;
    }
}
exports.TSTypeParameterInstantiation = TSTypeParameterInstantiation;
/**
 * TSAsExpression
 *
 * TypeScript `as` expression.
 */
class TSAsExpression extends Expression {
    constructor(expression, typeAnnotation) {
        super();
        this.type = 'TSAsExpression';
        this.expression = expression;
        this.typeAnnotation = typeAnnotation;
    }
}
exports.TSAsExpression = TSAsExpression;
/**
 * Comment
 *
 * Base class for comments.
 */
class Comment {
    constructor(originalText, commentType = 'leading') {
        this.originalText = '';
        this.leading = true;
        this.trailing = false;
        this.printed = true;
        this.originalText = originalText;
        if (commentType === 'trailing') {
            this.leading = false;
            this.trailing = true;
        }
        else {
            this.leading = true;
            this.trailing = false;
        }
    }
    getText() {
        return this.originalText;
    }
}
exports.Comment = Comment;
/**
 * LeadingComment
 *
 * Leading comment.
 */
class LeadingComment extends Comment {
    constructor(originalText) {
        super(originalText, 'leading');
    }
}
exports.LeadingComment = LeadingComment;
/**
 * BlockComment
 *
 * Block comment.
 */
class BlockComment extends LeadingComment {
    constructor(originalText) {
        super(originalText);
    }
}
exports.BlockComment = BlockComment;
/**
 * TrailingComment
 *
 * Trailing comment.
 */
class TrailingComment extends Comment {
    constructor(originalText) {
        super(originalText, 'trailing');
    }
}
exports.TrailingComment = TrailingComment;
/**
 * XRawJavaScript
 *
 * A raw JavaScript string that is to be spliced directly
 * into the output.
 *
 * Nonstandard ESTree extension.
 */
class XRawJavaScript extends TSNode {
    constructor(js) {
        super();
        this.type = 'XRawJavaScript';
        this.js = js;
    }
}
exports.XRawJavaScript = XRawJavaScript;
/**
 * Whether `obj` is an ESTree.
 *
 * This function only works on instantiated objects.
 */
function estreep(obj) {
    return obj instanceof Node;
}
exports.estreep = estreep;
estreep.lispSource = [Symbol.for('define'), [Symbol.for('estree?'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Node')]];
/**
 * Get the type of an ESTree node.
 */
function estreeType(node) {
    return node.type;
}
exports.estreeType = estreeType;
estreeType.lispSource = [Symbol.for('define'), [Symbol.for('estree-type'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('type'), Symbol.for('node')]];
/**
 * Whether the type of the ESTree node `node` is `typ`.
 */
function estreeTypeP(node, typ) {
    return estreeType(node) === typ;
}
exports.estreeIsP = estreeTypeP;
exports.estreeTypeP = estreeTypeP;
estreeTypeP.lispSource = [Symbol.for('define'), [Symbol.for('estree-type?'), Symbol.for('node'), Symbol.for('typ')], [Symbol.for('eq?'), [Symbol.for('estree-type'), Symbol.for('node')], Symbol.for('typ')]];
/**
 * Wrap a value in an ESTree node.
 *
 * Distinguishes between list values and atomic values.
 */
function wrapInEstree(x) {
    if (Array.isArray(x)) {
        return new ArrayExpression(x.map(function (x) {
            return wrapInEstree(x);
        }));
    }
    else {
        return new Literal(x);
    }
}
exports.wrapInEstree = wrapInEstree;
wrapInEstree.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-estree'), Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('x')], [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), Symbol.for('wrap-in-estree'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('x')]]]];

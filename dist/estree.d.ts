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
declare class Node {
    type: any;
    comments: any;
    loc: any;
    addComment(comment: any): any;
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
declare class TSNode extends Node {
    type: any;
    typeAnnotation: any;
    getType(): any;
    getTypeAnnotation(): any;
    hasType(): any;
    setType(type_: any): any;
    setTypeAnnotation(typeAnnotation: any): any;
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
declare class Expression extends TSNode {
    type: any;
}
/**
 * Program
 *
 * Creates an ESTree [`Program`][estree:program] node.
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
declare class Program extends TSNode {
    type: any;
    body: any;
    constructor(body: any);
}
/**
 * Statement
 *
 * Creates an ESTree [`Statement`][estree:statement] node.
 *
 * [estree:statement]: https://github.com/estree/estree/blob/master/es5.md#statements
 */
declare class Statement extends TSNode {
    type: any;
}
/**
 * IfStatement
 *
 * Creates an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
declare class IfStatement extends Statement {
    type: any;
    test: any;
    consequent: any;
    alternate: any;
    constructor(test: any, consequent: any, alternate: any);
}
/**
 * BlockStatement
 *
 * Creates an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
declare class BlockStatement extends Statement {
    type: any;
    body: any;
    constructor(body?: any);
}
/**
 * ExpressionStatement
 *
 * Creates an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
 *
 * [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
 */
declare class ExpressionStatement extends Statement {
    type: any;
    expression: any;
    constructor(expression: any);
}
/**
 * ReturnStatement
 *
 * Creates an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
declare class ReturnStatement extends Statement {
    type: any;
    argument: any;
    constructor(argument?: any);
}
/**
 * ThrowStatement
 *
 * Creates an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
declare class ThrowStatement extends Statement {
    type: any;
    argument: any;
    constructor(argument: any);
}
/**
 * BreakStatement
 *
 * Creates an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
declare class BreakStatement extends Statement {
    type: any;
    label: any;
    constructor(label?: any);
}
/**
 * ContinueStatement
 *
 * Creates an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
declare class ContinueStatement extends Statement {
    type: any;
    label: any;
    constructor(label?: any);
}
/**
 * WhileStatement
 *
 * Creates an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
declare class WhileStatement extends Statement {
    type: any;
    test: any;
    body: any;
    constructor(test: any, body: any);
}
/**
 * DoWhileStatement
 *
 * Creates an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
 *
 * [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
 */
declare class DoWhileStatement extends Statement {
    type: any;
    test: any;
    body: any;
    constructor(test: any, body: any);
}
/**
 * ForStatement
 *
 * Creates an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
declare class ForStatement extends Statement {
    type: any;
    init: any;
    test: any;
    update: any;
    body: any;
    constructor(init: any, test: any, update: any, body: any);
}
/**
 * ForInStatement
 *
 * Creates an ESTree [`ForInStatement`][estree:forinstatement] node.
 *
 * [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
 */
declare class ForInStatement extends Statement {
    type: any;
    left: any;
    right: any;
    body: any;
    constructor(left: any, right: any, body: any);
}
/**
 * ForOfStatement
 *
 * Creates an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
declare class ForOfStatement extends ForInStatement {
    type: any;
    left: any;
    right: any;
    body: any;
    constructor(left: any, right: any, body: any);
}
/**
 * TryStatement
 *
 * Creates an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
declare class TryStatement extends Statement {
    type: any;
    block: any;
    handler: any;
    finalizer: any;
    constructor(block: any, handler?: any, finalizer?: any);
}
/**
 * CatchClause
 *
 * Creates an ESTree [`CatchClause`][estree:catchclause] node.
 *
 * [estree:catchclause]: https://github.com/estree/estree/blob/master/es5.md#catchclause
 */
declare class CatchClause extends TSNode {
    type: any;
    param: any;
    body: any;
    constructor(param: any, body: any);
}
/**
 * Declaration
 *
 * Creates an ESTree [`Declaration`][estree:declaration] node.
 *
 * [estree:declaration]: https://github.com/estree/estree/blob/master/es5.md#declarations
 */
declare class Declaration extends Statement {
    type: any;
}
/**
 * VariableDeclaration
 *
 * Creates an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
declare class VariableDeclaration extends Declaration {
    type: any;
    declarations: any;
    kind: any;
    constructor(declarations: any, kind?: any);
}
/**
 * VariableDeclarator
 *
 * Creates an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
 *
 * [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
 */
declare class VariableDeclarator extends TSNode {
    type: any;
    id: any;
    init: any;
    constructor(id: any, init?: any);
}
/**
 * ClassDeclaration
 *
 * Creates an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
declare class ClassDeclaration extends Declaration {
    type: any;
    id: any;
    body: any;
    superClass: any;
    constructor(id: any, body?: any, superClass?: any);
}
/**
 * ImportOrExportDeclaration
 *
 * Creates an ESTree [`ImportOrExportDeclaration`][estree:importorexportdeclaration] node.
 *
 * [estree:importorexportdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importorexportdeclaration
 */
declare class ImportOrExportDeclaration extends Declaration {
    type: any;
}
/**
 * ModuleSpecifier
 *
 * Creates an ESTree [`ModuleSpecifier`][estree:modulespecifier] node.
 *
 * [estree:modulespecifier]: https://github.com/estree/estree/blob/master/es2015.md#modulespecifier
 */
declare class ModuleSpecifier extends TSNode {
    type: any;
    local: any;
    constructor(local: any);
}
/**
 * ImportDeclaration
 *
 * Creates an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
 *
 * [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
 */
declare class ImportDeclaration extends ImportOrExportDeclaration {
    type: any;
    specifiers: any;
    source: any;
    constructor(specifiers: any, source: any);
}
/**
 * ImportSpecifier
 *
 * Creates an ESTree [`ImportSpecifier`][estree:importspecifier] node.
 *
 * [estree:importspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importspecifier
 */
declare class ImportSpecifier extends ModuleSpecifier {
    type: any;
    local: any;
    imported: any;
    constructor(local: any, imported?: any);
}
/**
 * ImportDefaultSpecifier
 *
 * Creates an ESTree [`ImportDefaultSpecifier`][estree:importdefaultspecifier] node.
 *
 * [estree:importdefaultspecifier]: https://github.com/estree/estree/blob/master/es2015.md#importdefaultspecifier
 */
declare class ImportDefaultSpecifier extends ModuleSpecifier {
    type: any;
    local: any;
    constructor(local: any);
}
/**
 * ImportNamespaceSpecifier
 *
 * Creates an ESTree [`ImportNamespaceSpecifier`][estree:importnamespacespecifier] node.
 *
 * [estree:importnamespacespecifier]: https://github.com/estree/estree/blob/master/es2015.md#importnamespacespecifier
 */
declare class ImportNamespaceSpecifier extends ModuleSpecifier {
    type: any;
    local: any;
    constructor(local: any);
}
/**
 * ExportNamedDeclaration
 *
 * Creates an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
 *
 * [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
 */
declare class ExportNamedDeclaration extends ImportOrExportDeclaration {
    type: any;
    declaration: any;
    specifiers: any;
    source: any;
    constructor(declaration?: any, specifiers?: any, source?: any);
}
/**
 * ExportSpecifier
 *
 * Creates an ESTree [`ExportSpecifier`][estree:exportspecifier] node.
 *
 * [estree:exportspecifier]: https://github.com/estree/estree/blob/master/es2015.md#exportspecifier
 */
declare class ExportSpecifier extends ModuleSpecifier {
    type: any;
    local: any;
    exported: any;
    constructor(local: any, exported?: any);
}
/**
 * ExportAllDeclaration
 *
 * Creates an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
 *
 * [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
 */
declare class ExportAllDeclaration extends ImportOrExportDeclaration {
    type: any;
    source: any;
    constructor(source: any);
}
/**
 * ClassExpression
 *
 * Creates an ESTree [`ClassExpression`][estree:classexpression] node.
 *
 * [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
 */
declare class ClassExpression extends Expression {
    type: any;
    body: any;
    superClass: any;
    constructor(body?: any, superClass?: any);
}
/**
 * ClassBody
 *
 * Creates an ESTree [`ClassBody`][estree:classbody] node.
 *
 * [estree:classbody]: https://github.com/estree/estree/blob/master/es2015.md#classbody
 */
declare class ClassBody extends TSNode {
    type: any;
    body: any;
    constructor(body?: any);
}
/**
 * PropertyDefinition
 *
 * Creates an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
 *
 * [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
 */
declare class PropertyDefinition extends TSNode {
    type: any;
    key: any;
    value: any;
    static: any;
    /**
     * Accessibility (public or private).
     *
     * Not a standard ESTree property, but part of TypeScript.
     */
    accessibility: any;
    constructor(key: any, value: any, staticFlag?: any, accessibility?: any);
}
/**
 * MethodDefinition
 *
 * Creates an ESTree [`MethodDefinition`][estree:methoddefinition] node.
 *
 * [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
 */
declare class MethodDefinition extends TSNode {
    type: any;
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
    constructor(key: any, value: any, kind?: any, staticFlag?: any, override?: any, computed?: any, accessibility?: any);
}
/**
 * Identifier
 *
 * Creates an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
declare class Identifier extends Expression {
    type: any;
    name: any;
    optional: any;
    constructor(name: any, optional?: any);
}
/**
 * Literal
 *
 * Creates an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
declare class Literal extends Expression {
    type: any;
    value: any;
    raw: any;
    constructor(value: any, raw?: any);
}
/**
 * RegExpLiteral
 *
 * Creates an ESTree [`RegExpLiteral`][estree:regexpliteral] node.
 *
 * [estree:regexpliteral]: https://github.com/estree/estree/blob/master/es5.md#regexpliteral
 */
declare class RegExpLiteral extends Literal {
    type: any;
    pattern: any;
    flags: any;
    constructor(pattern: any, flags?: any);
}
/**
 * CallExpression
 *
 * Creates an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
declare class CallExpression extends Expression {
    type: any;
    callee: any;
    arguments: any;
    optional: any;
    constructor(callee: any, args?: any, optional?: any);
}
/**
 * MemberExpression
 *
 * Creates an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
declare class MemberExpression extends Expression {
    type: any;
    object: any;
    property: any;
    computed: any;
    constructor(object: any, property: any, computed?: any);
}
/**
 * ChainExpression
 *
 * Creates an ESTree [`ChainExpression`][estree:chainexpression] node.
 *
 * [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
declare class ChainExpression extends Expression {
    type: any;
    expression: any;
    constructor(expression: any);
}
/**
 * ChainElement
 *
 * Creates an ESTree [`ChainElement`][estree:chainelement] node.
 *
 * [estree:chainelement]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
declare class ChainElement extends Node {
    type: any;
    optional: any;
    constructor(optional?: any);
}
/**
 * UnaryExpression
 *
 * Creates an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
declare class UnaryExpression extends Expression {
    type: any;
    operator: any;
    prefix: any;
    argument: any;
    constructor(operator: any, prefix: any, argument: any);
}
/**
 * UpdateExpression
 *
 * Creates an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
declare class UpdateExpression extends Expression {
    type: any;
    operator: any;
    argument: any;
    prefix: any;
    constructor(operator: any, argument: any, prefix: any);
}
/**
 * BinaryExpression
 *
 * Creates an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
declare class BinaryExpression extends Expression {
    type: any;
    operator: any;
    left: any;
    right: any;
    constructor(operator: any, left: any, right: any);
}
/**
 * LogicalExpression
 *
 * Creates an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
declare class LogicalExpression extends Expression {
    type: any;
    operator: any;
    left: any;
    right: any;
    constructor(operator: any, left: any, right: any);
}
/**
 * AssignmentExpression
 *
 * Creates an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
declare class AssignmentExpression extends Expression {
    type: any;
    operator: any;
    left: any;
    right: any;
    constructor(operator: any, left: any, right: any);
}
/**
 * SequenceExpression
 *
 * Creates an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
declare class SequenceExpression extends Expression {
    type: any;
    expressions: any;
    constructor(expressions: any);
}
/**
 * ObjectExpression
 *
 * Creates an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
declare class ObjectExpression extends Expression {
    type: any;
    properties: any;
    constructor(properties?: any);
}
/**
 * Property
 *
 * Creates an ESTree [`Property`][estree:property] node.
 *
 * [estree:property]: https://github.com/estree/estree/blob/master/es5.md#property
 */
declare class Property extends TSNode {
    type: any;
    key: any;
    value: any;
    computed: any;
    kind: any;
    constructor(key: any, value: any, computed?: any, kind?: any);
}
/**
 * Function
 *
 * Creates an ESTree [`Function`][estree:function] node.
 *
 * [estree:function]: https://github.com/estree/estree/blob/master/es5.md#functions
 */
declare class Function extends TSNode {
    type: any;
    id: any;
    params: any;
    body: any;
    constructor(id: any, params: any, body: any);
}
/**
 * FunctionDeclaration
 *
 * Creates an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
declare class FunctionDeclaration extends Declaration {
    type: any;
    id: any;
    params: any;
    body: any;
    async: any;
    generator: any;
    returnType: any;
    constructor(id: any, params: any, body: any, async?: any, returnType?: any, generator?: any);
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
declare class FunctionExpression extends Expression {
    type: any;
    id: any;
    params: any;
    body: any;
    async: any;
    generator: any;
    returnType: any;
    constructor(params: any, body: any, async?: any, generator?: any);
    setType(type_: any): any;
}
/**
 * ArrowFunctionExpression
 *
 * Creates an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
declare class ArrowFunctionExpression extends FunctionExpression {
    type: any;
    constructor(params: any, body: any, async?: any);
}
/**
 * ConditionalExpression
 *
 * Creates an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
declare class ConditionalExpression extends Expression {
    type: any;
    test: any;
    consequent: any;
    alternate: any;
    constructor(test: any, consequent: any, alternate: any);
}
/**
 * Pattern
 *
 * Creates an ESTree [`Pattern`][estree:pattern] node.
 *
 * [estree:pattern]: https://github.com/estree/estree/blob/master/es5.md#patterns
 */
declare class Pattern extends TSNode {
    type: any;
}
/**
 * AssignmentPattern
 *
 * Creates an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
 *
 * [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
 */
declare class AssignmentPattern extends Pattern {
    type: any;
    left: any;
    right: any;
    constructor(left: any, right: any);
    getType(): any;
    setType(type_: any): any;
}
/**
 * ArrayPattern
 *
 * Creates an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
declare class ArrayPattern extends Pattern {
    type: any;
    elements: any;
    constructor(elements: any);
}
/**
 * ObjectPattern
 *
 * Creates an ESTree [`ObjectPattern`][estree:objectpattern] node.
 *
 * [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
 */
declare class ObjectPattern extends Pattern {
    type: any;
    properties: any;
    constructor(properties?: any);
}
/**
 * ThisExpression
 *
 * Creates an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
declare class ThisExpression extends Expression {
    type: any;
}
/**
 * ArrayExpression
 *
 * Creates an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
declare class ArrayExpression extends Expression {
    type: any;
    elements: any;
    constructor(elements?: any);
}
/**
 * RestElement
 *
 * Creates an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
declare class RestElement extends Pattern {
    type: any;
    argument: any;
    constructor(argument: any);
}
/**
 * SpreadElement
 *
 * Creates an ESTree [`SpreadElement`][estree:spreadelement] node.
 *
 * [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
declare class SpreadElement extends Expression {
    type: any;
    argument: any;
    constructor(argument: any);
}
/**
 * NewExpression
 *
 * Creates an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
declare class NewExpression extends Expression {
    type: any;
    callee: any;
    arguments: any;
    constructor(callee: any, args?: any);
}
/**
 * YieldExpression
 *
 * Creates an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
declare class YieldExpression extends Expression {
    type: any;
    argument: any;
    delegate: any;
    constructor(argument?: any, delegate?: any);
}
/**
 * AwaitExpression
 *
 * Creates an ESTree [`AwaitExpression`][estree:awaitexpression] node.
 *
 * [estree:awaitexpression]: https://github.com/estree/estree/blob/master/es2017.md#awaitexpression
 */
declare class AwaitExpression extends Expression {
    type: any;
    argument: any;
    constructor(argument?: any);
}
/**
 * TemplateLiteral
 *
 * Creates an ESTree [`TemplateLiteral`][estree:templateliteral] node.
 *
 * [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
 */
declare class TemplateLiteral extends Expression {
    type: any;
    quasis: any;
    expressions: any;
    constructor(quasis?: any, expressions?: any);
}
/**
 * TaggedTemplateExpression
 *
 * Creates an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
 *
 * [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
 */
declare class TaggedTemplateExpression extends Expression {
    type: any;
    tag: any;
    quasi: any;
    constructor(tag: any, quasi: any);
}
/**
 * TemplateElement
 *
 * Creates an ESTree [`TemplateElement`][estree:templateelement] node.
 *
 * [estree:templateelement]: https://github.com/estree/estree/blob/master/es2015.md#templateelement
 */
declare class TemplateElement extends TSNode {
    type: any;
    tail: any;
    value: any;
    constructor(tail: any, cooked: any, raw?: any);
}
/**
 * SwitchStatement
 *
 * Creates an ESTree [`SwitchStatement`][estree:switchstatement] node.
 *
 * [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
 */
declare class SwitchStatement extends Statement {
    type: any;
    discriminant: any;
    cases: any;
    constructor(discriminant: any, cases?: any);
}
/**
 * SwitchCase
 *
 * Creates an ESTree [`SwitchCase`][estree:switchcase] node.
 *
 * [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
 */
declare class SwitchCase extends TSNode {
    type: any;
    test: any;
    consequent: any;
    constructor(test: any, consequent?: any);
}
/**
 * ESTree extensions
 */
/**
 * TSIdentifier
 *
 * TSTree version of `Identifier`.
 */
declare class TSIdentifier extends Identifier {
    type: any;
    name: any;
    typeAnnotation: any;
    constructor(name: any, typeAnnotation: any);
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
declare class TSTypeAnnotation extends TSNode {
    type: any;
    typeAnnotation: any;
    constructor(typeAnnotation: any);
}
/**
 * TSKeywordTypeNode
 *
 * Base class for TSESTree keywords.
 */
declare class TSKeywordTypeNode extends Node {
    type: any;
}
/**
 * TSAnyKeyword
 *
 * TypeScript `any` type.
 */
declare class TSAnyKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSVoidKeyword
 *
 * TypeScript `void` type.
 */
declare class TSVoidKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSUndefinedKeyword
 *
 * TypeScript `undefined` type.
 */
declare class TSUndefinedKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSBooleanKeyword
 *
 * TypeScript `boolean` type.
 */
declare class TSBooleanKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSNumberKeyword
 *
 * TypeScript `number` type.
 */
declare class TSNumberKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSStringKeyword
 *
 * TypeScript `string` type.
 */
declare class TSStringKeyword extends TSKeywordTypeNode {
    type: any;
}
/**
 * TSArrayType
 *
 * TypeScript array type.
 */
declare class TSArrayType extends Node {
    type: any;
    elementType: any;
    constructor(elementType: any);
}
/**
 * TSTupleType
 *
 * TypeScript tuple type.
 */
declare class TSTupleType extends Node {
    type: any;
    elementTypes: any;
    constructor(elementTypes: any);
}
/**
 * TSLiteralType
 *
 * TypeScript literal type.
 */
declare class TSLiteralType extends Node {
    type: any;
    literal: any;
    constructor(literal: any);
}
/**
 * TSUnionType
 *
 * TypeScript union type.
 */
declare class TSUnionType extends Node {
    type: any;
    types: any;
    constructor(types: any);
}
/**
 * TSFunctionType
 *
 * TypeScript function type.
 */
declare class TSFunctionType extends Node {
    type: any;
    params: any;
    returnType: any;
    constructor(params: any, returnType: any);
}
/**
 * TSTypeAliasDeclaration
 *
 * TypeScript `type` alias declaration.
 */
declare class TSTypeAliasDeclaration extends Node {
    type: any;
    id: any;
    typeAnnotation: any;
    constructor(id: any, typeAnnotation: any);
}
/**
 * TSTypeReference
 *
 * TypeScript `boolean` type.
 */
declare class TSTypeReference extends Node {
    type: any;
    typeName: any;
    typeParameters: any;
    constructor(typeName: any, typeParameters?: any);
}
/**
 * TSTypeParameterInstantiation
 *
 * TypeScript type parameters.
 */
declare class TSTypeParameterInstantiation extends Node {
    type: any;
    params: any;
    constructor(params?: any);
}
/**
 * TSAsExpression
 *
 * TypeScript `as` expression.
 */
declare class TSAsExpression extends Expression {
    type: any;
    expression: any;
    constructor(expression: any, typeAnnotation: any);
}
/**
 * Comment
 *
 * Base class for comments.
 */
declare class Comment {
    originalText: any;
    leading: any;
    trailing: any;
    printed: any;
    constructor(originalText: any, commentType?: any);
    getText(): any;
}
/**
 * LeadingComment
 *
 * Leading comment.
 */
declare class LeadingComment extends Comment {
    constructor(originalText: any);
}
/**
 * BlockComment
 *
 * Block comment.
 */
declare class BlockComment extends LeadingComment {
    constructor(originalText: any);
}
/**
 * TrailingComment
 *
 * Trailing comment.
 */
declare class TrailingComment extends Comment {
    constructor(originalText: any);
}
/**
 * XRawJavaScript
 *
 * A raw JavaScript string that is to be spliced directly
 * into the output.
 *
 * Nonstandard ESTree extension.
 */
declare class XRawJavaScript extends TSNode {
    type: any;
    js: any;
    constructor(js: any);
}
/**
 * Whether `obj` is an ESTree.
 *
 * This function only works on instantiated objects.
 */
declare function estreep(obj: any): any;
declare namespace estreep {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Get the type of an ESTree node.
 */
declare function estreeType(node: any): any;
declare namespace estreeType {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether the type of the ESTree node `node` is `typ`.
 */
declare function estreeTypeP(node: any, typ: any): any;
declare namespace estreeTypeP {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Wrap a value in an ESTree node.
 *
 * Distinguishes between list values and atomic values.
 */
declare function wrapInEstree(x: any): any;
declare namespace wrapInEstree {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[])[];
}
export { Expression as ESTreeExpression, Node as ESTreeNode, Statement as ESTreeStatement, TSNode as TSESTreeNode, estreeTypeP as estreeIsP, ArrayExpression, ArrayPattern, ArrowFunctionExpression, AssignmentExpression, AssignmentPattern, AwaitExpression, BinaryExpression, BlockComment, BlockStatement, BreakStatement, CallExpression, CatchClause, ChainElement, ChainExpression, ClassBody, ClassDeclaration, ClassExpression, Comment, ConditionalExpression, ContinueStatement, DoWhileStatement, ExportAllDeclaration, ExportNamedDeclaration, ExportSpecifier, Expression, ExpressionStatement, ForInStatement, ForOfStatement, ForStatement, Function, FunctionDeclaration, FunctionExpression, Identifier, IfStatement, ImportDeclaration, ImportDefaultSpecifier, ImportNamespaceSpecifier, ImportSpecifier, LeadingComment, Literal, LogicalExpression, MemberExpression, MethodDefinition, NewExpression, Node, ObjectExpression, ObjectPattern, Program, Property, PropertyDefinition, RegExpLiteral, RestElement, ReturnStatement, SequenceExpression, SpreadElement, Statement, SwitchCase, SwitchStatement, TSAnyKeyword, TSArrayType, TSAsExpression, TSBooleanKeyword, TSFunctionType, TSIdentifier, TSLiteralType, TSNode, TSNumberKeyword, TSStringKeyword, TSTupleType, TSTypeAliasDeclaration, TSTypeAnnotation, TSTypeParameterInstantiation, TSTypeReference, TSUndefinedKeyword, TSUnionType, TSVoidKeyword, TaggedTemplateExpression, TemplateElement, TemplateLiteral, ThisExpression, ThrowStatement, TrailingComment, TryStatement, UnaryExpression, UpdateExpression, VariableDeclaration, VariableDeclarator, WhileStatement, XRawJavaScript, YieldExpression, estreeType, estreeTypeP, estreep, wrapInEstree };

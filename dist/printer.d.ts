/**
 * # ESTree printer implementation
 *
 * Printer code for ESTree trees, rose trees and raw S-expressions.
 *
 * ## Description
 *
 * TODO: Better description
 *
 * This is a partial, incomplete printer for ESTree trees, with
 * limited support for TSESTree extensions. With further work, it
 * might be developed into a complete implementation.
 *
 * Why not use an external library? For example, [recast][npm:recast]
 * could be an option as far as basic ESTree trees are concerned. The
 * challenge is that we also need to emit comments, and for the sake
 * of TypeScript support, the printer must be able to understand type
 * annotations in the form of [TSESTree][doc:typescript-estree]
 * extensions. However, comments and type annotations are both
 * nonstandard extensions to the [ESTree
 * specification][github:estree], and are not supported by recast.
 *
 * It might be possible to replace the whole lot with
 * [Prettier][www:prettier], which does support both comments and
 * types. However, it has proven difficult to [hook
 * into][doc:prettier:plugins] Prettier's [API][doc:prettier:api] in
 * the right way.
 *
 * Thus we are currently left with the option of writing our own
 * implementation. It is incomplete, and possibly buggy, but at least
 * there is a test suite that attempts to salvage the situation
 * somewhat.
 *
 * ## External links
 *
 * -   [The ESTree spec][github:estree]
 * -   [TSESTree details][doc:typescript-estree]
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [npm:recast]: https://www.npmjs.com/package/recast
 * [doc:typescript-estree]: https://typescript-eslint.io/packages/typescript-estree/
 * [github:estree]: https://github.com/estree/estree
 * [www:prettier]: https://prettier.io/
 * [doc:prettier:plugins]: https://prettier.io/docs/en/plugins#printers
 * [doc:prettier:api]: https://prettier.io/docs/en/api
 */
/**
 * Print an ESTree node or an S-expression.
 */
declare function print(obj: any, options?: any): any;
declare namespace print {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Print an ESTree node.
 */
declare function printEstree(node: any, options?: any): any;
declare namespace printEstree {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Print a rose tree.
 */
declare function printRose(node: any, options?: any): any;
declare namespace printRose {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Print an S-expression.
 */
declare function printSexp(exp: any, options?: any): any;
declare namespace printSexp {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Print an S-expression as an expression
 * that can be evaluated.
 */
declare function printSexpAsExpression(exp: any, options?: any): any;
declare namespace printSexpAsExpression {
    var lispSource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
}
/**
 * Print an S-expression to a string.
 */
declare function writeToString(obj: any, options?: any): any;
declare namespace writeToString {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (string | symbol)[])[])[];
}
/**
 * Print an ESTree node to a `Doc` object.
 */
declare function printNode(node: any, options?: any): any;
declare namespace printNode {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
export { printNode as printEstreeNode, printSexpAsExpression as printAsExpression, print, printEstree, printNode, printRose, printSexp, printSexpAsExpression, writeToString };

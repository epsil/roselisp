/**
 * # Rose trees
 *
 * Rose tree implementation.
 *
 * ## Description
 *
 * Implements a `Rose` class for representing
 * [rose trees][w:Rose tree]. A rose tree, in this context, is a data
 * structure that functions as a container or wrapper around an
 * S-expression. This is useful for annotating the expression with
 * additional metadata that is not contained in the expression
 * itself, such as comments and line numbers.
 *
 * Also provided here are various utilities for dealing with
 * S-expressions wrapped in rose trees.
 *
 * ### Nodes
 *
 * A rose tree node consists of two parts: a *label* and a *forest*.
 * The label is a value attached to that particular node, while the
 * forest is a set of outgoing edges from that node to other nodes.
 * (The forest is so named because it can be considered a multitude
 * of trees.)
 *
 * In addition to these, this implementation equips every node with a
 * key--value map of properties. This is useful for annotating the
 * node with metadata.
 *
 * ### Forest
 *
 * The forest is implemented in its own class, `Forest`. It contains
 * a list of pointers to other rose tree nodes. An empty forest means
 * that the node is a leaf node.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [w:Rose tree]: https://en.wikipedia.org/wiki/Rose_tree
 */
/**
 * Rose tree node class.
 */
declare class Rose {
    /**
     * The node's value.
     */
    value: any;
    /**
     * The node's forest.
     */
    forest: any;
    /**
     * The node's parent node, if any.
     */
    parent: any;
    /**
     * The node's properties.
     */
    properties: any;
    /**
     * Create a new rose tree node with a given value (optional)
     * and a given forest (optional). If not specified, the
     * value defaults to `undefined` and the forest defaults to
     * an empty forest (i.e., a leaf node).
     */
    constructor(value?: any, forest?: any);
    /**
     * Visitor pattern.
     */
    accept(visitor: any, recursive?: any): any;
    /**
     * Clear a node.
     * Deletes the value, empties the forest,
     * and removes all properties.
     */
    clear(clearValue?: any, clearForest?: any, clearProperties?: any): any;
    /**
     * Clear the value
     * (i.e., set it to `undefined`).
     *
     * Alias for `remove-value`.
     */
    clearValue(): any;
    /**
     * Clear the forest.
     */
    clearForest(): any;
    /**
     * Clear the properties.
     */
    clearProperties(): any;
    /**
     * Return the list of nodes obtained by
     * dropping the first `n` nodes from
     * the forest.
     */
    drop(n: any): any;
    /**
     * Return the forest obtained by dropping
     * the first `n` nodes from the forest.
     */
    dropForest(n: any): any;
    /**
     * Return the list of nodes obtained by dropping
     * the last `n` nodes from the forest.
     */
    dropRight(n: any): any;
    /**
     * Return the forest obtained by dropping
     * the last `n` nodes from the forest.
     */
    dropRightForest(n: any): any;
    /**
     * Return the first node in the forest,
     * or `undefined` if there is none.
     */
    first(): any;
    /**
     * Return the second node in the forest,
     * or `undefined` if there is none.
     */
    second(): any;
    /**
     * Return the third node in the forest,
     * or `undefined` if there is none.
     */
    third(): any;
    /**
     * Return the fourth node in the forest,
     * or `undefined` if there is none.
     */
    fourth(): any;
    /**
     * Return the fifth node in the forest,
     * or `undefined` if there is none.
     */
    fifth(): any;
    /**
     * Return the sixth node in the forest,
     * or `undefined` if there is none.
     */
    sixth(): any;
    /**
     * Return the seventh node in the forest,
     * or `undefined` if there is none.
     */
    seventh(): any;
    /**
     * Return the eight node in the forest,
     * or `undefined` if there is none.
     */
    eight(): any;
    /**
     * Return the ninth node in the forest,
     * or `undefined` if there is none.
     */
    ninth(): any;
    /**
     * Return the tenth node in the forest,
     * or `undefined` if there is none.
     */
    tenth(): any;
    /**
     * Return the last node in the forest,
     * or `undefined` if there is none.
     */
    last(): any;
    /**
     * Alias for `for-each-node`.
     */
    forEach(f: any): any;
    /**
     * Call `f` on this node and on
     * each node in the forest.
     */
    forEachNode(f: any, recursive?: any): any;
    /**
     * Get the `n`-th node in the forest.
     * Counting begins at zero.
     *
     * Alias for `nth`.
     */
    get(n: any): any;
    /**
     * Get the numerical index of `node` in the forest.
     * Returns `-1` if not found.
     */
    getIndex(node: any): any;
    /**
     * Alias for `get-value`.
     *
     * In the literature, the value of a node
     * is sometimes referred to as its "label".
     */
    getLabel(): any;
    /**
     * Get a list of all the nodes in the forest.
     */
    getNodes(): any;
    /**
     * Get the forest.
     * Returns an instance of `Forest`.
     */
    getForest(): any;
    /**
     * Get the parent node, or `undefined`
     * if this is a root node.
     */
    getParent(): any;
    /**
     * Get the node property `prop`.
     */
    getProperty(prop: any): any;
    /**
     * Get the node value.
     */
    getValue(): any;
    /**
     * Whether there is a node property `prop`.
     */
    hasProperty(prop: any): any;
    /**
     * Insert `node` into the forest.
     */
    insert(node: any): any;
    /**
     * Get the `n`-th node in the forest.
     * Counting begins at zero.
     */
    nth(n: any): any;
    /**
     * Remove the node whose index is `n`.
     */
    removeNode(n: any): any;
    /**
     * Remove the node property `prop`.
     */
    removeProperty(prop: any): any;
    /**
     * Remove the value
     * (i.e., set it to `undefined`).
     */
    removeValue(): any;
    /**
     * Alias for `set-value`.
     *
     * In the literature, the value of a node
     * is sometimes referred to as its "label".
     */
    setLabel(label: any): any;
    /**
     * Set the property `prop` to `value`.
     */
    setProperty(prop: any, value: any): any;
    /**
     * Set the parent of this node to `node`.
     */
    setParent(node: any): any;
    /**
     * Set the value of this node to `value`.
     */
    setValue(value: any): any;
    /**
     * Return the number of nodes in the forest.
     */
    size(): any;
    /**
     * Get the parent node.
     */
    parentNode(): any;
    /**
     * Get the first subnode.
     */
    firstChild(): any;
    /**
     * Get the last subnode.
     */
    lastChild(): any;
    /**
     * Get the previous sibling.
     */
    previousSibling(): any;
    /**
     * Get the next sibling.
     */
    nextSibling(): any;
    /**
     * Get the previous node.
     */
    previousNode(): any;
    /**
     * Get the next node.
     */
    nextNode(): any;
}
/**
 * Rose tree forest class.
 */
declare class Forest {
    /**
     * Node list.
     * The order is significant.
     */
    nodeList: any;
    /**
     * The node this forest belongs to.
     */
    parent: any;
    /**
     * Create a new forest containing `nodes`.
     */
    constructor(...nodes: any[]);
    /**
     * Clear the forest.
     */
    clear(): any;
    /**
     * Return the list of nodes obtained by
     * dropping the first `n` nodes.
     */
    drop(n: any): any;
    /**
     * Return the forest obtained by dropping
     * the first `n` nodes.
     */
    dropForest(n: any): any;
    /**
     * Return the list of nodes obtained by dropping
     * the last `n` nodes.
     */
    dropRight(n: any): any;
    /**
     * Return the forest obtained by dropping
     * the last `n` nodes.
     */
    dropRightForest(n: any): any;
    /**
     * Return the first node,
     * or `undefined` if there is none.
     */
    first(): any;
    /**
     * Return the second node,
     * or `undefined` if there is none.
     */
    second(): any;
    /**
     * Return the third node,
     * or `undefined` if there is none.
     */
    third(): any;
    /**
     * Return the fourth node,
     * or `undefined` if there is none.
     */
    fourth(): any;
    /**
     * Return the fifth node,
     * or `undefined` if there is none.
     */
    fifth(): any;
    /**
     * Return the sixth node,
     * or `undefined` if there is none.
     */
    sixth(): any;
    /**
     * Return the seventh node,
     * or `undefined` if there is none.
     */
    seventh(): any;
    /**
     * Return the eight node,
     * or `undefined` if there is none.
     */
    eight(): any;
    /**
     * Return the ninth node,
     * or `undefined` if there is none.
     */
    ninth(): any;
    /**
     * Return the tenth node,
     * or `undefined` if there is none.
     */
    tenth(): any;
    /**
     * Return the last node,
     * or `undefined` if there is none.
     */
    last(): any;
    /**
     * Alias for `for-each-node`.
     */
    forEach(f: any): any;
    /**
     * Call `f` on each node in the forest.
     */
    forEachNode(f: any, recursive?: any): any;
    /**
     * Get the `n`-th node.
     * Counting begins at zero.
     *
     * Alias for `nth`.
     */
    get(n: any): any;
    /**
     * Get the numerical index of `node`.
     * Returns `-1` if not found.
     */
    getIndex(node: any): any;
    /**
     * Get the list of all the nodes.
     */
    getNodes(): any;
    /**
     * Get the node this forest belongs to.
     */
    getParent(): any;
    /**
     * Insert `node` into the forest.
     */
    insert(node: any): any;
    /**
     * Get the `n`-th node.
     * Counting begins at zero.
     */
    nth(n: any): any;
    /**
     * Remove the node whose index is `n`.
     */
    removeNode(n: any): any;
    /**
     * Set the `parent` field, i.e., the node
     * this forest belongs to.
     */
    setParent(parent: any, updateNodes?: any): any;
    /**
     * Return the number of nodes in the forest.
     */
    size(): any;
}
/**
 * Special rose tree value.
 * Used for inserting directly into the forest.
 */
declare class RoseSplice {
}
/**
 * Whether something is a rose tree node.
 */
declare function rosep(obj: any): any;
/**
 * Whether something is a rose tree forest.
 */
declare function forestp(obj: any): any;
/**
 * Wrap a list of rose tree-wrapped S-expressions in
 * a `(begin ...)` form.
 *
 * Legacy function, but still used in a few places.
 */
declare function beginWrapRose(nodes: any): any;
/**
 * Wrap a list of rose tree-wrapped S-expressions in
 * a `(begin ...)` form. Does not wrap singleton lists.
 *
 * Legacy function, but still used in a few places.
 */
declare function beginWrapRoseSmart(nodes: any): any;
/**
 * Legacy function, but still used in a few places.
 */
declare function beginWrapRoseSmart1(nodes: any): any;
/**
 * Make a rose tree-wrapped S-expression.
 *
 * Converts a list or nested list of rose tree nodes
 * and other values to a rose tree.
 */
declare function makeRose(exp: any, node?: any): any;
/**
 * Insert an S-expression into a rose tree node.
 */
declare function insertSexpIntoRose(exp: any, node: any, cache?: any): any;
/**
 * Make a map mapping values to rose tree nodes,
 * but only one level down.
 */
declare function makeSimpleRoseMap(node: any): any;
/**
 * Make a rose tree map.
 *
 * Returns a map mapping a value to a list of rose tree nodes
 * containing that value.
 */
declare function makeRoseMap(node?: any): any;
declare function makeRoseNonrecursive(exp: any): any;
/**
 * Make a rose tree-wrapped list expression.
 */
declare function makeListRose(expressions?: any): any;
/**
 * Make a rose tree-wrapped S-expression.
 */
declare function makeSexpRose(exp?: any): any;
/**
 * Slice a list wrapped in a rose tree.
 * Returns a new rose tree node containing
 * the sliced list.
 */
declare function sliceRose(node: any, n: any): any;
/**
 * Wrap an S-expression in a rose tree.
 */
declare function wrapSexpInRose(exp: any, cache?: any): any;
/**
 * Transfer comments from `node1` to `node2`.
 *
 * The nodes may be rose tree nodes or ESTree nodes.
 */
declare function transferComments(from: any, to: any): any;
export { Forest, Rose, RoseSplice, beginWrapRose, beginWrapRoseSmart, beginWrapRoseSmart1, forestp, insertSexpIntoRose, makeListRose, makeRose, makeRoseMap, makeRoseNonrecursive, makeSexpRose, makeSimpleRoseMap, rosep, sliceRose, transferComments, wrapSexpInRose };

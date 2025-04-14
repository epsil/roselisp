"use strict";
// SPDX-License-Identifier: MPL-2.0
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.wrapSexpInRose = exports.transferComments = exports.sliceRose = exports.rosep = exports.makeSimpleRoseMap = exports.makeSexpRose = exports.makeRoseNonrecursive = exports.makeRoseMap = exports.makeRose = exports.makeListRose = exports.insertSexpIntoRose = exports.forestp = exports.beginWrapRoseSmart1 = exports.beginWrapRoseSmart = exports.beginWrapRose = exports.RoseSplice = exports.Rose = exports.Forest = void 0;
const visitor_1 = require("./visitor");
/**
 * Rose tree node class.
 */
class Rose {
    /**
     * Create a new rose tree node with a given value (optional)
     * and a given forest (optional). If not specified, the
     * value defaults to `undefined` and the forest defaults to
     * an empty forest (i.e., a leaf node).
     */
    constructor(value = undefined, forest = undefined) {
        /**
         * The node's value.
         */
        this.value = undefined;
        /**
         * The node's parent node, if any.
         */
        this.parent = undefined;
        /**
         * The node's properties.
         */
        this.properties = new Map();
        this.forest = (forest || new Forest()).setParent(this);
        if (value !== undefined) {
            this.setValue(value);
        }
    }
    /**
     * Visitor pattern.
     */
    accept(visitor, recursive = false) {
        let result = this;
        // If `recursive` is `#t`, visit sub-nodes first.
        // If it is `#f`, the visiting of sub-nodes is
        // left to the visitor.
        if (recursive) {
            let isNew = false;
            // Visit sub-nodes, if any.
            const resultNodes = this.getNodes().map(function (node) {
                const resultNode = node.accept(visitor);
                // If visiting a sub-node produces a new value,
                // set `is-new` to `#t`.
                if (!(isNew || (resultNode === node))) {
                    isNew = true;
                }
                return resultNode;
            });
            // If visiting the sub-nodes produces new values,
            // then create a new containing node to house them in.
            // (Note that the results of visiting sub-nodes may
            // be a non-`Rose` value.)
            if (isNew) {
                result = new Rose(this.getValue());
                for (let resultNode of resultNodes) {
                    result.insert(resultNode);
                }
            }
        }
        // Visit the containing node.
        return (0, visitor_1.visit)(visitor, result);
    }
    /**
     * Clear a node.
     * Deletes the value, empties the forest,
     * and removes all properties.
     */
    clear(clearValue = true, clearForest = true, clearProperties = true) {
        if (clearValue) {
            this.clearValue();
        }
        if (clearForest) {
            this.clearForest();
        }
        if (clearProperties) {
            this.clearProperties();
        }
        return this;
    }
    /**
     * Clear the value
     * (i.e., set it to `undefined`).
     *
     * Alias for `remove-value`.
     */
    clearValue() {
        return this.removeValue();
    }
    /**
     * Clear the forest.
     */
    clearForest() {
        this.getForest().clear();
        return this;
    }
    /**
     * Clear the properties.
     */
    clearProperties() {
        this.properties.clear();
        return this;
    }
    /**
     * Return the list of nodes obtained by
     * dropping the first `n` nodes from
     * the forest.
     */
    drop(n) {
        return this.getForest().drop(n);
    }
    /**
     * Return the forest obtained by dropping
     * the first `n` nodes from the forest.
     */
    dropForest(n) {
        return this.getForest().dropForest(n);
    }
    /**
     * Return the list of nodes obtained by dropping
     * the last `n` nodes from the forest.
     */
    dropRight(n) {
        return this.getForest().dropRight(n);
    }
    /**
     * Return the forest obtained by dropping
     * the last `n` nodes from the forest.
     */
    dropRightForest(n) {
        return this.getForest().dropRightForest(n);
    }
    /**
     * Return the first node in the forest,
     * or `undefined` if there is none.
     */
    first() {
        return this.getForest().first();
    }
    /**
     * Return the second node in the forest,
     * or `undefined` if there is none.
     */
    second() {
        return this.getForest().second();
    }
    /**
     * Return the third node in the forest,
     * or `undefined` if there is none.
     */
    third() {
        return this.getForest().third();
    }
    /**
     * Return the fourth node in the forest,
     * or `undefined` if there is none.
     */
    fourth() {
        return this.getForest().fourth();
    }
    /**
     * Return the fifth node in the forest,
     * or `undefined` if there is none.
     */
    fifth() {
        return this.getForest().fifth();
    }
    /**
     * Return the sixth node in the forest,
     * or `undefined` if there is none.
     */
    sixth() {
        return this.getForest().sixth();
    }
    /**
     * Return the seventh node in the forest,
     * or `undefined` if there is none.
     */
    seventh() {
        return this.getForest().seventh();
    }
    /**
     * Return the eight node in the forest,
     * or `undefined` if there is none.
     */
    eight() {
        return this.getForest().eight();
    }
    /**
     * Return the ninth node in the forest,
     * or `undefined` if there is none.
     */
    ninth() {
        return this.getForest().ninth();
    }
    /**
     * Return the tenth node in the forest,
     * or `undefined` if there is none.
     */
    tenth() {
        return this.getForest().tenth();
    }
    /**
     * Return the last node in the forest,
     * or `undefined` if there is none.
     */
    last() {
        return this.getForest().last();
    }
    /**
     * Alias for `for-each-node`.
     */
    forEach(f) {
        return this.forEachNode(f);
    }
    /**
     * Call `f` on this node and on
     * each node in the forest.
     */
    forEachNode(f, recursive = true) {
        f(this);
        this.getForest().forEachNode(f, recursive);
        return this;
    }
    /**
     * Get the `n`-th node in the forest.
     * Counting begins at zero.
     *
     * Alias for `nth`.
     */
    get(n) {
        return this.nth(n);
    }
    /**
     * Get the numerical index of `node` in the forest.
     * Returns `-1` if not found.
     */
    getIndex(node) {
        return this.getForest().getIndex(node);
    }
    /**
     * Alias for `get-value`.
     *
     * In the literature, the value of a node
     * is sometimes referred to as its "label".
     */
    getLabel() {
        return this.getValue();
    }
    /**
     * Get a list of all the nodes in the forest.
     */
    getNodes() {
        return this.getForest().getNodes();
    }
    /**
     * Get the forest.
     * Returns an instance of `Forest`.
     */
    getForest() {
        return this.forest;
    }
    /**
     * Get the parent node, or `undefined`
     * if this is a root node.
     */
    getParent() {
        return this.parent;
    }
    /**
     * Get the node property `prop`.
     */
    getProperty(prop) {
        return this.properties.get(prop);
    }
    /**
     * Get the node value.
     */
    getValue() {
        return this.value;
    }
    /**
     * Whether there is a node property `prop`.
     */
    hasProperty(prop) {
        return this.properties.has(prop);
    }
    /**
     * Insert `node` into the forest.
     */
    insert(node) {
        this.getForest().insert(node);
        return this;
    }
    /**
     * Get the `n`-th node in the forest.
     * Counting begins at zero.
     */
    nth(n) {
        return this.getForest().nth(n);
    }
    /**
     * Remove the node whose index is `n`.
     */
    removeNode(n) {
        this.getForest().removeNode(n);
        return this;
    }
    /**
     * Remove the node property `prop`.
     */
    removeProperty(prop) {
        this.properties.delete(prop);
        return this;
    }
    /**
     * Remove the value
     * (i.e., set it to `undefined`).
     */
    removeValue() {
        return this.setValue(undefined);
    }
    /**
     * Alias for `set-value`.
     *
     * In the literature, the value of a node
     * is sometimes referred to as its "label".
     */
    setLabel(label) {
        return this.setValue(label);
    }
    /**
     * Set the property `prop` to `value`.
     */
    setProperty(prop, value) {
        this.properties.set(prop, value);
        return this;
    }
    /**
     * Set the parent of this node to `node`.
     */
    setParent(node) {
        this.parent = node;
        return this;
    }
    /**
     * Set the value of this node to `value`.
     */
    setValue(value) {
        this.value = value;
        return this;
    }
    /**
     * Return the number of nodes in the forest.
     */
    size() {
        return this.getForest().size();
    }
    /**
     * Get the parent node.
     */
    parentNode() {
        return this.parent;
    }
    /**
     * Get the first subnode.
     */
    firstChild() {
        return this.first();
    }
    /**
     * Get the last subnode.
     */
    lastChild() {
        return this.last();
    }
    /**
     * Get the previous sibling.
     */
    previousSibling() {
        // TODO
        return undefined;
    }
    /**
     * Get the next sibling.
     */
    nextSibling() {
        // TODO
        return undefined;
    }
    /**
     * Get the previous node.
     */
    previousNode() {
        return this.previousSibling() || this.parentNode();
    }
    /**
     * Get the next node.
     */
    nextNode() {
        return this.nextSibling() || this.firstChild();
    }
}
exports.Rose = Rose;
/**
 * Rose tree forest class.
 */
class Forest {
    /**
     * Create a new forest containing `nodes`.
     */
    constructor(...nodes) {
        /**
         * Node list.
         * The order is significant.
         */
        this.nodeList = [];
        /**
         * The node this forest belongs to.
         */
        this.parent = undefined;
        for (let node of nodes) {
            this.insert(node);
        }
    }
    /**
     * Clear the forest.
     */
    clear() {
        this.nodeList = [];
        return this;
    }
    /**
     * Return the list of nodes obtained by
     * dropping the first `n` nodes.
     */
    drop(n) {
        const arr = this.nodeList;
        if (n === 0) {
            return arr;
        }
        else {
            return arr.slice(n);
        }
    }
    /**
     * Return the forest obtained by dropping
     * the first `n` nodes.
     */
    dropForest(n) {
        return new Forest(...this.drop(n));
    }
    /**
     * Return the list of nodes obtained by dropping
     * the last `n` nodes.
     */
    dropRight(n) {
        const arr = this.nodeList;
        if (n === 0) {
            return arr;
        }
        else {
            return arr.slice(0, -n);
        }
    }
    /**
     * Return the forest obtained by dropping
     * the last `n` nodes.
     */
    dropRightForest(n) {
        return new Forest(...this.dropRight(n));
    }
    /**
     * Return the first node,
     * or `undefined` if there is none.
     */
    first() {
        return this.nth(0);
    }
    /**
     * Return the second node,
     * or `undefined` if there is none.
     */
    second() {
        return this.nth(1);
    }
    /**
     * Return the third node,
     * or `undefined` if there is none.
     */
    third() {
        return this.nth(2);
    }
    /**
     * Return the fourth node,
     * or `undefined` if there is none.
     */
    fourth() {
        return this.nth(3);
    }
    /**
     * Return the fifth node,
     * or `undefined` if there is none.
     */
    fifth() {
        return this.nth(4);
    }
    /**
     * Return the sixth node,
     * or `undefined` if there is none.
     */
    sixth() {
        return this.nth(5);
    }
    /**
     * Return the seventh node,
     * or `undefined` if there is none.
     */
    seventh() {
        return this.nth(6);
    }
    /**
     * Return the eight node,
     * or `undefined` if there is none.
     */
    eight() {
        return this.nth(7);
    }
    /**
     * Return the ninth node,
     * or `undefined` if there is none.
     */
    ninth() {
        return this.nth(8);
    }
    /**
     * Return the tenth node,
     * or `undefined` if there is none.
     */
    tenth() {
        return this.nth(9);
    }
    /**
     * Return the last node,
     * or `undefined` if there is none.
     */
    last() {
        return this.nth(this.size() - 1);
    }
    /**
     * Alias for `for-each-node`.
     */
    forEach(f) {
        return this.forEachNode(f);
    }
    /**
     * Call `f` on each node in the forest.
     */
    forEachNode(f, recursive = true) {
        for (let node of this.nodeList) {
            if (recursive) {
                node.forEachNode(f, recursive);
            }
            else {
                f(node);
            }
        }
        return this;
    }
    /**
     * Get the `n`-th node.
     * Counting begins at zero.
     *
     * Alias for `nth`.
     */
    get(n) {
        return this.nth(n);
    }
    /**
     * Get the numerical index of `node`.
     * Returns `-1` if not found.
     */
    getIndex(node) {
        return this.getNodes().findIndex(function (x) {
            return x === node;
        });
    }
    /**
     * Get the list of all the nodes.
     */
    getNodes() {
        return this.nodeList;
    }
    /**
     * Get the node this forest belongs to.
     */
    getParent() {
        return this.parent;
    }
    /**
     * Insert `node` into the forest.
     */
    insert(node) {
        if (node.getValue() instanceof RoseSplice) {
            for (let subNode of node.drop(0)) {
                this.insert(subNode);
            }
        }
        else {
            // Update `node-list`.
            this.nodeList.push(node);
            node.setParent(this.getParent());
        }
        return this;
    }
    /**
     * Get the `n`-th node.
     * Counting begins at zero.
     */
    nth(n) {
        return this.nodeList[n];
    }
    /**
     * Remove the node whose index is `n`.
     */
    removeNode(n) {
        this.nodeList.splice(n, 1);
        return this;
    }
    /**
     * Set the `parent` field, i.e., the node
     * this forest belongs to.
     */
    setParent(parent, updateNodes = true) {
        this.parent = parent;
        if (updateNodes) {
            for (let node of this.nodeList) {
                node.setParent(parent);
            }
        }
        return this;
    }
    /**
     * Return the number of nodes in the forest.
     */
    size() {
        return this.nodeList.length;
    }
}
exports.Forest = Forest;
/**
 * Special rose tree value.
 * Used for inserting directly into the forest.
 */
class RoseSplice {
}
exports.RoseSplice = RoseSplice;
/**
 * Whether something is a rose tree node.
 */
function rosep(obj) {
    return obj instanceof Rose;
}
exports.rosep = rosep;
/**
 * Whether something is a rose tree forest.
 */
function forestp(obj) {
    return obj instanceof Forest;
}
exports.forestp = forestp;
/**
 * Wrap a list of rose tree-wrapped S-expressions in
 * a `(begin ...)` form.
 *
 * Legacy function, but still used in a few places.
 */
function beginWrapRose(nodes) {
    return makeListRose([Symbol.for('begin'), ...nodes]);
}
exports.beginWrapRose = beginWrapRose;
/**
 * Wrap a list of rose tree-wrapped S-expressions in
 * a `(begin ...)` form. Does not wrap singleton lists.
 *
 * Legacy function, but still used in a few places.
 */
function beginWrapRoseSmart(nodes) {
    if (!Array.isArray(nodes)) {
        return nodes;
    }
    else if ((nodes.length === 1) && Array.isArray(nodes[0].getValue()) && (nodes[0].getValue().length > 0) && (nodes[0].getValue()[0] === Symbol.for('begin'))) {
        return nodes[0];
    }
    else {
        return beginWrapRose(nodes);
    }
}
exports.beginWrapRoseSmart = beginWrapRoseSmart;
/**
 * Legacy function, but still used in a few places.
 */
function beginWrapRoseSmart1(nodes) {
    if (!Array.isArray(nodes)) {
        return nodes;
    }
    else if (nodes.length === 1) {
        return nodes[0];
    }
    else {
        return beginWrapRose(nodes);
    }
}
exports.beginWrapRoseSmart1 = beginWrapRoseSmart1;
/**
 * Make a rose tree-wrapped S-expression.
 *
 * Converts a list or nested list of rose tree nodes
 * and other values to a rose tree.
 */
function makeRose(exp, node = undefined) {
    const cache = makeRoseMap(node);
    const indices = new Map();
    return makeRoseHelper(exp, node, cache, indices);
}
exports.makeRose = makeRose;
/**
 * Helper function for `make-rose`.
 */
function makeRoseHelper(exp, node, cache, indices) {
    if (cache.has(exp)) {
        let idx = indices.get(exp) || 0;
        const entry = cache.get(exp);
        const val = entry[idx];
        if (!(idx >= (entry.length - 1))) {
            idx++;
        }
        indices.set(exp, idx);
        return val;
    }
    else if (exp instanceof Rose) {
        return exp;
    }
    else if (Array.isArray(exp)) {
        // We need to create a new list since
        // `exp` may be a list of rose tree nodes
        // and S-expressions.
        const lst = [];
        let result = new Rose(lst);
        let isModified = false;
        let elNode;
        for (let el of exp) {
            elNode = makeRoseHelper(el, undefined, cache, indices);
            if (elNode.getValue() !== el) {
                isModified = true;
            }
            lst.push(elNode.getValue());
            result.insert(elNode);
        }
        // If all of the sub-expressions are unchanged,
        // use the original list.
        if (!isModified) {
            result.setValue(exp);
        }
        if (node instanceof Rose) {
            result = transferComments(node, result);
        }
        roseMapSetX(cache, exp, result);
        return result;
    }
    else {
        let result = new Rose(exp);
        if (node instanceof Rose) {
            result = transferComments(node, result);
        }
        roseMapSetX(cache, exp, result);
        return result;
    }
}
/**
 * Insert an S-expression into a rose tree node.
 */
function insertSexpIntoRose(exp, node, cache = new Map()) {
    const cacheMap = (cache instanceof Rose) ? makeSimpleRoseMap(cache) : cache;
    node.clearForest();
    node.setValue(exp);
    if (Array.isArray(exp)) {
        for (let x of exp) {
            if (cacheMap.has(x)) {
                node.insert(cacheMap.get(x));
            }
            else {
                node.insert(insertSexpIntoRose(x, new Rose(), cacheMap));
            }
        }
    }
    return node;
}
exports.insertSexpIntoRose = insertSexpIntoRose;
/**
 * Make a map mapping values to rose tree nodes,
 * but only one level down.
 */
function makeSimpleRoseMap(node) {
    const map = new Map();
    node.forEachNode(function (x) {
        return map.set(x.getValue(), x);
    });
    return map;
}
exports.makeSimpleRoseMap = makeSimpleRoseMap;
/**
 * Make a rose tree map.
 *
 * Returns a map mapping a value to a list of rose tree nodes
 * containing that value.
 */
function makeRoseMap(node = undefined) {
    const map = new Map();
    if (node) {
        node.forEachNode(function (x) {
            const exp = x.getValue();
            return roseMapSetX(map, exp, x);
        });
    }
    return map;
}
exports.makeRoseMap = makeRoseMap;
/**
 * Set or update an entry in a rose tree map.
 */
function roseMapSetX(map, exp, node) {
    if (map.has(exp)) {
        map.get(exp).push(node);
    }
    else {
        map.set(exp, [node]);
    }
    return map;
}
function makeRoseNonrecursive(exp) {
    if (Array.isArray(exp)) {
        return makeListRose(exp);
    }
    else if (exp instanceof Rose) {
        return exp;
    }
    else {
        return new Rose(exp);
    }
}
exports.makeRoseNonrecursive = makeRoseNonrecursive;
/**
 * Make a rose tree-wrapped list expression.
 */
function makeListRose(expressions = []) {
    return makeSexpRose(expressions);
}
exports.makeListRose = makeListRose;
/**
 * Make a rose tree-wrapped S-expression.
 */
function makeSexpRose(exp = []) {
    if (Array.isArray(exp)) {
        const lst = [];
        const node = new Rose(lst);
        for (let x of exp) {
            if (x instanceof Rose) {
                lst.push(x.getValue());
                node.insert(x);
            }
            else {
                lst.push(x);
                node.insert(wrapSexpInRose(x));
            }
        }
        return node;
    }
    else if (exp instanceof Rose) {
        return exp;
    }
    else {
        return wrapSexpInRose(exp);
    }
}
exports.makeSexpRose = makeSexpRose;
/**
 * Slice a list wrapped in a rose tree.
 * Returns a new rose tree node containing
 * the sliced list.
 */
function sliceRose(node, n) {
    const value = node.getValue();
    if (Array.isArray(value)) {
        const slicedNode = new Rose((n === 0) ? value : value.slice(n));
        for (let x of node.drop(n)) {
            slicedNode.insert(x);
        }
        return slicedNode;
    }
    else {
        return node;
    }
}
exports.sliceRose = sliceRose;
/**
 * Wrap an S-expression in a rose tree.
 */
function wrapSexpInRose(exp, cache = new Map()) {
    if (cache.has(exp)) {
        return cache.get(exp);
    }
    else {
        const node = new Rose(exp);
        if (Array.isArray(exp)) {
            for (let x of exp) {
                node.insert(wrapSexpInRose(x, cache));
            }
        }
        return node;
    }
}
exports.wrapSexpInRose = wrapSexpInRose;
/**
 * Transfer comments from `node1` to `node2`.
 *
 * The nodes may be rose tree nodes or ESTree nodes.
 */
function transferComments(from, to) {
    const fromComments = (from instanceof Rose) ? from.getProperty('comments') : from.comments;
    if (fromComments) {
        if (to instanceof Rose) {
            const toComments = to.getProperty('comments') || [];
            const comments = [...fromComments, ...toComments];
            to.setProperty('comments', comments);
        }
        else {
            const toComments = to.comments || [];
            const comments = [...fromComments, ...toComments];
            to.comments = comments;
        }
    }
    return to;
}
exports.transferComments = transferComments;

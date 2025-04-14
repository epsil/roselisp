# Roselisp

Roselisp is a Scheme-like Lisp implementation that compiles to JavaScript. Roselisp is self-hosted and provides both a Lisp interpreter and a Lisp compiler, as well as a simple decompiler for translating from JavaScript to Roselisp.

Roselisp is free software and is licensed under the MPL license. The MPL is a library license that permits Roselisp to be linked with both free code and proprietary code.

## Usage

Roselisp is most easily [installed](#installation) through NPM. Once Roselisp is installed, one can start an interactive session with:

    roselisp

To interpret a Roselisp file:

    roselisp file.scm

To compile a Roselisp file to JavaScript code:

    roselisp -c file.scm

To decompile a JavaScript file to Roselisp code:

    roselisp -d file.js

## Installation

To install Roselisp globally:

    npm install -g roselisp

This command might require `sudo`:

    sudo npm install -g roselisp

To install Roselisp locally, as a library:

    npm install --save roselisp

It is also possible to fetch the latest source code from GitHub and install it with `npm link`.

## Design

Roselisp aims to be a classical Scheme implementation, reusing established Lisp conventions rather than inventing its own. Since it is still a work in progress, we refer to it as "Scheme-similar" for the time being. Roselisp is inspired by, and seeks commonality with, other Scheme implementations like Racket and Guile. However, as it is also inspired by Common Lisp, Clojure and Emacs Lisp, it has something of a heterodox flavor.

## License

Roselisp is licensed under the Mozilla Public License 2.0 (MPL 2.0).

For more on the MPL, see the [MPL 2.0 FAQ](https://www.mozilla.org/en-US/MPL/2.0/FAQ/#copyleft-scope).

## Credits

Roselisp is written by Vegard Øye.

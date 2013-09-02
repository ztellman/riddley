    Walker is my name
    and I am the same.
    Riddley Walker.
    Walking my riddels
    where ever theyve took me
    and walking them now
    on this paper the same.

from [Riddley Walker](http://en.wikipedia.org/wiki/Riddley_Walker) by Russell Hoban

### usage

```clj
[riddley "0.1.0-SNAPSHOT"]
```

Code may be data, but only some data is code.  If we want to perform a pervasive transform, using something like `clojure.walk` presents a few problems:

* binding forms are treated the same as actual expressions
* `clojure.walk/macroexpand-all` will pass in a nil `&env` to all macros

This means that transforms that we intend to apply to expressions may have unintended consequences on a `fn`, `let`, or `case` form.  It also means that any macro which relies on `&env` will not compose with our transformation.

Riddley provides `riddley.walk/macroexpand-all`, which preserves the binding information in `&env`.  It also provides a general code walking and transformation function, `riddley.walk/walk-exprs`.

`walk-exprs` takes two arguments, a `predicate` for whether it should transform the sub-form, and a `handler` for doing the transformation.

```clj
riddley.walk> (walk-exprs number? inc '(let [n 1] (+ n 1)))
(let* [n 2] (. clojure.lang.Numbers (add n 2)))
```

Notice that `walk-exprs` implicitly macroexpands the form, including the inline form for `+`.

Unlike `clojure.walk`, if `handler` is called, sub-forms will not be walked.  The handler function is responsible for recursively calling `walk-exprs` on the form it's handed.

### license

Copyright © 2013 Zachary Tellman

Distributed under the MIT License.

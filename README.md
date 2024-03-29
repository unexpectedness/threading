# threading

A Clojure/script library that provides the kind of arrows you need on an impulse as well as some arrow transformers called *fletchings*.

<p align="center">
  <img src="http://pilavakis.net/images/new_pa22.gif">
</p>

<p align="center">
  <i>
    Penelope now appears before the suitors in her glittering veil. In her hand is a stout bow left behind by Odysseus when he sailed for Troy. "Whoever strings this bow," she says, "and sends an arrow straight through the sockets of twelve ax heads lined in a row -- that man will I marry."
  </i>
</p>

<!-- MarkdownTOC levels="1,2,3" autolink="true" -->

- [Usage](#usage)
- [API doc](#api-doc)
- [Driving concepts](#driving-concepts)
  - [The problem](#the-problem)
  - [The solution](#the-solution)
- [Arrows](#arrows)
  - [The anti-threading arrow](#the-anti-threading-arrow)
  - [Conditionnals & boolean operators](#conditionnals--boolean-operators)
  - [Binding arrows](#binding-arrows)
  - [Other control flow](#other-control-flow)
  - [Debugging](#debugging)
  - [Sequential operations](#sequential-operations)
- [Fletchings](#fletchings)
  - [Basic fletchings: `>-` & `>>-`](#basic-fletchings-----)
  - [`>-args`](#-args)
  - [Teleport fletching/arrow](#teleport-fletchingarrow)
- [Defining new arrows](#defining-new-arrows)
- [Why use `threading` ?](#why-use-threading-)
- [TODO](#todo)

<!-- /MarkdownTOC -->

## Usage

```clojure
[net.clojars.unexpectedness/threading "0.4.1"]
```

```clojure
(ns my-ns
  (:require [threading.core :refer :all]))
```

## [API doc](https://unexpectedness.github.io/threading/index.html)

## Driving concepts

### The problem
I came up with this library to circumvent limitations of some arrows from `clojure.core` and other threading libraries, namely the unability for the programmer to decide whether an arrow should thread its expression in a specific *threading slot* or not.

Let's observe how `clojure.core/cond->` works:
```clojure
(cond-> THREADED-EXPR
  true THREADING-SLOT
  false THREADING-SLOT)
```
The problem I want to underline is the fact conditions in this threading arrow cannot receive the threaded expression. Similarly, the cases matching each condition must receive the threaded expression and we don't have control over this either.

Same problem with `pallet.thread-expr/if->`:
```clojure
(-> 1
    (if-> true
      THEN-THREADING-SLOT
      ELSE-THREADING-SLOT))
```
the condition cannot receive the threaded expr and the cases must receive it no matter what.

### The solution
**All subforms in a threading form are considered threading slots by arrows in this library.**

As an equivalent of `constantly`, the `<-` **antithreading** arrow can be used to make a form evaluate the threaded expression, discard the result and return an arbitrary value instead.

Thus, `if->` becomes:
```clojure
(-> 1 (if-> (<- true)
        inc
        (<- 0)))
```

or just as possibly,
```clojure
(-> 1 (if-> (-> number? not)
        (<- (throw (IllegalArgumentException. "not a number")))
        inc))
```

This is the central piece and the first goal of this library: to provide obvious and dearly missed arrows such as `cond->` and `if->` without sacrificing on flexibility thanks to the antithreading arrow `<-`.

The secondary goal of this lib is to provide arrow transformers called **fletchings**. They look pretty nice on the screen :-).

## Arrows

**All of the arrows below come with both a `->` variant and a `->>` variant.**

There is [a great and simple way to define them](#defining-new-arrows) and you're invited to collaborate!

### The anti-threading arrow

As said above, `<-` is an equivalent to `constantly`. Use `<-` in the context of a form threading in the style of `->`, and `<<-` in the context of arrows like `->>`

```clojure
(->  1 (<-  42)) ;; => 42
(->> 1 (<<- 42)) ;; => 42
```

### Conditionnals & boolean operators

- `if->`, `when->`, `if-not->`, `when-not->`
- `and->`, `or->`, `not->`

Contrary to their Clojure counterparts (`if`, `when`, etc ...), these arrows (except `not->`) return the threaded value rather than `nil` when they "fail".

Consider:
```clojure
(defn xxx [x]
  (-> x
      (when-> string? (str "_abc"))
      vector))

(xxx "bird")
;; => ["bird_abc"]

(xxx 123)
;; => [123] (rather than [nil])
```

### Binding arrows

`let->`, `binding->`, `when-let->`.

```clojure
(-> 1
    (let-> [double (* 2)]
      (+ double)))
;; => 3
```

### Other control flow

#### `juxt->` & `juxtm->`

```clojure
(-> 1 (juxt-> (/ 2) dec -))
;; => '(1/2 0 -1)
(-> 1 (juxtm-> :half (/ 2) :decd dec :neg -))
;; => '{:half 1/2, :decd 0, :neg -1}
```

#### `doto->`

```clojure
(-> 123
    (doto->  (str " -> the initial value")
             println)
    inc
    (doto->> (str "New value is : ")
             println))
;; 123 -> the initial value
;; New value is: 124
;; => 124
```

#### `dotos`

The `dotos` macro will work in a similar way:
```clojure
(dotos (inc 1) (println "Status: done"))
;; Status: done
;; => 2
```

Although it is not a threading arrow strictly speaking it will thread the passed value into any threading form that happens to be at the first level in its body.

Consider:
```clojure
(dotos (inc 1)
       (println "Status: done")
       (->> (println "Result:")))
;; Status: done
;; Result: 2
;; => 2
```

### Debugging

`pp->` and `pp->>` will execute each form, threading them together, and will display at each step a debug line, then return the value from the last expr.

```clojure
(pp->> {:a 1 :b 2}
       (merge {:c 3})
       (map (fn [[k n]] [k (inc n)]))
       (into {}))
;; ->>                               : {:a 1, :b 2}
;;     (merge {:c 3})                : {:c 3, :a 1, :b 2}
;;     (map (fn [[k n]] [k (inc n)])): ([:c 4] [:a 2] [:b 3])
;;     (into {})                     : {:c 4, :a 2, :b 3}

;; => {:c 4, :a 2, :b 3}
```

### Sequential operations

#### `map->` & `filter->`

```clojure
(-> [1 2 3] (map-> (+ 3))
;; => (4 5 6)
(-> [1 2 3] (filter-> odd?))
;; => (1 3)
```

Similarly, `mapv->`, `mapcat->`, `map-keys->` & `map-vals->`.


## Fletchings

### Basic fletchings: `>-` & `>>-`

Used to shift from a thread-first to a thread-last threading-style, or conversely from thread-last to thread-first:
  - `>>-` to shift into thread-first mode in the context of a `->>`-like arrow
  - `>-` to shift into thread-last mode in the context of a `->`-like arrow

#### Equivalences
```clojure
(->  x (>-  (->  y)))    <=>    (->  y x)    <=>    (->  x (>-> y))
(->  x (>-  (->> y)))    <=>    (->> y x)    <=>    (->  x (>->> y))
(->> x (>>- (->  y)))    <=>    (->  x y)    <=>    (->> x (>>-> y))
(->> x (>>- (->> y)))    <=>    (->> x y)    <=>    (->> x (>>->> y))
```

Examples:
```clojure
(->> 100   (>>- (->  (/ 10))))  ;; expands to (->  100 (/ 10))
(->> 10    (>>- (->> (/ 100)))) ;; expands to (->> 10  (/ 100))
(-> (/ 10) (>-  (->  100)))     ;; expands to (->  100 (/ 10))
(-> (/ 10) (>-  (->> 100)))     ;; expands to (->> 100 (/ 10))

```
### `>-args`

So that an arrows applies to a form's subforms rather than to the form itself.

```clojure
(-> 1 (>-args (-> (+ inc (/ 2))))) ;; 5/2
;; is equivalent to
(let [x 1]
  (+ (-> x inc) (-> x (/ 2))))
```

Similarly:
```clojure
(->  1 (>-args  (->> (+ inc (/ 2))))) ;; 4
(->> 1 (>>-args (->  (+ inc (/ 2))))) ;; 5/2
(->> 1 (>>-args (->> (+ inc (/ 2))))) ;; 4
```

### Teleport fletching/arrow

The `o-` fletching stores the threaded expression on the stack then threads it to the threading form. Any deep occurence of the `-o` arrow in the threading form will thread this stored value to the next expressions.

```clojure
(-> 1  (o-  (+ 100 (-o  (/ 2))))) ;; => 101.5 (+ 100 1 (/ 1 2))
(-> 1  (o-  (+ 100 (-oo (/ 2))))) ;; => 103   (+ 100 1 (/ 2 1))
(->> 1 (oo- (+ 100 (-o  (/ 2))))) ;; => 101.5 (+ 100 (/ 1 2) 1)
(->> 1 (oo- (+ 100 (-oo (/ 2))))) ;; => 103   (+ 100 (/ 2 1) 1)
```

## Defining new arrows

The challenge lying in defining both the `->` and `->>` variants, observe the actual definition of `doto->/doto->>`:

```clojure
(defthreading doto ;; name prefix (optional). You can also write "doto :suffix".
  "Threads the expr through the forms then returns the value of
  the initial expr.";; Doc body (optional)
  [->  "Threads like `->`."   ;; Doc suffixes (optional)
   ->> "Threads like `->>`."]
  [expr & forms]
  `(let [result# ~expr]
    ;; &threading-variant will be succesively bound to '-> and '->>
     (~&threading-variant result# ~@forms)
     result#))
```

See [`defthreading`](https://unexpectedness.github.io/threading/threading.core.html#var-defthreading) for details and the [sources](https://github.com/unexpectedness/threading/blob/master/src/threading/core.clj) for more simple examples to work from.

## Why use `threading` ?

#### Readability

Compare:
```clojure
(let [selection (fetch resource opts)]
  (if (<= (count selection) 1)
    (first selection)
    selection)))
```

against:
```clojure
(-> (fetch resource opts)
    (if-> (-> count (<= 1)) first))
```

## TODO

-  `cond->`, maybe `merge->`, etc... Contributions are welcomed if they are driven by *an impulse*.
-  `pp->` is a bit weird at times.

-------------------------------------------------------------------------------

Copyright © 2024 unexpectedness

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

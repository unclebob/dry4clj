# dry4clj

dry4clj finds candidate duplicate Clojure code across files and directories. It reports fuzzy structural matches by filename and line range so another mechanism can evaluate and reduce duplication.

## Overview

dry4clj compares top-level Clojure forms by first converting each form into
normalized syntax nodes. The normalized form is then walked to collect a set of
structural fingerprints, one for the whole form and one for each nested
subform.

Similarity is Jaccard similarity over those fingerprint sets:

```text
score = shared fingerprints / all fingerprints seen in either form
```

A score of `1.0` means the normalized structures have the same fingerprint set.
Lower scores mean the forms still share structure, but each form also has some
structure the other does not. The default `--threshold 0.82` reports candidates
whose normalized structures are close enough to be worth review.

For example, these two functions score `1.0`: their names, local names,
predicates, and mapped functions differ, but those incidental symbols normalize
away and the retained structure is identical.

```clojure
(defn alpha [xs]
  (let [ys (filter odd? xs)]
    (map inc ys)))

(defn beta [items]
  (let [kept (filter even? items)]
    (map dec kept)))
```

These two functions score about `0.89`, roughly `0.9`. Most of the normalized
structure is shared, but the second form has one extra binding, so the Jaccard
intersection is smaller than the union.

```clojure
(defn invoice-summary [orders]
  (let [paid (filter paid? orders), domestic (filter domestic? paid)
        sorted (sort-by :date domestic), amounts (map :amount sorted)
        taxes (map tax amounts), ids (map :id sorted)
        customers (map :customer sorted), regions (group-by :region sorted)
        flagged (filter flagged? sorted)]
    {:count (count sorted)
     :first-id (first ids), :last-id (last ids)
     :customers (set customers), :regions (keys regions)
     :flagged (count flagged), :total (reduce + 0 amounts)
     :tax (reduce + 0 taxes)}))

(defn receipt-summary [rows]
  (let [closed (filter closed? rows), local (filter local? closed)
        ordered (sort-by :date local), amounts (map :amount ordered)
        taxable (filter taxable? ordered), taxes (map tax amounts)
        ids (map :id ordered), customers (map :customer ordered)
        regions (group-by :region ordered)
        flagged (filter flagged? ordered)]
    {:count (count ordered)
     :first-id (first ids), :last-id (last ids)
     :customers (set customers), :regions (keys regions)
     :flagged (count flagged), :total (reduce + 0 amounts)
     :tax (reduce + 0 taxes)}))
```

## Usage

```bash
clj -M:dry4clj [options] [file-or-directory ...]
```

Options:

```text
--threshold N   Minimum structural similarity score, default 0.82
--min-lines N   Minimum source lines in a candidate form, default 4
--min-nodes N   Minimum normalized syntax nodes, default 20
--format F      text or edn, default text
--edn           Same as --format edn
--text          Same as --format text
```

Normalized syntax nodes are the structural pieces dry4clj counts after reading a
Clojure form and replacing incidental names and literal values with generic
markers. Collection shape, maps, sets, vectors, lists, and the head of a list
are preserved, so two forms with different local names or constants can still
match when their structure is similar. `--min-nodes` filters out normalized forms
smaller than the given node count before duplicate comparison.

Examples:

```bash
clj -M:dry4clj src
clj -M:dry4clj src/foo.cljc src/bar.cljc
clj -M:dry4clj --edn --threshold 0.9 src
```

Every file named on the command line participates in the same duplication
search. When an argument is a directory, dry4clj recursively includes every
`.clj`, `.cljc`, and `.cljs` file under that directory in the same search set.
For example, `clj -M:dry4clj src test/foo.clj` compares candidates from
`test/foo.clj` against candidates from every Clojure source file under `src`,
as well as comparing those files with each other.

Default text output is intended for quick reading:

```text
DUPLICATE score=0.89
  src/billing/invoice.clj:12-25
  src/billing/receipt.clj:30-44
```

EDN output is intended for tools:

```clojure
{:candidates
 [{:score 0.8909090909090909
   :left {:file "src/billing/invoice.clj", :start-line 12, :end-line 25}
   :right {:file "src/billing/receipt.clj", :start-line 30, :end-line 44}
   :left-nodes 88
   :right-nodes 91}]}
```

## Development

```bash
clj -M:spec
```

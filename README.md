# dry4clj

dry4clj finds candidate duplicate Clojure code across files and directories. It reports fuzzy structural matches by filename and line range so another mechanism can evaluate and reduce duplication.

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

Examples:

```bash
clj -M:dry4clj src
clj -M:dry4clj src/foo.cljc src/bar.cljc
clj -M:dry4clj --edn --threshold 0.9 src
```

## Development

```bash
clj -M:spec
```

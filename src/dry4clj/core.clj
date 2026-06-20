;; mutation-tested: yes
(ns dry4clj.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def source-extensions #{".clj" ".cljc" ".cljs" ".cljd"})

(def default-options
  {:paths ["src"]
   :threshold 0.82
   :min-lines 4
   :min-nodes 20
   :format :text})

(defn- source-file?
  [file]
  (and (.isFile file)
       (some #(str/ends-with? (.getName file) %) source-extensions)))

(defn- files-under
  [path]
  (let [file (io/file path)]
    (cond
      (source-file? file) [file]
      (.isDirectory file) (filter source-file? (file-seq file))
      :else [])))

(defn- read-one
  [reader]
  (binding [*default-data-reader-fn* tagged-literal]
    (read {:eof ::eof :read-cond :allow :features #{:clj}} reader)))

(defn- read-source-forms
  [file]
  (with-open [reader (clojure.lang.LineNumberingPushbackReader.
                      (io/reader file))]
    (loop [forms []]
      (let [form (read-one reader)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn- form-line
  [form]
  (or (:line (meta form)) 1))

(defn- line-children
  [form]
  (cond
    (map? form) (mapcat identity form)
    (coll? form) form))

(defn- max-line
  [form]
  (if-let [children (seq (line-children form))]
    (reduce max (form-line form) (map max-line children))
    (form-line form)))

(defn- preserve-head
  [form]
  (cond
    (symbol? form) [:symbol (str form)]
    (keyword? form) :keyword
    :else (if (coll? form) :form :literal)))

(declare normalize-form)

(defn- normalize-list
  [form]
  (let [[head & args] form]
    (vec (concat [:list (normalize-form head true)]
                 (map #(normalize-form % false) args)))))

(defn- normalize-map
  [form]
  (vec (cons :map (map (fn [[k v]]
                         [(normalize-form k false)
                          (normalize-form v false)])
                       form))))

(defn- normalize-coll
  [tag form]
  (vec (cons tag (map #(normalize-form % false) form))))

(defn- normalize-symbolic
  [form head?]
  (if head?
    (preserve-head form)
    (if (keyword? form) :keyword :symbol)))

(defn- normalize-collection
  [form]
  (cond
    (list? form) (normalize-list form)
    (vector? form) (normalize-coll :vector form)
    (map? form) (normalize-map form)
    (set? form) (normalize-coll :set form)))

(defn- normalize-atom
  [form head?]
  (if (or (symbol? form) (keyword? form))
    (normalize-symbolic form head?)
    :literal))

(defn- normalize-form
  ([form]
   (normalize-form form false))
  ([form head?]
   (or (normalize-collection form)
       (normalize-atom form head?))))

(defn- node-count
  [form]
  (if (coll? form)
    (inc (reduce + 0 (map node-count form)))
    1))

(defn- fingerprints
  [normalized]
  (letfn [(walk [form]
            (if (coll? form)
              (cons (pr-str form) (mapcat walk form))
              [(pr-str form)]))]
    (set (walk normalized))))

(defn- top-level-candidate?
  [min-lines min-nodes entry]
  (let [form (:form entry)]
    (and (seq? form)
         (not= 'ns (first form))
         (>= (inc (- (:end-line entry) (:start-line entry))) min-lines)
         (>= (:nodes entry) min-nodes))))

(defn- form-entry
  [file form]
  (let [start (or (:line (meta form)) 1)
        normalized (normalize-form form)]
    {:file (.getPath file)
     :start-line start
     :end-line (max-line form)
     :nodes (node-count normalized)
     :normalized normalized
     :fingerprints (fingerprints normalized)
     :form form}))

(defn- scan-files
  [paths]
  (->> paths
       (mapcat files-under)
       (sort-by #(.getPath %))
       (mapcat (fn [file]
                 (map #(form-entry file %) (read-source-forms file))))))

(defn- same-location?
  [left right]
  (and (= (:file left) (:file right))
       (= (:start-line left) (:start-line right))
       (= (:end-line left) (:end-line right))))

(defn- similarity
  [left right]
  (let [intersection (count (set/intersection (:fingerprints left) (:fingerprints right)))
        union (count (set/union (:fingerprints left) (:fingerprints right)))]
    (if (zero? union)
      0.0
      (/ intersection union))))

(defn- location
  [entry]
  {:file (:file entry)
   :start-line (:start-line entry)
   :end-line (:end-line entry)})

(defn- candidate
  [left right score]
  {:score (double score)
   :left (location left)
   :right (location right)
   :left-nodes (:nodes left)
   :right-nodes (:nodes right)})

(defn find-duplicates
  ([] (find-duplicates default-options))
  ([options]
   (let [{:keys [paths threshold min-lines min-nodes]} (merge default-options options)
         entries (->> (scan-files paths)
                      (filter #(top-level-candidate? min-lines min-nodes %))
                      vec)]
     (->> (for [i (range (count entries))
                j (range (inc i) (count entries))
                :let [left (entries i)
                      right (entries j)
                      score (similarity left right)]
                :when (and (not (same-location? left right))
                           (>= score threshold))]
            (candidate left right score))
          (sort-by (juxt (comp - :score)
                         (comp :file :left)
                         (comp :start-line :left)
                         (comp :file :right)
                         (comp :start-line :right)))
          vec))))

(defn- parse-number
  [s]
  (Double/parseDouble s))

(defn- parse-int-option
  [s]
  (Integer/parseInt s))

(def value-options
  {"--threshold" [:threshold parse-number]
   "--min-lines" [:min-lines parse-int-option]
   "--min-nodes" [:min-nodes parse-int-option]
   "--format" [:format keyword]})

(def flag-options
  {"--edn" [:format :edn]
   "--text" [:format :text]})

(def help-options #{"--help" "-h"})

(defn- finalize-paths
  [options]
  (update options :paths #(if (seq %) % ["src"])))

(defn- apply-value-option
  [options arg value]
  (let [[option-key parse] (value-options arg)]
    (assoc options option-key (parse value))))

(defn- apply-flag-option
  [options arg]
  (let [[option-key option-value] (flag-options arg)]
    (assoc options option-key option-value)))

(defn- parse-next-arg
  [options remaining]
  (let [[arg value & more] remaining]
    (cond
      (value-options arg) [(apply-value-option options arg value) (seq more)]
      (flag-options arg) [(apply-flag-option options arg) (next remaining)]
      (help-options arg) [(assoc options :help true) nil]
      :else [(update options :paths conj arg) (next remaining)])))

(defn parse-args
  [args]
  (loop [options (assoc default-options :paths [])
         remaining (seq args)]
    (if-not remaining
      (finalize-paths options)
      (let [[next-options next-remaining] (parse-next-arg options remaining)]
        (recur next-options next-remaining)))))

(def usage
  (str/join
   "\n"
   ["Usage: clj -M:dry4clj [options] [file-or-directory ...]"
    ""
    "Options:"
    "  --threshold N   Minimum structural similarity score, default 0.82"
    "  --min-lines N   Minimum source lines in a candidate form, default 4"
    "  --min-nodes N   Minimum normalized syntax nodes, default 20"
    "  --format F      text or edn, default text"
    "  --edn           Same as --format edn"
    "  --text          Same as --format text"]))

(defn- line-range
  [{:keys [file start-line end-line]}]
  (str file ":" start-line "-" end-line))

(defn- format-candidate
  [candidate]
  (str "DUPLICATE score=" (format "%.2f" (:score candidate)) "\n"
       "  " (line-range (:left candidate)) "\n"
       "  " (line-range (:right candidate))))

(defn- print-text
  [candidates]
  (if (seq candidates)
    (println (str/join "\n\n" (map format-candidate candidates)))
    (println "No duplicate candidates found.")))

(defn- exit
  [status]
  (System/exit status))

(defn- print-unknown-format
  [format]
  (binding [*out* *err*]
    (println "Unknown format:" (name format)))
  (exit 2))

(defn- print-candidates
  [{:keys [format] :as options}]
  (let [candidates (find-duplicates options)]
    (case format
      :edn (prn {:candidates candidates})
      :text (print-text candidates)
      (print-unknown-format format))))

(defn -main
  [& args]
  (let [options (parse-args args)]
    (if (:help options)
      (println usage)
      (print-candidates options))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-20T13:52:48.272369-05:00", :module-hash "833985876", :forms [{:id "form/0/ns", :kind "ns", :line 2, :end-line 5, :hash "738730266"} {:id "def/source-extensions", :kind "def", :line 7, :end-line 7, :hash "-2015356897"} {:id "def/default-options", :kind "def", :line 9, :end-line 14, :hash "1618189645"} {:id "defn-/source-file?", :kind "defn-", :line 16, :end-line 19, :hash "-531609688"} {:id "defn-/files-under", :kind "defn-", :line 21, :end-line 27, :hash "39338109"} {:id "defn-/read-one", :kind "defn-", :line 29, :end-line 32, :hash "-1529018993"} {:id "defn-/read-source-forms", :kind "defn-", :line 34, :end-line 42, :hash "2140894971"} {:id "defn-/form-line", :kind "defn-", :line 44, :end-line 46, :hash "1829661622"} {:id "defn-/line-children", :kind "defn-", :line 48, :end-line 52, :hash "1018687439"} {:id "defn-/max-line", :kind "defn-", :line 54, :end-line 58, :hash "886207293"} {:id "defn-/preserve-head", :kind "defn-", :line 60, :end-line 65, :hash "-1961550717"} {:id "form/11/declare", :kind "declare", :line 67, :end-line 67, :hash "1647626950"} {:id "defn-/normalize-list", :kind "defn-", :line 69, :end-line 73, :hash "-1338929278"} {:id "defn-/normalize-map", :kind "defn-", :line 75, :end-line 80, :hash "143800203"} {:id "defn-/normalize-coll", :kind "defn-", :line 82, :end-line 84, :hash "-1805122234"} {:id "defn-/normalize-symbolic", :kind "defn-", :line 86, :end-line 90, :hash "-2084642560"} {:id "defn-/normalize-collection", :kind "defn-", :line 92, :end-line 98, :hash "-1272367018"} {:id "defn-/normalize-atom", :kind "defn-", :line 100, :end-line 104, :hash "-1120776771"} {:id "defn-/normalize-form", :kind "defn-", :line 106, :end-line 111, :hash "-2070349657"} {:id "defn-/node-count", :kind "defn-", :line 113, :end-line 117, :hash "-951214240"} {:id "defn-/fingerprints", :kind "defn-", :line 119, :end-line 125, :hash "-258558512"} {:id "defn-/top-level-candidate?", :kind "defn-", :line 127, :end-line 133, :hash "-158662047"} {:id "defn-/form-entry", :kind "defn-", :line 135, :end-line 145, :hash "1704320196"} {:id "defn-/scan-files", :kind "defn-", :line 147, :end-line 153, :hash "1937169807"} {:id "defn-/same-location?", :kind "defn-", :line 155, :end-line 159, :hash "289597246"} {:id "defn-/similarity", :kind "defn-", :line 161, :end-line 167, :hash "2096049461"} {:id "defn-/location", :kind "defn-", :line 169, :end-line 173, :hash "807082359"} {:id "defn-/candidate", :kind "defn-", :line 175, :end-line 181, :hash "-714060629"} {:id "defn/find-duplicates", :kind "defn", :line 183, :end-line 203, :hash "1495561193"} {:id "defn-/parse-number", :kind "defn-", :line 205, :end-line 207, :hash "-227483849"} {:id "defn-/parse-int-option", :kind "defn-", :line 209, :end-line 211, :hash "-1617359596"} {:id "def/value-options", :kind "def", :line 213, :end-line 217, :hash "1305776259"} {:id "def/flag-options", :kind "def", :line 219, :end-line 221, :hash "-262408985"} {:id "def/help-options", :kind "def", :line 223, :end-line 223, :hash "1319960560"} {:id "defn-/finalize-paths", :kind "defn-", :line 225, :end-line 227, :hash "1109688683"} {:id "defn-/apply-value-option", :kind "defn-", :line 229, :end-line 232, :hash "1535273659"} {:id "defn-/apply-flag-option", :kind "defn-", :line 234, :end-line 237, :hash "843910386"} {:id "defn-/parse-next-arg", :kind "defn-", :line 239, :end-line 246, :hash "1496677820"} {:id "defn/parse-args", :kind "defn", :line 248, :end-line 255, :hash "-2094543293"} {:id "def/usage", :kind "def", :line 257, :end-line 268, :hash "-959575942"} {:id "defn-/line-range", :kind "defn-", :line 270, :end-line 272, :hash "1858464560"} {:id "defn-/format-candidate", :kind "defn-", :line 274, :end-line 278, :hash "973837906"} {:id "defn-/print-text", :kind "defn-", :line 280, :end-line 284, :hash "1915580061"} {:id "defn-/exit", :kind "defn-", :line 286, :end-line 288, :hash "38242339"} {:id "defn-/print-unknown-format", :kind "defn-", :line 290, :end-line 294, :hash "-1727026772"} {:id "defn-/print-candidates", :kind "defn-", :line 296, :end-line 302, :hash "-1900503801"} {:id "defn/-main", :kind "defn", :line 304, :end-line 309, :hash "-1620044253"}]}
;; clj-mutate-manifest-end

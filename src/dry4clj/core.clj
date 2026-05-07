;; mutation-tested: no
(ns dry4clj.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def source-extensions #{".clj" ".cljc" ".cljs"})

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
  (read {:eof ::eof :read-cond :allow :features #{:clj}} reader))

(defn- read-source-forms
  [file]
  (with-open [reader (clojure.lang.LineNumberingPushbackReader.
                      (io/reader file))]
    (loop [forms []]
      (let [form (read-one reader)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn- max-line
  [form]
  (let [line (:line (meta form))]
    (cond
      (map? form) (reduce max (or line 1) (map max-line (mapcat identity form)))
      (coll? form) (reduce max (or line 1) (map max-line form))
      :else (or line 1))))

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

(defn- normalize-form
  ([form]
   (normalize-form form false))
  ([form head?]
   (cond
     (list? form) (normalize-list form)
     (vector? form) (normalize-coll :vector form)
     (map? form) (normalize-map form)
     (set? form) (normalize-coll :set form)
     (or (symbol? form) (keyword? form)) (normalize-symbolic form head?)
     :else :literal)))

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

(defn parse-args
  [args]
  (loop [options (assoc default-options :paths [])
         remaining (seq args)]
    (if-not remaining
      (finalize-paths options)
      (let [[arg value & more] remaining]
        (cond
          (value-options arg) (recur (apply-value-option options arg value) (seq more))
          (flag-options arg) (recur (apply-flag-option options arg) (next remaining))
          (help-options arg) (assoc options :help true)
          :else (recur (update options :paths conj arg) (next remaining)))))))

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

(defn -main
  [& args]
  (let [options (parse-args args)]
    (if (:help options)
      (println usage)
      (let [candidates (find-duplicates options)]
        (case (:format options)
          :edn (prn {:candidates candidates})
          :text (print-text candidates)
          (do
            (binding [*out* *err*]
              (println "Unknown format:" (name (:format options))))
            (System/exit 2)))))))

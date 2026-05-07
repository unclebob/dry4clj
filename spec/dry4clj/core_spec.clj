(ns dry4clj.core-spec
  (:require [clojure.java.io :as io]
            [dry4clj.core :as dry]
            [speclj.core :refer :all]))

(defn- temp-dir
  []
  (.toFile (java.nio.file.Files/createTempDirectory
            "dry4clj-core-spec"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-source
  [dir name text]
  (let [file (io/file dir name)]
    (io/make-parents file)
    (spit file text)
    file))

(describe "dry4clj duplicate detection"
  (it "reports structural duplicate candidates with file and line ranges"
    (let [dir (temp-dir)
          left (write-source dir "left.cljc"
                             "(ns sample.left)\n\n(defn alpha [xs]\n  (let [ys (filter odd? xs)]\n    (map inc ys)))\n")
          right (write-source dir "right.cljc"
                              "(ns sample.right)\n\n(defn beta [items]\n  (let [kept (filter even? items)]\n    (map dec kept)))\n")
          candidates (dry/find-duplicates {:paths [(.getPath dir)]
                                           :threshold 0.80
                                           :min-lines 3
                                           :min-nodes 8})]
      (should= 1 (count candidates))
      (should= (.getPath left) (get-in (first candidates) [:left :file]))
      (should= 3 (get-in (first candidates) [:left :start-line]))
      (should= 5 (get-in (first candidates) [:left :end-line]))
      (should= (.getPath right) (get-in (first candidates) [:right :file]))
      (should= 3 (get-in (first candidates) [:right :start-line]))
      (should= 5 (get-in (first candidates) [:right :end-line]))))

  (it "matches fuzzy structures containing maps sets and keyword calls"
    (let [dir (temp-dir)]
      (write-source dir "left.cljc"
                    "(ns sample.left)\n\n(defn gamma [m]\n  (when (#{:a :b} (:kind m))\n    {:left (:a m) :right (:b m)}))\n")
      (write-source dir "right.cljc"
                    "(ns sample.right)\n\n(defn delta [row]\n  (when (#{:c :d} (:kind row))\n    {:left (:c row) :right (:d row)}))\n")
      (should= 1
               (count (dry/find-duplicates {:paths [(.getPath dir)]
                                            :threshold 0.80
                                            :min-lines 3
                                            :min-nodes 8})))))

  (it "filters forms shorter than the minimum line count"
    (let [dir (temp-dir)]
      (write-source dir "one.clj" "(ns one)\n(defn a [x] (+ x 1))\n")
      (write-source dir "two.clj" "(ns two)\n(defn b [y] (+ y 2))\n")
      (should= []
               (dry/find-duplicates {:paths [(.getPath dir)]
                                     :threshold 0.80
                                     :min-lines 3
                                     :min-nodes 1}))))

  (it "parses command line options and paths"
    (should= {:paths ["spec"]
              :threshold 0.9
              :min-lines 5
              :min-nodes 30
              :format :edn}
             (dry/parse-args ["--threshold" "0.9"
                              "--min-lines" "5"
                              "--min-nodes" "30"
                              "--edn"
                              "spec"])))

  (it "defaults to src when no paths are provided"
    (should= ["src"] (:paths (dry/parse-args []))))

  (it "formats text output with line ranges"
    (let [candidate {:score 0.875
                     :left {:file "a.clj" :start-line 10 :end-line 14}
                     :right {:file "b.clj" :start-line 20 :end-line 24}}]
      (should= "DUPLICATE score=0.88\n  a.clj:10-14\n  b.clj:20-24\n"
               (with-out-str (#'dry/print-text [candidate])))))

  (it "prints a clear message when no text candidates exist"
    (should= "No duplicate candidates found.\n"
             (with-out-str (#'dry/print-text []))))

  (it "prints help from main without scanning files"
    (should-contain "Usage: clj -M:dry4clj"
                    (with-out-str (dry/-main "--help"))))

  (it "prints edn from main"
    (let [dir (temp-dir)]
      (write-source dir "one.clj" "(ns one)\n(defn a [x] (+ x 1))\n")
      (should= "{:candidates []}\n"
               (with-out-str (dry/-main "--edn" (.getPath dir)))))))

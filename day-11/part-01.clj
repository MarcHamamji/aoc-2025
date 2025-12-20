(ns aoc.part-01)
(require '[clojure.string :as str])
(require '[loom.graph :as graph])
(require '[loom.io :as io])

(defn hashmap-from-entries [entries]
  (reduce #(assoc %1 (first %2) (second %2)) {} entries))

(defn dfs [g start end]
  (cond
    (= start end)
    1
    :else
    (reduce + 0 (map #(dfs g % end) (graph/successors g start)))))

(defn solve [lines]
  (let [g (graph/digraph (hashmap-from-entries lines))]
    (io/view g)
    (println (dfs g "you" "out"))))

(println
 "Final Answer:",
 (as-> *command-line-args* input
   (second input)
   (slurp input)
   (str/trim input)
   (str/split input #"\n")
   (map (fn [line]
          (let [split (str/split line #": ")]
            [(first split) (str/split (second split) #" ")]))
        input)
   (solve input)))

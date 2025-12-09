(ns aoc.part-01)
(require '[clojure.string :as str])

(defn dist [a b] (inc (abs (- a b))))

(defn get-area [t1 t2]
  (*
   (dist (first t1) (first t2))
   (dist (second t1) (second t2))))

(defn get-max-area [tiles]
  (apply max (for [t1 tiles
                   t2 tiles
                   :when (not= t1 t2)]
               (get-area t1 t2))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map #(map parse-long (str/split % #",")) input)
   (get-max-area input)))

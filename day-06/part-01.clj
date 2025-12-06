(ns aoc.part-01)
(require '[clojure.string :as str])

(defn calculate-result [input]
  (let [operation (last input)
        nums (pop input)]
    (reduce (if (= operation "+") + *) (map parse-long nums))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map #(str/trim %) input)
   (map #(str/split % #" +") input)
   (apply map vector input)
   (map calculate-result input)
   (reduce + input)))


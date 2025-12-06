(ns aoc.part-02)
(require '[clojure.string :as str])

(defn transpose [grid]
  (apply map vector grid))


(def operator-map {:+ +
                   :* *})

(defn calculate-result [input]
  (loop [i 0
         last-operator \space
         acc nil
         results []]
    (if (= i (count input))
      results
      (let [column (nth input i)
            digits (drop-last column)
            is-empty (every? str/blank? (map str digits))]
        (if is-empty
          (recur (inc i) last-operator nil (conj results acc))
          (let [operator (case (last column)
                           \+ :+
                           \* :*
                           last-operator)
                num (->> digits
                         (map str)
                         (map parse-long)
                         (map #(if (nil? %) "" %))
                         (apply str)
                         (parse-long))
                acc-or-default (if (nil? acc) (case operator
                                                :+ 0
                                                :* 1) acc)]
            (recur (inc i) operator ((operator-map operator) acc-or-default num) results)))))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map #(str % " ") input)
   (transpose input)
   ; (println input)
   (calculate-result input)
   (reduce + input)))


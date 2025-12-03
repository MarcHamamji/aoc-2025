(ns aoc.part-01
  (:require [clojure.string :as str]))

(defn get-max-digit [string start end]
  (if (>= start end)
    nil
    (loop [i start
           max-digit 0
           max-digit-index -1]
      (if (= i end)
        [max-digit max-digit-index]
        (let [current-digit (parse-long (str (nth string i)))]
          (if (<= current-digit max-digit)
            (recur (inc i) max-digit max-digit-index)
            (recur (inc i) current-digit i)))))))

(defn get-largest-joltage [bank]
  (let [[first-num first-num-index] (get-max-digit bank 0 (dec (count bank)))
        [second-num _second-num-index] (get-max-digit bank (inc first-num-index) (count bank))]
    (printf "Largest joltage for %s: " bank)
    (doto (parse-long (str first-num second-num)) println)))

(defn get-total-output-joltage [banks]
  (reduce + (map get-largest-joltage banks)))

(println
 "Final answer:"
 (-> (nth *command-line-args* 1)
     slurp
     str/trim
     (str/split #"\n")
     get-total-output-joltage))




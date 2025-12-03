(ns aoc.part-02
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

(defn get-largest-joltage [bank n]
  (printf "Largest joltage for %s: " bank)
  (doto (->> (loop [i n
                    last-battery-index -1
                    batteries []]
               (if (zero? i)
                 batteries
                 (let [[battery-value battery-index]
                       (get-max-digit
                        bank
                        (inc last-battery-index)
                        (inc (- (count bank) i)))]
                   (recur (dec i) battery-index (conj batteries battery-value)))))
             (apply str)
             parse-long) println))

(defn get-total-output-joltage [banks n]
  (reduce + (map #(get-largest-joltage % n) banks)))

(println
 "Final answer:"
 (-> (nth *command-line-args* 1)
     slurp
     str/trim
     (str/split #"\n")
     (get-total-output-joltage 12)))




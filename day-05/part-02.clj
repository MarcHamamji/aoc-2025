(ns aoc.part-02)
(require '[clojure.string :as str])

(def start first)
(def end second)

(defn set-top [coll x]
  (conj (pop coll) x))

(defn merge-sorted-ranges [sorted-ranges]
  (loop [i 1
         merged [(first sorted-ranges)]]
    (if (= i (count sorted-ranges))
      merged
      (let [last-merged (last merged)
            current-range (nth sorted-ranges i)]
        (if (<= (start current-range) (end last-merged))
          (recur
           (inc i)
           (set-top
            merged
            [(start last-merged)
             (max (end current-range) (end last-merged))]))
          (recur
           (inc i)
           (conj merged current-range)))))))

(defn count-fresh-ingredients [database]
  (let [[ranges-str] (str/split database #"\n\n")
        ranges (map #(map parse-long (str/split % #"-")) (str/split ranges-str #"\n"))
        sorted-ranges (sort-by #(nth % 0) ranges)
        merged-ranges (merge-sorted-ranges sorted-ranges)]
    (println sorted-ranges)
    (println merged-ranges)
    (reduce + (map #(inc (- (end %) (start %))) merged-ranges))))

(println
 "Final answer:"
 (-> (nth *command-line-args* 1)
     slurp
     str/trim
     count-fresh-ingredients))

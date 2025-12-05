(ns aoc.part-01)
(require '[clojure.string :as str])

(defn is-ingredient-fresh [ranges ingredient]
  (some (fn [[start end]]
          (and (>= ingredient start) (<= ingredient end)))
        ranges))

(defn count-fresh-ingredients [database]
  (let [[ranges-str ingredients-str] (str/split database #"\n\n")
        ranges (map #(map parse-long (str/split % #"-")) (str/split ranges-str #"\n"))
        ingredients (map parse-long (str/split ingredients-str #"\n"))]
    (count (filter #(is-ingredient-fresh ranges %) ingredients))))

(println
 "Final answer:"
 (-> (nth *command-line-args* 1)
     slurp
     str/trim
     count-fresh-ingredients))

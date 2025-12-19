(ns aoc.part-01)
(require '[clojure.string :as str])
(require '[clojure.data.priority-map :refer [priority-map]])
(require '[engelberg.data.union-find :as uf])

(defn distance-squared [b1 b2]
  (reduce + (map #(* % %) (map - b1 b2))))

(defn solve [boxes num-connections]
  (let [indexed-boxes (map-indexed vector boxes)
        tile-pairs (for [[i1 b1] indexed-boxes
                         [i2 b2] indexed-boxes
                         :when (< i1 i2)]
                     [[b1 b2] (distance-squared b1 b2)])
        tile-pairs-pq (into (priority-map) tile-pairs)]
    (loop [index 0
           uf (apply uf/union-find boxes)
           pq tile-pairs-pq]
      (if (= index num-connections)
        (do
          (println (uf/count-elements uf))
          (println (uf/elements uf))
          (println (uf/count-components uf))
          (println (uf/components uf))
          (reduce * (take 3 (reverse (sort (map count (uf/components uf)))))))
        (let [[[b1 b2] weight] (first pq)
              others (rest pq)]
          (println "linking" b1 "and" b2 "weight:" weight)
          (recur (inc index) (uf/connect uf b1 b2) others))))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map (fn [line]
          (map parse-long (str/split line #",")))
        input)
   (solve input 1000)))

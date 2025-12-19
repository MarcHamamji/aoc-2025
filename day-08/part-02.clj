(ns aoc.part-02)
(require '[clojure.string :as str])
(require '[clojure.data.priority-map :refer [priority-map]])
(require '[engelberg.data.union-find :as uf])

(defn distance-squared [b1 b2]
  (reduce + (map #(* % %) (map - b1 b2))))

(defn solve [boxes]
  (let [indexed-boxes (map-indexed vector boxes)
        tile-pairs (for [[i1 b1] indexed-boxes
                         [i2 b2] indexed-boxes
                         :when (< i1 i2)]
                     [[b1 b2] (distance-squared b1 b2)])
        tile-pairs-pq (into (priority-map) tile-pairs)]
    (loop [index 0
           uf (apply uf/union-find boxes)
           pq tile-pairs-pq]
      (let [[[b1 b2] weight] (first pq)
            new-pq (rest pq)
            new-uf (uf/connect uf b1 b2)]
        (println "linking" b1 "and" b2 "weight:" weight)
        (if (= (uf/count-components new-uf) 1)
          (* (first b1) (first b2))
          (recur (inc index) new-uf new-pq))))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map (fn [line]
          (map parse-long (str/split line #",")))
        input)
   (solve input)))

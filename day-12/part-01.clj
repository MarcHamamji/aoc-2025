(ns aoc.part-01)
(require '[clojure.string :as str])

(defn solve [input]
  (let [sections (str/split input #"\n\n")
        ; presents (map #(count (filter #{\#} %)) (take (- (count sections) 2) sections))
        presents (take (- (count sections) 2) (repeat 9))
        trees (map (fn [tree]
                     (let [[size & counts] (str/split tree #": ")]
                       {:size (map parse-long (str/split size #"x"))
                        :counts (map parse-long (str/split (first counts) #" "))}))

                   (str/split (last sections) #"\n"))
        areas (->> trees
                   (map :size)
                   (map (partial reduce *)))
        needed-presents-areas (->> trees
                                   (map :counts)
                                   (map #(map * % presents))
                                   (map #(reduce + %)))]
    (println "presents" presents)
    (println "trees" trees)
    (println "areas" areas)
    (println "needed-presents-areas" needed-presents-areas)
    (println (map > areas needed-presents-areas))
    (println (dec (count (filter identity (map > areas needed-presents-areas)))))
    ))

(println
 "Final Answer:"
 (->>
  *command-line-args*
  second
  slurp
  solve))

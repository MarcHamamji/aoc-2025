(ns aoc.part-02)
(require '[clojure.string :as str])
(require '[loom.graph :as graph])
(require '[loom.io :as io])
(require '[loom.alg :as alg])

(defn hashmap-from-entries [entries]
  (reduce #(assoc %1 (first %2) (second %2)) {} entries))

(defn- get-num-paths [topo-sorted g init-first start end]
  (loop [index 1
         ways (-> (hash-map)
                  (assoc (first topo-sorted) (if init-first 1 0)))]
    (println "index" index)
    (if
     (= index (count topo-sorted))
      (ways end)
      (let [current-node (nth topo-sorted index)]
        (if
         (= current-node start)
          (recur
           (inc index)
           (assoc ways start 1))
          (recur
           (inc index)
           (->> (vec (graph/predecessors g current-node))
                (map #(ways %))
                (reduce +)
                (assoc ways current-node))))))))

(defn solve [lines]
  (let [g (graph/digraph (hashmap-from-entries lines))
        topo-sorted  (alg/topsort g)]
    (println topo-sorted)
    (io/view g {:fmt :svg})

    (*
     (get-num-paths topo-sorted g true "svr" "fft")
     (get-num-paths topo-sorted g false "fft" "dac")
     (get-num-paths topo-sorted g false "dac" "out"))))

(println
 "Final Answer:",
 (as-> *command-line-args* input
   (second input)
   (slurp input)
   (str/trim input)
   (str/split input #"\n")
   (map (fn [line]
          (let [split (str/split line #": ")]
            [(first split) (str/split (second split) #" ")]))
        input)
   (solve input)))

(ns aoc.part-01)
(require '[clojure.string :as str])
(require '[loom.graph :as graph])
(require '[loom.io :as io])
(require '[loom.alg :as alg])

(defn get-content [string]
  (subs string 1 (dec (count string))))

(defn generate-combinations [n]
  (if (= 1 n)
    ["." "#"]
    (let [x (generate-combinations (dec n))]
      (concat
       (map #(str "." %) x)
       (map #(str "#" %) x)))))

(defn press-button [lights button]
  (apply str
         (reduce
          #(assoc %1 %2 (if (= (get lights %2) \#) "." "#"))
          (vec lights)
          button)))

(defn generate-dots [n]
  (apply str (take n (repeat "."))))

(defn- hashmap-from-entries [entries]
  (reduce #(assoc %1 (first %2) (second %2)) {} entries))

(defn gen-adj-list [light-combinations buttons]
  (let [entries
        (map (fn [combination]
               [combination
                (hashmap-from-entries (map
                                       (fn [button]
                                         [(press-button combination button) 1])
                                       buttons))])
             light-combinations)
        hashmap (hashmap-from-entries entries)]
    hashmap))

(defn shortest-path [machine]
  (let [size (count (:lights machine))
        source (apply str (:lights machine))
        dest (generate-dots size)
        light-combinations (generate-combinations size)
        adj-list (gen-adj-list light-combinations (:buttons machine))
        g (graph/weighted-graph adj-list)
        path (alg/dijkstra-path g source dest)]
    ; (println "lights")
    ; (println (str/join "\n" light-combinations))

    ; (io/view g)
    (dec (count path))))

(defn solve [machines]
  (reduce + (map shortest-path machines)))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   ; (take 1 input)
   (map (fn [line]
          (let [match (re-find #"(\[.+\]) (\(.*\)) (\{.*\})" line)]
            (hash-map
             :lights (get match 1)
             :buttons (get match 2)
             :joltage (get match 3))))
        input)
   (map (fn [machine]
          (hash-map
           :lights (str/split (get-content (:lights machine)) #"")
           :buttons (map (fn [button]
                           (map parse-long (str/split (get-content button) #",")))
                         (str/split (:buttons machine) #" "))
           :joltage (map parse-long (str/split (get-content (:joltage machine)) #","))))
        input)
   (solve input)))

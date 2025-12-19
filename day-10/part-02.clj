;;; I believe this works but it is not efficient enough to test

(ns aoc.part-02)
(require '[clojure.string :as str])

(defn get-content [string]
  (subs string 1 (dec (count string))))

(defn press-button [joltages button]
  (reduce (fn [v idx]
            (assoc v idx (inc (v idx))))
          joltages
          button))

(defn legal-button? [state goal button]
  (every?
   (fn [idx]
     (<= (inc (state idx)) (goal idx)))
   button))

(defn generate-initial-joltages [n]
  (vec (repeat n 0)))

(defn generate-children [state buttons goal visited]
  (for [b buttons
        :let [child (press-button state b)]
        :when (and (legal-button? state goal b)
                   (not (visited child)))]
    child))

(defn bfs [start end buttons]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{start}]
    (when-not (empty? queue)
      (let [[top depth] (peek queue)
            queue' (pop queue)]
        (if (= top end)
          depth
          (let [children (generate-children top buttons end visited)
                next-items (map #(vector % (inc depth)) children)]
            (recur (into queue' next-items)
                   (into visited children))))))))

(defn min-presses [machine]
  (let [size (count (:joltages machine))
        source (generate-initial-joltages size)
        dest (:joltages machine)
        solution (bfs source dest (:buttons machine))]
    (println solution)
    solution))

(defn solve [machines]
  (reduce + (pmap min-presses machines)))

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
             :joltages (get match 3))))
        input)
   (map (fn [machine]
          (hash-map
           :lights (str/split (get-content (:lights machine)) #"")
           :buttons (map (fn [button]
                           (map int (map parse-long (str/split (get-content button) #","))))
                         (str/split (:buttons machine) #" "))
           :joltages (vec (map int (map parse-long (str/split (get-content (:joltages machine)) #","))))))
        input)
   (solve input)))

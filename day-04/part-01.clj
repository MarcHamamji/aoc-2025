(ns aoc.part-01)
(require '[clojure.string :as str])

(def bool-to-int #(get {false 0 true 1} %))

(defn is-roll [diagram width height row-index col-index]
  (not (or
        (< row-index 0)
        (>= row-index width)
        (< col-index 0)
        (>= col-index height)
        (not (= (nth (nth diagram row-index) col-index) "@")))))

(defn is-roll-accessible [diagram width height row-index col-index]
  (<
   (reduce + (map (fn [row-offset]
                    (reduce + (map (fn [col-offset]
                                     (bool-to-int  (and
                                                    (not (and (zero? row-offset) (zero? col-offset)))
                                                    (is-roll
                                                     diagram
                                                     width
                                                     height
                                                     (+ row-index row-offset)
                                                     (+ col-index col-offset)))))
                                   (range -1 2))))
                  (range -1 2)))

   4))

(defn count-accessible-rolls [diagram]
  (let [width (count (first diagram))
        height (count diagram)]
    (reduce + (map-indexed
               (fn [row-index row]
                 (reduce + (map-indexed
                            (fn [col-index char]
                              (bool-to-int (and
                                            (= char "@")
                                            (is-roll-accessible diagram width height row-index col-index))))
                            (doto row println))))
               diagram))))

(print
 "Final answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/trim input)
   (str/split input #"\n")
   (map #(str/split % #"") input)
   (count-accessible-rolls input))
 "\n")


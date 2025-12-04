(ns aoc.part-02)
(require '[clojure.string :as str])

(defn get-element [grid row-index col-index]
  (nth (nth grid row-index) col-index))

(def bool-to-int #(get {false 0 true 1} %))

(defn is-roll [diagram width height row-index col-index]
  (not (or
        (< row-index 0)
        (>= row-index width)
        (< col-index 0)
        (>= col-index height)
        (not (= (get-element diagram row-index col-index) "@")))))

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

(defn get-accessible-rolls [diagram]
  (let [width (count (first diagram))
        height (count diagram)]
    (map-indexed
     (fn [row-index row]
       (map-indexed
        (fn [col-index element]
          (and
           (= element "@")
           (is-roll-accessible diagram width height row-index col-index)))
        row))
     diagram)))

(defn remove-accessible-rolls [{diagram :diagram _num-removed :num-removed}]
  (let [accessible-rolls (get-accessible-rolls diagram)
        num-accessible-rolls (reduce + (map #(reduce + (map bool-to-int %)) accessible-rolls))]
    (if (zero? num-accessible-rolls)
      (hash-map :diagram diagram :num-removed 0)
      (hash-map
       :diagram (map-indexed
                 (fn [row-index row]
                   (map-indexed
                    (fn [col-index element]
                      (if (true? (get-element accessible-rolls row-index col-index))
                        "."
                        element))
                    row))
                 diagram)
       :num-removed num-accessible-rolls))))

(defn count-removable-rolls [diagram]
  (->> (hash-map :diagram diagram :num-removed -1)
       (iterate remove-accessible-rolls)
       (take-while #(not (zero? (:num-removed %))))
       (map #(:num-removed %))
       rest
       (reduce +)))

(print
 "Final answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/trim input)
   (str/split input #"\n")
   (map #(str/split % #"") input)
   (count-removable-rolls input))
 "\n")


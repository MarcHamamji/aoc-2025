(ns aoc.part-02)
(require '[clojure.string :as str])

(defn dist [a b] (inc (abs (- a b))))

(defn get-area [t1 t2]
  (*
   (dist (first t1) (first t2))
   (dist (second t1) (second t2))))

(defn get-grid-char [grid row col]
  (get (get grid row) col))

(defn set-grid-char [grid row col char]
  (assoc grid row (assoc (get grid row) col char)))

(defn add-tile-to-grid [grid tile]
  (set-grid-char grid (second tile) (first tile) \X))

(defn add-edge-to-grid [grid [[t1-row t1-col] [t2-row t2-col]]]
  (let [edge-coords (if (= t1-row t2-row)
                      (map #(vec [% t1-row]) (range (inc (min t1-col t2-col)) (max t1-col t2-col)))
                      (map #(vec [t1-col %]) (range (inc (min t1-row t2-row)) (max t1-row t2-row))))
        char (if (= t1-row t2-row) \V \H)]
    (reduce (fn [acc [row col]] (set-grid-char acc row col char)) grid edge-coords)))

(defn is-inside-shape [grid row col]
  (and
   (= \. (get-grid-char grid row col))
   (loop [index (inc col)
          num-edges 0
          first-x false]
     (if
      (>= index (count (first grid)))
       (and (odd? num-edges) (not first-x))

       (let [current-char (get-grid-char grid row index)]
         (case current-char
           \V (recur (inc index) (inc num-edges) first-x)
           \H (recur (inc index) num-edges first-x)
           \X (recur (inc index) (inc num-edges) (if (zero? num-edges) true first-x))
           \. (recur (inc index) num-edges first-x)))))))

; (defn set-fill [grid [row col]]
;   (if (is-inside-shape grid row col)
;     (set-grid-char grid row col \I)
;     grid))

(defn fill-row [row]
  (loop [i 0
         filled-row []
         is-inside false]
    (if (= i (count row))
      filled-row
      (case (get row i)
        \V (recur (inc i) (conj filled-row \V) (not is-inside))
        \H (recur (inc i) (conj filled-row \H) is-inside)
        \. (recur (inc i) (conj filled-row (if is-inside \I \.)) is-inside)
        \X (recur (inc i) (conj filled-row \X) is-inside)))))

(defn fill-grid [grid]
  (for [row-index (range (count grid))]
    (let [row (get grid row-index)]

      (loop [col-index 0
             filled-row []
             is-inside false
             last-x-is-down nil]
        (if (= col-index (count row))
          filled-row
          (case (get row col-index)
            \V (recur (inc col-index) (conj filled-row \V) (not is-inside) nil)
            \H (recur (inc col-index) (conj filled-row \H) is-inside last-x-is-down)
            \. (recur (inc col-index) (conj filled-row (if is-inside \I \.)) is-inside nil)
            \X (if (nil? last-x-is-down)

                 ; landed on an edge
                 (recur
                  (inc col-index)
                  (conj filled-row \X)
                  is-inside
                  (= (get-grid-char grid (inc row-index) col-index) \V))

                 ; finishing an edge
                 (recur
                  (inc col-index)
                  (conj filled-row \X)
                  (if (=
                       (= (get-grid-char grid (inc row-index) col-index) \V); is the end pointing down
                       last-x-is-down)
                    is-inside
                    (not is-inside))
                  nil))))))))

(defn is-valid-rect [grid [t1-col t1-row] [t2-col t2-row]]
  (let [min-col (min t1-col t2-col)
        min-row (min t1-row t2-row)
        max-col (max t1-col t2-col)
        max-row (max t1-row t2-row)
        rect (map
              #(subvec
                %
                min-col
                (inc max-col))
              (subvec
               grid
               min-row
               (inc max-row)))]
    (every?
     (fn [row] (every? #(not= % \.) row))
     rect)))

(defn get-compactness-maps [tiles]
  (let [sorted-rows (sort (set (map first tiles)))
        indexed-rows (reduce #(assoc %1 (second %2) (* 2 (first %2))) (hash-map) (map-indexed vector sorted-rows))
        sorted-cols (sort (set (map second tiles)))
        indexed-cols (reduce #(assoc %1 (second %2) (* 2 (first %2))) (hash-map) (map-indexed vector sorted-cols))]
    [indexed-rows indexed-cols]))

(defn get-max-area [tiles]
  (let [[compact-rows-map compact-cols-map] (get-compactness-maps tiles)
        width (* 2 (count compact-cols-map))
        height (* 2 (count compact-rows-map))
        compact-tiles (map #(vec [(compact-rows-map (first %)) (compact-cols-map (second %))]) tiles)
        empty-grid (vec (repeat height (vec (repeat width \.))))
        grid-with-corners (reduce add-tile-to-grid empty-grid compact-tiles)
        grid-with-edges (reduce add-edge-to-grid grid-with-corners (conj (partition 2 1 compact-tiles) [(first compact-tiles) (last compact-tiles)]))
        grid-with-fill (fill-grid grid-with-edges)]

    (println "compact-rows-map")
    (println compact-rows-map)
    (println "compact-cols-map")
    (println compact-cols-map)
    (println "width")
    (println width)
    (println "height")
    (println height)
    (println "compact-tiles")
    (println compact-tiles)
    (println "empty-grid")
    (println (str/join "\n" empty-grid))
    (println "grid with corners")
    (println (str/join "\n" grid-with-corners))
    (println "grid with edges")
    (println (str/join "\n" grid-with-edges))
    (println "grid with fill")
    (println (str/join "\n" (map #(str/join "" %) grid-with-fill)))

    (apply max (for [t1 tiles
                     t2 tiles
                     :when (not= t1 t2)
                     :when (is-valid-rect
                            (vec grid-with-fill)
                            [(compact-rows-map (first t1)) (compact-cols-map (second t1))]
                            [(compact-rows-map (first t2)) (compact-cols-map (second t2))])]
                 (do
                   (println "for" t1 t2)
                   (get-area t1 t2))))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   (map (fn [line]
          (map #(int (parse-long %)) (str/split line #","))) input)
   (get-max-area input)))

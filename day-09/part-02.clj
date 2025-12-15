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

(defn set-fill [grid [row col]]
  (if (is-inside-shape grid row col)
    (set-grid-char grid row col \I)
    grid))

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

(defn get-max-area [tiles]
  (let [width (+ 2 (apply max (map first tiles)))
        height (+ 2 (apply max (map second tiles)))
        empty-grid (vec (repeat height (vec (repeat width \.))))
        grid-with-corners (reduce add-tile-to-grid empty-grid tiles)
        grid-with-edges (reduce add-edge-to-grid grid-with-corners (conj (partition 2 1 tiles) [(first tiles) (last tiles)]))
        grid-with-fill (reduce set-fill grid-with-edges (for [row (range height) col (range width)] [row col]))]

    (println "grid with corners")
    (println (str/join "\n" grid-with-corners))
    (println "grid with edges")
    (println (str/join "\n" grid-with-edges))
    (println "grid with fill")
    (println (str/join "\n" grid-with-fill))

    (apply max (for [t1 tiles
                     t2 tiles
                     :when (not= t1 t2)
                     :when (is-valid-rect grid-with-fill t1 t2)]
                 (get-area t1 t2)))))

(println
 "Final Answer:"
 (as-> *command-line-args* input
   (nth input 1)
   (slurp input)
   (str/split input #"\n")
   ; 1580
   (map (fn [line]
          (map #(int (parse-long %)) (str/split line #","))) input)
   (get-max-area input)))

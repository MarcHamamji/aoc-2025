(ns aoc.part-01)
(require '[clojure.string :as str])

(defn get-beams-row [width current-row previous-row]
  (map (fn [x]
         (let [current-char (nth current-row x)
               left-char (nth current-row (dec x) \.)
               right-char (nth current-row (inc x) \.)
               top-char (nth previous-row x \.)
               top-left-char (nth previous-row (dec x) \.)
               top-right-char (nth previous-row (inc x) \.)]
           (case current-char
             \S \|
             \. (if (= top-char \|)
                  \|
                  (if (or (and (= left-char \^) (= top-left-char \|))
                          (and (= right-char \^) (= top-right-char \|)))
                    \| ; split
                    \.))
             \^ \^)))
       (range width)))

(defn get-final-beams [diagram]
  (let [height (count diagram)
        width (count (first diagram))]
    (loop [i 0
           previous-row []
           splits-num 0]
      (if (= i height)
        splits-num
        (let [current-row (nth diagram i)
              beams-row (get-beams-row width current-row previous-row)
              splits-num-in-current-row (reduce + (map
                                                   #(if (and
                                                         (= (nth current-row %) \^)
                                                         (= (nth previous-row %) \|))
                                                      1 0)
                                                   (range width)))]
          (println (str/join "" previous-row))
          (recur
           (inc i)
           beams-row
           (+ splits-num-in-current-row splits-num)))))))

(println
 "Final Answer:"
 (-> (nth *command-line-args* 1)
     slurp
     (doto println)
     (str/split #"\n")
     (get-final-beams)))

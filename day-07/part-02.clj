(ns aoc.part-02)
(require '[clojure.string :as str])

(defn get-beams-row [width current-row previous-row]
  (map (fn [x]
         (let [current-char (nth current-row x)
               left-char (nth current-row (dec x) \0)
               right-char (nth current-row (inc x) \0)
               top-char (nth previous-row x \0)
               top-left-char (nth previous-row (dec x) \0)
               top-right-char (nth previous-row (inc x) \0)]
           (case current-char
             \S \1
             \0 (+
                 (or (parse-long (str top-char)) 0)
                 (if (= left-char \^) (parse-long (str top-left-char)) 0)
                 (if (= right-char \^) (parse-long (str top-right-char)) 0))
             \^ \^)))
       (range width)))

(defn get-final-beams [diagram]
  (let [height (count diagram)
        width (count (first diagram))]
    (loop [i 0
           previous-row []
           splits-num 0]
      (if (= i height)
        (reduce + previous-row)
        (let [current-row (nth diagram i)
              beams-row (get-beams-row width current-row previous-row)
              splits-num-in-current-row (reduce + (map
                                                   #(if (and
                                                         (= (nth current-row %) \^)
                                                         (= (nth previous-row %) \1))
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
     (str/replace #"\." "0")
     (doto println)
     (str/split #"\n")
     (get-final-beams)))

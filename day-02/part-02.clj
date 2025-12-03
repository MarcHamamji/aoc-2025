(ns aoc.part-02
  (:require [clojure.string :as str]))

; (defn is-id-repeating-n-times-rec?
;   ([id occurences] (let [len (count id)]
;                      (if (zero? (mod len occurences))
;                        (is-id-repeating-n-times-rec?
;                         id
;                         0
;                         occurences
;                         (range 0 len (/ len occurences)))
;                        false)))
;   ([id index occurences starting-points] (if (= index (nth starting-points 1))
;                                            true
;                                            (and (every? #(= (nth id index) (nth id (+ % index))) starting-points)
;                                                 (is-id-repeating-n-times-rec?
;                                                  id
;                                                  (inc index)
;                                                  occurences
;                                                  starting-points)))))

(defn is-id-repeating-n-times? [id occurences]
  (let [len (count id)]
    (if (zero? (mod len occurences))
      (let [starting-points (range 0 len (/ len occurences))]
        (loop [index 0]
          (if (= index (nth starting-points 1))
            true
            (if (every? #(= (nth id index) (nth id (+ % index))) starting-points)
              (recur (inc index))
              false))))

      false)))

(defn is-id-repeating? [id]
  (let [id-str (str id)
        len (count id-str)]
    (some
     (fn [occurences] (is-id-repeating-n-times? id-str occurences))
     (range 2 (inc len)))))

(defn sum-invalid-in-range [range-str]
  (let [[start end] (map parse-long (str/split range-str #"-"))]
    (reduce + (filter is-id-repeating? (range start (inc end))))))

(defn sum-invalid-id-ranges [ids]
  (reduce + (map sum-invalid-in-range ids)))

(print
 "Final answer:"
 (-> "input.txt"
     slurp
     str/trim
     (str/split #",")
     sum-invalid-id-ranges)
 "\n")


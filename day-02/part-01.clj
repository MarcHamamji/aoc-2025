(ns aoc.part-01
  (:require [clojure.string]))

(defn is_id_repeating?
  ([id] (is_id_repeating? id 0 (/ (count id) 2)))
  ([id index half_len] (if (= index half_len)
                         true
                         (and (= (nth id index) (nth id (+ index half_len)))
                              (is_id_repeating? id (inc index) half_len)))))

(defn is_id_valid? [id]
  (let [id_str (str id)]
    (and
     (even? (count id_str))
     (is_id_repeating? id_str))))

(defn sum_invalid_in_range [range_str]
  (let [range_vec (map Long/parseLong (clojure.string/split range_str #"-"))]
    (reduce + (filter is_id_valid? (range (nth range_vec 0) (inc (nth range_vec 1)))))))

(defn sum_invalid_id_ranges [ids]
  (->>
   (map sum_invalid_in_range ids)
   (reduce +)))

(print
 "Final answer:"
 (as-> "input.txt" input
   (slurp input)
   (clojure.string/trim input)
   (clojure.string/split input #",")
   (sum_invalid_id_ranges input))
 "\n")


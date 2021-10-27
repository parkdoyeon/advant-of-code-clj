(ns day-two
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.set :as set]))

; part 1
; Replace `frequencies`
(defn char_count [txt]
     (->> (str/split txt #"")
          (reduce (fn [m c]
                       (assoc m c (inc (get m c 0))))
                  {})
          ))

(comment (char_count "aabc"))

(defn map_has_target [target char_map]
     (some (fn [[_ v]] (= v target)) char_map))

; Warn! Walk traverses form, an **arbitrary** data structure
(->> "inputs/day-two.txt"
     slurp
     str/split-lines
     (map char_count)
     (walk/walk (fn [char_map]
                [(map_has_target 2 char_map), (map_has_target 3 char_map)]
                )
           #(reduce (fn [[two three], [has_two has_three]]
                        [(if has_two (inc two) two), (if has_three (inc three) three)])
                   [0 0] %))
     (apply *)
     )

; Use frequencies
; Replace walk/walk as reduce
(defn has_target [target char_map]
  (some (fn [x] (= x target)) char_map))

(->> "inputs/day-two.txt"
     slurp
     str/split-lines
     (map frequencies)
     (map vals)
     (reduce (fn [[two three] freq]
               [(if (has_target 2 freq) (inc two) two) (if (has_target 3 freq) (inc three) three)])
             [0, 0])
     (apply *)
     )


; part 2
;(defn has_one_diff [t1 t2]
;     (for [t1 t1 t2 t2])
;     )
;
;(let [txt_list (->> "inputs/day-two.txt"
;               slurp
;               str/split-lines
;               (map #(str/split % #"")))
;      ret ""]
;     (for [a txt_list
;           b txt_list
;           :when
;           (has_one_diff a b)]
;          [a, b]
;          )
;     )
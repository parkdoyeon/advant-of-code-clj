(ns day-one
   (:require [clojure.string :as str]))

; part 1
(->> "inputs/day-one.txt"
    slurp
    str/split-lines
    (map #(Integer/parseInt %))
    (apply +)
    )

(->> "inputs/day-one.txt"
     slurp
     str/split-lines
     (reduce #(+ %1 (Integer/parseInt %2)) 0)
     )


; part 2
; This needs an improvement since inputs have to be checked empty
(defn reached_twice [inputs]
  (loop [sum 0
         iter_inputs inputs
         reached #{}]
      (if (reached sum)
        sum
        (recur (+ sum (first (if (empty? iter_inputs) inputs iter_inputs)))
               (rest (if (empty? iter_inputs) inputs iter_inputs))
               (conj reached sum))
        )))

(->> "inputs/day-one.txt"
     slurp
     str/split-lines
     (map #(Integer/parseInt %))
     (reached_twice)
     )

; sample
; (reached_twice [1 2 -1])
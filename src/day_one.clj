(ns day-one
   (:require [clojure.string :as str]))

; part 1
; Use apply
(->> "inputs/day-one.txt"
    slurp
    str/split-lines
    (map #(Integer/parseInt %))
    (apply +)
    )

; Use reducer
(->> "inputs/day-one.txt"
     slurp
     str/split-lines
     (reduce #(+ %1 (Integer/parseInt %2)) 0)
     )

; part 2
; Use recur
(defn loop_reached_twice [inputs]
  (loop [sum 0
         inputs inputs
         reached #{}]
      (if (reached sum)
        sum
        (recur (+ sum (first inputs))
               (rest inputs)
               (conj reached sum))
        )))

; Use reducer
(defn reached_twice [inputs]
  (reduce
    (fn [[reached s] x]
      (if (reached (+ s x))
        (reduced (+ s x))
        [(conj reached (+ s x)) (+ s x)]))
    [#{} 0]
    inputs))

(->> "inputs/day-one.txt"
     slurp
     str/split-lines
     (map #(Integer/parseInt %))
     (cycle)
     (reached_twice)
     )

; sample
(comment (loop_reached_twice [1 2 -1]))
(comment (reached_twice [1 2 -1 1]))

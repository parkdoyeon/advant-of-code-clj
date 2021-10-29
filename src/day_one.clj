(ns day-one
  (:require [clojure.string :as str]))

; Set input data
(comment (def nums (->>
                     (str/split-lines (slurp "resources/day-one.txt"))
                     (map #(Integer/parseInt %)))))

; Part 1
; Use `apply` or `reduce`
(comment (apply + nums)
         (reduce + nums))

; Part 2
; Use `recur`
(defn loop-reached-twice [inputs]
  (loop [sum 0
         inputs inputs
         reached #{}]
    (if (reached sum)
      sum
      (recur (+ sum (first inputs))
             (rest inputs)
             (conj reached sum)))))

; Use `reduce`
(defn reached-twice [inputs]
  (reduce
    (fn [[reached s] x]
      (if (reached (+ s x))
        (reduced (+ s x))
        [(conj reached (+ s x)) (+ s x)]))
    [#{} 0]
    inputs))

; Sample
(comment (loop-reached-twice [1 2 -1])
         (reached-twice [1 2 -1 1]))

; Solution
(comment
  (->> nums
       cycle
       reached-twice)
  (->> nums
       cycle
       loop-reached-twice))


(ns day-one
  (:require [clojure.string :as str]))

; part 1
; Use apply
(comment
  (->> (str/split-lines (slurp "resources/day-one.txt"))
       (map #(Integer/parseInt %))
       (apply +)))

; Use reducer
(comment
  (->> (str/split-lines (slurp "resources/day-one.txt"))
       (reduce #(+ %1 (Integer/parseInt %2)) 0)))

; part 2
; Use recur
(defn loop-reached-twice [inputs]
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
(defn reached-twice [inputs]
  (reduce
    (fn [[reached s] x]
      (if (reached (+ s x))
        (reduced (+ s x))
        [(conj reached (+ s x)) (+ s x)]))
    [#{} 0]
    inputs))

(comment
  (->> (str/split-lines (slurp "resources/day-one.txt"))
       (map #(Integer/parseInt %))
       (cycle)
       (reached-twice)))

; sample
(comment (loop-reached-twice [1 2 -1]))
(comment (reached-twice [1 2 -1 1]))

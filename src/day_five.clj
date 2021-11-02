(ns day-five
  (:require [clojure.string :as str]))

(defn check [to-parse to-remain]
  (let [parsed (if (= [] to-parse) [(first to-remain)] to-parse)
        remains (if (= [] to-parse) (rest to-remain) to-remain)
        c1 (last parsed)
        c2 (first remains)]
    (cond
      (= c1 c2) [(conj parsed (first remains)) (rest remains)]
      (or (= c1 (.toUpperCase c2)) (= (.toUpperCase c1) c2)) [(pop parsed) (rest remains)]
      :else [(conj parsed (first remains)) (rest remains)])))

(defn react [txt-list]
  (loop [parsed []
         remains txt-list]
    (if (= 0 (count remains))
      parsed
      (let [[n-parsed n-remains] (check parsed remains)]
        (recur n-parsed n-remains)))))

(defn remove-by-alphabet [txt alphabet]
  (let [cap-removal-target (.toUpperCase alphabet)
        regex (java.util.regex.Pattern/compile (str alphabet "|" cap-removal-target))]
    (-> (str/replace txt regex "")
        (str/split #""))))

(comment
  ; test
  (check ["c"] ["C" "A" "b" "a"])                           ; results [[] ("A" "b" "a")]
  (react ["a" "A" "k" "c" "C" "K" "d" "b" "e" "E"])         ; results ["d" "b"]
  (remove-by-alphabet "aaaaBBBccc" "c")                     ; results ["a" "a" "a" "a" "B" "B" "B"]

  (def txt (slurp "./resources/day-five.txt"))

  ; Part 1
  (def reacted (-> txt
                   (str/split #"")
                   react))
  (def reacted-count (count reacted))

  ; Part 2
  (-> (map #(.toString (char %)) (range 97 123))
      (reduce (fn [acc alphabet]
                (update acc alphabet (fn [_] (-> (remove-by-alphabet txt alphabet)
                                                 react
                                                 count)))
                {}))
      (apply min-key val)))







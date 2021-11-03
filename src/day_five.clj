(ns day-five
  (:require [clojure.string :as str]))

(defn check [to-parse to-remain]
  (let [parsed (if (empty? to-parse) [(first to-remain)] to-parse)
        remains (if (empty? to-parse) (rest to-remain) to-remain)
        c1 (last parsed)
        c2 (first remains)]
    (cond
      (= c1 c2) [(conj parsed (first remains)) (rest remains)]
      (or (= c1 (.toUpperCase c2)) (= (.toUpperCase c1) c2)) [(pop parsed) (rest remains)]
      :else [(conj parsed (first remains)) (rest remains)])))

; Try using reduce
; FIXME try not even using reduce 🧐
(defn react [txt-list]
  (reduce (fn [[parsed remains] _]
            (if (empty? remains)
              (reduced parsed)
              (check parsed remains))) [[] txt-list] (range)))

; Try using loop
(defn loop-react [txt-list]
  (loop [parsed []
         remains txt-list]
    (if (empty? remains)
      parsed
      (let [[n-parsed n-remains] (check parsed remains)]
        (recur n-parsed n-remains)))))

(defn remove-by-alphabet [txt alphabet]
  (let [cap-removal-target (.toUpperCase alphabet)
        regex (re-pattern (str alphabet "|" cap-removal-target))]
    (-> (str/replace txt regex "")
        (str/split #""))))

(comment
  ; test
  (check ["c"] ["C" "A" "b" "a"])                           ; results [[] ("A" "b" "a")]
  (react ["a" "A" "k" "c" "C" "K" "d" "b" "e" "E"])         ; results ["d" "b"]
  (remove-by-alphabet "aaaaBBBcccC" "c")                    ; results ["a" "a" "a" "a" "B" "B" "B"]

  (def txt (slurp "./resources/day-five.txt"))

  ; Part 1
  (def reacted (-> txt
                   (str/split #"")
                   react))
  (def reacted-count (count reacted))

  ; Part 2
  (-> (map #(.toString (char %)) (range (int \a) (int \z)))
      (reduce (fn [acc alphabet]
                (update acc alphabet (fn [_] (-> (remove-by-alphabet txt alphabet)
                                                 react
                                                 count)))
                {}))
      (apply min-key val)))







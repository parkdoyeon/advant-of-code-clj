(ns day-five
  (:require [clojure.string :as str]))

(def txt (slurp "./resources/day-five.txt"))
(def txt-list (-> txt
                  (str/split #"")
                  (into [])))

(defn check
  ([[to-parse to-remain]]                                   ; in case of using iterate
   (check to-parse to-remain))
  ([to-parse to-remain]
   (let [parsed (if (empty? to-parse) [(first to-remain)] to-parse)
         remains (if (empty? to-parse) (rest to-remain) to-remain)
         c1 (peek parsed)
         c2 (first remains)]
     (if (or (= c1 (.toUpperCase c2)) (= (.toUpperCase c1) c2))
       [(pop parsed) (rest remains)]
       [(conj parsed (first remains)) (rest remains)]))))


(defn reduce-react [txt-list]
  (reduce (fn [[parsed remains] _]
            (if (empty? remains)
              (reduced parsed)
              (check parsed remains))) [[] txt-list] (range)))

(defn loop-react [txt-list]
  (loop [parsed []
         remains txt-list]
    (if (empty? remains)
      parsed
      (let [[n-parsed n-remains] (check parsed remains)]
        (recur n-parsed n-remains)))))


(defn eliminate-alphabet [string alphabet]
  "
  Eliminate upper/lower case alphabet from string
  Return vector list string
  "
  (let [cap-removal-target (.toUpperCase alphabet)
        regex (re-pattern (str alphabet "|" cap-removal-target))]
    (-> (str/replace string regex "")
        (str/split #""))))

(comment
  ; test
  (check ["c"] ["C" "A" "b" "a"])                           ; results [[] ("A" "b" "a")]
  (react ["a" "A" "k" "c" "C" "K" "d" "b" "e" "E"])         ; results ["d" "b"]
  (eliminate-alphabet "aaaaBBBcccC" "c")                    ; results ["a" "a" "a" "a" "B" "B" "B"]

  ; Part 1
  (-> txt-list
      reduce-react
      count)

  (-> txt-list
      loop-react
      count)

  (->> (iterate check [[] txt-list])
       (filter #(empty? (second %)))
       ffirst
       count)

  ; Part 2
  (->> (map #(.toString (char %)) (range (int \a) (int \z)))
       (map (fn [alphabet]
              (->> (eliminate-alphabet txt alphabet)
                   reduce-react
                   count)))
       min))
(ns day-two
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def txt-list (->> (str/split-lines (slurp "resources/day-two.txt"))
                   (map #(str/split % #""))))

(defn char-count [txt]
  "
  Replace `frequencies`
  in - aabc
  out - {a 2, b 1, c 1}
  "
  (->> (str/split txt #"")
       (reduce (fn [m c]
                 (assoc m c (inc (get m c 0))))
               {})))

(defn map-has-target [target char-map]
  (some (fn [[- v]] (= v target)) char-map))

(defn has-target [target char-map]
  (some #(= % target) char-map))

(defn common-chars [t1 t2]
  (map #(when (= %1 %2) %1) t1 t2))

(comment

  ; test
  (char-count "aabc")

  (common-chars ["a" "b" "c" "e" "f"] ["a" "b" "c" "k" "f"])

  ; part 1

  ; Warn! Walk traverses form, an *arbitrary* data structure
  ; Not recommended for explicit data structure
  (->> (str/split-lines (slurp "resources/day-two.txt"))
       (map char-count)
       (walk/walk (fn [char-map]
                    [(map-has-target 2 char-map), (map-has-target 3 char-map)])
                  #(reduce (fn [[two three], [has-two has-three]]
                             [(if has-two (inc two) two),
                              (if has-three (inc three) three)])
                           [0 0] %))
       (apply *))

  ; Trt using reduce
  (->> (str/split-lines (slurp "resources/day-two.txt"))
       (map frequencies)
       (map vals)
       (reduce (fn [[two three] freq]
                 [(if (has-target 2 freq) (inc two) two)
                  (if (has-target 3 freq) (inc three) three)])
               [0, 0])
       (apply *))

  ; part 2
  (->> (for [a txt-list
             b txt-list
             :let [common (common-chars a b)]
             :when (= (count (filter nil? common)) 1)]
         common)
       first
       (apply str)))
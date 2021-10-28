(ns day-two
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

; part 1
; Replace `frequencies`
(defn char-count [txt]
  (->> (str/split txt #"")
       (reduce (fn [m c]
                 (assoc m c (inc (get m c 0))))
               {})))


(comment (char-count "aabc"))

(defn map-has-target [target char-map]
  (some (fn [[- v]] (= v target)) char-map))

; Warn! Walk traverses form, an *arbitrary* data structure
; Not recommended for explicit data structure
(comment
  (->> (str/split-lines (slurp "resources/day-two.txt"))
       (map char-count)
       (walk/walk (fn [char-map]
                    [(map-has-target 2 char-map), (map-has-target 3 char-map)])
                  #(reduce (fn [[two three], [has-two has-three]]
                             [(if has-two (inc two) two), (if has-three (inc three) three)])
                           [0 0] %))
       (apply *)))


; Use frequencies
; Replace walk/walk as reduce
(defn has-target [target char-map]
  (some (fn [x] (= x target)) char-map))

(comment
  (->> (str/split-lines (slurp "resources/day-two.txt"))
       (map frequencies)
       (map vals)
       (reduce (fn [[two three] freq]
                 [(if (has-target 2 freq) (inc two) two) (if (has-target 3 freq) (inc three) three)])
               [0, 0])
       (apply *)))


; part 2
(defn common-chars [t1 t2]
  "
  in: [" a " " b " " c "] [" a " " b " " e "]
  out: '(" a " " b " nil)
  "
  (when (not= t1 t2)                                        ; FIXME
    (map (fn [a b] (when (= a b) a)) t1 t2)))

(comment (common-chars ["a" "b" "c" "e" "f"] ["a" "b" "c" "k" "f"]))

(comment (def txt-list (->> (str/split-lines (slurp "resources/day-two.txt"))
                            (map #(str/split % #"")))))

(comment (->> (for [a txt-list
                    b txt-list
                    :let [common (common-chars a b)]
                    :when (= (count (filter nil? common)) 1)]
                common)
              (first)
              (apply str)))
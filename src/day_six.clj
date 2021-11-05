(ns day-six
  (:require [clojure.string :as str]))

(defn parse-locations [str-location]
  (->> (str/split str-location #", ")
       (mapv #(Integer/parseInt %))))

(def locations (->> (str/split-lines (slurp "resources/day-six.txt"))
                    (map parse-locations)))

(def x-sorted (sort (map first locations)))
(def y-sorted (sort (map second locations)))

(defn abs [int]
  (max int (- int)))

(defn calc-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn infinite? [[x y]]
  (or (= x (first x-sorted))
      (= x (last x-sorted))
      (= y (first y-sorted))
      (= y (last y-sorted))))

(defn find-closest [current locations]
  (let [[[id1 dis1], [_ dis2]] (->> (map-indexed vector locations)
                                    (map (fn [[key [x y]]]
                                           [key (calc-distance current [x y])]))
                                    (sort-by second)
                                    (take 2))]
    (if (= dis1 dis2)
      "."
      id1)))

(defn mark-area-by-id [locations]
  (for [y (range (first y-sorted) (last y-sorted))
        x (range (first x-sorted) (last x-sorted))]
    [(if (infinite? [x y]) "inf" "fin")
     (find-closest [x y] locations)]))

(defn filter-infinite [areas]
  (let [infinite-ids (->> (filter #(= (first (first %)) "inf") areas)
                          (map first)
                          (map second)
                          (into #{}))]
    (->> areas
         (filter #(and (nil? (infinite-ids (second (first %))))
                       (= (first (first %)) "fin"))))))

(defn within? [current locations]
  (->> locations
       (map #(calc-distance current %))
       (apply +)
       (> 10000)))

(defn mark-area-within [locations]
  (for [y (range (- (first y-sorted) 1) (+ (last y-sorted) 2))
        x (range (- (first x-sorted) 1) (+ (last x-sorted) 2))]
    (within? [x y] locations)))

(comment
  ; part 1
  (->> locations
       mark-area-by-id
       frequencies
       filter-infinite
       (sort-by second)
       last)

  ; part 2
  (->> locations
       mark-area-within
       (remove false?)
       count))
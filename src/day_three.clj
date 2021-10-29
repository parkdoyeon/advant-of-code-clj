(ns day_three
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

; Create template
; (
; {:id "#1", :left 2, :top 2, :width 5, :height 5},
; {:id "#1", :left 2, :top 2, :width 5, :height 5},
; ...)

(defn parse-order [order-txt]
  (let [[id _ inch size] (str/split order-txt #" ")]
    {:id     id,
     :left   (Integer/parseInt (subs inch 0 (str/index-of inch ","))),
     :top    (Integer/parseInt (subs inch (+ (str/index-of inch ",") 1) (str/index-of inch ":"))),
     :width  (Integer/parseInt (subs size 0 (str/index-of size "x"))),
     :height (Integer/parseInt (subs size (+ (str/index-of size "x") 1)))}))

; Prepare global data
(comment (def orders (->>
                       (str/split-lines (slurp "resources/day-three.txt"))
                       (map parse-order)))
         (def ids (into #{} (map #(get % :id) orders))))


; part 1
(defn mark-fabric [fabric template]
  "
  in
    {:id \"#1\", :left 2, :top 2, :width 5, :height 5}
  out
  {2 {2 1, 3 1, 4 1, 5 1, 6 1},
   3 {2 1, 3 1, 4 1, 5 1, 6 1},
   4 {2 1, 3 1, 4 1, 5 1, 6 1},
   5 {2 1, 3 1, 4 1, 5 1, 6 1},
   6 {2 1, 3 1, 4 1, 5 1, 6 1}}
  "
  (let [left (get template :left)
        width (get template :width)
        top (get template :top)
        height (get template :height)]
    (reduce (fn [acc x]
              (reduce (fn [in-acc y]
                        (update-in in-acc [x y] (fnil inc 0)))
                      acc
                      (range left (+ left width))))
            fabric
            (range top (+ top height)))))

; sample
(comment (mark-fabric {} {:id "#1", :left 2, :top 2, :width 5, :height 5}))

(defn count-dup-orders [line]
  (reduce (fn [acc order] (if (> order 1) (inc acc) acc)) 0 line))

(comment
  (->> (reduce #(mark-fabric %1 %2) {} orders)
       vals
       (map vals)
       (map count-dup-orders)
       (apply +)))


;part 2
(defn mark-fabric-id [fabric template]
  "
  in
    {:id \"#1\", :left 2, :top 2, :width 2, :height 2}
  out
    {2 {2 #{\"#1\"},3 #{\"#1\"}},
     3 {2 #{\"#1\"},3 #{\"#1\"}}}
  "
  (let [id (get template :id)
        left (get template :left)
        width (get template :width)
        top (get template :top)
        height (get template :height)]
    (reduce (fn [acc x]
              (reduce (fn [in-acc y]
                        (update-in in-acc [x y] (fn [v]
                                                  (if (nil? v)
                                                    #{id}
                                                    (conj v id)))))
                      acc
                      (range left (+ left width))))
            fabric
            (range top (+ top height)))))

; sample
(comment (-> (mark-fabric-id {} {:id "#1", :left 2, :top 2, :width 2, :height 2})
             (mark-fabric-id {:id "#2", :left 2, :top 2, :width 2, :height 2})))

(comment
  (->> (reduce #(mark-fabric-id %1 %2) {} orders)
       vals                                                 ; remove keys
       (map vals)                                           ; remove inner keys
       flatten                                              ; remove list between 2-dimension arrays
       (filter #(> (count %) 1))                            ; remove fabric which has single orders
       (into #{})                                           ; remove duplicate set elements
       (reduce #(set/difference %1 %2) ids)))
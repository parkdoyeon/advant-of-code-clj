(ns day_four
  (:require [clojure.string :as str]
            [clj-time.format :as f]
            [clj-time.core :as t]))

(def formatter
  (f/formatter "yyyy-MM-dd"))

(defn parse [str-date]
  (f/parse formatter str-date))

(defn unparse [date]
  (f/unparse formatter date))

(defn parse-log [log]
  ;[out]
  ;(
  ; {:date (parse "1518-02-24"), :hour 23, :minute 57, :action "Guard", :maybe-id "#1913"}
  ; {:date (parse "1518-02-25"), :hour 0, :minute 17, :action "falls", :maybe-id "asleep"}
  ; ...)
  (let [[date time action maybe-id] (str/split log #" ")
        [hour minute] (str/split time #":")]
    {:date     (parse (subs date (+ (str/index-of date "[") 1))),
     :hour     (Integer/parseInt hour),
     :minute   (Integer/parseInt (subs minute 0 (str/index-of minute "]"))),
     :action   action
     :maybe-id maybe-id}))

; Set input data
(comment (def logs (->>
                     (str/split-lines (slurp "resources/day-four.txt"))
                     sort
                     (map parse-log))))

(defn insert-time [key acc minute]
  (-> acc
      (update-in [key :duration] conj minute)
      (update-in [key :time] conj minute)))

(defn calculate-time [key acc minute]
  (-> acc
      (update-in [key :duration] (fn [min-list]
                                   (let [last (peek min-list)]
                                     (conj (pop min-list) (- minute last)))))
      (update-in [key :time] conj minute)))

; Part 1
; 1. Parse daily action as single object
; 2. Find a guard sleep most
; 3. Find the time when the guard is asleep most frequently
(defn parse-action [logs]
  (->> logs
       (reduce (fn [[key acc] log]
                 (let [maybe-id (get log :maybe-id)
                       hour (get log :hour)
                       tmp-date (get log :date)
                       date (unparse (if (= hour 23)
                                       (t/plus tmp-date (t/days 1))
                                       tmp-date))
                       minute (get log :minute)
                       action (get log :action)]
                   (case action
                     "Guard" [[date maybe-id] (update acc [date maybe-id] (fn [_] {:duration [], :time []}))]
                     "falls" [key (insert-time key acc minute)]
                     "wakes" [key (calculate-time key acc minute)])))
               [nil {}])
       peek))

(defn mark-span [span time]
  (if (= [] time)
    span
    (->> time
         (reduce (fn [[start span] min]
                   (if (nil? start)
                     [min span]
                     [nil (reduce #(update %1 %2 inc) span (range start min))]))
                 [nil span])
         peek)))

(comment (mark-span (into [] (take 60 (repeat 0))) [1 5 5 10]))

(defn calculate-time-by-id [parsed-actions]
  "
  [in]
  {
   [1518-09-01 #3463] {:duration [30], :time [11 41]}},
   [1518-09-12 #463] {:duration [22 4], :time [12 34 47 51]}},
   ...
  }
  [out]
   {
     {#56 {:total 30, :frequency '(0 0 0 0 0 ...)}}
     ...
   }
  "
  (reduce (fn [acc [[_ id]
                    {duration :duration time :time}]]
            (-> acc
                (update-in
                  [id :total]
                  #(+ (if (nil? %) 0 %) (apply + duration)))
                (update-in
                  [id :frequency]
                  #(mark-span
                     (if (nil? %) (into [] (take 60 (repeat 0))) %)
                     time))))
          {}
          parsed-actions))

(defn find-top-frequency [calculated-times]
  (reduce (fn [acc [id {frequency :frequency}]]
            (let [max-frequency (apply max frequency)]
              (-> acc
                  (update-in [id :most-frequent] (fn [_] max-frequency))
                  (update-in [id :most-frequent-min] (fn [_] (.indexOf frequency max-frequency))))))
          {}
          calculated-times))

(comment
  (calculate-time-by-id {["1518-09-01" "#2363"] {:duration [10], :time [10 20]},
                         ["1518-09-12" "#463"]  {:duration [5 5], :time [1 6 5 11]}
                         ["1518-09-13" "#2363"] {:duration [10], :time [10 20]}})

  ; part 1
  (let [[id {frequency :frequency}] (->> logs
                                         parse-action
                                         calculate-time-by-id
                                         (sort-by #(:total (val %)) #(compare %2 %1))
                                         first)]
    (* (Integer/parseInt (subs id 1)) (.indexOf frequency (apply max frequency))))

  ; part 2
  (let [[id {most-frequent-min :most-frequent-min}] (->> logs
                                                         parse-action
                                                         calculate-time-by-id
                                                         find-top-frequency
                                                         (sort-by #(:most-frequent val %) #(compare %2 %1))
                                                         first)]
    (* (Integer/parseInt (subs id 1)) most-frequent-min)))





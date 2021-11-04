(ns day-seven
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn parse-relation [line]
  (-> line
      (str/replace "Step " "")
      (str/replace " must be finished before step " "")
      (str/replace " can begin." "")
      (str/split #"")))

(defn init [initializer] (->> (range (int \A) (inc (int \Z)))
                              (map (fn [val]
                                     (let [alphabet (.toString (char val))]
                                       {alphabet initializer})))
                              (into {})))
(comment
  (def info (->> (str/split-lines (slurp "resources/day-seven.txt"))
                 (map parse-relation)
                 (into [])
                 (reduce (fn [{connected :connected
                               in-degree :in-degree}
                              [p c]]
                           {:connected (update connected p (fn [value] (set/union value #{c})))
                            :in-degree (update in-degree c (fn [value] (if (nil? value)
                                                                         1
                                                                         (inc value))))})
                         {:connected (init #{})
                          :in-degree (init 0)})))

  (defn queueing [cur-queue idg ret]
    (let [found (->> idg
                     (filter #(and
                                (= 0 (val %))
                                (= -1 (.indexOf cur-queue (key %)))
                                (= -1 (.indexOf ret (key %))))))]
      (if (empty? found)
        cur-queue
        (apply conj (keys found) cur-queue))))

  ; test
  (queueing '("A") (get info :in-degree) ["G" "B"])

  (defn decrease-in-degree [idg current]
    (let [connections (get-in info [:connected current])]
      (->> idg
           (map (fn [[k v]]
                  (if (connections k)
                    [k (dec v)]
                    [k v])))
           (filter #(> (second %) -1))
           (into {}))))

  ; test
  (decrease-in-degree (get info :in-degree) "A")

  ; part 1
  (def order (reduce (fn [{idg  :in-degree,
                           ret  :return,
                           ques :queues}, _]
                       (if (empty? ques)
                         (reduced (apply str ret))
                         (let [current (first ques)
                               idg (decrease-in-degree idg current)
                               ret (conj ret current)
                               ques (sort (queueing (rest ques) idg ret))]
                           {:in-degree idg, :return ret, :queues ques})))
                     {:in-degree (get info :in-degree),
                      :return    [],
                      :queues    (queueing [] (get info :in-degree) [])}
                     (range)))

  (def loop-order (loop [idg (get info :in-degree)
                         ret []
                         queues (queueing [] idg ret)]
                    (if (empty? queues)
                      (apply str ret)
                      (let [current (first queues)
                            decreased-idg (decrease-in-degree idg current)
                            ret (conj ret current)]
                        (recur decreased-idg
                               ret
                               (sort (queueing (rest queues) decreased-idg ret)))))))

  (defn work-time [alphabet]
    (- (inc (int (first (.toCharArray "A")))) (int \A)))

  (def five-elves-order (reduce (fn [{idg  :in-degree,
                                      ret  :return,
                                      ques :queues}, _]
                                  (if (empty? ques)
                                    (reduced (apply str ret))
                                    (let [current (first ques)
                                          idg (decrease-in-degree idg current)
                                          ret (conj ret current)
                                          ques (sort (queueing (rest ques) idg ret))]
                                      {:in-degree idg, :return ret, :queues ques})))
                                {:in-degree (get info :in-degree),
                                 :return    [],
                                 :queues    (queueing [] (get info :in-degree) [])}
                                (range)))

  (def work-shift (reduce (fn [{}]))))

w
;
;
;(defn remove-redundant [acc [p c]]
;  (map (fn [[k sets]]
;         (if (set/subset? #{p c} sets)
;           [k (apply disj sets p c)]
;           [k sets]))
;       acc))
;
;(remove-redundant relation-map ["Q" "N"])
;
;(reduce remove-redundant relation-map relations)
;
;(reduce (fn [acc [p c]]
;          (map relation-map))
;        relation-map
;        relations)
;(defn union-find [roots c]
;  (loop [c c]
;    (let [p (get roots c)]
;      (if (= p c)
;        c
;        (recur p)))))
;
;(defn union [[roots ranks] [p c]]
;  (let [p (union-find roots p)
;        c (union-find roots c)
;        roots (assoc roots p c)]
;    (cond
;      (= p c) [roots ranks]
;      (> (get ranks p) (get ranks c)) [roots ranks]
;      (= (get ranks p) (get ranks c)) [roots (update ranks p #(inc %))]
;      :else [roots ranks])))
;
;(def init-roots (->> (range 65 91)
;                     (map (fn [val]
;                            (let [alphabet (str-as-unicode val)]
;                              [alphabet alphabet])))
;                     (into {})))
;

;
;(reduce union [init-roots init-ranks] relations)
;

;
;
;(defn update-orders [orders parent rank]
;  (loop [orders orders
;         parent parent
;         rank 0]
;    (if-let [child (first (get relation-map parent))]
;      (if ())
;      (recur (update orders parent (fn [_] [child (inc rank)]))
;             child
;             (inc rank))
;      orders)))
;
;(reduce #(update-orders %1 %2 (fnil inc 0)) {} (keys relation-map)))

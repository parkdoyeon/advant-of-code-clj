(ns day-seven-v2
  (:require [clojure.string :as str]))

(def step-regex #"Step (\w) must be finished before step (\w) can begin.")

(def edges (->> (str/split-lines (slurp "resources/day-seven.txt"))
                (mapv #(->> (re-find step-regex %) rest))))

(defn initialize-map [default] (->> (set (flatten edges))
                                    (into [])
                                    (map #(vector % default))
                                    (into {})))

(defn initialize-dashboard [edges elves-count]
  {:connections (reduce (fn [acc [from to]]
                          (update acc from conj to))
                        (initialize-map #{})
                        edges)
   :remain      (reduce (fn [acc [_, to]]
                          (update acc to inc))
                        (initialize-map 0)
                        edges)
   :todo        []
   :done        []
   :elves       []
   :available   elves-count
   :second      -1})                                        ; job is loaded from 0, time passed from the start


(defn decrease-remain [remain connections finished-jobs]
  (if (empty? finished-jobs)
    remain
    (reduce (fn [acc finished-job]
              (map (fn [[job count]]
                     (if ((connections finished-job) job)
                       [job (dec count)]
                       [job count]))
                   acc))
            remain
            finished-jobs)))

(defn work-time [alphabet]
  (if-let [alphabet alphabet]
    (->> (int (char \A))
         (- (inc (int (first (.toCharArray alphabet)))))
         (+ 60))))

(defn idle? [{:keys [elves available] :as dashboard}]
  (> available (count elves)))

(defn distribute-work [{:keys [todo] :as dashboard}]
  (let [job (peek todo)]
    (-> dashboard
        (update :elves conj {:do job, :end (work-time job)})
        (update :todo pop))))

(defn update-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(zero? (second %)))
                      (mapv first))]
    (-> dashboard
        (update :todo #(-> (apply conj % new-todo) sort reverse vec))
        (update :remain #(remove (fn [[_ count]] (zero? count)) %)))))

(defn done-distribute? [{:keys [todo] :as dashboard}]
  (or (not (idle? dashboard))
      (empty? todo)))

(defn update-done [{:keys [connections elves] :as dashboard}]
  (let [finished-jobs (->> elves
                           (filter #(zero? (:end %)))
                           (mapv #(:do %)))]
    (-> dashboard
        (update :done #(apply conj % finished-jobs))
        (update :elves (fn [elves] (->> elves (remove #(zero? (:end %))) (into []))))
        (update :remain #(decrease-remain % connections finished-jobs)))))

(defn tick-second [{:keys [elves] :as dashboard}]
  (let [minute-passed-elves (reduce-kv
                              #(assoc %1 %2 (update %3 :end dec))
                              []
                              elves)]
    (-> dashboard
        (assoc :elves minute-passed-elves)
        (update :second inc))))

(defn after-second [dashboard]
  "
  Every minutes
  - Check if there's finished job, update :done, decrease ahead job count in :remain
  - Find priory job (0 remains), update :todo, and remove nothing ahead job in :remain
  - Let idle elves work
  - Pass second and lessen each elves :end
  "
  (->> dashboard
       update-done
       update-todo
       (iterate distribute-work)
       (filter done-distribute?)
       first
       tick-second))

(comment
  ; part 1
  (->> (initialize-dashboard edges 1)
       (iterate after-second)
       (filter #(empty? (:elves %)))
       second
       :done
       (apply str))

  ; part 2
  (->> (initialize-dashboard edges 5)
       (iterate after-second)
       (filter #(empty? (:elves %)))
       second
       :second))
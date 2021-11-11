(ns day-seven-v2
  (:require [clojure.string :as str]))

(def step-regex #"Step (\w) must be finished before step (\w) can begin.")

(def edges (->> (str/split-lines (slurp "resources/day-seven.txt"))
                (mapv #(->> (re-find step-regex %) rest))))

(defn initialize-map [default] (->> (set (flatten edges))
                                    (into [])
                                    (map #(vector % default))
                                    (into {})))

(defn prepare-elves [elves-count]
  (->> (range elves-count)
       (map #(vector % {:do nil, :end 0}))
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
   :elves       (prepare-elves elves-count)
   :min         0})

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

(defn find-idle-elves [elves]
  (->> (filter #(>= 0 (:end (val %))) elves)
       (mapv key)))

(defn distribute-work [{:keys [elves todo] :as dashboard}]
  (let [job (peek todo)
        elf (peek (find-idle-elves elves))]
    (-> dashboard
        (assoc-in [:elves elf] {:do job, :end (work-time job)})
        (update :todo pop))))

(defn update-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(zero? (second %)))
                      (mapv first))]
    (-> dashboard
        (update :todo #(-> (apply conj % new-todo) sort reverse vec))
        (update :remain #(remove (fn [[_ count]] (zero? count)) %)))))

(defn done-distribute? [{:keys [elves todo] :as _dashboard}]
  (or (empty? (find-idle-elves elves))
      (empty? todo)))

(defn update-done [{:keys [connections elves] :as dashboard}]
  (let [finished-jobs (->> elves
                           (filter (fn [[_ {do :do end :end}]]
                                     (and (string? do) (zero? end))))
                           (mapv #(:do (val %))))]
    (-> dashboard
        (update :done #(apply conj % finished-jobs))
        (update :remain #(decrease-remain % connections finished-jobs)))))

(defn tick-minute [{:keys [elves] :as dashboard}]
  (let [minute-passed-elves (reduce-kv
                              #(assoc %1 %2 (update %3 :end dec))
                              {}
                              elves)]
    (-> dashboard
        (assoc :elves minute-passed-elves)
        (update :min inc))))

(defn passing-time [dashboard]
  "
  Every minutes
  - Check there's finished job, append to the :done and decrease connected job count in :remain
  - Find priory job (0 remains), update :todo, and remove 0 ahead job from :remain
  - Let idle elves work
  - Pass minute and lessen each elves :end
  "
  (->> dashboard
       update-done
       update-todo
       (iterate distribute-work)
       (filter done-distribute?)
       first
       tick-minute))

(comment
  ; part 1
  (->> (initialize-dashboard edges 1)
       (iterate passing-time)
       (filter #(= (count (:done %)) 26))
       first
       :done
       (apply str))

  ; part 2
  (->> (initialize-dashboard edges 5)
       (iterate passing-time)
       (filter #(= (count (:done %)) 26))
       first))
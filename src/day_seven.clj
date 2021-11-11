(ns day-seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
   :elves       (into [] (take elves-count (repeat [0 nil])))
   :current     0})


(defn do-work [dashboard]
  (let [job (peek (get dashboard :todo))
        connected (get-in dashboard [:connections job])]
    (-> dashboard
        (update :done #(conj % job))
        (update :remain #(map (fn [[job count]]
                                (if (connected job)
                                  [job (dec count)]
                                  [job count]))
                              %))
        (update :todo #(pop %)))))

(defn work-time [alphabet]
  (if-let [alphabet alphabet]
    (->> (int (char \A))
         (- (inc (int (first (.toCharArray alphabet)))))
         (+ 60))))

(defn allocate-work [elves connections new-job current]
  (let [new-job-time (work-time new-job)]
    (->> (reduce (fn [acc elf]
                   (let [[id [end-at cur-job]] elf]
                     (print connections (get connections cur-job) "\n")
                     (cond
                       (nil? new-job) (reduced (assoc elves id [current cur-job]))
                       (zero? end-at) (reduced (assoc elves id [new-job-time new-job]))
                       (or ((get connections cur-job #{}) new-job)
                           (nil? cur-job)) (reduced (assoc elves id [(+ end-at new-job-time) new-job]))
                       :else [acc elf])))
                 (map-indexed vector elves)
                 (map-indexed vector elves))
         sort
         vec)))

(defn do-work-by-time [dashboard]
  (let [job (peek (:todo dashboard))
        current (:current dashboard)
        connections (:connections dashboard)
        connects (get connections job)]
    (if (nil? job)
      (update dashboard :elves allocate-work connections job current)
      (-> dashboard
          (update :done conj job)
          (update :elves allocate-work connections job current)
          (update :remain #(map (fn [[job count]]
                                  (if (connects job)
                                    [job (dec count)]
                                    [job count]))
                                %))
          (update :todo #(if (empty? %) % (pop %)))))))

(defn pass-the-moment [dashboard]
  (let [priority-job (peek (:todo dashboard))
        priority-time (work-time priority-job)
        [fastest-idle-time _] (first (:elves dashboard))]
    (assoc dashboard :current (+ fastest-idle-time priority-time))))

(defn update-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(zero? (second %)))
                      (mapv first))]
    (-> dashboard
        (update :todo #(apply conj % (reverse new-todo)))
        (update :remain #(remove (fn [[_ count]] (zero? count)) %)))))

(defn sort-todo [dashboard]
  (update dashboard :todo #(-> % sort reverse vec)))

(defn work [dashboard]
  (->> dashboard
       update-todo
       sort-todo
       do-work))

(defn work-many [dashboard]
  (let [elves-count (count (:elves dashboard))]
    (->> dashboard
         update-todo
         pass-the-moment
         (iterate do-work-by-time)
         (take (inc elves-count))
         last)))

(comment
  ; test
  (->> (initialize-dashboard edges 5)
       #_work
       #_work
       work
       work)

  ; part 1
  (->> (initialize-dashboard edges 1)
       (iterate work)
       (filter #(empty? (:remain %)))
       first
       :done
       (apply str)))

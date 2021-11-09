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
   :elves       (into [] (take elves-count (repeat 0)))
   :idle-moment 0})


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

(defn allocate-work [elves job idle-moment]
  (print job (work-time job) elves "\n")
  (let [time-taken (work-time job)]
    (-> elves
        (update 0 #(if (nil? time-taken)
                     idle-moment
                     (+ % time-taken)))
        sort
        vec)))

(defn do-work-by-time [dashboard]
  (let [job (peek (get dashboard :todo))
        idle-moment (get dashboard :idle-moment)
        connected (get-in dashboard [:connections job])]
    (if (nil? job)
      (update dashboard :elves allocate-work job idle-moment)
      (-> dashboard
          (update :done conj job)
          (update :elves allocate-work job idle-moment)
          (update :remain #(map (fn [[job count]]
                                  (if (connected job)
                                    [job (dec count)]
                                    [job count]))
                                %))
          (update :todo #(if (empty? %) % (pop %)))))))

(defn update-idle-moment [dashboard]
  (let [priority-job (peek (get dashboard :todo))
        priority-time (work-time priority-job)
        fastest-idle-time (first (get dashboard :elves))]
    (assoc dashboard :idle-moment (+ fastest-idle-time priority-time))))

(defn update-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(zero? (second %)))
                      (mapv first))]
    (-> dashboard
        (update :todo #(-> (apply conj % new-todo) sort reverse vec))
        (update :remain #(remove (fn [[_ count]] (zero? count)) %)))))

(defn update-many-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(<= (second %) 0))
                      (mapv first))]
    (-> dashboard
        (update :todo #(-> (apply conj (vec (reverse (sort new-todo))) %)))
        (update :remain #(remove (fn [[_ count]] (<= count 0)) %)))))

(defn work [dashboard]
  (->> dashboard
       update-todo
       do-work))

(defn work-many [dashboard]
  (let [elves-count (count (:elves dashboard))]
    (->> dashboard
         update-many-todo
         update-idle-moment
         (iterate do-work-by-time)
         (take (inc elves-count))
         last)))

(comment
  ; test
  (->> (initialize-dashboard edges 1)
       #_work
       #_work
       work)

  ; part 1
  (->> (initialize-dashboard edges 1)
       (iterate work)
       (filter #(empty? (:remain %)))
       first
       :done
       (apply str))

  (->> (initialize-dashboard edges 5)
       update-many-todo
       do-work-by-time
       update-many-todo)

  ; test
  (->> (initialize-dashboard edges 5)
       (iterate work-many)
       (take 3)
       #_(filter #(empty? (:remain %)))
       last)

  ; part 2
  (->> (initialize-dashboard edges 5)
       (iterate work-many)
       #_(take 4)
       #_last
       (filter #(empty? (:remain %)))
       first))

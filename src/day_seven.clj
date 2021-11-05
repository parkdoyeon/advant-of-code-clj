(ns day-seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def step-regex #"Step (\w) must be finished before step (\w) can begin.")

(def edges (->> (str/split-lines (slurp "resources/day-seven.txt"))
                (mapv #(take-last 2 (re-find step-regex %)))))

(def connection-map (reduce (fn [acc [from to]]
                              (update acc from #(set/union % #{to})))
                            {}
                            edges))

(def alphabet-map (->> (range (int \A) (inc (int \Z)))
                       (map (fn [n] [(.toString (char n)) 0]))
                       (into {})))

(def dashboard (reduce (fn [acc [_ to]]
                         (update-in acc [:remain to] (fnil inc 0)))
                       {:todo [], :done [], :remain alphabet-map}
                       edges))

(defn do-work [dashboard]
  (let [job (peek (get dashboard :todo))
        connected (get connection-map job)]
    (-> dashboard
        (update :done #(conj % job))
        (update :remain #(map (fn [[job count]]
                                (if (connected job)
                                  [job (dec count)]
                                  [job count]))
                              %))
        (update :todo #(pop %)))))

(defn update-todo [dashboard]
  (let [new-todo (->> (get dashboard :remain)
                      (filter #(zero? (second %)))
                      (mapv first))]
    (-> dashboard
        (update :todo #(-> (apply conj % new-todo) sort reverse vec))
        (update :remain #(remove (fn [[_ count]] (zero? count)) %)))))

(defn work [dashboard]
  (->> dashboard
       update-todo
       do-work))

(comment
  (->> dashboard
       work
       work
       work
       work)

  (->> (iterate work dashboard)
       (take 27)
       last
       :done
       (apply str)))
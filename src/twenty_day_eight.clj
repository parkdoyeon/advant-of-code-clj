(ns twenty-day-eight
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[operator str-arg] (str/split line #" ")
        num-arg (Integer/parseInt str-arg)]
    [(keyword operator) num-arg]))

(def instructions (->>
                    (str/split-lines (slurp "resources/twenty-day-eight.txt"))
                    (map parse-instruction)))

(defn initialize-status [instructions]
  {:histories    {}
   :current      {:idx 0, :sum 0, :prev-sum 0}
   :flag         nil
   :instructions instructions})

(defn operate [status]
  (let [{current      :current
         instructions :instructions} status
        [operator arg] (nth instructions (:idx current))]
    (case operator
      :acc (-> status
               (assoc-in [:current :prev-sum] (:sum current))
               (update-in [:current :idx] inc)
               (update-in [:current :sum] + arg))
      :jmp (-> status
               (update-in [:current :idx] + arg))
      :nop (-> status
               (update-in [:current :idx] inc)))))

(defn label-non-acc-as-cycle [status]
  (let [current-idx (:idx (:current status))
        [operator _] (nth (:instructions status) current-idx)]
    (if (= :acc operator)
      status
      (assoc-in status [:histories current-idx] :cycle))))

(defn fix [instructions fix-idx]
  (let [[operator arg] (nth instructions fix-idx)
        new-operator (if (= :jmp operator) :nop :jmp)]
    (assoc instructions fix-idx [new-operator arg])))

(defn run-instruction [{:keys [current histories instructions] :as status}]
  (let [current-idx (:idx current)
        history (histories current-idx)]
    (if (= current-idx (dec (count instructions)))
      (-> status
          (assoc :flag :success)
          operate)
      (case history
        nil (-> status
                (assoc-in [:histories current-idx] :done)
                operate)
        :done (-> status
                  (assoc :flag :before-loop)
                  label-non-acc-as-cycle
                  operate)
        :cycle (-> status
                   (assoc :flag :in-loop))))))

(defn run-program [exit-when instructions]
  (->> (initialize-status instructions)
       (iterate run-instruction)
       (filter #(exit-when (:flag %)))
       first))

(defn get-maybe-broken-idx [status]
  (->> status
       :histories
       (filter #(= :cycle (second %)))
       (map first)))

(comment
  ; test
  (->> (initialize-status instructions)
       run-instruction
       run-instruction)

  ; part 1
  (->> (run-program #{:before-loop} instructions)
       :current
       :prev-sum)

  ; part 2
  (let [fixed-instructions (->> instructions
                                (run-program #{:in-loop})
                                get-maybe-broken-idx
                                (map #(fix instructions %)))]
    (->> fixed-instructions
         (map #(run-program #{:in-loop :success} %))
         (filter #(= (:flag %) :success))
         first
         :current
         :sum)))



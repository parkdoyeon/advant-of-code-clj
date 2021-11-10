(ns twenty-day-eight
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[operator str-arg] (str/split line #" ")
        num-arg (Integer/parseInt str-arg)]
    [operator num-arg]))

(def instructions (->>
                    (str/split-lines (slurp "resources/twenty-day-eight.txt"))
                    (mapv parse-instruction)))

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
      "acc" (-> status
                (assoc-in [:current :prev-sum] (:sum current))
                (update-in [:current :idx] inc)
                (update-in [:current :sum] + arg))
      "jmp" (-> status
                (update-in [:current :idx] + arg))
      "nop" (-> status
                (update-in [:current :idx] inc)))))

(defn label-non-acc-as-cycle [status]
  (let [{current      :current
         instructions :instructions} status
        current-idx (:idx current)
        [operator _] (nth instructions current-idx)]
    (if (= "acc" operator)
      status
      (assoc-in status [:histories current-idx] :cycle))))

(defn fix [instructions fix-idx]
  (let [[operator arg] (nth instructions fix-idx)
        new-operator (if (= "jmp" operator) "nop" "jmp")]
    (assoc instructions fix-idx [new-operator arg])))

(defn run-instruction [status]
  (let [{current      :current
         histories    :histories
         instructions :instructions} status
        current-idx (:idx current)
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
                   (assoc :flag :in-loop)
                   operate)))))

(defn run-program [exit-when instructions]
  (->> (initialize-status instructions)
       (iterate run-instruction)
       (filter #(exit-when (:flag %)))
       first))

(defn get-maybe-broken-idx [status]
  (->> status
       :histories
       (filterv #(= :cycle (second %)))
       (mapv first)))

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
  (->> instructions
       (run-program #{:in-loop})
       get-maybe-broken-idx
       (map #(fix instructions %))
       (map #(run-program #{:in-loop :success} %))
       (filter #(= (:flag %) :success))
       first
       :current
       :sum))



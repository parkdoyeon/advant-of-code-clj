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
  {:cycles       []
   :histories    {}
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

(defn fill-cycles [status]
  (let [cycles (->> (:histories status)
                    (filterv #(= :cycle (second %)))
                    (mapv first))]
    (-> status
        (assoc :cycles cycles))))

(defn fix-single-point [status]
  (let [{cycles       :cycles
         instructions :instructions} status
        idx (peek cycles)
        [operator arg] (nth instructions idx)
        new-operator (if (= "jmp" operator) "nop" "jmp")]
    (-> status
        (assoc-in [:instructions idx] [new-operator arg])
        (update :cycles pop))))

(defn reset [instructions status]
  (merge status {:current      {:idx 0, :sum 0, :prev-sum 0}
                 :flag         nil
                 :histories    {}
                 :instructions instructions}))

(defn run-program [status]
  (let [{current   :current
         histories :histories} status
        current-idx (:idx current)
        history (histories current-idx)]
    (case history
      nil (-> status
              (assoc-in [:histories current-idx] :done)
              operate)
      :done (-> status
                (assoc :flag :done)
                label-non-acc-as-cycle
                operate)
      :cycle (-> status
                 (assoc :flag :in-loop)
                 operate))))

(defn fix-and-run-program [status]
  (let [rerun-status (->> (reset instructions status)
                          fix-single-point
                          (iterate run-program)
                          (filter #(or (= (get-in % [:current :idx])
                                          (dec (count instructions)))
                                       (= (:flag %) :in-loop)))
                          first)]
    (if (= (:flag rerun-status) :in-loop)
      rerun-status
      (assoc rerun-status :flag :fixed))))

(comment
  ; test
  (->> (initialize-status instructions)
       run-program
       run-program)

  ; part 1
  (->> (initialize-status instructions)
       (iterate run-program)
       (filter #(= (:flag %) :done))
       first
       :cycles)

  ; part 2 test
  (->> (initialize-status instructions)
       (iterate run-program)
       (filter #(= (:flag %) :in-loop))
       first
       fill-cycles
       (iterate fix-and-run-program)
       (take 3)
       last)

  ; part 2
  (->> (initialize-status instructions)
       (iterate run-program)
       (filter #(= (:flag %) :in-loop))                     ; iterate and until second visit
       first
       fill-cycles                                          ; memorize second visit of 'nop' and 'jmp'
       (iterate fix-and-run-program)                        ; try fixing single point and rerun
       (filter #(= (:flag %) :fixed))                       ; iterate cycles until reaching at the end of the register
       :current
       :sum))



(ns twenty-day-four
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

; Parsers

(defn parse-value [field info]
  (case field
    :byr (Integer/parseInt info)
    :iyr (Integer/parseInt info)
    :eyr (Integer/parseInt info)
    :hgt info
    :hcl info
    :ecl info
    :pid info
    :cid info))

(defn parse-field-info [passport line]
  (->> (str/split line #" ")
       (reduce (fn [acc field]
                 (let [acc (if (nil? acc) {} acc)
                       [k v] (str/split field #":")
                       field (keyword k)
                       info (parse-value field v)]
                   (assoc acc field info)))
               passport)))

(defn line-into-passport [[acc passport-id] line]
  "
  in
  [{} 0] eyr:2040 pid:785862801
  out
  [{0 [eyr:2040 pid:785862801]} 0]
  "
  (if (= line "")
    [acc (inc passport-id)]
    [(update acc passport-id #(parse-field-info % line)) passport-id]))

(comment
  ; Parser test
  (line-into-object [{} 0] "eyr:2040 pid:785862801")

  (parse-field-info {} "iyr:2017 ecl:amb")

  ; test
  (assert
    (= (mapv height? ["123cm" "40in" "193cm" "200kg" "60in"])
       [nil
        nil
        {:height 193 :unit "cm"}
        nil
        {:height 60 :unit "in"}]))

  ; Spec functions

  (defn height? [height-str]
    (let [len (count height-str)]
      (if-let [unit (re-matches #"cm|in" (subs height-str (- len 2) len))]
        (if-let [number (Integer/parseInt (subs height-str 0 (- len 2)))]
          (if (= unit "cm")
            (when (and (>= number 150) (<= number 193)) {:height number :unit unit})
            (when (and (>= number 59) (<= number 76)) {:height number :unit unit}))))))

  (s/def :part-one/byr int?)
  (s/def :part-one/iyr int?)
  (s/def :part-one/eyr int?)
  (s/def :part-one/hgt string?)
  (s/def :part-one/hcl string?)
  (s/def :part-one/ecl string?)
  (s/def :part-one/pid string?)
  (s/def :part-one/cid string?)

  (s/def :part-one/passport (s/keys :req-un [:part-one/byr
                                             :part-one/iyr
                                             :part-one/eyr
                                             :part-one/hgt
                                             :part-one/hcl
                                             :part-one/ecl
                                             :part-one/pid]
                                    :opt-un [:part-one/cid]))

  (s/def :part-two/ecl-types #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
  (s/def :part-two/byr (s/int-in 1920 2003))
  (s/def :part-two/iyr (s/int-in 2010 2021))
  (s/def :part-two/eyr (s/int-in 2020 2031))
  (s/def :part-two/hgt #(height? %))
  (s/def :part-two/hcl #(re-matches #"#[0-9a-fA-F]{6}" %))
  (s/def :part-two/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
  (s/def :part-two/pid #(re-matches #"[0-9]{9}" %))
  (s/def :part-two/cid string?)

  (s/def :part-two/passport (s/keys :req-un [:part-two/byr
                                             :part-two/iyr
                                             :part-two/eyr
                                             :part-two/hgt
                                             :part-two/hcl
                                             :part-two/ecl
                                             :part-two/pid]
                                    :opt-un [:part-two/cid]))

  (def valid-pt1-passport? (partial s/valid? :part-one/passport))
  (def valid-pt2-passport? (partial s/valid? :part-two/passport))

  (comment
    ; data
    (def passports (let [[passports count] (->> (slurp "resources/twenty-day-four.txt")
                                                (str/split-lines)
                                                (reduce line-into-passport [{} 0]))]
                     (vals passports)))

    ; Part 1
    (->> (map valid-pt1-passport? passports)
         (filter true?)
         count)

    ; Part 2
    (->> (map valid-pt2-passport? passports)
         (filter true?)
         count)))


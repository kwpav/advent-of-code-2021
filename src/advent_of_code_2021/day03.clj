(ns advent-of-code-2021.day03
  (:require [clojure.edn :as edn]
            [advent-of-code-2021.util :as util]))

(defn flip-bits
  [bits]
  (apply str (map #(if (= \0 %) \1 \0) bits)))

(defn most-common
  [bits]
  (let [{zeroes \0 ones \1} (frequencies bits)]
    (if (> zeroes ones) 0 1)))

(defn get-rates
  [input]
  (let [diagnostics (for [n (range (count (first input)))] (apply str (map #(nth % n) input)))
        gamma (apply str (map most-common diagnostics))
        epsilon (flip-bits diagnostics)]
    {:gamma (Long/parseLong gamma 2)
     :epsilon (Long/parseLong epsilon 2)}))

(defn part1
  [input]
  (let [rates (get-rates input)]
    (* (:epsilon rates) (:gamma rates))))

(comment
  (def sample (util/read-input "day03sample.txt"))
  (= 198 (part1 sample))
  (= 1131506 (part1 (util/read-input "day03.txt")))

  (apply str (map #(first %) sample))
  (for [n (range (count (first sample)))] (apply str (map #(nth % n) sample)))

  (partition 1 1 sample)
  (take (count (first sample)) (iterate first sample))
  ,)

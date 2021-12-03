(ns advent-of-code-2021.day03
  (:require [clojure.edn :as edn]
            [advent-of-code-2021.util :as util]))

(defn count-bits
  [bits bit]
  (count (filter #(= bit %) bits)))

(defn flip-bits
  [bits]
  (apply str (map #(if (= \0 %) \1 \0) bits)))

(defn most-common
  [bits]
  (if (> (count-bits bits \0) (count-bits bits \1))
    0 1))

(defn bits->int
  [bits]
  (edn/read-string (str "2r" bits)))

(defn part1 
  [input]
  (let [gammas (for [n (range (count (first input)))] (apply str (map #(nth % n) input)))
        gamma (apply str (map most-common gammas))]
    {:gamma (bits->int gamma)
     :epsilon (bits->int (flip-bits gamma))}))

(comment
  (def sample (util/read-input "day03sample.txt"))
  (part1 sample)

  (apply str (map #(first %) sample))
  (for [n (range (count (first sample)))] (apply str (map #(nth % n) sample)))

  (partition 1 1 sample)
  (take (count (first sample)) (iterate first sample))
  ,)

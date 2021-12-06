(ns advent-of-code-2021.day03
  (:require [advent-of-code-2021.util :as util]))

(defn flip-bit
  [bit]
  (if (= \0 bit) \1 \0))

(defn flip-bits
  [bits]
  (apply str (map #(flip-bit %) bits)))

(defn most-common
  [bits]
  (let [{zeroes \0 ones \1} (frequencies bits)]
    (cond
      (nil? zeroes) \1
      (nil? ones) \0
      (> zeroes ones) \0
      :else \1)))

(defn parse-diagnostics
  [diagnostics]
  (for [n (range (count (first diagnostics)))]
    (apply str (map #(nth % n) diagnostics))))

(defn get-rates
  [diagnostics]
  (let [rate-diagnostics (parse-diagnostics diagnostics)
        gamma (->> rate-diagnostics
                   (map most-common)
                   (apply str))
        epsilon (flip-bits gamma)]
    {:gamma (Long/parseLong gamma 2)
     :epsilon (Long/parseLong epsilon 2)}))

;; part1
(defn power-consumption
  [input]
  (let [rates (get-rates input)]
    (* (:epsilon rates) (:gamma rates))))

;; part2
(defn oxygen-bit-criteria 
  [diagnostics n]
  (let [parsed (parse-diagnostics diagnostics)]
    (vec (filter #(= (most-common (nth parsed n)) (nth % n)) diagnostics))))

(defn co2-bit-criteria
  [diagnostics n]
  (let [parsed (parse-diagnostics diagnostics)]
    (vec (filter #(= (flip-bit (most-common (nth parsed n))) (nth % n)) diagnostics))))

(defn get-system-rating
  [diagnostics criteria]
  (loop [diagnostics diagnostics n 0]
    (if (= 1 (count diagnostics))
      (first diagnostics)
      (recur (criteria diagnostics n) (inc n)))))

(defn life-support-rating
  [input]
  (* (Long/parseLong (get-system-rating input oxygen-bit-criteria) 2)
     (Long/parseLong (get-system-rating input co2-bit-criteria) 2)))

(comment
  (def sample (util/read-input "day03sample.txt"))

  ;; part1
  (= 198 (power-consumption sample))
  (= 1131506 (power-consumption (util/read-input "day03.txt")))

  ;; part2
  (= 230 (life-support-rating sample))
  (= 7863147 (life-support-rating (util/read-input "day03.txt")))
  ,)

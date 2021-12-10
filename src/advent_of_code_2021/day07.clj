(ns day07
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]))

(defn init-crabs
  [input]
  (mapv #(Long/parseLong %) (str/split (first input) #",")))

(defn calc-crab-fuel
  [burn-rate crabs]
  (->> crabs
       (map burn-rate)
       (reduce +)))

(defn naive-burn-rate
  [x y]
  (Math/abs (- x y)))

(defn calc-min-crab-fuel
  [crabs]
  (->> crabs
       (map (fn [crab] (calc-crab-fuel #(naive-burn-rate % crab) crabs)))
       (apply min)))

(defn real-burn-rate
  [x y]
  (->> (iterate inc 1)
       (take (Math/abs (- x y)))
       (reduce +)))

(defn calc-crab-range
  [crabs]
  (take (- (apply max crabs) (apply min crabs)) (iterate inc 1)))

(defn calc-real-min-crab-fuel
  [crabs]
  (->> crabs
       calc-crab-range
       (map (fn [crab] (calc-crab-fuel #(real-burn-rate % crab) crabs)))
       (apply min)))

(comment
  (def sample (util/read-input "day07sample.txt"))

  (= 37 (calc-min-crab-fuel (init-crabs sample)))
  (= 345035 (calc-min-crab-fuel (init-crabs (util/read-input "day07.txt"))))

  (= 168 (calc-real-min-crab-fuel (init-crabs sample)))
  (= 97038163 (calc-real-min-crab-fuel (init-crabs (util/read-input "day07.txt"))))
  ,)

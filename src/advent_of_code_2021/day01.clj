(ns advent-of-code-2021.day01
  (:require [clojure.string :as s]))

(defn parse-input [input-location]
  (map #(Integer. %) (s/split-lines (slurp input-location))))

(defn part1
  [input]
  (count (filter #(> (second %) (first %)) (partition 2 1 input))))

(part1 (parse-input "src/advent_of_code_2021/input/day01.txt"))

(defn part2
  [input]
  (part1 (map #(reduce + %) (partition 3 1 input))))

(part2 (parse-input "src/advent_of_code_2021/input/day01.txt"))

(comment
  (def input (parse-input "src/advent_of_code_2021/input/day01test.txt"))
  (count (filter #(> (second %) (first %)) (partition 2 1 input)))
  (part1 (map (fn [n] (reduce + n))(partition 3 1 input)))
  ,)

(ns advent-of-code-2021.day02
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse-input
  [input-location]
  (->> input-location
       slurp
       str/split-lines
       (map #(format "[%s]" %))
       (map #(edn/read-string %))))

(defn calculate-direction
  [input direction]
  (->> input
       (filter #(= (first %) direction))
       (map #(second %))
       (reduce +)))

(defn calculate-position
  [input]
  {:depth (- (calculate-direction input 'down) (calculate-direction input 'up))
   :horizontal (+ (calculate-direction input 'forward))})

(defn part1
  [input]
  (let [pos (calculate-position input)]
    (* (:horizontal pos)
       (:depth pos))))

(defn calculate-position-with-aim
  [input]
  (loop [horizontal 0
         depth 0
         aim 0
         commands input]
    (if (empty? commands)
      {:horizontal horizontal :depth depth} 
      (let [command (first commands)]
        (cond (= (first command) 'down)
              (recur horizontal depth (+ aim (second command)) (rest commands))
              (= (first command) 'up)
              (recur horizontal depth (- aim (second command)) (rest commands))
              (= (first command) 'forward)
              (recur (+ horizontal (second command))
                     (+ depth (* aim (second command)))
                     aim
                     (rest commands)))))))

(defn part2
  [input]
  (let [pos (calculate-position-with-aim input)]
    (* (:horizontal pos)
       (:depth pos))))

(comment
  (def sample (parse-input "src/advent_of_code_2021/input/day02test.txt"))

  (= 150 (part1 sample))
  (= 1561344
     (part1 (parse-input "src/advent_of_code_2021/input/day02.txt")))

  (= 900 (part2 sample))
  (= 1848454425 (part2 (parse-input "src/advent_of_code_2021/input/day02.txt")))
  ,)

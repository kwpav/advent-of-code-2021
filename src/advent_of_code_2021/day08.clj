(ns day08
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]))

(defn parse-input
  [input]
  (->> (util/read-input input)
       (map #(str/split % #" \| "))
       (map (fn [[signal output]] {:signal-pattern (str/split signal #" ") :output-value (str/split output #" ")}))))

(defn count-easy-digits
  [input]
  (->> input
       (map :output-value)
       (map (fn [output-vals] (map count output-vals)))
       flatten
       (filter (fn [counts] (some #(= % counts) [2 4 3 7])))
       count))


(comment
  (def sample "day08sample.txt")

  ;; part 1
  (= 26 (count-easy-digits (parse-input sample)))
  (= 488 (count-easy-digits (parse-input "day08.txt")))
  ,)

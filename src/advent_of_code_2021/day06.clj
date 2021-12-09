(ns day06
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]))

(defn init-lanternfish
  [input]
  (mapv #(Long/parseLong %) (str/split (first input) #",")))

(defn spawn
  [lanternfish]
  (let [spawn-count (get (frequencies lanternfish) -1 0)]
    (if (pos? spawn-count)
      (into (mapv #(if (neg? %) 6 %) lanternfish)
            (repeat spawn-count 8))
      lanternfish)))

(defn cycle-lanternfish
  [lanternfish]
  (->> lanternfish
       (mapv dec)
       spawn))

(defn fast-spawn
  [lanternfish-freqs]
  (let [spawn-count (get lanternfish-freqs -1 0)]
    (if (pos? spawn-count)
      (-> lanternfish-freqs
          (update -1 (fnil - 0) spawn-count)
          (update 6 (fnil + 0) spawn-count)
          (update 8 (fnil + 0) spawn-count))
      lanternfish-freqs)))

(defn fast-cycle
  [lanternfish-freqs]
  (->> lanternfish-freqs
       (#(zipmap (map dec (keys %)) (vals %)))
       fast-spawn))

(defn count-fish-fast
  [input days]
  (->> input
       frequencies
       (iterate fast-cycle)
       (take days)
       last
       vals
       (reduce +)))

(comment
  (def sample (util/read-input "day06sample.txt"))

  ;; ANSWERS
  ;; part1
  (= 5934 (count-fish-fast (init-lanternfish sample ) 81))
  (= 360268 (count-fish-fast (init-lanternfish (util/read-input "day06.txt") ) 81))

  ;; part2
  (= 26984457539 (count-fish-fast (init-lanternfish sample) 257))
  (= 1632146183902 (count-fish-fast (init-lanternfish (util/read-input "day06.txt")) 257))

  ;; EXPERIMENTS
  (init-lanternfish sample)
  (cycle-lanternfish (init-lanternfish sample))

  ;; these work for part1, but not part2!
  (count (last (take 81 (iterate cycle-lanternfish (init-lanternfish sample)))))
  (count (last (take 81 (iterate cycle-lanternfish (init-lanternfish (util/read-input "day06.txt"))))))

  ;; These crash due to memory issues!
  (count (last (take 257 (iterate cycle-lanternfish (init-lanternfish sample)))))
  (count (last (take 257 (iterate cycle-lanternfish (init-lanternfish (util/read-input "day06.txt"))))))

  (take 81 (iterate fast-cycle (frequencies (init-lanternfish sample))))
  (reduce + (vals (last (take 81 (iterate fast-cycle (frequencies (init-lanternfish sample)))))))
  (reduce + (vals (last (take 257 (iterate fast-cycle (frequencies (init-lanternfish sample)))))))
  (reduce + (vals (last (take 257 (iterate fast-cycle (frequencies (init-lanternfish (util/read-input "day06.txt"))))))))
  ,)

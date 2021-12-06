(ns day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2021.util :as util]))

(defn init-pieces
  [row]
  (->> (str/split row #" ")
       (filter seq)
       (mapv #(Long/parseLong %))))

(defn init-row
  [row]
  (->> row
       (mapv init-pieces)))

(defn init-boards
  [raw-boards]
  (->> raw-boards
       (filter seq)
       (partition 5)
       (mapv init-row)))

(defn init-draws
  [raw-draws]
  (->> (str/split raw-draws #",")
       (mapv #(Long/parseLong %))))

(defn init-state
  [input]
  {:draws (init-draws (first input))
   :drawn []
   :boards (init-boards (rest input))
   :winners []})

(defn get-cols
  [board]
  (for [n (range 5)]
    (mapv #(nth % n) board)))

(defn matches
  [drawn pieces]
  (mapv #(set/intersection (set %) (set drawn)) pieces))

(defn winning-pieces
  [drawn board]
  (->> board
       (matches drawn)
       (filter #(= 5 (count %)))))

(defn winners
  [drawn board]
  (let [rows board
        cols (get-cols board)]
    (into (winning-pieces drawn rows) 
          (winning-pieces drawn cols))))

(defn take-turn
  [state]
  (let [draw (first (:draws state))]
    {:draws (rest (:draws state))
     :drawn (conj (:drawn state) draw)
     :boards (:boards state)
     :winners (map #(winners (:drawn state) %) (:boards state))}))

(defn play
  [state end-condition]
  (->> state
       (iterate take-turn)
       (take-while end-condition)
       last
       take-turn))

(defn win
  [input]
  (let [initial-state (init-state input)
        win-state (play initial-state #(not (some seq (:winners %))))]
    win-state))

(defn winning-board
  [state]
  (let [idx (.indexOf (map #(not (empty? %)) (:winners state)) true)]
    (nth (:boards state) idx)))

(defn score
  [board-fn state]
  (let [drawn (drop-last (:drawn state))]
    (->> state
         board-fn
         (map seq)
         flatten
         (filter (fn [n] (not (some #(= n %) drawn))))
         (reduce +)
         (* (last drawn)))))

(defn last-win-score
  [input]
  (let [initial-state (init-state input)
        last-win-state (play initial-state #(> (dec (count (:boards initial-state))) (count (filter seq (:winners %)))))
        nth-board (.indexOf (:winners last-win-state) '())
        final-win-state (play last-win-state #(> (count (:boards initial-state)) (count (filter seq (:winners %)))))]
    (score #(nth (:boards %) nth-board) final-win-state)))

(comment
  (def sample (util/read-input "day04sample.txt"))
  (def initial-state (init-state sample))

  (take 14 (iterate take-turn initial-state))

  ;; part1
  (= 4512 (score winning-board (win sample)))
  (= 87456 (score winning-board (win (util/read-input "day04.txt"))))

  ;; part2
  (= 1924 (last-win-score sample))
  (= 15561 (last-win-score (util/read-input "day04.txt")))
  ;; 41310 is too high
  ,)

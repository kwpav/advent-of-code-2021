(ns advent-of-code-2021.day05
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2021.util :as util]))

(defn make-point
  [points]
  (let [[x y] (mapv #(Long/parseLong %) points)]
    (zipmap [:x :y] [x y])))

(defn nearby-lines
  [input]
  (->> input
       (mapv #(str/split % #" -> "))
       (mapv (fn [v] (mapv #(str/split % #",") v)))
       (mapv (fn [v] (mapv make-point v)))))

(defn straight-line?
  [line]
  (or (= (:x (first line))
         (:x (last line)))
      (= (:y (first line))
         (:y (last line)))))

(defn next-n
  [current diff]
  (cond (= 0 diff) current
        (pos? diff) (dec current)
        (neg? diff) (inc current)))

(defn gen-next-point
  [point x-diff y-diff]
  {:x (next-n (:x point) x-diff)
   :y (next-n (:y point) y-diff)})

(defn all-points
  [line]
  (loop [line line
         x-diff (- (:x (first line)) (:x (last line)))
         y-diff (- (:y (first line)) (:y (last line)))
         last-point (first line)]
    (if (and (= 0 x-diff) (= 0 y-diff))
      line
      (let [next-point (gen-next-point last-point x-diff y-diff)]
        (recur (conj line next-point)
               (next-n x-diff x-diff)
               (next-n y-diff y-diff)
               next-point)))))

(defn remove-line
  [line lines]
  (filter #(not= line %) lines))

(defn find-intersections
  [line lines]
  (loop [intersections #{} lines lines]
    (if (empty? lines)
      intersections
      (recur (into intersections (set/intersection line (first lines)))
             (rest lines)))))

(defn all-intersections
  [lines]
  (map #(find-intersections % (remove-line % lines)) lines))

(defn reduce-lines
  [lines]
  (loop [points #{}
         lines lines]
    (if (empty? lines)
      points
      (recur (into points (first lines))
             (rest lines)))))

(defn count-overlaps
  [input]
  (->> input
       nearby-lines
       (filter straight-line?)
       (map all-points)
       (map set)
       all-intersections
       reduce-lines
       count))

(defn count-all-overlaps
  [input]
  (->> input
       nearby-lines
       (map all-points)
       (map set)
       all-intersections
       reduce-lines
       count))

(comment
  (def sample (util/read-input "day05sample.txt"))

  ;; part1
  (= 5 (count-overlaps sample))
  (= 7269 (count-overlaps (util/read-input "day05.txt")))

  (= 12 (count-all-overlaps sample))
  (= 21140 (count-all-overlaps (util/read-input "day05.txt")))
  ,)

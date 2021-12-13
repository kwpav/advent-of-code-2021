(ns advent-of-code-2021.day08
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn digit-set
  [digit]
  (set (str/split digit #"")))

(defn parse-digits
  [vs]
  (->> (str/split vs #" ")
       (map digit-set)
       ))

(defn parse-input
  [input]
  (->> (util/read-input input)
       (map #(str/split % #" \| "))
       (map (fn [[signal output]] {:signal-pattern (parse-digits signal) :output-value (parse-digits output)}))))

(defn find-easy-digit
  [val]
  (->> val
       (filter (fn [digit] (some #(= (count digit) %) [2 4 3 7])))))

(defn count-easy-digits
  [input]
  (->> input
       (map :output-value)
       (map find-easy-digit)
       (map count)
       (reduce +)))

(defn get-top
  [seven one]
  (set/difference seven one))

(defn get-bottom-left
  [one six-segments]
  (let [[six-or-nine nine-or-six] six-segments
        diff1 (set/difference six-or-nine nine-or-six)
        diff2 (set/difference nine-or-six six-or-nine)]
    (if (some #(= % (first diff1)) one)
      diff2
      diff1)))

(defn get-bottom-right
  [one top-right]
  (set/difference one top-right))

(defn get-top-right
  [one six-segments]
  (let [[six-or-nine nine-or-six] six-segments
        diff1 (set/difference six-or-nine nine-or-six)
        diff2 (set/difference nine-or-six six-or-nine)]
    (if (some #(not= % (first diff1)) one)
      diff1
      diff2)))

(defn get-bottom
  [top bottom-left four five-segments]
  (set/difference (reduce into five-segments) (reduce into [top bottom-left four])))

(defn get-middle
  [top top-right bottom-left bottom five-segments]
  (let [known-segments (reduce into [top top-right bottom-left bottom])]
    (set/difference (first (filter #(set/subset? known-segments %) five-segments))
                    known-segments)))

(defn get-top-left
  [middle one four]
  (set/difference four (set/union one middle)))

(defn get-nine
  [top-right six-segments]
  (filter (fn [s] (some #(= top-right %) s)) six-segments))

(defn find-segments
[segment-length vs]
(filter #(= (count %) segment-length) vs))

(defn get-all-segments
  "take parsed input and make digits"
  [input]
  (let [all-digits (into (:signal-pattern input) (:output-value input))
        [one] (find-segments 2 all-digits)
        [four] (find-segments 4 all-digits)
        [seven] (find-segments 3 all-digits)
        [eight] (find-segments 7 all-digits)
        six-segments (find-segments 6 all-digits)
        five-segments (find-segments 5 all-digits)
        top (get-top seven one)
        bottom-left (get-bottom-left one six-segments)
        top-right (get-top-right one six-segments)
        bottom-right (get-bottom-right one top-right)
        bottom (get-bottom top bottom-left four five-segments)
        middle (get-middle top top-right bottom-left bottom five-segments)
        top-left (get-top-left middle one four)]
    (zipmap (mapv first [top top-left top-right middle bottom bottom-left bottom-right])
            [:top :top-left :top-right :middle :bottom :bottom-left :bottom-right])))

(comment
  (def sample "day08sample.txt")

  ;; part 1
  (= 26 (count-easy-digits (parse-input sample)))
  (= 488 (count-easy-digits (parse-input "day08.txt")))

  ;; ultimate goal
  {\d :top
   \e :top-left
   \a :top-right
   \f :middle
   \g :bottom-left 
   \b :bottom-right
   \c :bottom}

  {[:top :top-left :top-right :bottom-left :bottom-right :bottom] 0
   [:top-right :bottom-right] 1
   [:top :top-right :middle :bottom-left :bottom] 2
   [:top :top-right :middle :bottom-right :bottom] 3
   [:top-left :top-right :middle :bottom-right] 4
   [:top :top-left :middle :bottom-right :bottom] 5
   [:top :top-left :middle :bottom-left :bottom-right :bottom] 6
   [:top :top-right :bottom-left] 7
   [:top :top-left :top-right :middle :bottom-left :bottom-right :bottom] 8
   [:top :top-left :top-right :middle :bottom-right :bottom] 9}

  {#{"d" "a" "b"} 1
   #{"g" "c" "d" "f" "a"} 2
   ;; ... 8
   }

  (get-top #{"d" "a" "b"} #{"a" "b"})

  (get-bottom-left #{"a" "b"} [#{"c" "d" "f" "g" "e" "b"} #{"c" "e" "f" "a" "b" "d"}])
  (get-top-right #{"a" "b"} [#{"c" "d" "f" "g" "e" "b"} #{"c" "e" "f" "a" "b" "d"}])

  (def simple-sample
    (->> ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
         (map #(str/split % #" \| "))
         (map (fn [[signal output]] {:signal-pattern (parse-digits signal) :output-value (parse-digits output)}))))

  (get-all-segments (first simple-sample))
  ,)

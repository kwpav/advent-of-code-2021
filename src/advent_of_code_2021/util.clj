(ns advent-of-code-2021.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [input]
  (-> input
      io/resource
      slurp
      str/split-lines))

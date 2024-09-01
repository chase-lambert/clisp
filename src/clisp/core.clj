(ns clisp.core
  (:require 
   [clojure.string :as string])
  ;;  [clojure.edn :as edn])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn paren-as-num [c]
  (case c
    "("  1
    ")" -1
    0))

(defn num-balanced [s-of-parens]
  (->> s-of-parens
       (map paren-as-num)
       (reduce +)
       abs))

(def s "(first (list 1 (+ 2 3) 9))")

(comment 
  (num-balanced s) 
  ,)

(defn tokenize [s]
  (remove empty?
    (-> s
        (string/replace "(" " ( ")
        (string/replace ")" " ) ")
        (string/split #" "))))

(defn parse-tokens [])

(defn parse-atom [])

(defn parse-list [])

(defn parse-expression [])

(defn eval [])


(defn -main [& args]
  (println args))

(ns clisp.core
  (:require 
   [clojure.string :as str])
  ;;  [clojure.edn :as edn])
  (:gen-class))

(set! *warn-on-reflection* true)

(def s "(first (list 1 (+ 2 3) 9))") 

(defn tokenize [s]
  (-> s
      (str/replace "(" " ( ")
      (str/replace ")" " ) ")
      (str/trim)
      (str/split #"\s+")))

(defn atomize [token]
  (or (parse-long token) 
      (parse-double token) 
      (str token)))

(defn parse-tokens [tokens]
  {:pre (seq (first tokens))}
  (let [[t & remaining] tokens]
    (case t
      "(" (loop [exprs []
                 rem-tokens remaining]
            (let [[expr new-rem-tokens] (parse-tokens rem-tokens)]
              (if (= ")" (first new-rem-tokens))
                [(conj exprs expr) (rest new-rem-tokens)]
                (recur (conj exprs expr) new-rem-tokens))))
      
      ")" [nil remaining]
    
      [(atomize t) remaining])))

(defn parse [s]
  (->> s            ;; "(first (list 1 (+ 2 3) 9))"
       tokenize     ;; ["(" "first" "(" "list" "1" "(" "+" "2" "3" ")" "9" ")" ")"]
       parse-tokens ;; [["first" ["list" 1 ["+" 2 3] 9]] ()]
       first))

(defn -main [s]
  (println (parse (tokenize s))))

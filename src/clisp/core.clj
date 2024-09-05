(ns clisp.core
  (:require 
   [clojure.string :as str])
  (:gen-class))

(set! *warn-on-reflection* true)

(comment
  (def s "(first (list 1 (+ 2 3) 9))") 
  ,)

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
      ;; (symbol token)))

(defn parse-tokens [tokens]
  {:pre (seq (first tokens))}
  (let [[token & remaining] tokens]
    (case token
      "(" (loop [parsed-exprs []
                 rem-tokens remaining]
            (let [[expr new-rem-tokens] (parse-tokens rem-tokens)]
              (if (= ")" (first new-rem-tokens))
                [(conj parsed-exprs expr) (rest new-rem-tokens)]
                (recur (conj parsed-exprs expr) new-rem-tokens))))
      
      ")" (throw (Exception. "unexpected ')'; no malformed code please"))
    
      [(atomize token) remaining])))

(defn parse [s]
  (->> s            ;; "(first (list 1 (+ 2 3) 9))"
       tokenize     ;; ["(" "first" "(" "list" "1" "(" "+" "2" "3" ")" "9" ")" ")"]
       parse-tokens ;; [["first" ["list" 1 ["+" 2 3] 9]] ()]
       first))      ;; ["first" ["list" 1 ["+" 2 3] 9]]

(comment
  (def s "(first (list 1 (+ 2 3) 9))") 
  (parse s)
  (parse-tokens "")
  ,)

(defn -main [s]
  (println (parse s))) 


(ns clisp.core
  (:require
   [clojure.string :as str])
  (:gen-class))

(set! *warn-on-reflection* true)

(comment
  (def s "(first (list 1 (+ 2 3) 9))")
  (tokenize s) ;; ["(" "first" "(" "list" "1" "(" "+" "2" "3" ")" "9" ")" ")"]
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
      (if (= token "false")
        false
        (str token))))
      ;; (symbol token)))

(declare parse-list)

(defn parse-tokens [tokens]
  {:pre (seq (first tokens))}
  (let [[token & remaining] tokens]
    (case token
      "(" (parse-list remaining)

      ")" (throw (Exception. "unexpected ')'; no malformed code please"))

      [(atomize token) remaining])))

(defn parse-list [tokens]
  (loop [parsed-exprs []
         rem-tokens tokens]
    (case (first rem-tokens)

      ")" [parsed-exprs (next rem-tokens)]

      (let [[expr rem-tokens] (parse-tokens rem-tokens)]
        (recur (conj parsed-exprs expr) rem-tokens)))))

(defn parse [s]
  (->> s            ;; "(first (list 1 (+ 2 3) 9))"
       tokenize     ;; ["(" "first" "(" "list" "1" "(" "+" "2" "3" ")" "9" ")" ")"]
       parse-tokens ;; [["first" ["list" 1 ["+" 2 3] 9]] ()]
       first))      ;; ["first" ["list" 1 ["+" 2 3] 9]]


(defn ceval 
  ([expr] (ceval expr {}))
  ([expr bindings] (if (vector? expr)
                     (let [[first-expr & args] expr]
                       (condp = first-expr
                         "if" (let [[condition then else] args]
                                (if (ceval condition bindings)
                                  (ceval then bindings)
                                  (ceval else bindings)))

                         "+" (apply + (map #(ceval % bindings) args))
        
                         "fn" (let [[params body] args]
                                {:params params
                                 :body   body})
        
                         (let [{:keys [params body]} (ceval first-expr bindings)
                               new-bindings (zipmap params (map #(ceval % bindings) args))] 
                           (ceval body new-bindings))))

                     (if (string? expr)
                       (get bindings expr)
                       expr))))

(comment
  (def s "(first (list 1 (+ 2 3) 9))")
  (parse s)
  (parse-tokens "")
  (parse "()")
  ,)

(defn -main [s]
  (println (parse s)))

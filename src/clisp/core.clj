(ns clisp.core
  (:require
   [clojure.string :as str])
  (:gen-class))

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
  (->> s 
       tokenize
       parse-tokens
       first))

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
                                {:params   params
                                 :body     body
                                 :bindings bindings})

                         (let [{:keys [params body] :as f} (ceval first-expr bindings)
                               new-bindings          (-> (:bindings f)
                                                       (into bindings)
                                                       (into (zipmap params (map #(ceval % bindings) args))))]
                           (ceval body new-bindings))))

                     (if (string? expr)
                       (get bindings expr)
                       expr))))

(defn repl []
  (println "Welcome to Clisp REPL. Type 'exit' to quit.")
  (flush)
  (loop []
    (print "clisp> ")
    (flush)
    (let [input (read-line)]
      (if (or (nil? input) ;; allows for Ctrl-D exit
              (= "exit" input)) 
        (do 
          (println "\nGoodbye!")
          (flush))

        (do
          (try
            (let [parsed (parse input)
                  result (ceval parsed)]
              (println result)
              (flush))
            (catch Exception e
              (println "Error:" (.getMessage e))
              (flush)))
          (recur))))))

(defn -main []
  (repl))

(comment
  (def s "(first (list 1 (+ 2 3) 9))")
  (parse s)
  (parse-tokens "")
  (parse "()")
  ,)


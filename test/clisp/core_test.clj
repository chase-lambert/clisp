(ns clisp.core-test
  (:require
   [pjstadig.humane-test-output :as humane]
   [clojure.test :refer :all]
   [clisp.core :refer :all]))

(humane/activate!)

(deftest good-input-parser-test
  (testing "testing well formed source code input"
    (let [s "(first (list 1 (+ 2 3) 9))"]
      (is (= (parse s)
            ["first" ["list" 1 ["+" 2 3] 9]])))))

(deftest bad-input-parser-test
  (testing "testing malformed source code input"
    (let [s ")"]
      (is (thrown-with-msg?
            Exception
            #"unexpected '\)'; no malformed code please"
            (parse s))))))

(deftest empty-input-parse-tokens-test
  (testing "testing empty code being sent to parse-tokens function"
    (let [s ""]
      (is (thrown? AssertionError (parse-tokens s))))))

(deftest eval-atom-test
  (testing "testing atom"
    (let [s "1"]
      (is (= 1 (ceval (parse s)))))))

(deftest eval-simple-expression-test
  (testing "testing simple expression"
    (let [s "(+ 1 2)"]
      (is (= 3 (ceval (parse s)))))))

(deftest eval-complex-expression-test
  (testing "testing complex expression"
    (let [s "(+ 1 (+ 2 3))"]
      (is (= 6 (ceval (parse s)))))))

(deftest eval-false-expression-test
  (testing "testing if expression"
    (let [s "false"]
      (is (= false (ceval (parse s)))))))

(deftest eval-if-expression-test
  (testing "testing if expression"
    (let [s "(if 1 2 3)"]
      (is (= 2 (ceval (parse s)))
        "should evaluate then expression"))
    (let [s "(if false 2 3)"]
      (is (= 3 (ceval (parse s)))
        "should evaluate else expression"))))

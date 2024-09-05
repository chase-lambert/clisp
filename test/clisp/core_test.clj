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

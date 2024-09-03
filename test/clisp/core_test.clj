(ns clisp.core-test
  (:require 
    [pjstadig.humane-test-output :as humane]
    [clojure.test :refer [deftest is testing]]
    [clisp.core :refer :all]))

(humane/activate!)

(deftest parser-test
  (testing "testing parser"
    (let [s "(first (list 1 (+ 2 3) 9))"] 
      (is (= (parse s)
             ["first" ["list" 0 ["+" 2 3] 9]])))))


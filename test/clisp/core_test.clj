(ns clisp.core-test
  (:require 
    [pjstadig.humane-test-output :as humane]
    [clojure.test :refer [deftest is testing]]
    [clisp.core :refer :all]))

(humane/activate!)

(deftest add-test
  (testing "testing add"
    (is (= 4 (+ 1 1)))))


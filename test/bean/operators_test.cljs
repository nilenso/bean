(ns bean.operators-test
  (:require [bean.operators :refer [bean-op-+ bean-op-*]]
            [clojure.test :refer [deftest testing is]]))

(deftest bean-op-+-test
  (testing "Adds two numbers"
    (is (= (bean-op-+ 2 3) 5)))
  (testing "Returns an error if an operand is an invalid data type"
    (is (= (bean-op-+ "1" 2) {:error "+ only works for Integers"
                              :representation "+ only works for Integers"}))))

(deftest bean-op-*-test
  (testing "Multiplies two numbers"
    (is (= (bean-op-* 2 3) 6)))
  (testing "Returns an error if one operand is a string"
    (is (= (bean-op-* 1 "2") {:error "* only works for Integers"
                              :representation "* only works for Integers"}))
    (is (= (bean-op-* "1" 2) {:error "* only works for Integers"
                              :representation "* only works for Integers"}))))
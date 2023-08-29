(ns bean-tests.interpreter
  (:require [bean.interpreter :refer [bean-op-+]]
            [clojure.test :refer [deftest testing is]]))

(deftest bean-op-+-test
  (testing "Adds two numbers"
    (is (= (bean-op-+ 2 3) 5)))
  (testing "Returns an error if an operand is an invalid data type"
    (is (= (bean-op-+ "1" 2) {:error "Addition only works for Integers"}))))

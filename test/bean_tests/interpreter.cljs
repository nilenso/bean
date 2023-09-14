(ns bean-tests.interpreter
  (:require [bean.interpreter :refer [bean-op-+ apply-op]]
            [clojure.test :refer [deftest testing is]]))

(deftest bean-op-+-test
  (testing "Adds two numbers"
    (is (= (bean-op-+ 2 3) 5)))
  (testing "Returns an error if an operand is an invalid data type"
    (is (= (bean-op-+ "1" 2) {:error "Addition only works for Integers"}))))

(deftest apply-op-test
  (testing "Applies an op to a matrix and a scalar"
    (is (= (apply-op {:value bean-op-+} {:value 1}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]})
           {:matrix
            [[{:value 2 :representation "2"}
              {:value 3 :representation "3"}
              {:value 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:value bean-op-+}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]}
                     {:value 1})
           {:matrix
            [[{:value 2 :representation "2"}
              {:value 3 :representation "3"}
              {:value 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:value bean-op-+}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]})
           {:matrix
            [[{:value 2 :representation "2"}
              {:value 4 :representation "4"}
              {:value 2 :representation "2"}]]}))))

(ns bean.interpreter-test
  (:require [bean.interpreter :refer [apply-op]]
            [bean.operators :as operators]
            [clojure.test :refer [deftest testing is]]))

(deftest apply-op-test
  (testing "Applies an op to a matrix and a scalar"
    (is (= (apply-op {:value operators/bean-op-+} {:value 1}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]})
           {:matrix
            [[{:value 2 :representation "2"}
              {:value 3 :representation "3"}
              {:value 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:value operators/bean-op-+}
                     {:matrix [[{:value 1}
                                {:value 2}
                                {:value 1}]]}
                     {:value 1})
           {:matrix
            [[{:value 2 :representation "2"}
              {:value 3 :representation "3"}
              {:value 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:value operators/bean-op-+}
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

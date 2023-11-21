(ns bean.interpreter-test
  (:require [bean.interpreter :refer [apply-op]]
            [bean.operators :as operators]
            [clojure.test :refer [deftest testing is]]))

(deftest apply-op-test
  (testing "Applies an op to a matrix and a scalar"
    (is (= (apply-op {:scalar operators/bean-op-+} {:scalar 1}
                     {:matrix [[{:scalar 1}
                                {:scalar 2}
                                {:scalar 1}]]})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 3 :representation "3"}
              {:scalar 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:scalar operators/bean-op-+}
                     {:matrix [[{:scalar 1}
                                {:scalar 2}
                                {:scalar 1}]]}
                     {:scalar 1})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 3 :representation "3"}
              {:scalar 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (apply-op {:scalar operators/bean-op-+}
                     {:matrix [[{:scalar 1}
                                {:scalar 2}
                                {:scalar 1}]]}
                     {:matrix [[{:scalar 1}
                                {:scalar 2}
                                {:scalar 1}]]})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 4 :representation "4"}
              {:scalar 2 :representation "2"}]]}))))

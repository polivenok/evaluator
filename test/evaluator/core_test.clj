(ns evaluator.core-test
  (:require [clojure.test :refer :all]
            [evaluator.core :refer :all]))


(deftest evaluate-test
  (testing "With numbers only"
    (is (= 4 (evaluate {} '(* 2 (+ 1 1))))))
  (testing "With variables"
    (is (= 100 (evaluate {:x 10} '(* x x)))))

  (deftest optimize-test
    (testing "With multiplier by zero and expression evaluations"
      (is (= 10 (optimize '(+ 10 (* x 0)))))
      (testing "With sub zero and variables"
        (is (= '(+ x y) (optimize '(+ x (- y 0)))))))

    (deftest ->javascript-test
      (testing "Simple expression"
        (is (= "function example(x) { return 1 + x * x; }"
               (->javascript "example" '(+ 1 (* x x)))))))))


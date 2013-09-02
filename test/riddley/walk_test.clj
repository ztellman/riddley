(ns riddley.walk-test
  (:require
    [clojure.test :refer :all]
    [riddley.walk :refer :all]))

(defmacro inc-numbers [& body]
  (walk-exprs
    number?
    inc
    `(do ~@body)))

(deftest test-walk-exprs
  ;; the first and third numbers get incremented, but not the second
  (is (= 4 (inc-numbers (case 1 2 3))))
  
  (is (= 2 ((inc-numbers (fn [] 1)))))
  (is (= 4 (inc-numbers (+ 1 1))))
  (is (= 4 (inc-numbers (let [n 1] (+ n 1))))))

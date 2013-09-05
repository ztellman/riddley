(ns riddley.walk-test
  (:require
    [clojure.test :refer :all]
    [riddley.walk :as r]))

(defmacro inc-numbers [& body]
  (r/walk-exprs
    number?
    inc
    `(do ~@body)))

(defrecord Test [x])

(deftest test-walk-exprs
  ;; the first and third numbers get incremented, but not the second
  (is (= 4 (inc-numbers (case 1 2 3))))

  (is (= (Test. 1) (inc-numbers #riddley.walk_test.Test{:x 1})))
  
  (is (= 2 ((inc-numbers (fn [] 1)))))
  (is (= 4 (inc-numbers (+ 1 1))))
  (is (= 4 (inc-numbers (let [n 1] (+ n 1)))))
  (is (= 42 (inc-numbers
              (let [n 1]
                (if (= n 1)
                  41
                  0)))))
  (is (= 2 (inc-numbers
             (try
               (/ 2 -1) ()
               (catch Exception e
                 1))))))

(deftest test-macro-shadowing
  (is (= :yes
        (inc-numbers
          ((fn let [x]
             (if (= x 0)
               :yes
               (let 0)))
           2)))))

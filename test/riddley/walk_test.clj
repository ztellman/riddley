(ns riddley.walk-test
  (:require
    [clojure.test :refer :all]
    [riddley.compiler :as c]
    [riddley.walk :as r]))

(defmacro inc-numbers [& body]
  (r/walk-exprs
   number?
   inc
   `(do ~@body)))

(defmacro external-references [expr]
  (let [log (atom #{})]
    (r/walk-exprs
      symbol?
      (fn [x]
        (when (or (contains? &env x)
                  (not (contains? (c/locals) x)))
          (swap! log conj x))
        x)
      expr)
    (list 'quote @log)))

(defmacro identify-special-forms [expr]
  (list 'quote (r/walk-exprs
                 symbol?
                 (comp boolean r/special-form?)
                 expr)))

(defrecord Test [x])

(defprotocol TestP
  (n [_]))

(deftest merge-meta-test
  (is (= {:a 1}
         (meta (r/merge-meta {} {:a 1}))))
  (is (= nil
         (meta (r/merge-meta nil {:a 1} {:c 1}))))
  (is (= {:a 2, :b 1, :c 1}
         (meta (r/merge-meta (with-meta {} {:a 2}) {:a 1, :b 1} {:c 1})))))

(deftest inline-fn-test
  (doseq [[form expected] {'(+ 1)   nil
                           `(+ 1)   nil
                           '(+ 1 2) true
                           `(+ 1 2) true
                           '(int 1) true
                           `(int 1) true
                           `(n 1)   nil
                           `+       nil
                           `n       nil}]
    (testing (binding [*print-meta* true] (pr-str (list 'inline-fn? form)))
      (if expected
        (is (fn? (r/inline-fn form)))
        (is (= expected
               (r/inline-fn form)))))))

(deftest head=-test
  (is (= true
         (#'r/head= '(a b c) 'a)))
  (is (= false
         (#'r/head= '(a b c) 'b)))
  (is (= false
         (#'r/head= 1 'a))))

(deftest expand-inline-fn-test
  (is (= '(. clojure.lang.Numbers (and 2 1))
         (r/expand-inline-fn '(bit-and 2 1))))
  (testing "Should throw an Exception if the form is not inlineable"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Form is not an inlineable function call"
         (r/expand-inline-fn '(+ 1))))))

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
                1)))))

  (is (= 4 (n
            (inc-numbers
             (let [n 1]
               (reify TestP (n [_] (+ 1 n))))))))

  (is (= 4 (n
            (let [n 100]
              (eval
               '(riddley.walk-test/inc-numbers
                 (deftype Foo [n]
                   riddley.walk-test/TestP
                   (n [_] (+ 1 n)))
                 (Foo. 1))))))))

(deftest test-macro-shadowing
  (is (= :yes
        (inc-numbers
          ((fn let [x]
             (if (= x 0)
               :yes
               (let 0)))
           2)))))

(def foo 1)

(deftest test-var-evaluation
  (is (= #{#'riddley.walk-test/foo}
         (let [acc (atom #{})]
           (r/walk-exprs
            (constantly true)
            #(do (swap! acc conj %) %)
            '#'riddley.walk-test/foo)
           @acc))))

(deftest test-doesnt-walk-var-if-not-requested
  (is (= #{}
         (let [acc (atom #{})]
           (r/walk-exprs
            (constantly false)
            #(do (swap! acc conj %) %)
            '#'riddley.walk-test/foo)
           @acc))))

(deftest try-catch-finally-locals
  (is (= '(let* [catch inc, finally dec, throw +]
            (try (throw (catch 100) (finally 200))
                 (catch Exception e)
                 (finally nil)))

         (r/walk-exprs (constantly false) identity
           '(let [catch inc, finally dec, throw +]
             (try (throw (catch 100) (finally 200))
                  (catch Exception e)
                  (finally nil)))))))

(deftest try-catch-finally-locals-in-env
  (let [catch inc, finally dec, throw +]
    (is (= nil ((external-references
                  (try (throw (catch 100) (finally 200))
                       (catch Exception e)
                       (finally nil)))
                'e)))))

(deftest letfn-binds-locals-recursively
  (is (= nil ((external-references
                (letfn [(f1 [x] (inc (f2 x)))
                        (f2 [x] (* x 100))]
                  (f1 (f2 100))))
              'f2))))

(deftest special-forms-identified
  (is (= (identify-special-forms
           (let* [catch inc, finally dec, throw +]
             (try (throw (catch 100) (finally 200))
                  (catch Exception e)
                  (finally nil))))
         '(let* [catch false, finally false, throw false]
            (try (true (false 100) (false 200))
                 (catch Exception e)
                 (true nil))))))

(deftest catch-old-fn*-syntax
  (is (= (r/walk-exprs (constantly false) identity
                       '(fn* tst [x seq]))
         '(fn* tst ([x seq])))))

(deftest dot-expansion
  (is (= (r/macroexpand-all '(bit-and 2 1))
         '(. clojure.lang.Numbers (and 2 1)))))

(deftest do-not-macroexpand-quoted-things
  (is (= '(def p '(fn []))
        (r/walk-exprs
          (constantly false)
          identity
          '(def p '(fn []))))))

(deftest walk-quotes-if-allowed
  (is (= #{'(quote (do 1 2 3))}
         (let [acc (atom #{})]
           (r/walk-exprs
            #(and (seq? %) (#{'quote} (first %)))
            #(do (swap! acc conj %) %)
            '(quote (do 1 2 3)))
           @acc))))

(deftest dont-walk-quotes-if-not-allowed
  (is (= #{}
         (let [acc (atom #{})]
           (r/walk-exprs
            #{'do}
            #(do (swap! acc conj %) %)
            '(quote (do 1 2 3)))
           @acc))))

(deftest handle-def-with-docstring
  (is (= '(def x "docstring" (. clojure.lang.Numbers (add 1 2)))
         (r/walk-exprs (constantly false) identity '(def x "docstring" (+ 1 2))))))

(deftest walk-over-instance-expression-in-dot-forms
  (is (= '(. (. clojure.lang.Numbers (add 1 2)) toString)
         (r/macroexpand-all '(.toString (+ 1 2))))))

(deftest meta-data-on-inline-function-macro-expasion
  (is (= {:foo :bar}
         (meta (r/macroexpand (with-meta '(+ 1 1) {:foo :bar}))))))

(deftest expand-nested-inline-fn-calls-test
  (testing "Nested inlined function calls should get expanded correctly (#33)"
    (is (= '(.
             clojure.lang.Util
             clojure.core/equiv
             (. clojure.lang.RT (clojure.core/count [1 2]))
             (. clojure.lang.RT (clojure.core/count [3 4])))
           (r/macroexpand-all `(= (count [1 2]) (count [3 4]))))))
  (testing "Make sure inline functions that are expanded to Java methods with the same name don't cause infinite recursive expansion"
    (testing "(inc 1) expands to (. clojure.lang.Numbers (inc 1)) -- make sure the resultant `(inc 1)` isn't expanded again"
      (is (= '(. clojure.lang.Numbers (inc 1))
             (r/macroexpand-all `(inc 1)))))
    (testing "(. clojure.lang.Numbers (inc 1)) on its own shouldn't get expanded"
      (is (= '(. clojure.lang.Numbers (inc 1))
             (r/macroexpand-all '(. clojure.lang.Numbers (inc 1))))))
    (testing "(inc (inc 1)) *should* have get its nested inc call expanded"
      (is (= '(. clojure.lang.Numbers (inc (. clojure.lang.Numbers (inc 1))))
             (r/macroexpand-all `(inc (inc 1))))))))

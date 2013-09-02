(ns riddley.compiler
  (:import
    [clojure.lang
     Compiler
     Compiler$ObjMethod
     Compiler$C]))

(def context
  {:expression Compiler$C/EXPRESSION
   :statement  Compiler$C/STATEMENT
   :return     Compiler$C/RETURN
   :eval       Compiler$C/EVAL})

(def stub-method
  (proxy [Compiler$ObjMethod] [nil nil]))

(defn analyze
  ([form]
     (analyze :expression form))
  ([ctx form]
     (with-bindings {Compiler/METHOD stub-method}
       (Compiler/analyze (context ctx) form))))



(ns riddley.compiler
  (:import
    [clojure.lang
     Var
     Compiler
     Compiler$ObjMethod]
    [riddley
     Util]))

(def stub-method
  (proxy [Compiler$ObjMethod] [nil nil]))

(defn tag-of [x]
  (when-let [tag (-> x meta :tag)]
    (let [sym (symbol
                (if (instance? Class tag)
                  (.getName ^Class tag)
                  (name tag)))]
      (when-not (= sym 'java.lang.Object)
        sym))))

(let [n (atom 0)]
  (defn- local-id []
    (swap! n inc)))

(defn locals []
  (when (.isBound Compiler/LOCAL_ENV)
    @Compiler/LOCAL_ENV))

(defmacro with-lexical-scoping [& body]
  `(with-bindings {Compiler/LOCAL_ENV (locals)}
     ~@body))

(defmacro with-stub-vars [& body]
  `(with-bindings {Compiler/CLEAR_SITES nil
                   Compiler/METHOD nil}
     ~@body))

;; if we don't do this in Java, the checkcasts emitted by Clojure cause an
;; IllegalAccessError on Compiler$Expr.  Whee.
(defn register-local [v x]
  (with-stub-vars
    (when-not (locals)
      (alter-var-root Compiler/LOCAL_ENV (constantly {})))
    (.set ^Var Compiler/LOCAL_ENV
      (assoc (locals)
        v (Util/localBinding (local-id) v (tag-of v) x)))))

(defn register-arg [x]
  (with-stub-vars
    (when-not (locals)
      (alter-var-root Compiler/LOCAL_ENV (constantly {})))
    (.set ^Var Compiler/LOCAL_ENV
      (assoc (locals)
        x (Util/localArgument (local-id) x (tag-of x))))))




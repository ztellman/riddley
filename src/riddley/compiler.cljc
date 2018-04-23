(ns riddley.compiler
  (:import
    [clojure.lang
     Var
     Compiler
     #?@(:clj [Compiler$ObjMethod
               Compiler$ObjExpr])]
     #?(:clj  [riddley Util]
        :cljr [Riddley Util ObjMethod])))

(defn- stub-method []
  #?(:clj (proxy [Compiler$ObjMethod] [(Compiler$ObjExpr. nil) nil])
     :cljr (ObjMethod.)))


(defn tag-of
  "Returns a symbol representing the tagged class of the symbol, or `nil` if none exists."
  [x]
  (when-let [tag (-> x meta :tag)]
    (let [sym (symbol
                #?(:clj (if (instance? Class tag)
                          (.getName ^Class tag)
                          (name tag))
                   :cljr (if (instance? Type tag)
                          (.FullName ^Type tag)
                          (name tag))
                        ))]
      (when-not (= #?(:clj 'java.lang.Object
                      :cljr 'System.Object)
                   sym)
        sym))))

(let [n (atom 0)]
  (defn- local-id []
    (swap! n inc)))

(defn locals
  "Returns the local binding map, equivalent to the value of `&env`."
  []
  #?(:clj (when (.isBound Compiler/LOCAL_ENV)
            @Compiler/LOCAL_ENV)
     :cljr (when (.isBound Compiler/LocalEnvVar)
             @Compiler/LocalEnvVar)))

(defmacro with-base-env [& body]
  `(binding [*warn-on-reflection* false]
     (with-bindings (if (locals)
                      {}
                      {#?(:clj Compiler/LOCAL_ENV
                          :cljr clojure.lang.Compiler/LocalEnvVar) {}})
       ~@body)))

(defmacro with-lexical-scoping
  "Defines a lexical scope where new locals may be registered."
  [& body]
  `(with-bindings {#?(:clj Compiler/LOCAL_ENV
                      :cljr clojure.lang.Compiler/LocalEnvVar) (locals)}
     ~@body))

#?(:cljr (defn get-method-var []
           (let [field
                 (.GetField Compiler
                            "MethodVar"
                            (enum-or System.Reflection.BindingFlags/NonPublic
                                     System.Reflection.BindingFlags/Static))]
             (.GetValue field Compiler))))

(defmacro with-stub-vars [& body]
  `(with-bindings #?(:clj {Compiler/CLEAR_SITES nil
                           Compiler/METHOD (stub-method)}
                     :cljr {(get-method-var) (stub-method)})
     ~@body))

;; if we don't do this in Java, the checkcasts emitted by Clojure cause an
;; IllegalAccessError on Compiler$Expr.  Whee.
(defn register-local
  "Registers a locally bound variable `v`, which is being set to form `x`."
  [v x]
  (with-stub-vars
    (.set ^Var #?(:clj Compiler/LOCAL_ENV :cljr Compiler/LocalEnvVar)

      ;; we want to allow metadata on the symbols to persist, so remove old symbols first
      (-> (locals)
        (dissoc v)
        (assoc v (try
                   (Util/LocalBinding (local-id) v (tag-of v) x)
                   (catch Exception _
                     ::analyze-failure)))))))

(defn register-arg
  "Registers a function argument `x`."
  [x]
  (with-stub-vars
    (.set ^Var #?(:clj Compiler/LOCAL_ENV :cljr Compiler/LocalEnvVar)
      (-> (locals)
        (dissoc x)
        (assoc x (Util/LocalArgument (local-id) x (tag-of x)))))))




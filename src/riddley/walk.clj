(ns riddley.walk
  (:require
    [riddley.compiler :as cmp]))

(defn- walkable? [x]
  (and
    (sequential? x)
    (not (vector? x))
    (not (instance? java.util.Map$Entry x))))

(defn- macroexpand+
  "Expands both macros and inline functions."
  [x]
  (let [x* (macroexpand x)]
    (if-let [inline-fn (and (seq? x*)
                         (symbol? (first x*))
                         (not (-> x* meta ::transformed))
                         (-> x first resolve meta :inline))]
      (let [x** (apply inline-fn (rest x*))]
        (recur
          ;; unfortunately, static function calls can look a lot like what we just
          ;; expanded, so prevent infinite expansion
          (if (= '. (first x**))
            (concat (butlast x**) [(with-meta (last x**) {::transformed true})])
            x**)))
      x*)))

;;;

(defn- fn-handler [f x]
  (let [prelude (take-while (complement sequential?) x)
        remainder (drop (count prelude) x)
        body-handler (fn [x]
                       (cmp/with-lexical-scoping
                         (doseq [arg (first x)]
                           (cmp/register-arg arg))
                         (doall
                           (list* (first x)
                             (map f (rest x))))))]
    (concat
      prelude
      (map body-handler remainder))))

(defn- let-bindings [f x]
  (->> x
    (partition-all 2)
    (mapcat
      (fn [[k v]]
        (let [[k v] [k (f v)]]
          (cmp/register-local k v)
          [k v])))
    vec))

(defn- let-handler [f x]
  (cmp/with-lexical-scoping
    (doall
      (list*
        (first x)
        (let-bindings f (second x))
        (map f (drop 2 x))))))

(defn- case-handler [f x]
  (let [prefix (butlast (take-while (complement map?) x))
        default (last (take-while (complement map?) x))
        body (first (drop-while (complement map?) x))
        suffix (rest (drop-while (complement map?) x))]
    (concat
      prefix
      [(f default)]
      [(->> body
         (map
           (fn [[k [idx form]]]
             [k [idx (f form)]]))
         (into {}))]
      suffix)))

(defn- catch-handler [f x]
  (let [[_ type var & body] x]
    (cmp/with-lexical-scoping
      (cmp/register-arg (with-meta var {:tag (.getName ^Class type)}))
      (list* 'catch type var
        (map f body)))))

(defn walk-exprs
  "A walk function which only traverses valid Clojure expressions.  The `predicate` describes
   whether the sub-form should be transformed.  If it returns true, `handler` is invoked, and
   returns a transformed form.

   Unlike `clojure.walk`, if the handler is called, the rest of the sub-form is not walked.
   The handler function is responsible for recursively calling `walk-exprs` on the form it is
   given."
  [predicate handler x]
  (let [x (macroexpand+ x)
        walk-exprs (partial walk-exprs predicate handler)
        x' (cond

             (predicate x)
             (handler x)
             
             (walkable? x)
             ((condp = (first x)
                'fn*    fn-handler
                'let*   let-handler
                'loop*  let-handler
                'letfn* let-handler
                'case*  case-handler
                'catch  catch-handler
                #(map %1 %2))
              walk-exprs x)
             
             (instance? java.util.Map$Entry x)
             (clojure.lang.MapEntry.
               (walk-exprs (key x))
               (walk-exprs (val x)))
             
             (vector? x)
             (vec (map walk-exprs x))
             
             (map? x)
             (into {} (map walk-exprs x))
             
             (set? x)
             (set (map walk-exprs x))
             
             :else
             x)]
    (if (instance? clojure.lang.IObj x')
      (with-meta x' (merge (meta x) (meta x')))
      x')))

;;;

(defn macroexpand-all
  "Recursively macroexpands all forms, preserving the &env special variables."
  [x]
  (with-bindings {clojure.lang.Compiler/LOCAL_ENV {}}
    (walk-exprs (constantly false) nil x)))

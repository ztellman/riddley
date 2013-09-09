(ns riddley.walk
  (:refer-clojure :exclude [macroexpand])
  (:require
    [riddley.compiler :as cmp]))

(defn- walkable? [x]
  (and
    (sequential? x)
    (not (vector? x))
    (not (instance? java.util.Map$Entry x))))

(defn macroexpand
  "Expands both macros and inline functions. Optionally takes a set of `special-forms` which
   shouldn't be macroexpanded, and honors local bindings."
  ([x]
     (macroexpand x nil))
  ([x special-forms]
     (cmp/with-base-env
       (if (seq? x)
         (let [frst (first x)]

           (if (or
                 (contains? (set special-forms) frst)
                 (contains? (cmp/locals) frst))
             
             ;; might look like a macro, but for our purposes it isn't
             x
             
             (let [x' (macroexpand-1 x)]
               (if-not (identical? x x')
                 (macroexpand x' special-forms)
                 
                 ;; if we can't macroexpand any further, check if it's an inlined function
                 (if-let [inline-fn (and (seq? x')
                                      (symbol? (first x'))
                                      (-> x' meta ::transformed not)
                                      (or
                                        (-> x' first resolve meta :inline-arities not)
                                        ((-> x' first resolve meta :inline-arities)
                                         (count (rest x'))))
                                      (-> x' first resolve meta :inline))]
                   (let [x'' (apply inline-fn (rest x'))]
                     (macroexpand
                       ;; unfortunately, static function calls can look a lot like what we just
                       ;; expanded, so prevent infinite expansion
                       (if (= '. (first x''))         
                         (with-meta
                           (concat (butlast x'')
                             [(if (instance? clojure.lang.IObj (last x''))
                                (with-meta (last x'')
                                  (merge
                                    (meta (last x''))
                                    {::transformed true}))
                                (last x''))])
                           (meta x''))
                         x'')
                       special-forms))
                   x')))))
         x))))

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

    (cmp/with-lexical-scoping

      ;; register a local for the function, if it's named
      (when-let [nm (second prelude)]
        (cmp/register-local nm
          (list* 'fn* nm
            (map #(take 1 %) remainder))))
    
      (concat
        prelude
        (if (seq? (first remainder))
          (doall (map body-handler remainder))
          [(body-handler remainder)])))))

(defn- def-handler [f x]
  (let [[_ n form] x]
    (cmp/with-lexical-scoping
      (cmp/register-local n '())
      (list 'def n (f form)))))

(defn- let-bindings [f x]
  (->> x
    (partition-all 2)
    (mapcat
      (fn [[k v]]
        (let [[k v] [k (f v)]]
          (cmp/register-local k v)
          [k v])))
    vec))

(defn- reify-handler [f x]
  (let [[_ classes & fns] x]
    (list* 'reify* classes
      (doall
        (map
          (fn [[nm args & body]]
            (cmp/with-lexical-scoping
              (doseq [arg args]
                (cmp/register-arg arg))
              (list* nm args (doall (map f body)))))
          fns)))))

(defn- deftype-handler [f x]
  (let [[_ type resolved-type args _ interfaces & fns] x]
    (cmp/with-lexical-scoping
      (doseq [arg args]
        (cmp/register-arg arg))
      (list* 'deftype* type resolved-type args :implements interfaces
        (doall
          (map
            (fn [[nm args & body]]
              (cmp/with-lexical-scoping
                (doseq [arg args]
                  (cmp/register-arg arg))
                (list* nm args (doall (map f body)))))
            fns))))))

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
      (cmp/register-arg (with-meta var {:tag type}))
      (list* 'catch type var
        (map f body)))))

(defn walk-exprs
  "A walk function which only traverses valid Clojure expressions.  The `predicate` describes
   whether the sub-form should be transformed.  If it returns true, `handler` is invoked, and
   returns a transformed form.

   Unlike `clojure.walk`, if the handler is called, the rest of the sub-form is not walked.
   The handler function is responsible for recursively calling `walk-exprs` on the form it is
   given.

   Macroexpansion can be halted by defining a set of `special-forms` which will be left alone.
   Including `fn`, `let`, or other binding forms can break local variable analysis, so use
   with caution."
  ([predicate handler x]
     (walk-exprs predicate handler nil x))
  ([predicate handler special-forms x]
     (cmp/with-base-env
       (let [x (macroexpand x special-forms)
             walk-exprs (partial walk-exprs predicate handler special-forms)
             x' (cond

                  (predicate x)
                  (handler x)
                  
                  (walkable? x)
                  ((condp = (first x)
                     'def    def-handler
                     'fn*    fn-handler
                     'let*   let-handler
                     'loop*  let-handler
                     'letfn* let-handler
                     'case*  case-handler
                     'catch  catch-handler
                     'reify* reify-handler
                     'deftype* deftype-handler
                     #(doall (map %1 %2)))
                   walk-exprs x)
                  
                  (instance? java.util.Map$Entry x)
                  (clojure.lang.MapEntry.
                    (walk-exprs (key x))
                    (walk-exprs (val x)))
                  
                  (vector? x)
                  (vec (map walk-exprs x))

                  (instance? clojure.lang.IRecord x)
                  x
                  
                  (map? x)
                  (into {} (map walk-exprs x))
                  
                  (set? x)
                  (set (map walk-exprs x))
                  
                  :else
                  x)]
         (if (instance? clojure.lang.IObj x')
           (with-meta x' (merge (meta x) (meta x')))
           x')))))

;;;

(defn macroexpand-all
  "Recursively macroexpands all forms, preserving the &env special variables."
  [x]
  (walk-exprs (constantly false) nil x))

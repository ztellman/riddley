(ns riddley.walk
  (:refer-clojure :exclude [macroexpand])
  (:require
   [riddley.compiler :as cmp]))

(declare macroexpand)

(defn merge-meta
  "If `form` can have metadata, merge other `metadatas` into its metadata. Keys in `form`'s metadata are preferred over
  those in `metadatas`.

    (meta (merge-meta {} {:a 1}))                          ;-> {:a 1}
    (meta (merge-meta nil {:a 1}))                         ;-> nil
    (meta (merge-meta (with-meta {} {:a 2}) {:a 1, :b 1})) ;-> {:a 2, :b 1}"
  [form & metadatas]
  (cond-> form
    (instance? clojure.lang.IObj form) (vary-meta (apply partial merge metadatas))))

(defn- head=
  "True if `form` is list-like and the first element is `x`.

    (head= '(a b c) 'a) ;-> true"
  [form x]
  (and (seq? form)
       (= (first form) x)))

(defn inline-fn
  "If `form` represents a call to a function that will be replaced with an inlined version by the compiler, returns the
  `:inline` function used to create the replacement form; otherwise returns `nil`.

    (inline-fn '(+ 1 2)) ; -> #function[clojure.core/nary-inline/fn--5541]
    (inline-fn '(+ 1 2)) ; -> nil"
  [form]
  (when (seq? form)
    (let [[fn-symbol & args] form]
      (when (symbol? fn-symbol)
        ;; a function is inlineable if it has an `:inline` function in its metadata, and, if it has an
        ;; `:inline-arities` function in its metadata, returns truthy when passed the number of args in this form
        (let [{:keys [inline inline-arities]} (meta (resolve fn-symbol))]
          (when (and inline
                     (or (not inline-arities)
                         (inline-arities (count args))))
            inline))))))

(defn expand-inline-fn
  "Expand an inline function call `form` into the inlined version."
  ([form]
   (expand-inline-fn form nil))

  ([form special-form?]
   (let [inline-fn (or (inline-fn form)
                       (throw (ex-info "Form is not an inlineable function call." {:form form})))]
     (macroexpand
      (with-meta (apply inline-fn (rest form)) (meta form))
      special-form?))))

(defn- expand-list-like
  "Expand a list-like `form`."
  ([form]
   (expand-list-like form nil))

  ([[head :as form] special-form?]
   (if (or (and special-form? (special-form? head))
           (contains? (cmp/locals) head))
     ;; might look like a macro, but for our purposes it isn't
     form
     ;; otherwise attempt to macroexpand
     (let [expanded (macroexpand-1 form)]
       (cond
         (not (identical? form expanded))
         (macroexpand expanded special-form?)

         ;; if we can't macroexpand any further, check if it's an inlined function
         (inline-fn expanded)
         (expand-inline-fn expanded special-form?)

         :else
         form)))))

(defn macroexpand
  "Expands both macros and inline functions. Optionally takes a `special-form?` predicate which identifies first
  elements of expressions that shouldn't be macroexpanded, and honors local bindings."
  ([form]
   (macroexpand form nil))

  ([form special-form?]
   (cmp/with-base-env
     (if-not (seq? form)
       form
       (expand-list-like form special-form?)))))

;;;

(def ^:private special-forms
  (into #{} (keys (. clojure.lang.Compiler specials))))

(defn- special-meta [[op & body]]
  (list* (vary-meta op assoc ::special true) body))

(defn- do-handler [f [_ & body]]
  (list* 'do
    (doall
      (map f body))))

(defn- fn-handler [f x]
  (let [prelude (take-while (complement sequential?) x)
        remainder (drop (count prelude) x)
        remainder (if (vector? (first remainder))
                    (list remainder) remainder)
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
  (let [[_ n & r] x]
    (cmp/with-lexical-scoping
      (cmp/register-local n '())
      (list* 'def (f n) (doall (map f r))))))

(defn- let-bindings [f x recursive?]
  (let [pairs (partition-all 2 x)]
    (when recursive?
      (doall (map (fn [[k v]] (cmp/register-local k nil)) pairs)))
    (->> pairs
         (mapcat
           (fn [[k v]]
             (let [[k v] [k (f v)]]
               (cmp/register-local k v)
               [k v])))
         vec)))

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

(defn- let-handler
  ([f x]
   (let-handler f x nil))
  ([f x recursive?]
   (cmp/with-lexical-scoping
     (doall
       (list*
         (first x)
         (let-bindings f (second x) recursive?)
         (map f (drop 2 x)))))))

(defn- letfn-handler [f x]
  (let-handler f x true))

(defn- case-handler [f [_ ge shift mask default imap switch-type check-type skip-check]]
  (let [prefix  ['case* ge shift mask]
        suffix  [switch-type check-type skip-check]]
    (concat
      prefix
      [(f default)]
      [(let [m (->> imap
                 (map
                   (fn [[k [idx form]]]
                     [k [idx (f form)]]))
                 (into {}))]
         (if (every? number? (keys m))
           (into (sorted-map) m)
           m))]
      suffix)))

(defn- catch-handler [f x]
  (let [[_ type var & body] x]
    (cmp/with-lexical-scoping
      (when var
        (cmp/register-arg (vary-meta var assoc :tag type)))
      (list* 'catch type var
        (doall (map f body))))))

(defn- try-handler [f x]
  (let [[_ & body] x]
    (list* 'try (doall (map #(f % :try-clause? true) body)))))

(defn- dot-handler
  "Handle java interop forms."
  [f [_ class-or-instance & more]]
  ;; form is either of the syntax
  ;;
  ;; (. class-or-instance method & args)
  ;; or
  ;; (. class-or-instance (method & args))
  ;;
  ;; both syntaxes are possible and equivalent
  (if (seq? (first more))
    ;; (. class-or-instance (method & args))
    (let [[[method & args]] more]
      (list '. (f class-or-instance) (cons method (doall (map f args)))))
    ;; (. class-or-instance method & args)
    (let [[method & args] more]
      (list* '. (f class-or-instance) method (doall (map f args))))))

(defn walk-exprs
  "A walk function which only traverses valid Clojure expressions.  The `predicate` describes
   whether the sub-form should be transformed.  If it returns true, `handler` is invoked, and
   returns a transformed form.

   Unlike `clojure.walk`, if the handler is called, the rest of the sub-form is not walked.
   The handler function is responsible for recursively calling `walk-exprs` on the form it is
   given.

   Macroexpansion can be halted by defining a set of `special-form?` which will be left alone.
   Including `fn`, `let`, or other binding forms can break local variable analysis, so use
   with caution.

   The :try-clause? option indicates that a `try` clause is being walked. The special forms
   `catch` and `finally` are only special in `try` clauses."
  ([predicate handler x]
   (walk-exprs predicate handler nil x))

  ([predicate handler special-form? x & {:keys [try-clause?]}]
   (cmp/with-base-env
     (let [x (try
               (macroexpand x special-form?)
               (catch ClassNotFoundException _
                 x))
           walk-exprs' (partial walk-exprs predicate handler special-form?)
           x' (cond

                (and (head= x 'var) (predicate x))
                (handler (eval x))

                (and (head= x 'quote)  (not (predicate x)))
                x

                (predicate x)
                (handler x)

                (seq? x)
                (if (or (and (not try-clause?)
                             (#{'catch 'finally} (first x)))
                        (not (contains? special-forms (first x))))
                  (doall (map walk-exprs' x))
                  ((condp = (first x)
                     'do        do-handler
                     'def       def-handler
                     'fn*       fn-handler
                     'let*      let-handler
                     'loop*     let-handler
                     'letfn*    letfn-handler
                     'case*     case-handler
                     'try       try-handler
                     'catch     catch-handler
                     'reify*    reify-handler
                     'deftype*  deftype-handler
                     '.         dot-handler
                     #(doall (map %1 %2)))
                   walk-exprs' (special-meta x)))

                (instance? java.util.Map$Entry x)
                (clojure.lang.MapEntry.
                 (walk-exprs' (key x))
                 (walk-exprs' (val x)))

                (or
                 (set? x)
                 (vector? x))
                (into (empty x) (map walk-exprs' x))

                (instance? clojure.lang.IRecord x)
                x

                (map? x)
                (into (empty x) (map walk-exprs' x))

                ;; special case to handle clojure.test
                (and (symbol? x) (-> x meta :test))
                (vary-meta x update-in [:test] walk-exprs')

                :else
                x)]
       (merge-meta x' (meta x))))))

;;;

(defn macroexpand-all
  "Recursively macroexpands all forms, preserving the &env special variables."
  [x]
  (walk-exprs (constantly false) nil x))

(defn special-form?
  "Given sym, a symbol produced by walk-exprs, returns true if sym is a special form."
  [x]
  (when (symbol? x) (::special (meta x))))

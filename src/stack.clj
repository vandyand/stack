(ns stack)

(defn throw-err [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))

(defn string-cutoff-end [s]
  (if (= 0 (count s))
    s
    (subs s 0 (- (count s) 1))))

(defn is-var? [op]
  (and (symbol? op)
       (= \! (-> op str first))
       (not= \+ (-> op str last))))

(defn is-var-def? [op]
  (and (symbol? op)
       (= \! (-> op str first))
       (not= \+ (-> op str reverse second))
       (= \+ (-> op str last))))

(defn is-func? [op]
  (and
   (seq? op)
   (= 3 (count op))
   (= 'invoke> (first op))))

(defn sym-contains? [sym-vec sym]
  (boolean (some #(= % sym) sym-vec)))

(defn is-if-else? [op]
  (and
   (seq? op)
   (= 'if> (first op))
   (sym-contains? op 'else>)))

(defn is-pop? [op]
  (= op '<pop>))

(defn is-valid-form? [op]
  (or
   (is-func? op)
   (is-if-else? op)))

(defn is-constant? [op]
  (or
   (number? op)
   (string? op)
   (boolean? op)))

(defn valid-op? [op]
  (or
   (is-constant? op)
   (is-var? op)
   (is-var-def? op)
   (is-pop? op)
   (is-valid-form? op)))

(defn verify-op [op]
  (when (-> op valid-op? not) (throw-err "Invalid operation: " op)))

(defn verify-vars [ops inputs]
  (loop [[ops valid-vars] [ops (set inputs)]]
    (let [op (first ops)]
      (when ops
        (cond
          (is-var-def? op) (recur [(next ops) (conj valid-vars (-> op str string-cutoff-end symbol))])
          (or (-> op is-var? not) (and (is-var? op) (sym-contains? valid-vars op))) (recur [(next ops) valid-vars])
          :else (throw-err "Undefined variable: " op))))))

(defn verify-inputs [inputs]
  (doseq [input inputs]
    (when (-> input is-var? not) (throw-err "Invalid input variable: " input))))

(defn verify-ops [ops inputs]
  (verify-vars ops inputs)
  (mapv verify-op ops))

(defmacro defstackfn [function-name inputs & ops]
  (verify-inputs inputs)
  (verify-ops ops inputs)
  `(defn ~function-name ~inputs
     (loop [[ops# stack# inner-bindings#]
            [~(mapv (fn [op] `'~op) ops)
             []
             ~(zipmap (map keyword inputs) inputs)]]
       (let [op# (first ops#)]
         (if ops#
           (recur
            [(if (is-if-else? op#)
               (let [branches# (map rest (split-with #(not= % ~`'~'else>) op#))
                     remaining-ops# (rest ops#)]
                 (if (last stack#)
                   (into (vec (first branches#)) (vec remaining-ops#))
                   (into (vec (second branches#)) (vec remaining-ops#))))
               (next ops#))
             (cond
               (is-constant? op#) (conj stack# op#)
               (is-var? op#) (conj stack# (get inner-bindings# (keyword op#)))
               (is-pop? op#) (if (> (count stack#) 0) (subvec stack# 0 (-> stack# count (- 1))) stack#)
               (is-func? op#) (let [fn-args# (subvec stack# (- (count stack#) (last op#)))
                                    fn-res# (apply (-> op# second eval) fn-args#)]
                                (conj
                                 (subvec stack# 0 (- (count stack#) (last op#)))
                                 fn-res#))
               :else stack#)
             (if (is-var-def? op#)
               (assoc
                inner-bindings#
                (-> op# str string-cutoff-end keyword)
                (last stack#))
               inner-bindings#)])
           (last stack#))))))

(ns jasca.impl.grammar
  (:require [clojure.set :as set])
  (:import [clojure.lang IPersistentVector Keyword]
           [com.fasterxml.jackson.core JsonToken JsonParser]
           [java.util Map HashMap]))

;;;; # FIRSTS Set

(defprotocol Firsts
  (-firsts* [production nt-firsts]))

(defn- firsts* [nt-firsts production] (-firsts* production nt-firsts))

(deftype Epsilon [])

(def ^:private epsilon (Epsilon.))

(defn- grammar-firsts [grammar]
  (loop [nt-firsts (zipmap (keys grammar) (repeat #{}))]
    (let [nt-firsts* (into {} (map (fn [[name production]] [name (firsts* nt-firsts production)]))
                           grammar)]
      (if (= nt-firsts* nt-firsts)
        nt-firsts
        (recur nt-firsts*)))))

;;;; # FOLLOW Set

(defprotocol Follows
  (-follows* [production follows nt-firsts nt-follows]))

(defn- follows* [nt-firsts nt-follows production follows] (-follows* production follows nt-firsts nt-follows))

(defn- grammar-follows [nt-firsts grammar start-nts]
  (loop [nt-follows (merge (zipmap (keys grammar) (repeat #{}))
                           (zipmap start-nts (repeat #{nil})))]
    (let [nt-follows* (reduce (fn [nt-follows [name production]]
                                (follows* nt-firsts nt-follows production (get nt-follows name)))
                              nt-follows grammar)]
      (if (= nt-follows* nt-follows)
        nt-follows*
        (recur nt-follows*)))))

;;;; # Lookahead Sets

(defn- ->lookaheads [firsts follows]
  (if (contains? firsts epsilon)
    (set/union (disj firsts epsilon) follows)
    firsts))

(defprotocol Lookaheads
  (get-lookaheads [production])
  (with-lookaheads [production nt-firsts nt-follows follows]))

(defn- grammar-with-lookaheads [nt-firsts nt-follows grammar]
  (into {} (map (fn [[name production]]
                  [name (with-lookaheads production nt-firsts nt-follows (get nt-follows name))]))
        grammar))

;;;; # Parsers

(defprotocol ToParser
  (->parser [production grammar ^Map parsers]))

(defn- grammar-parsers [grammar]
  (let [parsers (HashMap.)]
    (into {} (map (fn [[name production]] [name (->parser production grammar parsers)]))
          grammar)))

(defn- parse-error? [v] (identical? v ::parse-error))

(defmacro ^:private elet [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [[b v & bindings] bindings]
      `(let [~b ~v]
         (if (parse-error? ~b)
           ~b
           (elet ~(vec bindings)
             ~@body))))))

;;;; # Grammar AST

(deftype Terminal [lookaheads token]
  Firsts
  (-firsts* [_ _] #{token})

  Follows
  (-follows* [_ _ _ nt-follows] nt-follows)

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [self _ _ _] self)

  ToParser
  (->parser [_ _ _]
    (fn [^JsonParser tokens]
      (let [token* (.currentToken tokens)]
        (if (identical? token* token)
          (do
            (.nextToken tokens)
            token)
          ::parse-error)))))

(defn- terminal [token] (Terminal. #{token} token))

(deftype TerminalValue [lookaheads token get-value]
  Firsts
  (-firsts* [_ _] #{token})

  Follows
  (-follows* [_ _ _ nt-follows] nt-follows)

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [self _ _ _] self)

  ToParser
  (->parser [_ _ _]
    (fn [^JsonParser tokens]
      (let [token* (.currentToken tokens)]
        (if (identical? token* token)
          (let [v (get-value tokens)]
            (.nextToken tokens)
            v)
          ::parse-error)))))

(defn terminal-value [token get-value] (TerminalValue. #{token} token get-value))

(deftype NonTerminal [lookaheads name]
  Firsts
  (-firsts* [_ nt-firsts] (get nt-firsts name))

  Follows
  (-follows* [_ follows _ nt-follows] (update nt-follows name set/union follows))

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ nt-firsts nt-follows _]
    (NonTerminal. (->lookaheads (get nt-firsts name) (get nt-follows name)) name))

  ToParser
  (->parser [_ grammar parsers]
    (let [^Map parsers parsers]
      (if-some [pbox (.get parsers name)]
        (or @pbox                                           ; black: direct reference acquired
            (fn [tokens] (@pbox tokens)))                   ; grey: lazily wait until parse time
        (let [pbox (volatile! nil)
              _ (.put parsers name pbox)                    ; shade it grey
              p (->parser (get grammar name) grammar parsers)]
          (vreset! pbox p)                                  ; blacken
          p)))))

(defn- nonterminal [name] (NonTerminal. nil name))

(deftype Functor [lookaheads f args]
  Firsts
  (-firsts* [_ nt-firsts]
    (reduce (fn [firsts arg] (->lookaheads firsts (firsts* nt-firsts arg)))
            #{epsilon} args))

  Follows
  (-follows* [_ follows nt-firsts nt-follows]
    (first (reduce (fn [[nt-follows follows] arg]
                     [(follows* nt-firsts nt-follows arg follows)
                      (->lookaheads (firsts* nt-firsts arg) follows)])
                   [nt-follows follows] (rseq args))))

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ nt-firsts nt-follows follows]
    (let [[args lookaheads] (reduce (fn [[args follows] arg]
                                      [(conj args (with-lookaheads arg nt-firsts nt-follows follows))
                                       (->lookaheads (firsts* nt-firsts arg) follows)])
                                    [() follows] (rseq args))]
      (Functor. lookaheads f (vec args))))

  ToParser
  (->parser [_ grammar parsers]
    (case (count args)
      0 (f)
      1 (let [arg-parser (->parser (get args 0) grammar parsers)]
          (fn [tokens]
            (elet [v (arg-parser tokens)]
              (f v))))
      2 (let [arg-parser (->parser (get args 0) grammar parsers)
              arg-parser* (->parser (get args 1) grammar parsers)]
          (fn [tokens]
            (elet [v (arg-parser tokens)
                   v* (arg-parser* tokens)]
              (f v v*))))
      3 (let [arg-parser (->parser (get args 0) grammar parsers)
              arg-parser* (->parser (get args 1) grammar parsers)
              arg-parser** (->parser (get args 2) grammar parsers)]
          (fn [tokens]
            (elet [v (arg-parser tokens)
                   v* (arg-parser* tokens)
                   v** (arg-parser** tokens)]
              (f v v* v**))))
      4 (let [arg-parser (->parser (get args 0) grammar parsers)
              arg-parser* (->parser (get args 1) grammar parsers)
              arg-parser** (->parser (get args 2) grammar parsers)
              arg-parser*** (->parser (get args 3) grammar parsers)]
          (fn [tokens]
            (elet [v (arg-parser tokens)
                   v* (arg-parser* tokens)
                   v** (arg-parser** tokens)
                   v*** (arg-parser*** tokens)]
              (f v v* v** v***))))
      (let [p (transduce (map-indexed vector)
                         (completing
                           (fn [p [revi arg]]
                             (let [^int i (- (count args) 1 revi)
                                   arg-parser (->parser arg grammar parsers)]
                               (fn [tokens ^"[Ljava.lang.Object;" args]
                                 (elet [v (arg-parser tokens)]
                                   (aset args i v)
                                   (p tokens args))))))
                         (fn [_ args] (apply f args))
                         (rseq args))]
        (fn [tokens] (p tokens (object-array (count args))))))))

(defn- fmap [f args] (Functor. nil f args))

(defn- check-alts-conflicts [alts]
  (loop [alts-las (mapv get-lookaheads alts)]
    (when (seq alts-las)
      (let [[alt-las & ralts-las] alts-las]
        (run! (fn [alt*-las]
                (let [conflict (set/intersection alt-las alt*-las)]
                  (when (seq conflict)
                    (throw (ex-info ":or lookaheads conflict"
                                    {:lookaheads alt-las, :lookaheads* alt*-las})))))
              ralts-las)
        (recur ralts-las)))))

(deftype Alt [lookaheads alts]
  Firsts
  (-firsts* [_ nt-firsts]
    (transduce (map #(firsts* nt-firsts %)) set/union
               #{} alts))

  Follows
  (-follows* [_ follows nt-firsts nt-follows]
    (reduce (fn [nt-follows alt] (follows* nt-firsts nt-follows alt follows))
            nt-follows alts))

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ nt-firsts nt-follows follows]
    (let [[alts lookaheads] (reduce (fn [[alts lookaheads] alt]
                                      (let [alt (with-lookaheads alt nt-firsts nt-follows follows)]
                                        [(conj alts alt)
                                         (set/union lookaheads (get-lookaheads alt))]))
                                    [[] #{}] alts)]
      (Alt. lookaheads alts)))

  ToParser
  (->parser [_ grammar parsers]
    (check-alts-conflicts alts)
    (reduce (fn [p alt]
              (let [alt-parser (->parser alt grammar parsers)]
                (fn [tokens]
                  (let [v (alt-parser tokens)]
                    (if-not (parse-error? v)
                      v
                      (p tokens))))))
            (->parser (peek alts) grammar parsers)
            (rest (rseq alts)))))

(defn- alt [alts] (Alt. nil alts))

(def ^:private array-firsts #{JsonToken/START_ARRAY})

(def ^:private array-start-parser (->parser (terminal JsonToken/START_ARRAY) nil nil))
(def ^:private array-end-parser (->parser (terminal JsonToken/END_ARRAY) nil nil))

(deftype JsonArray [elem]
  Firsts
  (-firsts* [_ _] array-firsts)

  Follows
  (-follows* [_ _ nt-firsts nt-follows]
    (follows* nt-firsts nt-follows elem (conj (firsts* nt-firsts elem) JsonToken/END_ARRAY)))

  Lookaheads
  (get-lookaheads [_] array-firsts)
  (with-lookaheads [_ nt-firsts nt-follows _]
    (JsonArray. (with-lookaheads elem nt-firsts nt-follows
                                 (conj (firsts* nt-firsts elem) JsonToken/END_ARRAY))))

  ToParser
  (->parser [_ grammar parsers]
    (let [elem-parser (->parser elem grammar parsers)]
      (fn [tokens]
        (elet [_ (array-start-parser tokens)]
          (loop [coll (transient [])]
            (if (parse-error? (array-end-parser tokens))
              (elet [v (elem-parser tokens)]
                (recur (conj! coll v)))
              (persistent! coll))))))))

(def ^:private array-of ->JsonArray)

(def ^:private obj-firsts #{JsonToken/START_OBJECT})

(def ^:private obj-start-parser (->parser (terminal JsonToken/START_OBJECT) nil nil))
(def ^:private obj-end-parser (->parser (terminal JsonToken/END_OBJECT) nil nil))

(deftype JsonObjectOf [kf value]
  Firsts
  (-firsts* [_ _] obj-firsts)

  Follows
  (-follows* [_ _ nt-firsts nt-follows]
    (follows* nt-firsts nt-follows value (conj (firsts* nt-firsts value) JsonToken/END_OBJECT)))

  Lookaheads
  (get-lookaheads [_] obj-firsts)
  (with-lookaheads [_ nt-firsts nt-follows _]
    (JsonObjectOf. kf (with-lookaheads value nt-firsts nt-follows
                                       (conj (firsts* nt-firsts value) JsonToken/END_OBJECT))))

  ToParser
  (->parser [_ grammar parsers]
    (let [key-parser (->parser (terminal-value JsonToken/FIELD_NAME (fn [^JsonParser tokens] (kf (.getText tokens))))
                               grammar parsers)
          value-parser (->parser value grammar parsers)]
      (fn [tokens]
        (elet [_ (obj-start-parser tokens)]
          (loop [m (transient {})]
            (if (parse-error? (obj-end-parser tokens))
              (elet [k (key-parser tokens)
                     v (value-parser tokens)]
                (recur (assoc! m k v)))
              (persistent! m))))))))

(def ^:private object-of ->JsonObjectOf)

(deftype JsonObject [kf fields]
  Firsts
  (-firsts* [_ _] obj-firsts)

  Follows
  (-follows* [_ _ nt-firsts nt-follows]
    (reduce-kv (fn [nt-follows _ value]
                 (follows* nt-firsts nt-follows value (conj (firsts* nt-firsts value) JsonToken/END_OBJECT)))
               nil fields))

  Lookaheads
  (get-lookaheads [_] obj-firsts)
  (with-lookaheads [_ nt-firsts nt-follows _]
    (JsonObject. kf (into {} (map (fn [[k v]]
                                    [k (with-lookaheads v nt-firsts nt-follows
                                                        (conj (firsts* nt-firsts v) JsonToken/END_OBJECT))]))
                          fields)))

  ToParser
  (->parser [_ grammar parsers]
    (let [key-parser (->parser (terminal-value JsonToken/FIELD_NAME (fn [^JsonParser tokens] (kf (.getText tokens))))
                               grammar parsers)
          parsers (into {} (map (fn [[k v]] [k (->parser v grammar parsers)]))
                        fields)]
      (fn [tokens]
        (elet [_ (obj-start-parser tokens)]
          (loop [m (transient {})]
            (if (parse-error? (obj-end-parser tokens))
              (elet [k (key-parser tokens)]
                (if-some [val-parser (get parsers k)]
                  (elet [v (val-parser tokens)]
                    (recur (assoc! m k v)))
                  ::parse-error))
              (persistent! m))))))))

(defn ^:private obj
  ([fields] (obj identity fields))
  ([kf fields] (JsonObject. kf fields)))

;;;; # Analyze into Grammar AST

(defprotocol Analyzable
  (-analyze [form grammar]))

(defn- syntax-error [form] (throw (RuntimeException. (str "Invalid grammar: " form))))

(defn analyze [grammar form] (-analyze form grammar))

(extend-protocol Analyzable
  IPersistentVector
  (-analyze [coll grammar]
    (if (seq coll)
      (let [[op & args] coll]
        (case op
          :-> (if (seq args)
                (let [f (peek coll)]
                  (if (ifn? f)
                    (fmap f (mapv #(analyze grammar %) (butlast args)))
                    (throw (RuntimeException. (str "Last arg of " op " is not an IFn")))))
                (throw (RuntimeException. (str "Empty " op " args"))))

          :or (if (seq args)
                (alt (mapv #(analyze grammar %) args))
                (throw (RuntimeException. (str "Empty " op " args"))))

          :array-of (if (= (count args) 1)
                      (array-of (analyze grammar (first args)))
                      (throw (RuntimeException. (str op " expected one arg, got " (count args)))))

          :object-of (if (= (count args) 2)
                       (let [[kf value] args]
                         (if (ifn? kf)
                           (object-of kf (analyze grammar value))
                           (throw (RuntimeException. (str op " key-fn is not an IFn")))))
                       (throw (RuntimeException. (str op " expected two args, got " (count args)))))

          :object (case (count args)
                    1 (let [[fields] args]
                        (if (map? fields)
                          (obj (into {} (map (fn [[k v]] [k (analyze grammar v)])) fields))
                          (throw (RuntimeException. (str op "fields is not a map")))))
                    2 (let [[kf fields] args]
                        (if (map? fields)
                          (if (ifn? kf)
                            (obj kf (into {} (map (fn [[k v]] [k (analyze grammar v)])) fields))
                            (throw (RuntimeException. (str op " key-fn is not an IFn"))))
                          (throw (RuntimeException. (str op "fields is not a map")))))
                    (throw (RuntimeException. (str op " expected one or two args, got " (count args)))))

          (throw (RuntimeException. (str "Invalid grammar operator: " op)))))
      (syntax-error coll)))

  Keyword
  (-analyze [kw grammar]
    (if (contains? grammar kw)
      (nonterminal kw)
      (throw (RuntimeException. (str "Nonterminal " kw " does not exist in grammar")))))

  Class
  (-analyze [class grammar]
    (condp = class
      String (terminal-value JsonToken/VALUE_STRING (fn [^JsonParser tokens] (.getText tokens)))
      Long (terminal-value JsonToken/VALUE_NUMBER_INT (fn [^JsonParser tokens] (.getLongValue tokens)))
      Double (terminal-value JsonToken/VALUE_NUMBER_FLOAT (fn [^JsonParser tokens] (.getDoubleValue tokens)))
      Boolean (analyze grammar [:or true false])))

  Boolean
  (-analyze [b _] (->> (if b JsonToken/VALUE_TRUE JsonToken/VALUE_FALSE) terminal vector (fmap (fn [_] b))))

  nil
  (-analyze [_ _] (->> JsonToken/VALUE_NULL terminal vector (fmap (fn [_] nil)))))

(defn analyze-grammar [grammar]
  (if (map? grammar)
    (into {} (map (fn [[k v]] [k (analyze grammar v)]))
          grammar)
    (throw (RuntimeException. "Grammar is not a map"))))

;;;; # Full Parserification

(defn parsers [grammar start-nts]
  (let [grammar (analyze-grammar grammar)
        nt-firsts (grammar-firsts grammar)
        nt-follows (grammar-follows nt-firsts grammar start-nts)
        la-grammar (grammar-with-lookaheads nt-firsts nt-follows grammar)
        parsers (grammar-parsers la-grammar)]
    (mapv (fn [name]
            (let [p (get parsers name)]
              (fn [^JsonParser tokens]
                (.nextToken tokens)
                (p tokens))))
          start-nts)))

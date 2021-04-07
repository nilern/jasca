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

(defn- map-success [f v] (if-not (parse-error? v) (f v) v))

(defn- success-or-else [f v] (if-not (parse-error? v) v (f)))

;;;; # Grammar AST

(deftype Terminal [lookaheads token]
  Firsts
  (-firsts* [_ _] #{token})

  Follows
  (-follows* [_ _ _ nt-follows] nt-follows)

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ _ _ _] (Terminal. #{token} token))

  ToParser
  (->parser [_ _ _]
    (fn [^JsonParser tokens]
      (let [token* (.currentToken tokens)]
        (if (identical? token* token)
          (do
            (.nextToken tokens)
            token)
          ::parse-error)))))

(defn- terminal [token] (Terminal. nil token))

(deftype TerminalValue [lookaheads token get-value]
  Firsts
  (-firsts* [_ _] #{token})

  Follows
  (-follows* [_ _ _ nt-follows] nt-follows)

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ _ _ _] (TerminalValue. #{token} token get-value))

  ToParser
  (->parser [_ _ _]
    (fn [^JsonParser tokens]
      (let [token* (.currentToken tokens)]
        (if (identical? token* token)
          (let [v (get-value tokens)]
            (.nextToken tokens)
            v)
          ::parse-error)))))

(defn terminal-value [token get-value] (TerminalValue. nil token get-value))

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
    (let [p (transduce (map-indexed vector)
                       (completing
                         (fn [p [revi arg]]
                           (let [^int i (- (count args) 1 revi)
                                 arg-parser (->parser arg grammar parsers)]
                             (fn [tokens ^"[Ljava.lang.Object;" args]
                               (->> (arg-parser tokens)
                                    (map-success (fn [v]
                                                   (aset args i v)
                                                   (p tokens args))))))))
                       (fn [_ args] (apply f args))
                       (rseq args))]
      (fn [tokens] (p tokens (object-array (count args)))))))

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
                  (->> (alt-parser tokens)
                       (success-or-else (fn [] (p tokens)))))))
            (->parser (peek alts) grammar parsers)
            (rest (rseq alts)))))

(defn- alt [alts] (Alt. nil alts))

(deftype Many [lookaheads elem]
  Firsts
  (-firsts* [_ nt-firsts] (conj (firsts* nt-firsts elem) epsilon))

  Follows
  (-follows* [_ follows nt-firsts nt-follows] (follows* nt-firsts nt-follows elem follows))

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ nt-firsts nt-follows follows]
    (let [elem (with-lookaheads elem nt-firsts nt-follows follows)]
      (Many. (->lookaheads (get-lookaheads elem) follows) elem)))

  ToParser
  (->parser [_ grammar parsers]
    (let [elem-parser (->parser elem grammar parsers)]
      (fn [tokens]
        (loop [coll (transient [])]
          (let [v (elem-parser tokens)]
            (if (parse-error? v)
              (persistent! coll)
              (recur (conj! coll v)))))))))

(defn- many [elem] (Many. nil elem))

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

          :* (if (= (count args) 1)
               (many (analyze grammar (first args)))
               (throw (RuntimeException. (str op " expected one arg, got " (count args)))))

          (throw (RuntimeException. (str "Invalid grammar operator: " op)))))
      (syntax-error coll)))

  Keyword
  (-analyze [kw grammar]
    (if (contains? grammar kw)
      (nonterminal kw)
      (throw (RuntimeException. (str "Nonterminal " kw " does not exist in grammar")))))

  Class
  (-analyze [class _]
    (condp = class
      String (terminal-value JsonToken/VALUE_STRING (fn [^JsonParser tokens] (.getValueAsString tokens)))
      Long (terminal-value JsonToken/VALUE_NUMBER_INT (fn [^JsonParser tokens] (.getValueAsLong tokens)))
      Double (terminal-value JsonToken/VALUE_NUMBER_FLOAT (fn [^JsonParser tokens] (.getValueAsDouble tokens)))))

  Character
  (-analyze [c _]
    (terminal (case c
                \{ JsonToken/START_OBJECT
                \} JsonToken/END_OBJECT
                \[ JsonToken/START_ARRAY
                \] JsonToken/END_ARRAY
                (syntax-error c))))

  Boolean
  (-analyze [b _] (->> (if b JsonToken/VALUE_TRUE JsonToken/VALUE_FALSE) terminal vector (fmap (fn [_] b))))

  nil
  (-analyze [_ _] (->> JsonToken/VALUE_NULL terminal vector (fmap (fn [_] nil))))

  ;; HACK
  JsonToken
  (-analyze [token _]
    (if (identical? token JsonToken/FIELD_NAME)
      (terminal-value JsonToken/FIELD_NAME (fn [^JsonParser tokens] (.getValueAsString tokens)))
      (throw (RuntimeException. (str "Invalid grammar: " token))))))

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

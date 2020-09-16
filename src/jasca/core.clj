(ns jasca.core
  (:import [com.fasterxml.jackson.core JsonToken JsonParser]
           [clojure.lang Var]))

(defprotocol JascaParser
  (probe [self ^JsonToken token])
  (-parse [self ^JsonParser tokens]))

(deftype FailParser [msg error]
  JascaParser
  (probe [_ _] :nonconsuming)
  (-parse [self tokens]
    (throw (ex-info msg error
                    {:cause self, :location (.getCurrentLocation ^JsonParser tokens)}))))

(def fail ->FailParser)

(deftype PureParser [v]
  JascaParser
  (probe [_ _] :nonconsuming)
  (-parse [_ _] v))

(def pure ->PureParser)

(deftype FunctorParser [f inner]
  JascaParser
  (probe [_ token] (probe inner token))
  (-parse [_ tokens] (f (-parse inner tokens))))

(def fmap ->FunctorParser)

(deftype ApplicativeParser [fp inner]
  JascaParser
  (probe [_ token] (probe inner token))
  (-parse [_ tokens]
    (let [f (-parse fp tokens)]
      (f (parse inner tokens)))))

(def fapply ->ApplicativeParser)

(defmacro plet [bindings & body]
  (letfn [(body-fn [binders]
            (if (empty? binders)
              `(do ~@body)
              `(fn [~(first binders)]
                 ~(body-fn (rest binders)))))]
    (let [binders (take-nth 2 bindings)
          actions (take-nth 2 (rest bindings))]
      (assert (= (count binders) (count actions)))
      (if (empty? binders)
        `(do ~@body)
        (reduce (fn [acc action] `(fapply ~acc ~action))
                `(fmap ~(body-fn binders) ~(first actions))
                (rest actions))))))

(deftype AlternativeParser [p p*]
  JascaParser
  (probe [_ token]
    (let [probed (probe p token)]
      (case probed
        :fail (probe p* token)
        probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (case (probe p (.currentToken tokens))
        :fail (-parse p* tokens)
        (-parse p tokens)))))

(def alt ->AlternativeParser)

(defmacro orp [& alts]
  (if (empty? alts)
    `(fail "orp: out of options")
    `(alt ~(first alts) (orp ~@(rest alts)))))

(deftype SatParser [pred]
  JascaParser
  (probe [_ token] (if (pred token) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens
          token (.currentToken tokens)]
      (if (pred token)
        token
        (ex-info "unsatisfactory token"
                 {:expected pred, :received token}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def sat ->SatParser)

(defn start-array? [token] (identical? token JsonToken/START_ARRAY))
(def start-array (sat start-array?))
(defn end-array? [token] (identical? token JsonToken/END_ARRAY))
(def end-array (sat end-array?))

(defn start-object? [token] (identical? token JsonToken/START_OBJECT))
(def start-object (sat start-object?))
(defn end-object? [token] (identical? token JsonToken/END_OBJECT))
(def end-object (sat end-object?))

(defn true-token? [token] (identical? token JsonToken/VALUE_TRUE))
(def truep (sat true-token?))
(defn false-token? [token] (identical? token JsonToken/VALUE_FALSE))
(def falsep (sat false-token?))

(deftype NullParser []
  JascaParser
  (probe [_ token] (if (identical? token JsonToken/VALUE_NULL) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NULL)
        nil
        (ex-info "unsatisfactory token"
                 {:expected JsonToken/VALUE_NULL, :received (.currentToken tokens)}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def nullp (NullParser.))

(deftype IntParser []
  JascaParser
  (probe [_ token] (if (identical? token JsonToken/VALUE_NUMBER_INT) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NUMBER_INT)
        (.getNumberValue tokens)
        (ex-info "unsatisfactory token"
                 {:expected JsonToken/VALUE_NUMBER_INT, :received (.currentToken tokens)}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def intp (IntParser.))

(deftype FloatParser []
  JascaParser
  (probe [_ token] (if (identical? token JsonToken/VALUE_NUMBER_FLOAT) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NUMBER_FLOAT)
        (.getNumberValue tokens)
        (ex-info "unsatisfactory token"
                 {:expected JsonToken/VALUE_NUMBER_FLOAT, :received (.currentToken tokens)}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def intp (FloatParser.))

(deftype StringParser []
  JascaParser
  (probe [_ token] (if (identical? token JsonToken/VALUE_STRING) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_STRING)
        (.getText tokens)
        (ex-info "unsatisfactory token"
                 {:expected JsonToken/VALUE_STRING, :received (.currentToken tokens)}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def stringp (StringParser.))

(extend-protocol JascaParser
  Var
  (probe [self token] (probe @self token))
  (-parse [self tokens] (-parse @self tokens)))

(defn opt [parser] (alt parser (pure nil)))

(deftype ManyParser [inner]
  JascaParser
  (probe [_ token]
    (let [probed (probe inner token)]
      (case probed
        :fail :nonconsuming
        probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (loop [vs []]
        (case (probe inner (.currentToken ^JsonParser tokens))
          :fail vs
          (let [v (-parse inner tokens)]
            (.nextToken tokens)
            (recur (conj vs v))))))))

(def many ->ManyParser)

(defn parse [parser ^JsonParser tokens]
  (.nextToken tokens)
  (-parse parser tokens))

(ns jasca.core
  (:import [com.fasterxml.jackson.core JsonToken JsonParser]
           [clojure.lang Var]))

(defprotocol JascaParser
  (-probe [self ^JsonToken token])
  (-parse [self ^JsonParser tokens]))

(deftype FailParser [msg error]
  JascaParser
  (-probe [_ _] :nonconsuming)
  (-parse [self tokens]
    (throw (ex-info msg {:failure error
                         :parser self, :location (.getCurrentLocation ^JsonParser tokens)}))))

(def fail ->FailParser)

(deftype PureParser [v]
  JascaParser
  (-probe [_ _] :nonconsuming)
  (-parse [_ _] v))

(def pure ->PureParser)

(deftype FunctorParser [f inner]
  JascaParser
  (-probe [_ token] (-probe inner token))
  (-parse [_ tokens] (f (-parse inner tokens))))

(def fmap ->FunctorParser)

(deftype ApplicativeParser [fp inner]
  JascaParser
  (-probe [_ token]
    (let [-probed (-probe fp token)]
      (case -probed
        :nonconsuming (-probe inner token)
        -probed)))
  (-parse [_ tokens]
    (let [f (-parse fp tokens)]
      (f (-parse inner tokens)))))

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
  (-probe [_ token]
    (let [-probed (-probe p token)]
      (case -probed
        :fail (-probe p* token)
        -probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (case (-probe p (.currentToken tokens))
        :fail (-parse p* tokens)
        (-parse p tokens)))))

(def alt ->AlternativeParser)

(defmacro orp
  ([] `(fail "jasca.core/orp: out of alternatives" :orp-alternatives))
  ([p] p)
  ([p & ps] `(alt ~p (orp ~@ps))))

(deftype SatParser [pred]
  JascaParser
  (-probe [_ token] (if (pred token) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens
          token (.currentToken tokens)]
      (if (pred token)
        (do (.nextToken tokens)
            token)
        (throw (ex-info "unsatisfactory token"
                        {:expected pred, :received token
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def sat ->SatParser)

(defn start-array? [token] (identical? token JsonToken/START_ARRAY))
(def start-array (sat start-array?))
(defn end-array? [token] (identical? token JsonToken/END_ARRAY))
(def end-array (sat end-array?))

(defn start-object? [token] (identical? token JsonToken/START_OBJECT))
(def start-object (sat start-object?))
(defn end-object? [token] (identical? token JsonToken/END_OBJECT))
(def end-object (sat end-object?))

(deftype FalseParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_FALSE) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_FALSE)
        (do (.nextToken tokens)
            false)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_FALSE, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def falsep (FalseParser.))

(deftype TrueParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_TRUE) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_TRUE)
        (do (.nextToken tokens)
            true)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_TRUE, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def truep (TrueParser.))

(def booleanp (orp truep falsep))

(deftype NullParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_NULL) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NULL)
        (do (.nextToken tokens)
            nil)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_NULL, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def nullp (NullParser.))

(deftype IntParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_NUMBER_INT) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NUMBER_INT)
        (let [n (.getNumberValue tokens)]
          (.nextToken tokens)
          n)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_NUMBER_INT, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def intp (IntParser.))

(deftype FloatParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_NUMBER_FLOAT) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_NUMBER_FLOAT)
        (let [n (.getNumberValue tokens)]
          (.nextToken tokens)
          n)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_NUMBER_FLOAT, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def floatp (FloatParser.))

(deftype StringParser []
  JascaParser
  (-probe [_ token] (if (identical? token JsonToken/VALUE_STRING) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/VALUE_STRING)
        (let [n (.getText tokens)]
          (.nextToken tokens)
          n)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/VALUE_STRING, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def stringp (StringParser.))

(extend-protocol JascaParser
  Var
  (-probe [self token] (-probe @self token))
  (-parse [self tokens] (-parse @self tokens)))

(defn opt [parser] (alt parser (pure nil)))

(deftype ManyParser [inner]
  JascaParser
  (-probe [_ token]
    (let [-probed (-probe inner token)]
      (case -probed
        :fail :nonconsuming
        -probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (loop [vs (transient [])]
        (case (-probe inner (.currentToken ^JsonParser tokens))
          :fail (persistent! vs)
          (recur (conj! vs (-parse inner tokens))))))))

(def many ->ManyParser)

(defn array-of [item]
  (plet [_ start-array
         items (many item)
         _ end-array]
    items))

(defn parse [parser ^JsonParser tokens]
  (.nextToken tokens)
  (-parse parser tokens))

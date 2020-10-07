(ns jasca.core
  (:import [com.fasterxml.jackson.core JsonToken JsonParser]
           [clojure.lang Var]))

(defprotocol JascaParser
  (-probe [self ^JsonParser tokens])
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

;;; TODO: DRY:

(deftype Functor0Parser [f]
  JascaParser
  (-probe [_ _] :nonconsuming)
  (-parse [_ _] (f)))

(deftype Functor1Parser [f inner]
  JascaParser
  (-probe [_ tokens] (-probe inner tokens))
  (-parse [_ tokens] (f (-parse inner tokens))))

(deftype Functor2Parser [f inner inner*]
  JascaParser
  (-probe [_ tokens]
    (let [probed (-probe inner tokens)]
      (if (identical? probed :nonconsuming)
        (-probe inner* tokens)
        probed)))
  (-parse [_ tokens]
    (let [arg (-parse inner tokens)
          arg* (-parse inner* tokens)]
      (f arg arg*))))

(deftype Functor3Parser [f inner inner* inner**]
  JascaParser
  (-probe [_ tokens]
    (let [probed (-probe inner tokens)]
      (if (identical? probed :nonconsuming)
        (let [probed (-probe inner* tokens)]
          (if (identical? probed :nonconsuming)
            (-probe inner** tokens)
            probed))
        probed)))
  (-parse [_ tokens]
    (let [arg (-parse inner tokens)
          arg* (-parse inner* tokens)
          arg** (-parse inner** tokens)]
      (f arg arg* arg**))))

(deftype FunctorParser [f ^"[Ljava.lang.Object;" inners]
  JascaParser
  (-probe [_ tokens]
    (loop [i 0]
      (let [probed (-probe (aget inners i) tokens)]
        (if (identical? probed :nonconsuming)
          (if inners
            (recur (inc i))
            :nonconsuming)
          probed))))
  (-parse [_ tokens]
    (let [args (object-array (alength inners))]
      (dotimes [i (alength inners)]
        (aset args i (-parse (aget inners i) tokens)))
      (apply f args))))

(defn fmap
  ([f] (Functor0Parser. f))
  ([f p] (Functor1Parser. f p))
  ([f p p*] (Functor2Parser. f p p*))
  ([f p p* p**] (Functor3Parser. f p p* p**))
  ([f p p* p** & ps] (FunctorParser. f (object-array (list* p p* p** ps)))))

(defmacro plet [bindings & body]
  (let [binders (take-nth 2 bindings)
        actions (take-nth 2 (rest bindings))]
    (assert (= (count binders) (count actions)))
    `(fmap (fn [~@binders] ~@body) ~@actions)))

(deftype AlternativeParser [p p*]
  JascaParser
  (-probe [_ tokens]
    (let [probed (-probe p tokens)]
      (if (identical? probed :fail)
        (-probe p* tokens)
        probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (-probe p tokens) :fail)
        (-parse p* tokens)
        (-parse p tokens)))))

(def alt ->AlternativeParser)

(defmacro orp
  ([] `(fail "jasca.core/orp: out of alternatives" :orp-alternatives))
  ([p] p)
  ([p & ps] `(alt ~p (orp ~@ps))))

(deftype TokenParser [expected-token]
  JascaParser
  (-probe [_ tokens] (if (identical? (.currentToken ^JsonParser tokens) expected-token) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens
          token (.currentToken tokens)]
      (if (identical? token expected-token)
        (do (.nextToken tokens)
            token)
        (throw (ex-info "unsatisfactory token"
                        {:expected expected-token, :received token
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def tokenp ->TokenParser)

(def start-array (tokenp JsonToken/START_ARRAY))
(def end-array (tokenp JsonToken/END_ARRAY))

(def start-object (tokenp JsonToken/START_OBJECT))
(def end-object (tokenp JsonToken/END_OBJECT))

(def falsep (fmap (fn [_] false) (tokenp JsonToken/VALUE_FALSE)))
(def truep (fmap (fn [_] true) (tokenp JsonToken/VALUE_TRUE)))

(def booleanp (orp truep falsep))

(def nullp (fmap (fn [_] nil) (tokenp JsonToken/VALUE_NULL)))

;;; TODO: DRY:

(deftype IntParser []
  JascaParser
  (-probe [_ tokens] (if (identical? (.currentToken ^JsonParser tokens) JsonToken/VALUE_NUMBER_INT) :consuming :fail))
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
  (-probe [_ tokens] (if (identical? (.currentToken ^JsonParser tokens) JsonToken/VALUE_NUMBER_FLOAT) :consuming :fail))
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
  (-probe [_ tokens] (if (identical? (.currentToken ^JsonParser tokens) JsonToken/VALUE_STRING) :consuming :fail))
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

(deftype FieldNameParser []
  JascaParser
  (-probe [_ tokens] (if (identical? (.currentToken ^JsonParser tokens) JsonToken/FIELD_NAME) :consuming :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens]
      (if (identical? (.currentToken tokens) JsonToken/FIELD_NAME)
        (let [n (.getText tokens)]
          (.nextToken tokens)
          n)
        (throw (ex-info "unsatisfactory token"
                        {:expected JsonToken/FIELD_NAME, :received (.currentToken tokens)
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def field-name (FieldNameParser.))

(deftype FieldNameEqParser [goal-name]
  JascaParser
  (-probe [_ tokens]
    (if (and (identical? (.currentToken ^JsonParser tokens) JsonToken/FIELD_NAME)
             (= (.getText ^JsonParser tokens) goal-name))
      :consuming
      :fail))
  (-parse [self tokens]
    (let [^JsonParser tokens tokens
          n (.getText tokens)]
      (if (and (identical? (.currentToken tokens) JsonToken/FIELD_NAME)
               (= n goal-name))
        (do (.nextToken tokens)
            n)
        (throw (ex-info "unsatisfactory token"
                        {:expected [JsonToken/FIELD_NAME goal-name]
                         :received [(.currentToken tokens) (.getText tokens)]
                         :parser self, :location (.getCurrentLocation tokens)}))))))

(def field-name= ->FieldNameEqParser)

(defprotocol Fix
  (-fix [self f]))

(deftype FixParser [^:volatile-mutable inner]
  Fix
  (-fix [self f]
    (let [v (f self)]
      (set! inner v)
      v))

  JascaParser
  (-probe [_ tokens] (-probe inner tokens))
  (-parse [_ tokens] (-parse inner tokens)))

(defn fix [f] (-fix (FixParser. nil) f))

(extend-protocol JascaParser
  Var
  (-probe [self tokens] (-probe @self tokens))
  (-parse [self tokens] (-parse @self tokens)))

(defn opt [parser] (alt parser (pure nil)))

(deftype ManyReducingParser [f make-acc inner]
  JascaParser
  (-probe [_ tokens]
    (let [probed (-probe inner tokens)]
      (if (identical? probed :fail)
        :nonconsuming
        probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (loop [acc (make-acc)]
        (if (identical? (-probe inner tokens) :fail)
          acc
          (recur (f acc (-parse inner tokens))))))))

(def many-reducing ->ManyReducingParser)

(defn many [p]
  (->> (many-reducing conj! #(transient []) p)
       (fmap persistent!)))

(defn array-of [item]
  (plet [_ start-array
         items (many item)
         _ end-array]
    items))

(deftype ManyReducingKVParser [f make-acc kp vp]
  JascaParser
  (-probe [_ tokens]
    (let [probed (-probe kp tokens)]
      (condp identical? probed
        :fail :nonconsuming
        :nonconsuming (let [probed (-probe vp tokens)]
                        (if (identical? probed :fail)
                          :nonconsuming
                          probed))
        probed)))

  (-parse [_ tokens]
    (let [^JsonParser tokens tokens]
      (loop [acc (make-acc)]
        (condp identical? (-probe kp tokens)
          :fail acc
          :nonconsuming (if (identical? (-probe vp tokens) :fail)
                          acc
                          (let [k (-parse kp tokens)
                                v (-parse vp tokens)]
                            (recur (f acc k v))))
          (let [k (-parse kp tokens)
                v (-parse vp tokens)]
            (recur (f acc k v))))))))

(def many-reducing-kv ->ManyReducingKVParser)

(defn object-of [kp vp]
  (plet [_ start-object
         obj (->> (many-reducing-kv (fn [obj k v] (assoc! obj k v)) #(transient {})
                                    kp vp)
                  (fmap persistent!))
         _ end-object]
    obj))

(defn parse [parser ^JsonParser tokens]
  (.nextToken tokens)
  (-parse parser tokens))

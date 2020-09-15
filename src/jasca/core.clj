(ns jasca.core
  (:import [com.fasterxml.jackson.core JsonToken JsonParser]
           [clojure.lang Var]))

(defprotocol JascaParser
  (probe [self ^JsonToken token])
  (parse [self ^JsonParser tokens]))

(deftype FailParser [msg error]
  JascaParser
  (probe [_ _] :nonconsuming)
  (parse [self tokens]
    (throw (ex-info msg error
                    {:cause self, :location (.getCurrentLocation ^JsonParser tokens)}))))

(def fail ->FailParser)

(deftype PureParser [v]
  JascaParser
  (probe [_ _] :nonconsuming)
  (parse [_ _] v))

(def pure ->PureParser)

(deftype FunctorParser [f inner]
  JascaParser
  (probe [_ token] (probe inner token))
  (parse [_ tokens] (f (parse inner tokens))))

(def fmap ->FunctorParser)

(deftype ApplicativeParser [fp inner]
  JascaParser
  (probe [_ token] (probe inner token))
  (parse [_ tokens]
    (let [f (parse fp tokens)]
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

  (parse [_ tokens]
    (let [token (.currentToken ^JsonParser tokens)]
      (case (probe p token)
        :fail (parse p* tokens)
        (parse p tokens)))))

(def alt ->AlternativeParser)

(defmacro orp [& alts]
  (if (empty? alts)
    `(fail "orp: out of options")
    `(alt ~(first alts) (orp ~@(rest alts)))))

(deftype SatParser [pred]
  JascaParser
  (probe [_ token] (if (pred token) :consuming :fail))
  (parse [self tokens]
    (let [^JsonParser tokens tokens
          token (.nextToken tokens)]
      (if (pred token)
        token
        (ex-info "unsatisfactory token"
                 {:expected pred, :received token}
                 {:cause self, :location (.getCurrentLocation tokens)})))))

(def sat ->SatParser)

(extend-protocol JascaParser
  Var
  (probe [self token] (probe @self token))
  (parse [self tokens] (parse @self tokens)))

(defn opt [parser] (alt parser (pure nil)))

(deftype ManyParser [inner]
  JascaParser
  (probe [_ token]
    (let [probed (probe inner token)]
      (case probed
        :fail :nonconsuming
        probed)))

  (parse [_ tokens]
    (loop [vs []]
      (case (probe inner (.currentToken ^JsonParser tokens))
        :fail vs
        (recur (conj vs (parse inner tokens)))))))

(def many ->ManyParser)

(ns jasca.impl.grammar
  (:require [clojure.set :as set])
  (:import (clojure.lang IPersistentVector Keyword)
           (com.fasterxml.jackson.core JsonToken)))

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

;;;; # Grammar AST

(deftype Terminal [lookaheads token]
  Firsts
  (-firsts* [_ _] #{token})

  Follows
  (-follows* [_ _ _ nt-follows] nt-follows)

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ _ _ _] (Terminal. #{token} token)))

(defn- terminal [token] (Terminal. nil token))

(deftype NonTerminal [lookaheads name]
  Firsts
  (-firsts* [_ nt-firsts] (get nt-firsts name))

  Follows
  (-follows* [_ follows _ nt-follows] (update nt-follows name set/union follows))

  Lookaheads
  (get-lookaheads [_] lookaheads)
  (with-lookaheads [_ nt-firsts nt-follows _]
    (NonTerminal. (->lookaheads (get nt-firsts name) (get nt-follows name)) name)))

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
      (Functor. lookaheads f (vec args)))))

(defn- fmap [f args] (Functor. nil f args))

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
      (Alt. lookaheads alts))))

(defn- alt [alts] (Alt. nil alts))

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
          (throw (RuntimeException. (str "Invalid grammar operator: " op)))))
      (syntax-error coll)))

  Keyword
  (-analyze [kw grammar]
    (if (contains? grammar kw)
      (nonterminal kw)
      (throw (RuntimeException. (str "Nonterminal " kw " does not exist in grammar")))))

  Character
  (-analyze [c _]
    (terminal (case c
                \{ JsonToken/START_OBJECT
                \} JsonToken/END_OBJECT
                \[ JsonToken/START_ARRAY
                \] JsonToken/END_ARRAY
                (syntax-error c))))

  Boolean
  (-analyze [b _] (terminal (if b JsonToken/VALUE_TRUE JsonToken/VALUE_FALSE)))

  nil
  (-analyze [_ _] (terminal JsonToken/VALUE_NULL)))

(defn analyze-grammar [grammar]
  (if (map? grammar)
    (into {} (map (fn [[k v]] [k (analyze grammar v)]))
          grammar)
    (throw (RuntimeException. "Grammar is not a map"))))

;;;;

(comment
  (def generic
    {:value   [:or :object :array :boolean :null]
     :object  [:-> \{ \} (fn [_ _] {})]
     :array   [:-> \[ \] (fn [_ _] [])]
     :boolean [:or true false]
     :null    nil}))

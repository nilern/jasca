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

(defn grammar-firsts [grammar]
  (loop [nt-firsts (zipmap (keys grammar) (repeat #{}))]
    (let [nt-firsts* (into {} (map (fn [[name production]] [name (firsts* nt-firsts production)]))
                           grammar)]
      (if (= nt-firsts* nt-firsts)
        nt-firsts
        (recur nt-firsts*)))))

;;;; # Grammar AST

(extend-protocol Firsts
  JsonToken
  (-firsts* [token _] #{token}))

(defrecord NonTerminal [name]
  Firsts
  (-firsts* [_ nt-firsts] (get nt-firsts name)))

(defrecord Functor [f args]
  Firsts
  (-firsts* [_ nt-firsts]
    (reduce (fn [firsts arg]
              (if (contains? firsts epsilon)
                (set/union (disj firsts epsilon) (firsts* nt-firsts arg))
                (reduced firsts)))
            #{epsilon} args)))

(defrecord Alt [alts]
  Firsts
  (-firsts* [_ nt-firsts]
    (transduce (map #(firsts* % nt-firsts)) set/union
               #{} alts)))

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
                    (Functor. f (mapv #(analyze grammar %) (butlast args)))
                    (throw (RuntimeException. (str "Last arg of " op " is not an IFn")))))
                (throw (RuntimeException. (str "Empty " op " args"))))

          :or (if (seq args)
                (Alt. (mapv #(analyze grammar %) args))
                (throw (RuntimeException. (str "Empty " op " args"))))
          (throw (RuntimeException. (str "Invalid grammar operator: " op)))))
      (syntax-error coll)))

  Keyword
  (-analyze [kw grammar]
    (if (contains? grammar kw)
      (NonTerminal. kw)
      (throw (RuntimeException. (str "Nonterminal " kw " does not exist in grammar")))))

  Character
  (-analyze [c _]
    (case c
      \{ JsonToken/START_OBJECT
      \} JsonToken/END_OBJECT
      \[ JsonToken/START_ARRAY
      \] JsonToken/END_ARRAY
      (syntax-error c)))

  Boolean
  (-analyze [b _] (if b JsonToken/VALUE_TRUE JsonToken/VALUE_FALSE))

  nil
  (-analyze [_ _] JsonToken/VALUE_NULL))

(defn analyze-grammar [grammar]
  (if (map? grammar)
    (into {} (map (fn [[k v]] [k (analyze grammar v)]))
          grammar)
    (throw (RuntimeException. "Grammar is not a map"))))

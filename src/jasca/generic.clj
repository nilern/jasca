(ns jasca.generic
  (:refer-clojure :exclude [read-string])
  (:require [jasca.core :as core :refer [plet orp]]
            [jasca.impl.grammar :as impl])
  (:import [com.fasterxml.jackson.core JsonFactory JsonToken]))

(def skip-value
  (core/fix (fn [skip-value]
              (orp (core/object-of identity skip-value)
                   (core/array-of skip-value)
                   (core/tokenp JsonToken/VALUE_STRING)
                   (core/tokenp JsonToken/VALUE_NUMBER_INT)
                   (core/tokenp JsonToken/VALUE_NUMBER_FLOAT)
                   core/truep
                   core/falsep
                   core/nullp))))

(def ^JsonFactory +factory+ (JsonFactory.))

(def grammar
  {:value [:or :object :array :string :int :float :boolean :null]
   :object [:object-of identity :value]
   :array [:-> \[ [:* :value] \] (fn [_ vs _] vs)]
   :string String
   :int Long
   :float Double
   :boolean [:or true false]
   :null nil})

(def read-value
  (let [parser (-> grammar (impl/parsers [:value]) first)]
    (fn [^String s]
      (parser (.createParser +factory+ s)))))

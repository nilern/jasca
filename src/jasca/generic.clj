(ns jasca.generic
  (:refer-clojure :exclude [read-string])
  (:require [jasca.core :as core :refer [plet orp]])
  (:import [com.fasterxml.jackson.core JsonFactory JsonToken]))

(def value
  (core/fix (fn [value]
              (orp (core/object-of core/field-name value)
                   (core/array-of value)
                   core/stringp
                   core/intp
                   core/floatp
                   core/truep
                   core/falsep
                   core/nullp))))

(def skip-value
  (core/fix (fn [skip-value]
              (orp (core/object-of (core/tokenp JsonToken/FIELD_NAME) skip-value)
                   (core/array-of skip-value)
                   (core/tokenp JsonToken/VALUE_STRING)
                   (core/tokenp JsonToken/VALUE_NUMBER_INT)
                   (core/tokenp JsonToken/VALUE_NUMBER_FLOAT)
                   core/truep
                   core/falsep
                   core/nullp))))

(def ^JsonFactory +factory+ (JsonFactory.))

(defn read-value [^String s]
  (core/parse value (.createParser +factory+ s)))

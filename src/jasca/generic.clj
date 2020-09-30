(ns jasca.generic
  (:refer-clojure :exclude [read-string])
  (:require [jasca.core :as core :refer [plet orp]])
  (:import [com.fasterxml.jackson.core JsonFactory]))

(declare objectp arrayp)

(def value
  (orp #'objectp
       #'arrayp
       core/stringp
       core/intp core/floatp
       core/truep
       core/falsep
       core/nullp))

(def arrayp (core/array-of value))

(def objectp (core/object-of core/field-name value))

(declare skip-object skip-array)

(def skip-value
  (orp #'skip-object
       #'skip-array
       core/stringp
       core/intp core/floatp
       core/truep
       core/falsep
       core/nullp))

(def skip-array
  (plet [_ core/start-array
         _ (core/many-reducing (fn [_ _] nil) (fn [] nil) skip-value)
         _ core/end-array]
    nil))

(def skip-object
  (plet [_ core/start-object
         _ (core/many-reducing-kv (fn [_ _ _] nil) (fn [] nil) core/field-name skip-value)
         _ core/end-object]
    nil))

(def ^JsonFactory +factory+ (JsonFactory.))

(defn read-value [^String s]
  (core/parse value (.createParser +factory+ s)))

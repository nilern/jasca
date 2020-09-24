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

(def objectp
  (plet [_ core/start-object
         obj (->> (core/many-reducing-kv (fn [obj k v] (assoc! obj k v)) #(transient {})
                                         core/field-name value)
                  (core/fmap persistent!))
         _ core/end-object]
    obj))

(def ^JsonFactory +factory+ (JsonFactory.))

(defn read-value [^String s]
  (core/parse value (.createParser +factory+ s)))

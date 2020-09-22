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

(def member
  (plet [k core/field-name
         v value]
    [k v]))

(def objectp
  (plet [_ core/start-object
         kvs (core/many member)                             ; OPTIMIZE
         _ core/end-object]
    (into {} kvs)))

(def ^JsonFactory +factory+ (JsonFactory.))

(defn read-value [^String s]
  (core/parse value (.createParser +factory+ s)))

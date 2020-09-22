(ns jasca.generic
  (:require [jasca.core :as core :refer [plet orp]]))

(declare objectp arrayp)

(def value
  (orp #'objectp
       #'arrayp
       core/stringp
       core/intp core/floatp
       core/truep
       core/falsep
       core/nullp))

(def arrayp (array-of value))

(def member
  (plet [k core/field-name
         v value]
    [k v]))

(def objectp
  (plet [_ core/start-object
         kvs (core/many member)                             ; OPTIMIZE
         _ core/end-object]
    (into {} kvs)))

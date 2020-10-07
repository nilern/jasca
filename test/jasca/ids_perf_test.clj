(ns jasca.ids-perf-test
  (:require [clojure.test :refer :all]
            [cheshire.core :as cheshire]
            [jsonista.core :as j]
            [jasca.core :as core :refer [plet orp]]
            [jasca.generic :as generic]
            [criterium.core :as cc])
  (:import [com.fasterxml.jackson.core JsonToken]))

(defn- ids-cheshire [s]
  (->> (cheshire/parse-string s true)
       :results
       (mapv (fn [user] (-> user :id :value)))))

(defn- ids-jsonista [s]
  (->> (j/read-value s j/keyword-keys-object-mapper)
       :results
       (mapv (fn [user] (-> user :id :value)))))

(def ^:private ids-parser
  (let [sentinel (object-array 0)
        braces (fn [p]
                 (plet [_ core/start-object
                        v p
                        _ core/end-object]
                   v))
        fieldp (fn [kp vp]
                 (orp (plet [_ kp
                             v vp]
                        v)
                      (plet [_ (core/tokenp JsonToken/FIELD_NAME)
                             _ generic/skip-value]
                        sentinel)))
        obj-field (fn [kp vp]
                    (braces (core/many-reducing (fn [ids ids*] (if (identical? ids sentinel) ids* ids))
                                                (fn [] sentinel)
                                                (fieldp kp vp))))]
    (obj-field (core/field-name= "results")
               (core/array-of (obj-field (core/field-name= "id")
                                         (obj-field (core/field-name= "value")
                                                    (orp core/stringp core/nullp)))))))

(defn decode-perf-size [size]
  (let [file (str "dev-resources/json" size ".json")
        data (cheshire/parse-string (slurp file))
        json (cheshire/generate-string data)]

    (println file)

    (println "decode ids: cheshire")
    (cc/quick-bench (ids-cheshire json))

    (println "decode ids: jsonista")
    (cc/quick-bench (ids-jsonista json))

    (println "decode ids: jasca")
    (cc/quick-bench (core/parse ids-parser (.createParser generic/+factory+ json)))))

(defn decode-perf-different-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (decode-perf-size size)))

(ns jasca.custom-decode-perf-test
  (:require [clojure.test :refer :all]
            [cheshire.core :as cheshire]
            [jsonista.core :as j]
            [jasca.impl.grammar :as impl]
            [jasca.generic :as generic]
            [criterium.core :as cc])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn- parse-hex-string [^String s] (bigint (BigInteger. s 16)))

(def ^:private parse-randomuser-instant
  (let [formatter (DateTimeFormatter/ofPattern "uuuu-MM-dd HH:mm:ss")]
    (fn [s] (LocalDateTime/parse s formatter))))

(defn- postprocess-location [location] (update location :postcode str))

(defn- postprocess-login [login]
  (-> login
      (dissoc :password :salt)
      (update :md5 parse-hex-string)
      (update :sha1 parse-hex-string)
      (update :sha256 parse-hex-string)))

(defn- postprocess-user [user]
  (-> user
      (update :location postprocess-location)
      (update :login postprocess-login)
      (update :dob parse-randomuser-instant)
      (update :registered parse-randomuser-instant)))

(defn- decode-cheshire [s]
  (->> (cheshire/parse-string s true)
       :results
       (mapv postprocess-user)))

(defn- decode-jsonista [s]
  (->> (j/read-value s j/keyword-keys-object-mapper)
       :results
       (mapv postprocess-user)))

(def ^:private decode-jasca
  (let [grammar (assoc generic/grammar
                  :hex-hash [:-> String parse-hex-string]
                  :randomuser-instant [:-> String parse-randomuser-instant]

                  :user-id [:object keyword {:name String, :value String}]

                  :login [:-> [:object keyword
                               {:username String
                                :password String
                                :salt String
                                :md5 :hex-hash
                                :sha1 :hex-hash
                                :sha256 :hex-hash}]
                          #(dissoc % :password :salt)]

                  :user-name [:object keyword
                              {:title String
                               :first String
                               :last String}]

                  :location [:object keyword
                             {:street String
                              :city String
                              :state String
                              :postcode [:-> Long str]}]

                  :user [:object keyword
                         {:id :user-id
                          :login :login
                          :name :user-name
                          :gender String
                          :dob :randomuser-instant
                          :registered :randomuser-instant
                          :nat String
                          :location :location
                          :email String
                          :phone String
                          :cell String
                          :picture [:object keyword
                                    {:large String
                                     :medium String
                                     :thumbnail String}]}]

                  :response [:-> [:object {"results" [:array-of :user]
                                           "info" :value}]  ; TODO: :skip-value
                             #(get % "results")])]
    (-> grammar (impl/parsers [:response]) first)))

(defn decode-perf-size [size]
  (let [file (str "dev-resources/json" size ".json")
        data (cheshire/parse-string (slurp file))
        json (cheshire/generate-string data)]

    (println file)

    (println "decode ids: cheshire")
    (cc/quick-bench (decode-cheshire json))

    (println "decode ids: jsonista")
    (cc/quick-bench (decode-jsonista json))

    (println "decode ids: jasca")
    (cc/quick-bench (decode-jasca (.createParser generic/+factory+ json)))))

(defn decode-perf-different-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (decode-perf-size size)))

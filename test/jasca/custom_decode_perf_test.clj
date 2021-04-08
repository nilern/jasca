(ns jasca.custom-decode-perf-test
  (:require [clojure.test :refer :all]
            [cheshire.core :as cheshire]
            [jsonista.core :as j]
            [jasca.impl.grammar :as impl]
            [jasca.generic :as generic]
            [malli.core :as m]
            [criterium.core :as cc])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def ^:private LocalDatetime
  (m/-simple-schema {:type :local-date-time, :pred #(instance? LocalDateTime %)}))

(def ^:private UserId
  (m/schema
    [:map {:closed true}
     [:name :string]
     [:value [:maybe :string]]]))

(def ^:private Login
  (m/schema
    [:map {:closed true}
     [:username :string]
     [:md5 integer?]
     [:sha1 integer?]
     [:sha256 integer?]]))

(def ^:private UserName
  (m/schema
    [:map {:closed true}
     [:title :string]
     [:first :string]
     [:last :string]]))

(def ^:private Location
  (m/schema
    [:map {:closed true}
     [:street :string]
     [:city :string]
     [:state :string]
     [:postcode :string]]))

(def ^:private User
  (m/schema
    [:map {:closed true}
     [:email :string]
     [:phone :string]
     [:name UserName]
     [:nat :string]
     [:login Login]
     [:dob LocalDatetime]
     [:id UserId]
     [:picture [:map {:closed true}
                [:large :string]
                [:medium :string]
                [:thumbnail :string]]]
     [:gender :string]
     [:registered LocalDatetime]
     [:cell :string]
     [:location Location]]))

(def ^:private validate-users (m/validator [:vector User]))

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
  (let [users (->> (cheshire/parse-string s true)
                   :results
                   (mapv postprocess-user))]
    (assert (validate-users users))
    users))

(defn- decode-jsonista [s]
  (let [users (->> (j/read-value s j/keyword-keys-object-mapper)
                   :results
                   (mapv postprocess-user))]
    (assert (validate-users users))
    users))

(def ^:private decode-jasca
  (let [grammar (assoc generic/grammar
                  :hex-hash [:-> String parse-hex-string]
                  :randomuser-instant [:-> String parse-randomuser-instant]

                  :user-id [:object keyword {:name String
                                             :value [:or String nil]}]

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
                              :postcode [:or String [:-> Long str]]}]

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

    (println "custom decode: cheshire")
    (cc/quick-bench (decode-cheshire json))

    (println "custom decode: jsonista")
    (cc/quick-bench (decode-jsonista json))

    (println "custom decode: jasca")
    (cc/quick-bench (decode-jasca (.createParser generic/+factory+ json)))))

(defn decode-perf-different-sizes []
  (doseq [size ["1k" "10k" "100k"]]
    (decode-perf-size size)))
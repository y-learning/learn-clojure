(ns learn-clojure.atomcache)

(defn create
  []
  (atom {}))

(defn get-key
  [cache key]
  (key @cache))

(defn put
  ([cache value-map]
   (swap! cache merge value-map))
  ([cache key value]
   (swap! cache assoc key value)))

(def accounts (ref [{:id 11 :balance 0}
                    {:id 12 :balance 160}
                    {:id 13 :balance 56}]))

(defn- credit-
  [accs acc-id amount]
  (vec
    (map #(if (= acc-id (:id %))
            (assoc % :id acc-id :balance (+ (:balance %) amount))
            %)
         accs)))

(defn withdraw [balance amount]
  (if (> balance amount)
    (- balance amount)
    balance))

(defn- debit-
  [accs id amount]
  (vec
    (map #(if (= id (:id %))
            (assoc % :id id :balance (withdraw (:balance %) amount))
            %)
         accs)))

(defn credit
  [account-id amount]
  (dosync (alter accounts credit- account-id amount)))


(defn debit
  [account-id amount]
  (dosync (alter accounts debit- account-id amount)))
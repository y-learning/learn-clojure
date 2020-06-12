(ns potion-store.order-details
  (:gen-class))

(def order-details
  {:name  "Why Drago"
   :email "whydrago@gmail.com"})


(def order-details-validations
  {:name  ["Please enter a name" not-empty]
   :email ["Please enter an email address" not-empty
           "Your email address doesn't look like an email address"
           #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[field-key validation-rules] validation
                  value (get to-validate field-key)
                  error-messages (error-messages-for value validation-rules)]
              (if (empty? error-messages)
                errors
                (assoc errors field-key error-messages))))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(defmacro when-valid
  [to-validate validations & then-else]
  `(if (empty? (validate ~to-validate ~validations))
     (do ~@then-else)))

(defmacro -or
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (-or ~@next)))))
(ns learn-clojure.core
  (:gen-class)
  (:import (clojure.lang PersistentVector PersistentList PersistentArrayMap
                         PersistentHashSet)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defmacro report
  [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful:" result#)
       (println (quote ~to-try) "was not successful:" result#))))

(defmacro doseq-macro [macroname & codes]
  `(do
     ~@(map (fn [arg] (list macroname arg)) codes)))

;;

(def yak-butter-international
  {:store      "Yak Butter International"
   :price      90
   :smoothness 90})

(def butter-than-nothing
  {:store      "Butter Than Nothing"
   :price      150
   :smoothness 83})

(def baby-got-yak
  {:store      "Baby Got Yak"
   :price      94
   :smoothness 99})

(defn api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

;(time
;  (let [butter-promise (promise)]
;    (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
;      (future (if-let [satisfactory-butter (satisfactory? (api-call butter))]
;                (deliver butter-promise satisfactory-butter))))
;    (println "And the winner is:" (deref butter-promise 1000 "timeout!"))))

(defmacro wait
  "Sleep `timeout` seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

;(let [saying3 (promise)]
;  (future (deliver saying3 (wait 100 "Cheerio!")))
;  @(let [saying2 (promise)]
;     (future (deliver saying2 (wait 400 "Pip pip!")))
;     @(let [saying1 (promise)]
;        (future (deliver saying1 (wait 200 "'Ello, gov'na!")))
;        (println @saying1)
;        saying1)
;     (println @saying2)
;     saying2)
;  (println @saying3)
;  saying3)

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

;(-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
;    (enqueue saying (wait 400 "Pip pip!") (println @saying))
;    (enqueue saying (wait 100 "Cheerio!") (println @saying)))

(def default-search-engines {:bing  "https://bing.com/search?q="
                             :yahoo "https://search.yahoo.com/search?p="})

(defn search
  ([subject search-engines]
   (let [result (promise)]
     (doseq [search-engine search-engines]
       (future (deliver result (slurp (str (val search-engine) subject)))))
     @result))

  ([subject] (search subject default-search-engines)))

(defn get-urls
  [source]
  (re-seq #"https?://[^\"]*" source))

(defn search-promise [subject search-engine]
  (let [result (promise)]
    (future (deliver result (slurp (str (val search-engine) subject))))
    result))

(defn search-promises [subject search-engines]
  (map #(search-promise subject %) search-engines))

(defn search-v2 [subject search-engines]
  (vec (flatten (map #(get-urls (deref %))
                     (search-promises subject search-engines)))))

(def power-source "hair")

(alter-var-root
  #'power-source
  (fn [_] "7-eleven parking lot"))

;;;;;;;;;;;;;;; {Polymorphism} ;;;;;;;;;;;;;;;;;;;

;; defmulti
(defmulti coll-type
          (fn [x] (type x)))

(defmethod coll-type PersistentVector [coll] :vector)

(defmethod coll-type PersistentList [coll] :list)

(defmethod coll-type PersistentArrayMap [coll] :map)

(defmethod coll-type PersistentHashSet [coll] :set)

;; Protocols
(defprotocol CollType
  (coll-type-v2 [_]))

(extend-protocol CollType
  PersistentVector
  (coll-type-v2 [_] :vector)

  PersistentList
  (coll-type-v2 [_] :list)

  PersistentArrayMap
  (coll-type-v2 [_] :map))

(extend-protocol CollType
  PersistentHashSet
  (coll-type-v2 [_] :set!))

(defprotocol Compass
  (direction [c])
  (left [c])
  (right [c]))

(def directions [:north :east :south :west])

(defn turn [base amount]
  (rem (+ base amount) (count directions)))

(defrecord SimpleCompass [bearing]
  Compass

  (direction [_] (directions bearing))
  (left [_] (SimpleCompass. (turn bearing 3)))
  (right [_] (SimpleCompass. (turn bearing 1)))

  Object
  (toString [this] (str "[" (direction this) "]")))

(def c (SimpleCompass. 0))

(defmacro unless [test this else]
  (list 'if (list 'not test) this else))
(ns clj-logic.datascript
  (:refer-clojure :exclude [==])
  (:require [datascript.db :as db]
            [datascript.core :as d])
   (:use [clojure.core.logic.protocols]
         [clojure.core.logic]))

(defn unify-with-datom* [u v s]
  (when (and (instance? clojure.lang.PersistentVector v)
             (> (count v) 1))
    (loop [i 0 v v s s]
      (if (empty? v)
        s
        (when-let [s (unify s (first v) (nth u i))]
          (recur (inc i) (next v) s))))))

(extend-type datascript.db.Datom
  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-datom* u v s)))

 (extend-type clojure.lang.PersistentVector
     IUnifyTerms
     (unify-terms [u v s]
       (if (d/datom? v)
         (unify-with-datom* v u s)
         (when (sequential? v)
           (unify-with-sequential* u v s)))))

(defn query [db q]
  (fn [a] (to-stream
           (map #(unify a q %) (d/datoms db :eavt)))))

;; demo

(comment

(def conn (->> {:aka {:db/cardinality :db.cardinality/many}}
            d/create-conn))


(d/transact! conn [{:db/id -1
                    :name  "Maksim"
                    :age   45
                    :aka   ["Maks Otto von Stirlitz", "Jack Ryan"]}])

(= (d/q '[:find  ?n ?a
          :where [?e :aka "Maks Otto von Stirlitz"]
          [?e :name ?n]
          [?e :age  ?a]]
        @conn)
   #{["Maksim" 45]})  
  
  
(= (run* [q]
     (fresh [e a v t]
       (== q v)
       (== (first (d/datoms @conn :eavt)) [e a v t] )))
   '(45))

(run* [q]
  (fresh  [e a v t]
          (query @conn [e a v t])
          (== q [e a v t])))

(run* [e a v t]
      (query @conn [e a v t]))

(run* [q]
  (fresh  [e a v t]
          (== v 45)
          (query @conn [e a v t])
          (== q [e a v t])))

)

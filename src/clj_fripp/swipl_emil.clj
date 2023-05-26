(ns 
  ^{:author "Peter Feldtmann"
    :doc "Demo code for the Clojure SWI-Prolog bridge."}
  clj.swipl.emil
  (:require [clj.swipl.core :as pl]
            [clj.swipl.protocols :as p]) 
  (:use clojure.pprint clojure.repl))


;;------------------------------------------------------------------------
;; emil test functions
;;------------------------------------------------------------------------

(defn emil2
  []
  (pl/consult "D:/ws/prolog/emil/emilTests.pl")
  (let [query     (p/build-q "test2(Varianten).")
        solutions (p/run-q query)]
    (println "emil " (p/pl-to-text query)  " ==> " 
             (map p/pl-to-text solutions)))
  
  )
;;------------------------------------------------------------------------
;; run emil
;;------------------------------------------------------------------------

(emil2)

;; EOF

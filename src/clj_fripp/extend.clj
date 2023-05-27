(ns clj-fripp.extend
  "Extensions to core.logic."
  (:require [clojure.core.logic :as cl]
            [clojure.core.logic.pldb :as pldb]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.set :as cset])
   #_(:use clojure.pprint clojure.repl)
  )

;(defmacro ifo
;  [conditions thens elses]
;  `(cl/or* [(cl/and* [~@conditions ~@thens]) ~@elses]))
;  

;; adapted from  "odin" query-var?, body-lvars 
(defn is-fresh-var? [v]
  (and (symbol? v)
       (not (namespace v))
       (str/starts-with? (name v) "?")))

(defn transform-body-to-fresh [body]
  (let [lvars (atom #{})
        transformed-body (walk/postwalk
                           (fn [v]
                             (cond
                               (or (= v '_)(= v '?_))
                                  `(cl/lvar)
                               (is-fresh-var? v)
                                  (do (swap! lvars conj v)
                                      v)
                               :else v))
                           body)]
    `(cl/fresh ~(vec @lvars) ~@transformed-body)))

;; Solving stuff

(defn get-fresh-lvars 
  "Deduce fresh logical variables by ?xxx names."
  [bindings goals]
  (vec (cset/difference
         (set (filter #(and (symbol? %)(.startsWith (name %) "?" ))
                      (flatten goals)))
         (set bindings))))

#_(defn get-project-vars
  "Deduce logical vars for project function."
  [goals]
  )

(defn- print-log-item [a item]
  `(if (cl/lvar? ~item)
     (print (cl/-reify ~a ~item))
     (print ~item)))

(defmacro log-info
  "Goal for println with lvar projection."
  [ & s]
  (let [a (gensym "a")]
    `(fn [~a]
       ~@(map (partial print-log-item a) s)
       (println)
       ~a)))


(defmacro solve
  "Like fresh, but automatic definition of fresh logical variables 
   by their ?xyz names."
  [& goals]
  `(cl/fresh ~(get-fresh-lvars #{} goals) ~@goals))

(defmacro infer-n
  "Infer n solutions."
  [n bindings & goals]
  (let [vars# (get-fresh-lvars bindings goals)]
    (if (empty? vars#)
       `(cl/-run {:occurs-check true :n ~n :db cl/*logic-dbs*} ~bindings ~@goals)
       `(cl/-run {:occurs-check true :n ~n :db cl/*logic-dbs*} ~bindings 
              (cl/fresh ~vars# ~@goals)))))

(defmacro infer
  "Infer all solutions."
  [& goals]
  (let [bindings# (get-fresh-lvars nil goals)]
       `(cl/-run {:occurs-check true :n nil :db cl/*logic-dbs*} ~bindings# ~@goals)
       ))

(defmacro infer-1
  "Infer first solution."
  [& goals]
  (let [bindings# (get-fresh-lvars nil goals)]
       `(first(cl/-run {:occurs-check true :n 1 :db cl/*logic-dbs*} ~bindings# ~@goals)
       )))

(defmacro infer-n-ex
  "Infer n solutions."
  [n & goals]
  (let [bindings# (get-fresh-lvars nil goals)]
       `(cl/-run {:occurs-check true :n ~n :db cl/*logic-dbs*} ~bindings# ~@goals)
       ))

(defmacro find-all
  "collect all solutions of goal in logic var 'sols'.
   Uses the current state of the dbs." 
  [q goal sols]
  `(fn [a#]
     (cl/unify a# ~sols (cl/run-db* (-> a# meta :db) [~q] ~goal))))

(defmacro find-n-sols
  "collect first n solutions of goal in logic var 'sols'.
   Uses the current state of the dbs." 
  [n q goal sols]
  `(fn [a#]
     (cl/unify a# ~sols (cl/run-db ~n (-> a# meta :db) [~q] ~goal))))

(defmacro instantiate-sols 
  [& lis]
  `(first (doall ~@lis)))


;; Extensions - mostly non-relational

(defn noto 
  "logical negation."
  [goal]
  (cl/conda
    [goal cl/u#]
    [cl/s#]))

(defn ignoreo
  "ignore result, but retain substitutions and always succeed."
  [goal]
  (fn [orig-a]
    (if-let [new-a (goal orig-a)]
      new-a
      orig-a) 
    ))

(defmacro doo
   "Goal for executing side-effecting clojure code. Always succeeds."
  [& s]
  `(fn [a#]
     (do ~@s)
     a#))

(defmacro do-falseo
   "Goal for executing side-effecting clojure code. Always fails."
  [& s]
  `(fn [a#]
     (do ~@s)
     cl/u#))

(defmacro do-truthyo
   "Goal for executing side-effecting clojure code. Succeeds or fails 
    depending on the truthy result of the last do statement."
  [& s]
  `(fn [a#]
     (if (do ~@s)
       a#
       cl/u#)))

(defn trueo 
  "Succeed if called clojure code yields true."
  [code]
  (cl/== true code)) 

(defn falseo 
   "Succeed if called clojure code yields false."
  [code]
  (cl/== false code)) 

(defn truthyo 
  "Succeed if called clojure code yields logical true value."
  [code]
  (cl/== true (boolean code))) 

(defn falseyo 
  "Succeed if called clojure code yields logical false value (false or nil)."
  [code]
  (cl/== false (boolean code))) 


(defmacro asserto
  "do fact assertion as a goal." 
  [& fact]
  `(fn [a#]
     (vary-meta a# assoc-in [:db] (list (pldb/db-fact (-> a# meta :db first)  ~@fact)))
     ))

(defmacro retracto
  "do fact retraction as a goal." 
  [& fact]
  `(fn [a#] 
    (vary-meta a# assoc-in [:db] (list (pldb/db-retraction (-> a# meta :db first) ~@fact)))
    )) 

(defn retract-rel
  "delete relation with name and arity."
  [db name arity] 
  (dissoc db (str (ns-name *ns*) "/" name "_" arity)))

(defmacro retract-allo
  "do relation retraction as a goal." 
  [name arity]
  `(fn [a#] 
    (vary-meta a# assoc-in [:db] (list (retract-rel (-> a# meta :db first) ~name ~arity )))
    )) 

(defn traceo-s-meta 
  "Goal that prints the meta data of the current substitution."
  []
  (fn [a]
    (println "s-meta: " (meta a))
    a))

; emulating global vars from SWI-Prolog: nb_setval/2, nb_getval/2

(def ^:dynamic global-values (atom {}))

(defn init-valueso
  "Initialize the global value stack."
  []
   (fn [a]
     (reset! global-values {})
     a))

(defn set-valo
  "Set global value."
  [key value]
  (fn [a]
     (swap! global-values assoc key value)
     a))

(defn get-val
  "Get global value. Non-logical."
  [key]
  (key @global-values))

(defn get-valo
  "Get global value. Logical."
  [key value]
  (fn [a]
     (cl/unify a value (get-val key))))

(defn inc-valo
  "Increment (numeric) global value."
  [key]
  (fn [a]
    (swap! global-values assoc key (-> @global-values key inc))
     a))

(defn dec-valo
  "Decrement (numeric) global value."
  [key]
  (fn [a]
    (swap! global-values assoc key (-> @global-values key dec))
     a))


;; emulating flag(key, -OldValue, +NewValue) from SWI-Prolog

(defn set-flago 
  "Set key with value. Key must be a keyword."
  [key value]
  (fn [a]
     (vary-meta a assoc-in [:flags key] value)))

(defn get-flago 
  "Get value of key. Key must be a keyword."
  [key value]
  (fn [a]
     (cl/unify a value (-> a meta :flags key))))

(defn inc-flago 
  "Set key to value + 1. Key must be a keyword."
  [key]
  (fn [a]
     (vary-meta a assoc-in [:flags key] (inc (-> a meta :flags key)))))

(defn dec-flago 
  "Set key to value - 1. Key must be a keyword."
  [key]
  (fn [a]
     (vary-meta a assoc-in [:flags key] (dec (-> a meta :flags key)))))


;;------------------------------------------------------------------

;; examples
(comment

  (declare ?1 ?2)

  (infer-1 (cl/conso ?1 ?2 (cl/lcons 1 2)))
  ;;==> [1 2]
  
  (infer (cl/appendo ?1 ?2 '(1 2 3)))
  ;; ([() (1 2 3)] [(1) (2 3)] [(1 2) (3)] [(1 2 3) ()])
  )


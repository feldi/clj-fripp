(ns 
  ^{:author "Peter Feldtmann"
    :doc "A Clojure SWI-Prolog bridge.
          Call prolog goals directly from clojure code."}
  clj-logic.jpl
  (:use clojure.pprint clojure.repl)
  (:import [jpl JPL Atom Compound JPLException
            JRef PrologException Query Term
            Util Variable])
  )

; to install the swi-prolog java bridge jpl.jar:
; get and install the leiningen plugin 'localrepo'
; lein localrepo install 'path-to-swi-prolog'/lib/jpl.jar jpl 3.1.4-alpha

(set! *warn-on-reflection* true)

;; JPL main class

(defn get-version-as-string
  "Get jpl version." 
  []
  (JPL/version_string))

(defn init
  "Explicit initialization. Most often not needed."
  []
  (JPL/init))

(defn init-with-args
  "Explicit initialization with list of arguments, 
   provided as a vector. Most often not needed."
  [args-list]
  (JPL/init (into-array String args-list)))

(defn get-default-init-args
  []
  (into [] (JPL/getDefaultInitArgs)))

(defn set-default-init-args!
  [args-list]
  (JPL/setDefaultInitArgs (into-array String args-list)))

(defn get-actual-init-args
  []
  (into [] (JPL/getActualInitArgs)))

(defn set-dtm-mode!
  "Set or unset 'dont-tell-me' mode."
  [^Boolean mode]
  (JPL/setDTMMode mode))

(defn tag? 
  [^String s]
  (JPL/isTag s))


;; Term

(defn show-term
  [^Term term]
  (.toString term))

(defn ^String get-name
  [^Term term]
  (.name term))

;; Compound

(defn make-compound
  [name terms]
  (Compound. name (into-array Term terms)))

(defn ^String get-name
  [^Compound c]
  (.name c))

(defn ^int get-arity
  [^Compound c]
  (.arity c))

(defn ^Term get-ith-arg
  [^Compound c înt i]
  (.arg c i))


;; Atom

(defn make-atom
  [^String s]
  (Atom. s))

(defn ^int get-atom-type
  [^Atom a]
  (.type a))

(defn ^String get-atom-type-name
  [^Atom a]
  (.typeName a))


;; Variable

(defn make-variable
  ([]
    (Variable.))
  ([^String name]
    (Variable. name)))


;; Integer

(defn make-integer
  [^long l]
  (jpl.Integer. l))

(defn get-value
  [^jpl.Integer f]
  (.longValue f))

(defn get-long-value
  [^jpl.Integer i]
  (.longValue i))

(defn get-int-value
  [^jpl.Integer i]
  (.intValue i))


;; Float

(defn make-float
  [^double d]
  (jpl.Float. d))

(defn get-double-value
  [^jpl.Float f]
  (.doubleValue f))

(defn get-float-value
  [^jpl.Float f]
  (.floatValue f))


;; Query

(defn make-query-from-source
  "build a new query from prolog source text."
  [^String source]
  (Query. source))

(defn make-query-from-term
  "build a new query from a single term (goal)."
  [^Term term]
  (Query. term))

(defn make-query-with-parms
  "build a new query from text with '?'-parameter substitutions."
  [^String source  args]
  (Query. source (into-array Term  args)))

(defn has-solution
  "Returns true or false." 
  [^Query q]
  (.hasSolution q))

(defn one-solution
  [^Query q]
  (into {} (.oneSolution q)))

(def first-solution one-solution)

(defn all-solutions
  [^Query q]
  (mapv #(into {} %) (.allSolutions q)))

(defn get-var 
  [var solution]
  (if (instance? Variable var)
    (get solution (get-name var))
    (get solution var)))

(defn close-query
  [^Query q]
  (.close q))

(defn show-query
  [^Query q]
  (.toString q))


;; Util

(defn ^Term text-to-term
  [^String text]
  (Util/textToTerm text))

(defn ^Term text-with-params-to-term
  [^String text args]
  (Util/textParamsToTerm text (into-array Term  args)))



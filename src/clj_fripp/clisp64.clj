(ns clj-logic.clisp64
   (:refer-clojure :exclude [*])
  )


(def T true)

(def F false)

(def PLUS +)

(def DIFFERENCE -)

(def TIMES clojure.core/*)

(def QUOTIENT /)

(def REMAINDER rem)

; TODO
;(cond (x1 y1a y1b ...)
;      (x2 y2a y2b ...)
;      (T y3a y3b ...)) 
;-> (cond
;     x1 (do y1a y1b ...) 
;     x2 (do y2a y2b ...
;     :else (do y3a y3b ...)
(defmacro COND
  [clauses])

;; Simulating dotted pairs

(def . '.)

(defn CONS 
  [a b]
  (if (seq? b)
    (cons a b)
    (list a . b)))

(defn CONSP
  [x]
  (seq? x))

(def CAR first)

(defn CDR 
  [x]
  (if (= . (second x))
    (first (nnext x))
    (next x)))

(def CAAR ffirst)

(defn CADR
  [l]
  (CAR (CDR l)))

(defn CDDR
  [l]
  (CDR (CDR l)))

(defn CDAR
  [l]
  (CDR (CAR l)))

(defn ATOM
  [x]
  (not (seq? x)))

(defn CALL
  [addr]
  (println "CALL of machine program at address" addr "not supported.")
  nil)

(defn ASSOC
  [x l]
  (when (seq l)
    (if (= x (ffirst l))
      l
      (ASSOC x (next l)))))

(def COPY identity)

(defmacro DE 
  [name arglist & body]
  `(defn ~name [~@arglist]  ~@body))

;; TODO
(defmacro DF
  [name arglist & body]
  `(defmacro ~name [~@arglist]  ~@body))

(defmacro DM
  [name arglist & body]
  `(defmacro ~name [~@arglist]  ~@body))

(def EQ identical?)

(def EQUAL =)

(def ZEROP zero?)

(def LAST last)

(def EVAL eval)

(defn EXIT
  []
  (System/exit 0))

(def GREATERP >)

(def LESSP <)

(def LIST list)

(def LENGTH count)

(defn LINE
  []
  nil
  )

(def MAP map)

(defn MINUS
  [x]
  (clojure.core/* x -1))

(defn MINUSP
  [x]
  (< x 0))

(def NOT not)

(defn NTH 
  [l n]
  (when (seq l)
    (if (< n 2)
      l
      (NTH (next l) (dec n))))
  )

(defn NULL
  [x]
  (or (nil? x) (and (seq? x) (empty? x))))

(def NUMBERP number?)


(defmacro * 
  [& l]
  nil)

(defn ABS 
  [x]
  (Math/abs x))

(def ADD1 inc)

(defmacro AND 
  [& args]
  `(and ~@args) )

(defmacro OR 
  [& args]
  `(or ~@args) )

(def APPEND concat)

(defmacro APPLY 
  [fn arglist]
   `(apply (eval ~fn) (list ~@(eval arglist))))

(defmacro APPLY*
  [fn & arglist]
   `(apply (eval ~fn) (list ~@arglist)))

(defn ASC
  [s]
  (int (first s)))

(defn CHAR
  [i]
  (.toString (char i)))

(defn RANDOM
  [a b]
  (+ a (rand-int (- b a))))

(defn REMOVE
  [x l]
  (remove #(= x %) l))

(def REVERSE reverse)

(def STRINGP string?)

(def SUB1 dec)

(defn SPACES 
  [n]
  (repeatedly n #(print " "))
  nil)

(defn TERPRI
  []
  (println))

(def ZEROP zero?)

(def NIL nil)

;; Demos







(ns clj-logic.structedit)

(use 'clojure.pprint)
(use 'clojure.repl)

;; from LISP64: editor.lsp

(def ^:dynamic *OLD* nil)
(def ^:dynamic *CLU* nil)
(def ^:dynamic *CL* '(A B (C D E) F G H))
(def ^:dynamic *TR* nil)


(defn- adjust-index
  [x cl] 
  (cond
    (neg? x)(Math/abs (+ 1 x (count cl)))
    (> x (count cl))(count cl)
  :other x))

(defmacro e
 "Evaluate expression."
  [x]
  (pprint (eval x)))

(defn ppr
  "Pretty print current expression."
  []
  (pprint *CL*))


(defn- p-flat
  [expr] 
  (if (seq? expr)
    (if (seq? (next expr))
      (cons (first expr)
            (map #(if (seq? %) '& %) (rest expr) ))
      expr)
    expr))

(defn p 
  [] 
  (pprint(p-flat *CL*)))

(defn edit 
  [expr]
  (binding [*OLD* expr
            *CL* expr
            *CLU* (list expr)
            *TR* (list *CLU*)]
    (loop []
      (print "*ED*: ")
      (let [inp (read-line)]
        (println inp)
        (if (= inp "ok" )
          (first (last *TR*))
          (do 
            (p)
            (recur)))
        )
      ))
  )

  
;;-----------------------------------------------

(comment 

(SETQ EDFNS
   '(EDFNS CLR-VARS NX FIND FI B COLON LO 
    LI R REPL RO BO BI RI E _ N A DEL 
    UNDO P@ BACK G P P& H EDIT EDITF 
    EDITV EDITP EDIT-EVAL))

(DE CLR-VARS NIL
  (MAPC '(LAMBDA(X)
      (REMPROP X
         'VALUE))
    (CDR EDFNS)))

(DE NX NIL
  (COND((ATOM(CDR CLU)))
    (T(SETQ CLU
        (CDR CLU))
      (SETQ CL
        (CAR CLU)))))

(DE FIND
  (L I X W)
  (COND((ATOM L)
       NIL)
    ((MEMBER I
      (CAR L))
      (LIST X))
    ((SETQ W
      (FIND(CAR L) I 1))
      (CONS X W))
    (T(FIND(CDR L) I
        (ADD1 X)))))

(DF FI
  (I)
  (MAPC 'G
    (FIND CL I 1 NIL))
  (P))

(DF B L
  (RPLACA CLU
    (SETQ CL
      (NCONC L
        (CAR CLU))))
  (BACK))

(DF COLON L
  (RPLACA CLU
    (SETQ CL L)))

(DE LO
  (X Y Z)
  (SETQ Z
    (NTH CL
      (H X)))
  (SETQ Y
    (NTH CL
      (H(SUB1 X))))
  (COND((CONSP(CAR Z))
      (RPLACD Y
        (CAR Z)))))

(DE LI
  (X)
  (BI X -1))

(DF R
  (X Y)
  (REPL X Y CL))

(DE REPL
  (X Y L)
  (COND((ATOM L)
       NIL)
    ((EQUAL(CAR L) X)
      (RPLACA L Y)
      (REPL X Y
        (CDR L)))
    ((EQUAL(CDR L) X)
      (RPLACD L Y))
    (T(REPL X Y
        (CAR L))
      (REPL X Y
        (CDR L)))))

(DE RO
  (X Y)
  (COND((NULL Y)
      (SETQ Y
        (LENGTH CL))))
  (SETQ X
    (NTH CL
      (H X)))
  (SETQ Y
    (NTH CL
      (H Y)))
  (RPLACA X
    (CONC(CAR X)
      (CDR X)))
  (RPLACD X
    (CDR Y))
  (RPLACD Y NIL))

(DE BO
  (X)
  (SETQ X
    (NTH CL
      (H X)))
  (SETQ Y
    (NCONC(CAR X)
      (CDR X)))
  (RPLACA X
    (CAR Y))
  (RPLACD X
    (CDR Y)))

(DE BI
  (X Y)
  (COND((NULL Y)
      (SETQ Y X)))
  (SETQ X
    (NTH CL
      (H X)))
  (SETQ Y
    (NTH CL
      (H Y)))
  (SETQ Z
    (CDR Y))
  (RPLACD Y NIL)
  (RPLACA X
    (CONS(CAR X)
      (CDR X)))
  (RPLACD X Z))

(DE RI
  (X Y)
  (SETQ X
    (NTH CL
      (H X)))
  (SETQ Y
    (NTH(CAR X)
      (H Y)))
  (SETQ Z
    (CDR X))
  (RPLACD X
    (CDR Y))
  (NCONC(CDR Y) Z)
  (RPLACD Y NIL))


(DE _ NIL
  (SETQ CLU
    (LAST TR))
  (SETQ TR
    (LIST CLU))
  (SETQ CL
    (CAR CLU)))

(DF N L
  (NCONC CL L))

(DF A L
  (RPLACD CLU
    (NCONC L
      (CDR CLU)))
  (BACK))

(DE DEL
  (X L)
  (SETQ X
    (H X))
  (COND((ATOM CL)
       CL)
    ((ZEROP X)
      (RPLACA CLU
        (SETQ CL
          (NCONC L CL))))
    ((EQ X 1)
      (RPLACA CLU
        (SETQ CL
          (NCONC L
            (CDR CL)))))
    (T(RPLACD(NTH CL
          (SUB1 X))
        (NCONC L
          (NTH CL
            (ADD1 X)))))))

(DE UNDO NIL
  (SETQ LIS
    (COPY OLD))
  (SETQ CLU
    (LIST LIS))
  (SETQ TR
    (LIST CLU))
  (SETQ CL
    (CAR CLU)))


(DE BACK NIL
  (COND((ATOM(CDR TR))
       CL)
    (T(SETQ CLU
        (CAR TR))
      (SETQ TR
        (CDR TR))
      (SETQ CL
        (CAR CLU)))))

(DE G
  (X)
  (SETQ X
    (H X))
  (COND((ZEROP X)
      (BACK))
    ((GREATERP X
      (LENGTH CL))
       CL)
    (T(SETQ TR
        (CONS CLU TR))
      (SETQ CLU
        (NTH CL X))
      (SETQ CL
        (CAR CLU)))))



(DE EDIT
  (L)
  (CLR-VARS)
  (PROG(OLD TR CL CLU E X LIS)
    (SETQ OLD L)
    (SETQ LIS
      (COPY L))
    (SETQ CLU
      (LIST LIS))
    (SETQ TR
      (LIST CLU))
    (SETQ CL
      (CAR CLU))
    (P) LOOP1
    (MSG "*ED*: ")
    (SETQ E
      (READL)) LOOP2
    (COND((ATOM E)
        (GO LOOP1)))
    (SETQ X
      (CAR E))
    (COND((NUMBERP X)
        (G X))
      ((EQ X
         'OK)
        (RETURN(CAR(LAST TR))))
      ((EQ X
         'PP)
        (PP CL))
      ((ATOM X)
        (EDIT-EVAL(LIST X)))
      ((NUMBERP(CAR X))
        (DEL(CAR X)
          (CDR X)))
      (T(EDIT-EVAL X)))
    (SETQ E
      (CDR E))
    (GO LOOP2)))

(DF EDITF
  (F L)
  (COND((SETQ L
      (APPLY 'GETDEF
        (LIST F)))
      (EVAL(CONS(CAR L)
          (CONS(CADR L)
            (EDIT(CDDR L))))))))

(DF EDITV
  (F)
  (SET F
    (EDIT(EVAL F))))

(DF EDITP
  (A P)
  (PUTPROP A P
    (EDIT(GETPROP A P))))

(DE EDIT-EVAL
  (L)
  (COND((MEMBER(CAR L) EDFNS)
      (EVAL L))
    (T(MSG "      ? ? ?" T))))
)


(defn f2 
  "I don't do a lot."
  [x y] 
  ;; I am a comment.
  (comment "I am another comment.") 
  (str x y))


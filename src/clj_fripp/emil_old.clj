(ns clj-logic.emil-old
  (:require [clojure.core.logic :as cl]
            [clojure.core.logic.arithmetic :as cla]
            [clojure.core.logic.pldb :as pldb]
            [pettomato.goalie :refer (with-traced-goals with-hooks)]
            [pettomato.goalie.print :refer (print-node)])
   (:use clojure.pprint clojure.repl)
  )

(def emil-db (atom {}))

;(defn facts-for-emil [dbs kname]
;      (println "dbs:" dbs ) 
;      (println "kname:" kname) 
;       (println "facts:" 
;                 (mapcat #(get-in % [kname :clojure.core.logic.pldb/unindexed]) [dbs])    ) 
; (let [facts (mapcat #(get-in % [kname :clojure.core.logic.pldb/unindexed]) [dbs])]
;   (if (seq? facts) 
;     facts
;      (mapcat #(get-in % [kname ::unindexed]) [dbs])
;   )))
       

       
(defmacro db-rel-emil [name & args]
  (let [arity
        (count args)

        kname
        (str name "_" arity)

        indexes
        (vec (map pldb/indexed? args))]
    `(def ~name
       (with-meta
         (fn [& query#]
           (fn [subs#]
             (let [dbs#
                   (-> subs# clojure.core/meta :db)

                   facts#
                   (if-let [index# (pldb/index-for-query subs# query# ~indexes)]
                     (pldb/facts-using-index dbs#
                                        ~kname
                                        index#
                                        (cl/walk* subs# (nth query# index#)))
                     (pldb/facts-for dbs# ~kname))]
               ;(println "query#:" (map #(cl/walk* subs# %) query#)   ) 
                 ;;(println "dbs#:" dbs# ) 
               (cl/to-stream (map (fn [potential#]
                                   ((cl/== query# potential#) subs#))
                                 facts#)))))
         {:rel-name ~kname
          :indexes ~indexes}))))


(defn assert-emil 
  [fact] 
  (reset! emil-db (apply pldb/db-fact @emil-db fact)))
 
    
(defn retract-emil 
  [fact]
  (reset! emil-db (apply pldb/db-retraction @emil-db fact)))

;; some logic helpers

(defn noto 
  "logical negation."
  [goal]
  (cl/conda
    [goal cl/u#]
    [cl/s#]))

(defn ignoreo
  "ignore result of evaluations, always succeed."
  [goal]
  ;(println "goal = " goal)
  (fn [orig-a]
    (if-let [new-a (goal orig-a)]
      new-a
      orig-a) 
    ))

(defmacro executeo [& s]
  "Goal for executing clojure code."
  `(fn [a#]
     (do ~@s)
     a#))

(defmacro asserto
  "do fact assertion as a goal." 
  [& fact]
  `(fn [a#]
     ; (println "asserto: " ~@(rest fact) ) 
     (vary-meta a# assoc-in [:db] (list (apply pldb/db-fact (-> a# meta :db first) (vector ~@fact))))
     ))
    
(defmacro retracto
  "do fact retraction as a goal." 
  [& fact]
  `(fn [a#] 
    ;(println "retracto: " ~@(rest fact) ) 
    (vary-meta a# assoc-in [:db] (list (apply pldb/db-retraction (-> a# meta :db first) (vector ~@fact))))
    )) 

(defmacro seto-emil-db 
  []
  "Goal that sets the fact db"
  `(fn [a#]
     (reset! emil-db (-> a# meta :db first))
     a#))

(defmacro trace-s-meta []
  "Goal that prints the current substitutions meta data."
  `(fn [a#]
     (println "s-meta: " (meta a#))
     a#))

;% Brettstellung
;:- dynamic pos/3.
(db-rel-emil pos  farbe figur feld) 

(defn retract-all-pos
  "delete all 'pos' facts, i.e. clear the board."
  [] 
  (swap! emil-db dissoc "pos_3")
    )

(defn show-pos
  "print the board." 
  [] 
  (pprint 
    (cl/run-db* @emil-db [q] 
                (cl/fresh [x y z]
                          (pos x y z)
                          (cl/== q ['pos x y z])
                          ))))

;% Schlagfelder
;:- dynamic capture/4.
(db-rel-emil capture ply farbe figur feld) 

(defn show-captures
  "print the captures." 
  [] 
  (cl/run-db* @emil-db [q]
   	  (cl/fresh [w x y z]
   	    (capture w x y z)
        (cl/project [w x y z]   
           (cl/log w x y z)) 
        ))
  'done)

;% Zugzaehler
;% :- dynamic moveCounter/1.
(def ^:dynamic move-counter (atom 0))

(defn init-move-counter
  []
  (reset! move-counter 0))

(defn set-move-counter
  [n]
  (reset! move-counter n))

(defn incr-move-counter
  []
  (swap! move-counter inc))

(defn decr-move-counter 
  []
  (swap! move-counter dec))

(defn get-move-counter
  []
   @move-counter)  

;% aktuelle Farbe
;:- dynamic currColor/1.
(def ^:dynamic curr-color (atom :w))

;% aktuelle Farbe setzen
;initCurrColor :-
;        assertz((currColor(w))).
;setCurrColor(Col) :-
;        retract((currColor(_))),
;        assertz((currColor(Col))).
;toggleCurrColor :-
;        retract((currColor(Col))),
;        oppcol(Col, NewCol),
;        assertz((currColor(NewCol))).
(defn init-curr-color
  []
  (reset! curr-color :w))

(defn set-curr-color
  [col]
  (reset! curr-color col))

(defn toggle-curr-color
  []
  (if (= @curr-color :w)
    (reset! curr-color :b)
    (reset! curr-color :w)))

(defn get-curr-color
  []
   @curr-color)  

; Brettrichtungen white (a1->a8), black (a8->a1):
(db-rel-emil richtung p) 
(assert-emil [richtung :w]) 
(assert-emil [richtung :b])

; opponent colors w<->b:
(db-rel-emil oppcol c p) 
(assert-emil [oppcol :w :b]) 
(assert-emil [oppcol :b :w ]) 

; Halbzugzähler
(def ^:dynamic ply-counter (atom 0))

(defn init-ply-counter
  []
  (reset! ply-counter 1))

(defn incr-ply-counter
  []
  (swap! ply-counter inc))

(defn decr-ply-counter 
  []
  (swap! ply-counter dec))

(defn get-ply-counter
  []
   @ply-counter)  


;% leeres Brett aufbauen
;clearBoard :-
;        retractall(pos(_,_,_)),
;        retractall(plyCounter(_)),
;        retractall(currColor(_)),
;        initPlyCounter(1),
;        initCurrColor.
(defn clear-board
  []
  (retract-all-pos)
  (init-ply-counter)
  (init-move-counter) 
  (init-curr-color)
  )

;% Teststellung #1: W Kh6, Ta8, S Kh8, #
(defn makeTest1
  []
 (clear-board)
 (assert-emil [pos :w :k 86])
 (assert-emil [pos :w :r 18])
 (assert-emil [pos :b :k 88])
 )

(defn test1
  []
  (makeTest1)
  )

;% Teststellung #2: W Kh6, Ta1, S Kh8, #1 -> 1.Ta8#
(defn makeTest2
  []
  (clear-board)
  (assert-emil [pos :w :k 86])
  (assert-emil [pos :w :r 11])
  (assert-emil [pos :b :k 88])
  )

(defn test2
  []
  (makeTest2)
  ;(mattsuche :w 2)
  )

;% Teststellung #34: W Ke1, Dd1, S Kh2, #2 -> 1.Kf2!
(defn makeTest3
  []
  (clear-board)
  (assert-emil [pos :w :k 51])
  (assert-emil [pos :w :q 41])
  (assert-emil [pos :b :k 82])
)  

(defn test3
  []
  (makeTest3)
  ;(mattsuche :w 2)
  )

(defn makeTest3a
  []
  (clear-board)
  (assert-emil [pos :w :k 62])
  (assert-emil [pos :w :q 41])
  (assert-emil [pos :b :k 83])
)  

(defn test3a
  []
  (makeTest3a)
  ;(mattsuche :w 2)
  )

;% Teststellung #4: W Kd5, Th1, S Ka8 , #3-> 1.Kc6!
;makeTest4 :-
;        clearBoard,
;        assertz(pos(w, k, 45)),
;        assertz(pos(w, r, 81)),
;        assertz(pos(b, k, 18)).
;
;test4 :-
;        makeTest4,
;        mattsuche(w, 3).
(defn makeTest4
  []
  (clear-board)
  (assert-emil [pos :w :k 45])
  (assert-emil [pos :w :r 81])
  (assert-emil [pos :b :k 18])
  )

(defn test4
  []
  (makeTest4)
  ;(mattsuche :w 3)
  )

; Prüfung, ob Feld zum Schachbrett gehört (11 .. 88):
;istBrettfeld(Feld) :- 10 < Feld,
;                Feld < 89,
;                Einer is Feld mod 10,
;                0 < Einer,
;                Einer < 9.
                
(defn istBrettfeld
  [Feld]
  (cl/fresh [Einer] 
    (cla/< 10 Feld)
    (cla/< Feld 89)
    (cl/is Einer Feld #(mod % 10))
    (cla/< 0 Einer)
    (cla/< Einer 9))
  )              

; Prüfung, ob das Feld besetzt ist:
;besetzt(Feld) :- pos(_, _, Feld).
(defn besetzt
  [Feld]
  (cl/fresh [a1 a2]
         (pos a1 a2 Feld))) 

; Prüfung, ob das Feld von einer Figur bestimmter Farbe besetzt ist:
;besetzt(Feld, Farbe) :- pos(Farbe, _, Feld).
(defn besetztFarbe
  [Feld Farbe]
  (cl/fresh [a2]
         (pos Farbe a2 Feld))) 

; Prüfung, ob das Feld unbesetzt ist:
;frei(Feld) :- not(pos(_, _, Feld)).
;(defn frei
;  [Feld]
;  (cl/nafc besetzt Feld))

(defn frei
  [Feld]
  (noto (besetzt Feld)))


;;------------------------------------------------------------------
;; move gen

;% Bewegungsarten der Figuren:
;bewegungsart(k, single).
;bewegungsart(n, single).
;bewegungsart(b, multiple).
;bewegungsart(r, multiple).
;bewegungsart(q, multiple).
;bewegungsart(p, pawn).
(db-rel-emil bewegungsart p1 p2) 
(assert-emil [bewegungsart :k :single])
(assert-emil [bewegungsart :n :single])
(assert-emil [bewegungsart :b :multiple])
(assert-emil [bewegungsart :r :multiple])
(assert-emil [bewegungsart :q :multiple])
(assert-emil [bewegungsart :p :pawn])

;
;% Einzelschritte der Figuren:
      
(cl/defne schritt 
  [p1 p2]
;% r =  rook, Turm
;schritt(r, 1).
;schritt(r, 10).
[[:r 1]] 
[[:r 10]] 
;% b = bishop, Laeufer
;schritt(b, 9).
;schritt(b, 11).
[[:b 9]] 
[[:b 11]] 
;% n = knight, Springer
;schritt(n, 8).
;schritt(n, 12).
;schritt(n, 19).
;schritt(n, 21).
[[:n 8]] 
[[:n 12]] 
[[:n 19]] 
[[:n 21]] 
;% k= king, Koenig
[[:k _] (schritt :r p2) ]  
[[:k _] (schritt :b p2) ]  
;% q = queen, Dame
;schritt(q,S) :- schritt(k, S).
[[:q _] (schritt :k p2)]  
)

;
;% Einzelschritt-Ausführung:

;einzelschritt(w,Von, Nach, Distanz) :-
;                Nach is Von + Distanz.
;einzelschritt(b, Von, Nach, Distanz) :-
;                Nach is Von - Distanz.
(defn einzelschritt
  [col von nach distanz]
  (cl/project [distanz] 
              (cl/conde 
                [(cl/== col :w) (cl/is nach von #(+ % distanz))]
                [(cl/== col :b) (cl/is nach von #(- % distanz))]
                )
  )
)

;% Distanzzuege bei Mehrschrittlern:
;distanzzug(multiple, Von, Nach, Distanz, Richtung) :-
;                einzelschritt(Richtung, Von, Zwischenfeld, Distanz),
;                istBrettfeld(Zwischenfeld),
;                frei(Zwischenfeld),
;                distanzzug(multiple, Zwischenfeld, Nach, Distanz, Richtung).
;
;distanzzug(_, Von, Nach, Distanz, Richtung) :-
;                einzelschritt(Richtung, Von, Nach, Distanz),
;                istBrettfeld(Nach).
(defn distanzzug
  [multiple von nach distanz richtung]
  (cl/conde
    [(cl/== multiple :multiple) 
    ; (cl/log "disz multiple")
     (cl/fresh [zwischenfeld] 
               (einzelschritt richtung von zwischenfeld distanz)
               (istBrettfeld zwischenfeld)
               (frei zwischenfeld)
               (distanzzug multiple zwischenfeld nach distanz richtung))
     ]
    [cl/s#
      ; (cl/log "disz einzel")
     (einzelschritt richtung von nach distanz)
     (istBrettfeld nach)]
    ))

;% Bauernzüge weiss
;bauernzug(w, Von, Nach) :-
;        Linie is Von mod 10,
;        Linie < 3,              % Doppelschritt
;        Zwischenfeld is Von + 1,
;        frei(Zwischenfeld),
;        Nach is Von + 2,
;        frei(Nach).
;bauernzug(w, Von, Nach) :-
;        Nach is Von + 1,        % Einzelschritt
;        frei(Nach).
;bauernzug(w, Von, Nach) :-
;        Nach is Von - 9,
;        besetzt(Nach, b).       % Schlagzug
;bauernzug(w, Von, Nach) :-
;        Nach is Von + 11,
;        besetzt(Nach, b).       % Schlagzug
;
;% Bauernzüge schwarz
;bauernzug(b, Von, Nach) :-
;        Linie is Von mod 10,
;        Linie > 6,              % Doppelschritt
;        Zwischenfeld is Von - 1,
;        frei(Zwischenfeld),
;        Nach is Von - 2,
;        frei(Nach).
;bauernzug(b, Von, Nach) :-
;        Nach is Von - 1,        % Einzelschritt
;        frei(Nach).
;bauernzug(b, Von, Nach) :-
;        Nach is Von - 11,
;        besetzt(Nach, w).       % Schlagzug
;bauernzug(b, Von, Nach) :-
;        Nach is Von + 9,
;        besetzt(Nach, w).       % Schlagzug
;
;% Ermitteln eines pseudolegalen Zuges:
;pseudoZug(Farbe, Figur, Von, Nach) :-
;        pos(Farbe, Figur, Von),
;        bewegungsart(Figur, pawn),
;        bauernzug(Farbe, Von, Nach).
;pseudoZug(Farbe, Figur, Von, Nach) :-
;        pos(Farbe, Figur, Von),
;        schritt(Figur, Distanz),
;        richtung(Richtung),
;        bewegungsart(Figur, Bewegung),
;        distanzzug(Bewegung, Von, Nach, Distanz, Richtung),
;        not(pos(Farbe, _, Nach)).
(defn pseudoZug
  [farbe figur von nach]
  (cl/fresh [distanz richt bewart blockeur]
         ; (cl/trace-lvars "pz-anfa:" farbe figur von nach)
         (pos farbe figur von)
         ;(cl/trace-lvars "pz-anf2:" farbe figur von nach)
         (schritt figur distanz)
         ;(cl/log "nach schritt") 
         (richtung richt)
         ;(cl/log "nach richtung") 
         (bewegungsart figur bewart)
         ;(cl/log "nach bewegungsart") 
         ;(cl/trace-lvars "pz-anf3:" farbe figur von nach richt distanz bewart)
         (distanzzug bewart von nach distanz richt)
         (noto (pos farbe blockeur nach)) 
         ;(cl/trace-lvars "pz-ende:" farbe figur von nach)
          )
  )

;
;% Zug auf dem Brett ausführen:
;
;% Schlagzug ausführen:
;makeZug(Farbe, Figur, Von, Nach) :-
;        besetzt(Nach), !,
;        pos(Gegfarbe, Gegfigur, Nach),
;        flag(plyCounter, PlyCounter, PlyCounter),
;        assertz((capture(PlyCounter, Gegfarbe, Gegfigur, Nach))),
;        retract((pos(Gegfarbe, Gegfigur, Nach))),
;        makeZug1(Farbe, Figur, Von, Nach).
;
;% normalen Zug ausführen:
;makeZug(Farbe, Figur, Von, Nach) :-
;            makeZug1(Farbe, Figur, Von, Nach).
(declare makeZug1)
(defn makeZug
  [Farbe, Figur, Von, Nach]
  (cl/project [Farbe, Figur, Von, Nach] 
              ; (cl/trace-lvars "makeZug:" Farbe Figur Von Nach) 
              (cl/fresh [Gegfarbe, Gegfigur]
                        (cl/conda
                          [(pos Gegfarbe Gegfigur Nach)
                           ;(cl/trace-lvars "makeZug Gegfigur:" Gegfarbe Gegfigur Nach) 
                           (cl/project [Gegfarbe Gegfigur]
                                       (asserto capture (get-ply-counter) Gegfarbe Gegfigur Nach)
                                       (retracto pos Gegfarbe Gegfigur Nach))
                           ;;(cl/log "zw1-1"  )
                           (makeZug1 Farbe Figur Von Nach) 
                           ;;(cl/log "zw1-2") 
                           ]
                          [;(cl/log "zw2-1")
                           (makeZug1 Farbe, Figur, Von, Nach ) 
                           ;(cl/log "zw2-2")
                           ]))
              cl/s#
              )
  )

;
;% Hilfsprädikat:           
;makeZug1(Farbe, Figur, Von, Nach) :-
;        ignore(incrPlyCounter),
;        retract((pos(Farbe, Figur, Von))),
;        assertz((pos(Farbe, Figur, Nach))).
(defn makeZug1
  [Farbe, Figur, Von, Nach]
  (cl/all 
    ;(cl/log "makeZug1-1")
    ;(cl/trace-lvars "makeZug:" Farbe Figur Von Nach) 
    (executeo (incr-ply-counter))
    (retracto pos Farbe, Figur, Von)
    ;;(cl/log "makeZug1-2")
    (asserto pos Farbe, Figur, Nach ))
  )

;
;% ausgefuehrten Zug zuruecknehmen:
;
;% Schlagzug zurücknehmen:
;unmakeZug(Farbe, Figur, Von, Nach) :-
;        flag(plyCounter, PlyCounter, PlyCounter),
;        N is PlyCounter - 1,
;        capture(N, Gegfarbe, Gegfigur, Nach), !,
;        assertz((pos(Gegfarbe, Gegfigur, Nach))),
;        retract((capture(N, Gegfarbe, Gegfigur, Nach))),
;        unmakeZug1(Farbe, Figur, Von, Nach).
;
;% normalen Zug zurücknehmen:
;unmakeZug(Farbe, Figur, Von, Nach) :-
;        unmakeZug1(Farbe, Figur, Von, Nach).
(declare unmakeZug1)
(defn unmakeZug
  [Farbe, Figur, Von, Nach]
  ; (println "unmake!!!!")
  (cl/project [Farbe, Figur, Von, Nach]
              ;(cl/log "unmake called") 
              (cl/fresh [N Gegfarbe Gegfigur]
                        (cl/== N (dec (get-ply-counter)))
                       ; (cl/trace-lvars "N = " N) 
                        (cl/conda
                          [(capture N Gegfarbe Gegfigur Nach)
                           (cl/all 
                             (cl/log "unmake capture") 
                             (cl/project [N Gegfarbe Gegfigur] 
                                         (asserto pos Gegfarbe Gegfigur Nach)
                                         (retracto capture N Gegfarbe Gegfigur Nach))
                             (unmakeZug1 Farbe, Figur, Von, Nach )
                             ;(cl/log "unmake if")
                             )
                           ]
                          [(unmakeZug1 Farbe, Figur, Von, Nach)
                           ;(cl/trace-lvars "unmakeZug:" Farbe Figur Von Nach) 
                           ;(cl/log "unmaked non capture move")
                           ]))
              cl/s#
              )
  ) 

;% Hilfsprädikat:
;unmakeZug1(Farbe, Figur, Von, Nach) :-
;        ignore(decrPlyCounter),
;        retract((pos(Farbe, Figur, Nach))),
;        assertz((pos(Farbe, Figur, Von))).
(defn unmakeZug1
  [Farbe, Figur, Von, Nach]
  (cl/all 
    ;(cl/log "unmakeZug1-1")
    ;;(ignoreo (decr-ply-counter))
    (retracto pos Farbe, Figur, Nach)
    (asserto  pos Farbe, Figur, Von))
  )


;% Prüfung, ob der König der Seite "Farbe" im Schach steht:
;imSchach(Farbe, Flag) :-
;        pos(Farbe, k, Nach),
;        oppcol(Farbe, FarbeGegner),
;        pseudoZug(FarbeGegner, _, _, Nach),
;        !,
;        Flag is 1.
;        
;imSchach(_, Flag) :-
;        Flag is 0.

(defn imSchach
  [Farbe]
   (cl/fresh [Nach Farbegegner Gegnerfigur Von X Y Z]
                       ;  (cl/trace-lvars "imSchach: vor pos" Farbe Farbegegner)
                        (pos Farbe :k Nach)
                       ; (cl/log "nch pos")
                        (oppcol Farbe Farbegegner)
                        ;(cl/trace-lvars "imSchach: vor pz" Farbe Farbegegner Gegnerfigur Von Nach)
                        (pseudoZug Farbegegner Gegnerfigur Von Nach)
                     ;   (cl/trace-lvars "imSchach: nch pz " Farbegegner Gegnerfigur Von Nach)
                     ;   (cl/log "im Schach!")
  ))

(defn testImSchach
  [Farbe SFlag]
  (cl/conda  
    [ (imSchach Farbe)
     (cl/== SFlag 1)
     ]
    [ (cl/== SFlag 0)
     ]
    ))

;% Ermitteln legaler Züge:
;legalZug(Farbe, Figur, Von, Nach) :-
;        pseudoZug(Farbe, Figur, Von, Nach),
;        makeZug(Farbe, Figur, Von, Nach),
;        ignore(imSchach(Farbe, Flag)),
;        unmakeZug(Farbe, Figur, Von, Nach),
;        Flag = 0.
(defn legalZug
  [Farbe, Figur, Von, Nach]
  (cl/fresh [SFlag]
    ;(cl/log "legalZug: vor  pz")
    (pseudoZug Farbe, Figur, Von, Nach) 
    ;(cl/trace-lvars "legalZug: nach pz" Farbe, Figur, Von, Nach)
    (makeZug Farbe, Figur, Von, Nach)
    ;(executeo (show-pos))
    ;(cl/log "legalZug: nach makeZug")
    (ignoreo (testImSchach Farbe SFlag))
    ;(executeo (show-pos))
    (unmakeZug Farbe, Figur, Von, Nach)
    ; (cl/log "legalZug: nach unmakeZug")
    (cl/== SFlag 0) 
    ;(cl/log "legalZug: SFlag = 0, OK, Zug gefunden")
    ) 
  )

;% Prüfung, ob die Seite "Farbe" pattgesetzt ist:
;patt(Farbe, Flag) :-
;        ignore(imSchach(Farbe, ImSchach)),
;        ImSchach = 0,
;        not(legalZug(Farbe, _, _, _)), !,
;        Flag is 1.
;patt(_, Flag) :-
;        Flag is 0.
(defn patt 
  [Farbe] 
   (cl/fresh [Figur Von Nach] 
             ;(cl/log "Pattprüfung")
             (noto (imSchach Farbe))
             (noto (legalZug Farbe Figur Von Nach)))
)

;% Prüfung, ob die Seite "Farbe" mattgesetzt ist:
;matt(Farbe, Flag) :-
;        ignore(imSchach(Farbe, ImSchach)),
;        ImSchach = 1,
;        not(legalZug(Farbe, _, _, _)), !,
;        write('#'), nl,
;        Flag is 1.
;
;matt(_, Flag) :-
;        write('---'), nl,
;        Flag is 0.

(defn matt 
  [Farbe] 
   (cl/fresh [Figur Von Nach ImSchach] 
             ;(cl/log "Mattprüfung")
             (imSchach Farbe)  
             (noto (legalZug Farbe Figur Von Nach))
             ))
 
;%------------------------------------------------------------------------------
;% Testtreiber
;%------------------------------------------------------------------------------
;
;% alle pseudolegalen Züge generieren (Test):
;zuggen :-       
;        richtung(Farbe),
;        pseudoZug(Farbe, Figur, Von, Nach),
;        writeZug(Farbe, Figur, Von, Nach),
;        fail.
(defn zuggen
  []
  (cl/fresh [Farbe Figur Von Nach]
            (richtung Farbe)
            (pseudoZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/u#))

;% alle legalen Züge für eine Seite (w,b) generieren:
;zuggen(Farbe) :-
;        legalZug(Farbe, Figur, Von, Nach),
;        writeZug(Farbe, Figur, Von, Nach),
;        fail.
(defn legal-zuggen
  [Farbe]
  (cl/fresh [Figur Von Nach]
            (legalZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/u#))

;% alle legalen Züge für eine Figur generieren:
;zuggen(Farbe, Figur) :- 
;        legalZug(Farbe, Figur, Von, Nach),
;        writeZug(Farbe, Figur, Von, Nach),
;        fail.
(defn legal-figur-zuggen
  [Farbe Figur]
  (cl/fresh [Von Nach]
            (legalZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/u#))

;% Mattsuche
(declare setztMattInGenauN wirdMattInGenauN)
;
;setztMattInGenauN(Farbe, N, MFlag) :-
;    legalZug(Farbe, Figur, Von, Nach),
;	not(istHarmloserKoenigszug(Figur, N, Nach)),
;	oppcol(Farbe, GegFarbe),
;	pos(GegFarbe, k, KFeld),
;	istTurmMattzug(Figur, N, Nach, KFeld),
;	istLaeuferMattzug(Figur, N, Von, KFeld),
;	istSpringerMattzug(Figur, N, Von, KFeld),
;    % write('pruefe Zug: '),
;    writeZug(Farbe, Figur, Von, Nach), % nl,
;    makeZug(Farbe, Figur, Von, Nach),
;    ignore(wirdMattInGenauN(GegFarbe, N, MFlag)),
;    unmakeZug(Farbe, Figur, Von, Nach),
;    MFlag == 1, !, % bei 0 Backtracking
;    write('Mattzug: '),
;    writeZug(Farbe, Figur, Von, Nach), nl.
;
;
;setztMattInGenauN(_, _, MFlag) :-
;        % write('no mating move'), nl,
;        MFlag is 0.
(defn setztMattInGenauN
  [Farbe N MFlag]
   (cl/conda
     [(cl/fresh [Figur Von Nach GegFarbe]
                ;(cl/trace-lvars "prüfzug beginn:" N Farbe ) 
                (legalZug Farbe Figur Von Nach)
                (oppcol Farbe, GegFarbe) 
                (makeZug Farbe, Figur, Von, Nach)
                ;(cl/trace-lvars "prüfe zug:" N Farbe Figur Von Nach) 
                (ignoreo (wirdMattInGenauN GegFarbe, N, MFlag))
                (unmakeZug Farbe, Figur, Von, Nach)
                ;(cl/trace-lvars "prüfe zug fertig, ergebnis: " N Farbe Figur Von Nach MFlag) 
                (cl/== MFlag 1)
                ;(ignoreo (cl/is PV PV #(conj % [ N Farbe Figur Von Nach]  )))
                (cl/trace-lvars "Mattzug:" N Farbe Figur Von Nach ) 
                
                 )
      (cl/log "Matt !--------------------------------------------------------------------------------------------------------------------------")
      cl/s#
      ]
     [cl/s#
      ;(cl/log "kein matt gefunden")
      (cl/== MFlag 0)]
     ))

;% Mattprüfungen
;
;
;wirdMattInGenauN(Farbe, 1, MFlag) :-
;        ignore(matt(Farbe, MFlag)).
;
;wirdMattInGenauN(Farbe, _, MFlag) :-
;		ignore(patt(Farbe, PFlag)),
;		PFlag == 1, !,
;		write('patt'), nl,
;		MFlag is 0.
;
;wirdMattInGenauN(Farbe, N, MFlag) :-
;        legalZug(Farbe, Figur, Von, Nach),
;        % write('Pruefe Entgegnung :'),
;        writeZug(Farbe, Figur, Von, Nach), nl,
;        makeZug(Farbe, Figur, Von, Nach),
;        oppcol(Farbe, GegFarbe),
;        NewN is N - 1,
;        ignore(setztMattInGenauN(GegFarbe, NewN, MFlag)),
;        unmakeZug(Farbe, Figur, Von, Nach),
;        MFlag == 0, !, % bei 1 Backtracking
;        writeZug(Farbe, Figur, Von, Nach),
;        % write('Zug ist Widerlegung!'), nl .
;        write('!'), nl .
;
;wirdMattInGenauN(_, _, MFlag) :-
;        % write('?'), nl,
;        MFlag is 1.
(defn wirdMattInGenauN
  [Farbe N MFlag]
  (cl/conda
    [(cl/all (cl/== N 1) 
             (matt Farbe))
     (cl/log " matt")
     (cl/== MFlag 1)
     cl/s#
     ] 
    
     [(cl/== N 1) 
      ;(cl/log " kein matt")
      (cl/== MFlag 0)
      cl/s#
      ] 
     
     [(patt Farbe)
      (cl/log " patt")
      (cl/== MFlag 0)
      cl/s#
      ] 
    [(cl/fresh [Figur Von Nach GegFarbe NewN]
              (legalZug Farbe Figur Von Nach)
              (makeZug Farbe, Figur, Von, Nach)
              (oppcol Farbe, GegFarbe) 
              (cl/is NewN N dec)
              ;(cl/trace-lvars "prüfe entgegnung:" N NewN Farbe Figur Von Nach) 
              (ignoreo (setztMattInGenauN GegFarbe, NewN, MFlag))
              (unmakeZug Farbe, Figur, Von, Nach)
              ;(cl/trace-lvars "prüfe entgegnung fertig. ergebnis:" N NewN Farbe Figur Von Nach  MFlag) 
              (cl/== MFlag 0)
              cl/s#
              )]
    [cl/s#
     (cl/log "durchgefallen, matt!")
     (cl/== MFlag 1)
     cl/s#
     ]
    ))

;% Handler fuer Mattsuche
;mattsuche(Farbe, N) :-
;    ignore(setztMattInGenauN(Farbe, N, IstMatt)),
;	IstMatt == 1.
(defn mattsuche 
  [Farbe, N] 
  (cl/fresh [IstMatt]
            (ignoreo(setztMattInGenauN Farbe, N, IstMatt))
            (cl/== IstMatt 1)))

(defn vtb 
  [n1 n2] 
  (str "vtb." n1 "." n2)
  ) 

;; Ausgabe

;convFeld(Feld, [ReiheAsc, LinieAsc]):-
;        nonvar(Feld),
;        conv2Feld(Feld, Reihe, Linie),
;        ReiheAsc is Reihe + 48,
;        LinieAsc is Linie + 96,!.
;convFeld(Feld, [ReiheAsc, LinieAsc]):-
;        Reihe is ReiheAsc - 48,
;        Linie is LinieAsc - 96,
;        conv2Feld(Feld, Reihe, Linie),!.
;
;% Hilfsprädikate
;conv2Feld(Feld, Reihe, Linie) :-
;        nonvar(Feld),!,
;        Reihe is Feld // 10,
;        Linie is Feld mod 10.
;conv2Feld(Feld, Reihe, Linie):-
;        Feld is  Reihe * 10 + Linie.
;
;% Figur ausgeben
;writeFigur(Farbe, Figur) :-
;        write(Farbe), 
;        % upcase_atom(Figur, UFigur),
;        % write(UFigur).
;        write(Figur).
;        
;
;% Feld a1..h8 ausgeben
;writeFeld(Feld) :- 
;        convFeld(Feld, [A,B]),
;        name(Feldname, [B,A]),
;        write(Feldname),!.
;
;% Rochadezüge ausgeben:
;writeZug(_, _, _, 99) :-
;	!, 
;	writeCounter,
;    write('0-0 ').
;writeZug(_, _, _, 00) :-
;	!, 
;	writeCounter,
;    write('0-0-0 '). 
;        
;% Zug ausgeben:
;writeZug(Farbe, Figur, Von, Nach) :-
;        besetzt(Nach), !,       % Schlagzug
;        writeZug1(Farbe, Figur, Von, Nach, 'x').
;
;writeZug(Farbe, Figur, Von, Nach) :-
;        writeZug1(Farbe, Figur, Von, Nach, '-').
;
;	
;writeZug1(Farbe, Figur, Von, Nach, Modus) :-
;        writeCounter,
;        writeFigur(Farbe, Figur), 
;        write(':'),
;        writeFeld(Von), write(Modus),
;        writeFeld(Nach), write(' ').
;        
;writeCounter :-
;	 flag(plyCounter, PlyCounter, PlyCounter),
;        MoveCounter is ((PlyCounter-1)//2) + 1 ,
;        writef('%r', ['           ', MoveCounter - 1]),
;        write(MoveCounter),
;        write('. ').

; vtb = variation tree branch

;;------------------------------------------------------------------
;; runs


  
(test3)

(show-pos)

;(def x (cl/run-db* @emil-db [q]
;         (makeZug :w :r 18 28)
;         (pos :b :k q)
;         ))
;
;(println "x:" x)

(comment 

  (defn print-schritt
  [] 
  (cl/run-db* emil-db [q]
   	  (cl/fresh [x y]
   	    (schritt x y)
        ;;;(cl/trace-lvars "schritt: " x y) 
        (cl/project [x y]   
           (cl/log x y)) 
        ))
  'done)

  (cl/run-db* @emil-db [q]
              (cl/fresh [x]
                        (richtung x)
                        (cl/== q x) 
                        )) 

(cl/run-db* @emil-db [q]
	  (cl/fresh [x y]
      (richtung x) 
	    (oppcol x y )
      (cl/== q [x y]) 
	  )) 

(cl/run-db* @emil-db [q]
      	  (cl/fresh [x]
            (cl/== x 78 ) 
      	     (istBrettfeld x)
            (cl/== q x) 
      	  ))

(cl/run-db* @emil-db [q]
	  (cl/fresh [x]
	    (besetzt x)
      (cl/== q x) 
	  )) 

(cl/run-db* @emil-db [q]
   	 (frei 44)
     (cl/== q true))

(cl/run-db* @emil-db [q]
    (frei 45)
    (cl/== q true))

;; alle Züge wTh1
(sort 
  (cl/run-db* @emil-db [q]
    (pseudoZug :w :r 81 q)
    ))

;; alle Züge weiss
(sort (cl/run-db* @emil-db [q]
            (cl/fresh [x y] 
                   (pseudoZug :w x y q))))

;; alle Züge aller Figuren
(pprint 
  (sort (cl/run-db* @emil-db [q]
               (cl/fresh [x y z q1] 
                         (pseudoZug x y z q1)
                         (cl/== q [x y :from z :to q1])))))


(cl/run-db* @emil-db [q]
    (asserto pos :w :q 66)
    (printlno "asserted.") 
    (cl/== q true))

(cl/run-db* @emil-db [q]
    (retracto pos :w :q 66)
    (printlno "retracted.") 
    (cl/== q true))

(cl/run-db* @emil-db [q]
         (cl/log "ply counter before is " (get-ply-counter)) 
         (ignoreo (incr-ply-counter))
         (cl/log "ply counter now is " (get-ply-counter)) 
         (cl/== q true))

(def x (cl/run-db* @emil-db [q]
         (makeZug :w :r 11 18)
         (cl/== q true) 
         ))


(def x (cl/run-db* @emil-db [q]
         (imSchach :b)
         (cl/== q true) 
         ))

(def x(cl/run-db* @emil-db [q]
        (setztMattInGenauN :w, 1, q)
         (cl/== q 1) 
         ))

(def x(cl/run-db* @emil-db [q]
                  (mattsuche :w, 2)
                  (cl/== q 1) 
         ))

(with-traced-goals [legalZug pseudoZug imSchach ]       ; The goals to instrument, e.g., show up in stacktraces.
      (with-hooks  [legalZug pseudoZug imSchach ]                  ; The goals to add hooks to.
        print-node                           ; Included hook; prints debug info.
        print-node
        (doall (cl/run-db* @emil-db [q] 
               (cl/fresh [x y z q1] 
                         (legalZug :w :k y z )
                         (cl/== q [:k y z])))
                           )))

)

(ns clj-logic.emil-save
  "Emil umgesetzt mit Clojure core.logic."
  (:require [clojure.core.logic :as cl]
            [clojure.core.logic.arithmetic :as cla]
            [clojure.core.logic.pldb :as pldb])
   (:use clojure.pprint clojure.repl)
  )

(def emil-db (atom {}))

(defn assert-emil 
  [fact] 
  (reset! emil-db (apply pldb/db-fact @emil-db fact)))
     
(defn retract-emil 
  [fact]
  (reset! emil-db (apply pldb/db-retraction @emil-db fact)))


;% Brettstellung
(pldb/db-rel pos ^:index farbe ^:index figur ^:index feld) 

;% Schlagfelder
(pldb/db-rel capture ^:index ply ^:index farbe ^:index figur ^:index feld) 

;% Zug
(pldb/db-rel move ply farbe figur von nach) 

; Brettrichtungen white (a1->a8), black (a8->a1):
(pldb/db-rel richtung ^:index p) 
(assert-emil [richtung :w]) 
(assert-emil [richtung :b])

; opponent colors w<->b:
(pldb/db-rel oppcol ^:index c p) 
(assert-emil [oppcol :w :b]) 
(assert-emil [oppcol :b :w ]) 

;% Bewegungsarten der Figuren:
(pldb/db-rel bewegungsart ^:index p1 p2) 
(assert-emil [bewegungsart :k :single])
(assert-emil [bewegungsart :n :single])
(assert-emil [bewegungsart :b :multiple])
(assert-emil [bewegungsart :r :multiple])
(assert-emil [bewegungsart :q :multiple])
(assert-emil [bewegungsart :p :pawn])


;; some logic helpers

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

(defmacro inst-sols 
  [& lis]
  `(first (doall ~@lis)))


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
   "Goal for executing clojure code."
  [& s]
  `(fn [a#]
     (do ~@s)
     a#))

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

(defmacro retractallo
  "do relation retraction as a goal." 
  [name arity]
  `(fn [a#] 
    (vary-meta a# assoc-in [:db] (list (retract-rel (-> a# meta :db first) ~name ~arity )))
    )) 

(defn traceo-s-meta []
  "Goal that prints the current substitutions meta data."
  (fn [a]
     (println "s-meta: " (meta a))
     a))

;; emil helpers

(defn retract-all-emil
  "delete relation from emil DB."
  [rel arity] 
  (swap! emil-db retract-rel rel arity))

(defn retract-all-pos-emil
  "delete all 'pos' facts, i.e. clear the board."
  [] 
  (retract-all-emil "pos" 3))

(defn seto-emil-db 
  "Goal that sets the fact db"
  []
  (fn [a]
     (reset! emil-db (-> a meta :db first))
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


; Ausgabe

(cl/defne piece-to-stringo 
   "Convert color & peace keys to string representation."
  [color piece s]
[[:w :k "wK"]] 
[[:w :q "wQ"]] 
[[:w :r "wR"]] 
[[:w :b "wB"]] 
[[:w :n "wN"]] 
[[:w :p "wP"]] 
[[:b :k "bK"]] 
[[:b :q "bQ"]] 
[[:b :r "bR"]] 
[[:b :b "bB"]] 
[[:b :n "bN"]] 
[[:b :p "bP"]])

(def lower-a-offset (dec (int \a)))

(defn field-to-string 
  "Convert numeric field to string representation. Functional version."
  [field]
  (let [row  (quot field 10)
        line (mod field 10)]
    (str (char (+ lower-a-offset line)) row)))

(defn field-to-stringo 
  "Convert numeric field to string representation. Relational version."
  [field s]
  (fn [a]
    (cl/unify a s (field-to-string (cl/walk* a field)))))


;; REPL helpers

(defn show-pos
  "print the board." 
  [] 
  (pprint 
    (cl/run-db* @emil-db [q] 
                (cl/fresh [x y z s1 s2]
                          (pos x y z)
                          (piece-to-stringo x y s1)
                          (field-to-stringo z s2)
                          (cl/project [s1 s2]
                                      (cl/== q (str s1 s2)))))))

(defn show-captures
  "print the captures." 
  [] 
  (pprint 
    (cl/run-db* @emil-db [q]
   	  (cl/fresh [w x y z]
   	    (capture w x y z)
        (cl/project [w x y z]   
           (cl/log w x y z)) 
        )))
  )

;% Move_ID

(def ^:dynamic move-id (atom 0))

(defn init-move-id
  []
  (reset! move-id 0))

(defn next-move-id
  []
  (swap! move-id inc))

(defn get-move-id
  []
   @move-id)  


;% Zugzaehler

(defn init-move-countero
  []
  (set-flago :move-counter 0))

(defn set-move-countero
  [n]
  (set-flago :move-counter n))

(defn inc-move-countero
  []
  (inc-flago :move-counter))

(defn dec-move-countero 
  []
  (dec-flago :move-counter))

(defn get-move-countero
  [value]
  (get-flago :move-counter value))  


; Halbzugzähler

(defn init-ply-countero
  []
  (set-flago :ply-counter 0))

(defn inc-ply-countero
  []
  (inc-flago :ply-counter))

(defn dec-ply-countero 
  []
  (dec-flago :ply-counter))

(defn get-ply-countero
  [c]
  (get-flago :ply-counter c))  

(defn get-decremented-ply-countero
  [c]
  (cl/fresh [cnt]
            (get-flago :ply-counter cnt)
            (cl/project [cnt]
                        (cl/== c (dec cnt)))))  


;% leeres Brett aufbauen
(defn clear-board-emil
  []
  (retract-all-pos-emil)
  )


; Prüfung, ob Feld zum Schachbrett gehört (11 .. 88):
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
(defn besetzt
  [Feld]
  (cl/fresh [a1 a2]
         (pos a1 a2 Feld))) 

;(cl/defne besetzt
;  [Feld]
;   [[(pos _ _ Feld)] cl/s# ] ) 

; Prüfung, ob das Feld von einer Figur bestimmter Farbe besetzt ist:
(defn besetztFarbe
  [Feld Farbe]
  (cl/fresh [a2]
         (pos Farbe a2 Feld))) 

; Prüfung, ob das Feld unbesetzt ist:
(defn frei
  [Feld]
  (noto (besetzt Feld)))


;;------------------------------------------------------------------
;; move gen

;
;% Einzelschritte der Figuren:
      
(cl/defne schritt 
  [p1 p2]
;% r =  rook, Turm
[[:r 1]] 
[[:r 10]] 
;% b = bishop, Laeufer
[[:b 9]] 
[[:b 11]] 
;% n = knight, Springer
[[:n 8]] 
[[:n 12]] 
[[:n 19]] 
[[:n 21]] 
;% k= king, Koenig
[[:k _] (schritt :r p2) ]  
[[:k _] (schritt :b p2) ]  
;% q = queen, Dame
[[:q _] (schritt :k p2)]  
)

;% Einzelschritt-Ausführung:
(defn einzelschritt
  [col von nach distanz]
  (cl/project [distanz] 
             (cl/conde 
                [(cl/== col :w) (cl/is nach von #(+ % distanz))]
                [(cl/== col :b) (cl/is nach von #(- % distanz))]
                )))

;% Distanzzuege bei Mehrschrittlern:
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
     (istBrettfeld nach)]))

(defn bauernzug-w
  [von nach]
  (cl/fresh [Zwischenfeld] 
            (cl/conde
              [(cla/< von 30) ; Doppelschritt
               (cl/is Zwischenfeld von #(+ % 10))
               (frei Zwischenfeld)
               (cl/is nach Zwischenfeld #(+ % 10))
               (frei nach)]
               [(cl/is nach von #(+ % 10)) ;; Einzelschritt
                (frei nach)]
               [(cl/is nach von #(+ % 9))
                (besetztFarbe nach :b)] ;; Schlagzug
               [(cl/is nach von #(+ % 11))
                (besetztFarbe nach :b)] ;; Schlagzug
              )))  

(defn bauernzug-b
  [von nach]
  (cl/fresh [Zwischenfeld] 
            (cl/conde
              [(cla/> von 70) ; Doppelschritt
               (cl/is Zwischenfeld von #(- % 10))
               (frei Zwischenfeld)
               (cl/is nach Zwischenfeld #(- % 10))
               (frei nach)]
               [(cl/is nach von #(- % 10)) ;; Einzelschritt
                (frei nach)]
               [(cl/is nach von #(- % 11))
                (besetztFarbe nach :w)] ;; Schlagzug
               [(cl/is nach von #(- % 9))
                (besetztFarbe nach :w)] ;; Schlagzug
              )))              

(cl/defne bauernzug 
  [color von nach]
  [[:w _ _] (bauernzug-w von nach) ]  
  [[:b _ _] (bauernzug-b von nach) ]  
)


;% Ermitteln eines pseudolegalen Zuges:
(defn pseudoZug
  [farbe figur von nach]
  (cl/fresh [distanz richt bewart blockeur]
            (cl/conde
              [(cl/all
                 (pos farbe figur von)
                 (bewegungsart figur :pawn))
               (bauernzug farbe von nach)]
              [cl/s#
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
               ]) 
            ))

;
;% Zug auf dem Brett ausführen:
(declare makeZug1)
(defn makeZug
  [Farbe, Figur, Von, Nach]
  (cl/project [Farbe, Figur, Von, Nach] 
              ; (cl/trace-lvars "makeZug:" Farbe Figur Von Nach) 
              (cl/fresh [Gegfarbe, Gegfigur N1]
                        (cl/conda
                          [(pos Gegfarbe Gegfigur Nach)
                           (get-ply-countero N1)
                           ;(cl/trace-lvars "makeZug Gegfigur:" Gegfarbe Gegfigur Nach) 
                           (cl/project [Gegfarbe Gegfigur N1]
                                       (asserto capture N1 Gegfarbe Gegfigur Nach)
                                       (retracto pos Gegfarbe Gegfigur Nach))
                           (makeZug1 Farbe Figur Von Nach) 
                           ]
                          [(makeZug1 Farbe, Figur, Von, Nach ) 
                           ]))
              cl/s#
              ))

;
;% Hilfsprädikat:           
(defn makeZug1
  [Farbe, Figur, Von, Nach]
  (cl/all 
    (doo (next-move-id))
    (inc-ply-countero)
    (retracto pos Farbe, Figur, Von)
    (asserto pos Farbe, Figur, Nach)
    ;(asserto move N Farbe, Figur, Von, Nach)
    ))

;
;% ausgefuehrten Zug zuruecknehmen:
(declare unmakeZug1)
(defn unmakeZug
  [Farbe, Figur, Von, Nach]
  ; (println "unmake!!!!")
  (cl/project [Farbe, Figur, Von, Nach]
              (cl/fresh [N1 Gegfarbe Gegfigur]
                        (get-decremented-ply-countero N1)
                        (cl/conda
                          [(capture N1 Gegfarbe Gegfigur Nach)
                           (cl/all 
                             (cl/project [N1 Gegfarbe Gegfigur] 
                                         (asserto pos Gegfarbe Gegfigur Nach)
                                         (retracto capture N1 Gegfarbe Gegfigur Nach))
                             (unmakeZug1 Farbe, Figur, Von, Nach )
                             )
                           ]
                          [(unmakeZug1 Farbe, Figur, Von, Nach)
                           ;(cl/log "unmaked non capture move")
                           ]))
              cl/s#
              )
  ) 

;% Hilfsprädikat:
(defn unmakeZug1
  [Farbe, Figur, Von, Nach]
  (cl/all 
    (dec-ply-countero)
    (retracto pos Farbe, Figur, Nach)
    (asserto  pos Farbe, Figur, Von)))


;% Prüfung, ob der König der Seite "Farbe" im Schach steht:
(defn imSchach
  [Farbe]
   (cl/fresh [Nach Farbegegner Gegnerfigur Von X Y Z]
             ;  (cl/trace-lvars "imSchach: vor pos" Farbe Farbegegner)
             (pos Farbe :k Nach)
             (oppcol Farbe Farbegegner)
             (pseudoZug Farbegegner Gegnerfigur Von Nach)
             ;   (cl/log "im Schach!")
             ))

(defn testImSchach
  [Farbe SFlag]
  (cl/conda  
    [ (imSchach Farbe)
      ;(cl/log (str Farbe " im Schach "))
     (cl/== SFlag 1)
     ]
    [ (cl/== SFlag 0)
      ;(cl/log (str Farbe " nicht im Schach "))
     ]
    ))

;% Ermitteln legaler Züge:
(defn legalZug
  [Farbe, Figur, Von, Nach]
  (cl/fresh [SFlag]
    (pseudoZug Farbe, Figur, Von, Nach) 
    (makeZug Farbe, Figur, Von, Nach)
    ;(doo (show-pos))
    (ignoreo (testImSchach Farbe SFlag))
    (unmakeZug Farbe, Figur, Von, Nach)
    (cl/== SFlag 0) 
    ))

;% Prüfung, ob die Seite "Farbe" pattgesetzt ist:
(defn patt 
  [Farbe] 
   (cl/fresh [Figur Von Nach] 
             ;(cl/log "Pattprüfung")
             (noto (imSchach Farbe))
             (noto (legalZug Farbe Figur Von Nach)))
)

;% Prüfung, ob die Seite "Farbe" mattgesetzt ist:
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
(defn zuggen
  []
  (cl/fresh [Farbe Figur Von Nach]
            (richtung Farbe)
            (pseudoZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/fail))

(defn zuggen-farbe
  [Farbe]
  (cl/fresh [Figur Von Nach]
            (pseudoZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/fail))

;% alle legalen Züge für eine Seite (w,b) generieren:
(defn legal-zuggen
  [Farbe]
  (cl/fresh [Figur Von Nach]
            (legalZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/fail))

;% alle legalen Züge für eine Figur generieren:
(defn legal-figur-zuggen
  [Farbe Figur]
  (cl/fresh [Von Nach]
            (legalZug Farbe Figur Von Nach)
            (cl/trace-lvars "Zug " Farbe Figur Von Nach)
            cl/fail))

;% Mattsuche
(declare wirdMattInGenauN)

(defn setztMattInGenauN
  [Farbe N MFlag]
   (cl/conda
     [(cl/fresh [Figur Von Nach GegFarbe VonS NachS FigS Ply]
                (legalZug Farbe Figur Von Nach)
                (oppcol Farbe, GegFarbe) 
                (makeZug Farbe, Figur, Von, Nach)
                (piece-to-stringo Farbe Figur FigS)
                (field-to-stringo Von VonS)
                (field-to-stringo Nach NachS)
                (get-ply-countero Ply)
                (cl/project [N FigS VonS NachS Ply]
                         (cl/log "Probe white:" Ply "." FigS VonS "-" NachS "(" (get-move-id) ")")
                         (ignoreo (wirdMattInGenauN GegFarbe, N, MFlag))
                         (unmakeZug Farbe, Figur, Von, Nach)
                         (cl/== MFlag 1)
                         (cl/log "!!!Mate move:" Ply "." FigS VonS "-" NachS  "(" (get-move-id) ")"))
                ;(ignoreo (cl/is PV PV #(conj % [ N Farbe Figur Von Nach]  )))
               
                )
      ;;(cl/log "Matt gefunden!")
      ]
     [cl/s#
      ;(cl/log "kein matt gefunden")
      (cl/== MFlag 0)]
     ))

;% Mattprüfungen
(defn wirdMattInGenauN
  [Farbe N MFlag]
  (cl/project [Farbe]
              ; (cl/log (str "wirdMattInGenauN: " Farbe N ))
              (cl/condu
                [(cl/all (cl/== N 1) 
                         (matt Farbe))
                 ;(cl/log "wirdMattInGenauN: matt")
                 (cl/== MFlag 1)
                 ] 
    
                 [(cl/== N 1) 
                  ;(cl/log "wirdMattInGenauN: kein matt")
                   (cl/log "........................... no mate")
                  (cl/== MFlag 0)
                  ] 
                 
                 [(patt Farbe)
                  ;(cl/log "wirdMattInGenauN: patt")
                   (cl/log "........................... stalemate!")
                  (cl/== MFlag 0)
                  ] 
                 [(cl/fresh [Figur Von Nach GegFarbe NewN VonS NachS FigS Ply]
                            (legalZug Farbe Figur Von Nach)
                            (makeZug Farbe, Figur, Von, Nach)
                            (piece-to-stringo Farbe Figur FigS)
                            (field-to-stringo Von VonS)
                            (field-to-stringo Nach NachS)
                            (get-ply-countero Ply)
                            (cl/project [N FigS VonS NachS Ply]
                                        (cl/log "........................... Answr black:"
                                                Ply "." FigS VonS "-" NachS  "(" (get-move-id) ")")
                                        (oppcol Farbe, GegFarbe) 
                                        (cl/is NewN N dec)
                                        (ignoreo (setztMattInGenauN GegFarbe, NewN, MFlag))
                                        (unmakeZug Farbe, Figur, Von, Nach)
                                        (cl/== MFlag 0)
                                        ))
                  ]
                 [(cl/== MFlag 1)
                  (cl/log "........................... mate")
                  ]
                 )))

;% Handler fuer Mattsuche
(defn mattsuche 
  [Farbe, N] 
  (cl/fresh [IstMatt]
            (doo (init-move-id))
            (init-move-countero)
            (init-ply-countero)
            (ignoreo(setztMattInGenauN Farbe, N, IstMatt))
            (cl/== IstMatt 1)))

(defn matt-in?
  [n]
  (time (doall (cl/run-db* @emil-db [q]
                    (mattsuche :w n)
                    (cl/== q "Found mate.")))))
        

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
         ;; Teststellungen
  
;% Teststellung #1: W Kh6, Ta8, S Kh8, #
(defn makeTest1
  []
  (clear-board-emil)
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
  (clear-board-emil)
  (assert-emil [pos :w :k 86])
  (assert-emil [pos :w :r 11])
  (assert-emil [pos :b :k 88])
  )

(defn test2
  []
  (makeTest2)
  ;(mattsuche :w 2)
           )

;% Teststellung #3: W Ke1, Dd1, S Kh2, #2 -> 1.Kf2!
(defn makeTest3
  []
  (clear-board-emil)
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
  (clear-board-emil)
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
(defn makeTest4
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 45])
  (assert-emil [pos :w :r 81])
  (assert-emil [pos :b :k 18])
  )

(defn test4
  []
  (makeTest4)
  ;(mattsuche :w 3)
  )

;% Teststellung #4a: W Ka5, Th1, S Ka8 , #2-> 1.Kb6!
(defn makeTest4a
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 51])
  (assert-emil [pos :w :r 18])
  (assert-emil [pos :b :k 81])
  )

(defn test4a
  []
  (makeTest4a)
  ;(mattsuche :w 3)
  )

;% Teststellung #5: W Kd6, Th1, S Kd8 , #1-> 1.Th8
(defn makeTest5
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 64])
  (assert-emil [pos :w :r 18])
  (assert-emil [pos :b :k 84])
  )

(defn test5
  []
  (makeTest5)
  ;(mattsuche :w 1)
  )


;% Teststellung #6: W Kh1, Db1, Te1, Lh6; S Kc3 -> 1.Db5!
(defn makeTest6
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 18])
  (assert-emil [pos :w :q 12])
  (assert-emil [pos :w :r 15])
  (assert-emil [pos :w :b 68])
  (assert-emil [pos :b :k 33])
  )

(defn test6
  []
  (makeTest6)
  ;(mattsuche :w 2)
  )

;% Teststellung : W Kc8, Ta1, Bb6, S Ka8, Lb8, Ba7,b7 -> 1.Ta6!
(defn makeTest7
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 83])
  (assert-emil [pos :w :r 11])
  (assert-emil [pos :w :p 62])
  (assert-emil [pos :b :k 81])
  (assert-emil [pos :b :b 82])
  (assert-emil [pos :b :p 71])
  (assert-emil [pos :b :p 72])
  )

(defn test7
  []
  (makeTest7)
  ;(mattsuche :w 2)
  )

; Teststellung für Bauernzüge
(defn makeTest8
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 18])
  (assert-emil [pos :w :p 27])
  (assert-emil [pos :w :p 28])
  (assert-emil [pos :b :p 38])
  (assert-emil [pos :b :p 75])
  (assert-emil [pos :w :p 64])
  (assert-emil [pos :w :p 66])
  )

(defn test8
  []
  (makeTest8)
  ;(mattsuche :w 2)
  )

;;------------------------------------------------------------------
;; runs


(test7)

(show-pos)



(comment 
  
  (cl/run-db* @emil-db [q]
              (set-flago :a 42)
              (inc-flago :a)
              (traceo-s-meta)
              (dec-flago :a)
              (dec-flago :a)
              (get-flago :a q)) ;; = 41
  
  (def x (cl/run-db* @emil-db [q]
                     (makeZug :w :r 18 28)
                     (pos :b :k q)
                     ))
  (println "x:" x)
  
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
  
  (cl/run-db* @emil-db [q]
              (zuggen)
              (cl/== q 1) 
              )
  
  (doall (cl/run-db* @emil-db [q]
                     (find-all sol 
                               (cl/fresh [figur von nach]
                                         (pseudoZug :w figur von nach)
                                         (cl/== sol [figur von nach]))
                               q)))
  (doall (cl/run-db* @emil-db [q]
                     (find-n-sols 4 sol 
                                  (cl/fresh [figur von nach]
                                            (pseudoZug :w figur von nach)
                                            (cl/== sol [figur von nach]))
                                  q)))
  
  ;; alle Züge aller Figuren
  (pprint 
    (sort (cl/run-db* @emil-db [q]
                      (cl/fresh [x y z q1] 
                                (pseudoZug x y z q1)
                                (cl/== q [x y :from z :to q1])))))
  
  (doall (cl/run-db* @emil-db [q]
                     (cl/fresh [figur von nach]
                               (pseudoZug :w figur von nach)
                               (cl/== q [figur von nach]))))
  
  
  (cl/run-db* @emil-db [q]
              (asserto pos :w :q 66)
              (cl/log "asserted.") 
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (retracto pos :w :q 66)
              (cl/log "retracted.") 
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (asserto pos :w :q 66)
              (cl/log "asserted.") 
              (traceo-s-meta)
              (retracto pos :w :q 66)
              (cl/log "retracted.") 
              (traceo-s-meta)
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (retractallo "pos" 3)
              (cl/log "retracted pos.") 
              (traceo-s-meta)
              (cl/== q true))
  
  
  (cl/run-db* @emil-db [q]
              (cl/log "ply counter before is " (get-ply-counter)) 
              (doo (incr-ply-counter))
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
  
  (def x (cl/run-db* @emil-db [q]
                     (setztMattInGenauN :w, 1, q)
                     (cl/== q 1) 
                     ))
  
  (time
    (doall
      (cl/run-db* @emil-db [q]
                  (mattsuche :w, 2)
                  (cl/== q 1))))
  
  (matt-in? 2)
  
  )

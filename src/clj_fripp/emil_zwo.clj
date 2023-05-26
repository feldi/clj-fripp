(ns clj-logic.emil-zwo
  "Emil umgesetzt mit Clojure core.logic."
  (:require [clojure.core.logic :as cl]
            [clojure.core.logic.arithmetic :as cla]
            [clojure.core.logic.pldb :as pldb]
            [clj-logic.extend :as cle])
   (:use clojure.pprint clojure.repl)
  )

;; the emil chess database

(def emil-db (atom {}))

(defn seto-emil-db 
  "Goal that resets the chess facts database."
  []
  (fn [a]
     (reset! emil-db (-> a meta :db first))
     a))

(defn assert-emil 
  [fact] 
  (reset! emil-db (apply pldb/db-fact @emil-db fact)))
     
(defn retract-emil 
  [fact]
  (reset! emil-db (apply pldb/db-retraction @emil-db fact)))

(defn retract-all-emil
  "delete relation from emil DB."
  [rel arity] 
  (swap! emil-db cle/retract-rel rel arity))

(defn retract-all-pos-emil
  "delete all 'pos' facts, i.e. clear the board."
  [] 
  (retract-all-emil "pos" 3))

; leeres Brett aufbauen
(defn clear-board-emil
  []
  (retract-all-pos-emil))


;; Relations for the move generator

; Brettstellung / chess position
(pldb/db-rel pos ^:index color ^:index piece ^:index square) 

; Schlagfelder / captures
(pldb/db-rel capture ^:index ply ^:index color ^:index piece ^:index square) 

; Zug / move
(pldb/db-rel move ^:index move-id ply color piece from to) 

; Brettrichtungen white (a1->a8), black (a8->a1):
(pldb/db-rel direction ^:index p) 
(assert-emil [direction :w]) 
(assert-emil [direction :b])

; opponent colors w<->b:
(pldb/db-rel oppcol ^:index c p) 
(assert-emil [oppcol :w :b]) 
(assert-emil [oppcol :b :w ]) 

; Bewegungsarten der Figuren:
(pldb/db-rel move-type ^:index p1 p2) 
(assert-emil [move-type :k :single])
(assert-emil [move-type :n :single])
(assert-emil [move-type :b :multiple])
(assert-emil [move-type :r :multiple])
(assert-emil [move-type :q :multiple])
(assert-emil [move-type :p :pawn])


; output

(cl/defne piece-to-stringo 
   "Convert color & peace keys to string representation."
  [color piece s]
[[:w :k "K"]] 
[[:w :q "Q"]] 
[[:w :r "R"]] 
[[:w :b "B"]] 
[[:w :n "N"]] 
[[:w :p "P"]] 
[[:b :k "k"]] 
[[:b :q "q"]] 
[[:b :r "r"]] 
[[:b :b "b"]] 
[[:b :n "n"]] 
[[:b :p "p"]])

(def lower-a-offset (dec (int \a)))

(defn field-to-string 
  "Convert numeric field to string representation. Functional version."
  [field]
  (let [row  (mod field 10)
        line (quot field 10)]
    (str (char (+ lower-a-offset row)) line)))

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

; Move_ID

(defn init-move-ido
  []
  (cle/set-valo :move-id 0))

(defn next-move-ido
  [next-id]
  (cl/all
    (cle/inc-valo :move-id)
    ;;(cl/log "next move id: " (cle/get-val :move-id))
    (cl/== next-id (cle/get-val :move-id))))

(defn get-move-ido
  [id]
  (cle/get-valo :move-id id))  


; Node count

(defn set-node-countero
  [n]
  (cle/set-valo :node-counter n))

(defn inc-node-countero
  []
    (cle/inc-valo :node-counter))

(defn get-node-countero
  [value]
  (cle/get-valo :node-counter value))  


; Zugzaehler

(defn set-move-countero
  [n]
  (cle/set-flago :move-counter n))

(defn inc-move-countero
  []
  (cle/inc-flago :move-counter))

(defn dec-move-countero 
  []
  (cle/dec-flago :move-counter))

(defn get-move-countero
  [value]
  (cle/get-flago :move-counter value))  

(cl/defne inc?-move-countero 
  [color]
  [[:w] (inc-move-countero) ]  
  [[:b] cl/s# ])

(cl/defne dec?-move-countero 
  [color]
  [[:w] (dec-move-countero) ]  
  [[:b] cl/s# ])


; Halbzugzähler

(defn set-ply-countero
  [n]
  (cle/set-flago :ply-counter n))

(defn inc-ply-countero
  [ply]
  (cl/all
    (cle/inc-flago :ply-counter)
    (cle/get-flago :ply-counter ply)))

(defn dec-ply-countero 
  []
  (cle/dec-flago :ply-counter))

(defn get-ply-countero
  [n]
  (cle/get-flago :ply-counter n))  

(defn get-decremented-ply-countero
  [c]
  (cl/fresh [cnt]
            (cle/get-flago :ply-counter cnt)
            (cl/project [cnt]
                        (cl/== c (dec cnt)))))  


; Prüfung, ob Feld zum Schachbrett gehört (11 .. 88):
(defn on-boardo
 [square]
  (cl/fresh [e] 
    (cla/< 10 square)
    (cla/< square 89)
    (cl/is e square #(mod % 10))
    (cla/< 0 e)
    (cla/< e 9))
  )  

#_(defn on-boardo
  [square]
   (fn [a]
     (let [fld (cl/walk* a square)
           e (mod fld 10)]
       (if (and (< 10 fld)
                (< fld 89)
                (< 0 e)
                (< e 9))
         (cl/succeed a)
         (cl/fail a)))))  

; Prüfung, ob das Feld besetzt ist:
(defn square-occupiedo
  [sq]
  (cl/fresh [a1 a2]
         (pos a1 a2 sq))) 

;(cl/defne square-occupiedo
;  [sq]
;   [[(pos _ _ sq)]] ) 

; Prüfung, ob das Feld von einer Figur bestimmter Farbe besetzt ist:
(defn square-occupied-by-coloro
  [sq color]
  (cl/fresh [a2]
         (pos color a2 sq))) 

; Prüfung, ob das Feld unbesetzt ist:
(defn square-emptyo
  [sq]
  (cle/noto (square-occupiedo sq)))


;;------------------------------------------------------------------
; Logging / Printing

(defn log-move
  [title piece from to move-num ply move-id]
  (cl/project [title piece from to move-num ply move-id]
              #_(cl/log "emil-log:" title
                      (str move-num ". " piece from "-" to
                         " (ply: " ply ", move-id: " move-id ")"))
            (cle/log-info "emil-info: " title
                       move-num ". " piece from "-" to
                           " (ply: " ply ", move-id: " move-id ")")
            )
  )


;;------------------------------------------------------------------
;; move gen

;
;% Einzelschritte der Figuren:
      
(cl/defne stepo 
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
[[:k _] (stepo :r p2) ]  
[[:k _] (stepo :b p2) ]  
;% q = queen, Dame
[[:q _] (stepo :k p2)]  
)

;% Einzelschritt-Ausführung:
(defn single-stepo
  [col from to distance]
  (cl/project [distance] 
             (cl/conde 
                [(cl/== col :w) (cl/is to from #(+ % distance))]
                [(cl/== col :b) (cl/is to from #(- % distance))]
                )))

;% Distanzzuege bei Mehrschrittlern:
(defn multi-stepo
  [multiple from to distance direction]
  (cl/conde
    [(cl/== multiple :multiple) 
     (cl/fresh [z] 
               (single-stepo direction from z distance)
               (on-boardo z)
               (square-emptyo z)
               (multi-stepo multiple z to distance direction))
     ]
    [cl/s#
     (single-stepo direction from to distance)
     (on-boardo to)]))

(defn pawn-move-whiteo
  [from to]
  (cl/fresh [z] 
            (cl/conde
              [(cla/< from 30) ; Doppelschritt
               (cl/is z from #(+ % 10))
               (square-emptyo z)
               (cl/is to z #(+ % 10))
               (square-emptyo to)]
              [(cl/is to from #(+ % 10)) ;; Einzelschritt
               (square-emptyo to)]
              [(cl/is to from #(+ % 9))
               (square-occupied-by-coloro to :b)] ;; Schlagzug
              [(cl/is to from #(+ % 11))
               (square-occupied-by-coloro to :b)] ;; Schlagzug
              )))  

(defn pawn-move-blacko
  [from to]
  (cl/fresh [z] 
            (cl/conde
              [(cla/> from 70) ; Doppelschritt
               (cl/is z from #(- % 10))
               (square-emptyo z)
               (cl/is to z #(- % 10))
               (square-emptyo to)]
               [(cl/is to from #(- % 10)) ;; Einzelschritt
                (square-emptyo to)]
               [(cl/is to from #(- % 11))
                (square-occupied-by-coloro to :w)] ;; Schlagzug
               [(cl/is to from #(- % 9))
                (square-occupied-by-coloro to :w)] ;; Schlagzug
              )))              

(cl/defne pawn-moveo 
  [color from to]
  [[:w _ _] (pawn-move-whiteo from to) ]  
  [[:b _ _] (pawn-move-blacko from to) ])


;% Ermitteln eines pseudolegalen Zuges:
(defn pseudo-moveo
  [color piece from to]
  (cl/fresh [distance dir move-typ blocker]
            (cl/conde
              [(cl/all
                 (pos color piece from)
                 (move-type piece :pawn))
               (pawn-moveo color from to)]
              [cl/s#
               (pos color piece from)
               (stepo piece distance)
               (direction dir)
               (move-type piece move-typ) 
               (multi-stepo move-typ from to distance dir)
               (cle/noto (pos color blocker to))
               ]) 
            ))

;
;% Zug auf dem Brett ausführen:
(declare make-move1o)
(defn make-moveo
  [color, piece, from, to]
  (cl/project [color, piece, from, to] 
              (cl/fresh [opp-color, opp-piece N1]
                        (cl/conda
                          [(pos opp-color opp-piece to)
                           (get-ply-countero N1)
                           (cl/project [opp-color opp-piece N1]
                                       (cle/asserto capture N1 opp-color opp-piece to)
                                       (cle/retracto pos opp-color opp-piece to))
                           (make-move1o color piece from to) 
                           ]
                          [(make-move1o color, piece, from, to ) 
                           ]))
              cl/s#
              ))

;
;% Hilfsprädikat:           
(defn make-move1o
  [color, piece, from, to]
  (cl/fresh [next-id ply] 
    ;;(cl/log "make a move" color piece from to)
    (next-move-ido next-id)
    (inc-node-countero)
    (inc-ply-countero ply)
    (inc?-move-countero color)
    (cle/retracto pos color, piece, from)
    (cle/asserto pos color, piece, to)
    ;;(cle/asserto move next-id ply color, piece, from, to)
    ))

;
;% ausgefuehrten Zug zuruecknehmen:
(declare unmake-move1o)
(defn unmake-moveo
  [color, piece, from, to]
  (cl/project [color, piece, from, to]
              (cl/fresh [N1 opp-color opp-piece]
                        (get-decremented-ply-countero N1)
                        (cl/conda
                          [(capture N1 opp-color opp-piece to)
                           (cl/all 
                             (cl/project [N1 opp-color opp-piece] 
                                         (cle/asserto pos opp-color opp-piece to)
                                         (cle/retracto capture N1 opp-color opp-piece to))
                             (unmake-move1o color, piece, from, to )
                             )
                           ]
                          [(unmake-move1o color, piece, from, to)
                           ;(cl/log "unmaked non capture move")
                           ]))
              cl/s#
              )
  ) 

;% Hilfsprädikat:
(defn unmake-move1o
  [color, piece, from, to]
  (cl/all 
    (dec-ply-countero)
    (dec?-move-countero color)
    (cle/retracto pos color, piece, to)
    (cle/asserto  pos color, piece, from)))


;% Prüfung, ob der König der Seite "color" im Schach steht:
(defn in-checko
  [color]
   (cl/fresh [to opp-color opp-piece from X Y Z]
             ; (cl/trace-lvars "in-checko: vor pos" color opp-color)
             (pos color :k to)
             (oppcol color opp-color)
             (pseudo-moveo opp-color opp-piece from to)
             ; (cl/log "im Schach!")
             ))

(defn check?o
  [color SFlag]
  (cl/conda  
    [ (in-checko color)
      ;(cl/log (str color " im Schach "))
     (cl/== SFlag 1)
     ]
    [ (cl/== SFlag 0)
      ;(cl/log (str color " nicht im Schach "))
     ]
    ))

;% Ermitteln legaler Züge:
(defn legal-moveo
  [color, piece, from, to]
  (cl/fresh [SFlag]
            (pseudo-moveo color, piece, from, to) 
            (make-moveo color, piece, from, to)
            (cle/ignoreo (check?o color SFlag))
            (unmake-moveo color, piece, from, to)
            (cl/== SFlag 0) 
    ))

;% Prüfung, ob die Seite "color" pattgesetzt ist:
(defn stalemate?o 
  [color] 
   (cl/fresh [piece from to] 
             (cle/noto (in-checko color))
             (cle/noto (legal-moveo color piece from to)))
)

;% Prüfung, ob die Seite "color" mattgesetzt ist:
(defn mate?o 
  [color] 
   (cl/fresh [piece from to] 
             ;(cl/log "Mattprüfung")
             (in-checko color)  
             (cle/noto (legal-moveo color piece from to))
             ))
 
;%------------------------------------------------------------------------------
;% Testtreiber
;%------------------------------------------------------------------------------
;
;% alle pseudolegalen Züge generieren (Test):
(defn move-geno
  []
  (cl/fresh [color piece from to]
            (direction color)
            (pseudo-moveo color piece from to)
            (cl/trace-lvars "Move " color piece from to)
            cl/fail))

(defn move-geno-color
  [color]
  (cl/fresh [piece from to]
            (pseudo-moveo color piece from to)
            (cl/trace-lvars "Move " color piece from to)
            cl/fail))

;% alle legalen Züge für eine Seite (w,b) generieren:
(defn legal-move-geno
  [color]
  (cl/fresh [piece from to]
            (legal-moveo color piece from to)
            (cl/trace-lvars "Move " color piece from to)
            cl/fail))

;% alle legalen Züge für eine Figur generieren:
(defn legal-piece-move-geno
  [color piece]
  (cl/fresh [from to]
            (legal-moveo color piece from to)
            (cl/trace-lvars "Move " color piece from to)
            cl/fail))

;% Mattsuche / Search

(declare mated-in-n-moveso)

(defn mates-in-n-moveso
  [color N MFlag pv-b pv-new]
   (cl/conda
     [
     (cl/fresh [piece from to opp-color fromS toS pieceS move-num ply move-id pv-w mflag-opp]
               (legal-moveo color piece from to)
               (oppcol color, opp-color) 
               (make-moveo color, piece, from, to)
               (get-move-ido move-id)
               ;;(log-move "Make-white-move " piece from to nil nil move-id)
               (piece-to-stringo color piece pieceS)
               (field-to-stringo from fromS)
               (field-to-stringo to toS)
               (get-move-countero move-num)
               (get-ply-countero ply)
               (cl/project [N pieceS fromS toS move-num ply move-id]
                        #_(log-move "White probe: " pieceS fromS toS move-num ply move-id)
                        (cle/ignoreo (mated-in-n-moveso opp-color N mflag-opp pv-b pv-w))
                        (unmake-moveo color, piece, from, to)
                        (cl/conda
                         [(cl/== mflag-opp 1)
                          (log-move "Mating move: " pieceS fromS toS move-num ply move-id)
                          (cl/is pv-new pv-w #(conj [[:mating piece from to move-num ply move-id]] %))
                          (cl/== MFlag 1)
                          ]
                         [(cl/is pv-new pv-w #(conj [[:not-mating piece from to move-num ply move-id]] %))
                          (log-move "no Mating move: " pieceS fromS toS move-num ply move-id)
                          (cl/== MFlag 0)
                          cl/u#
                          ])
                        ))
     #_(cl/log "Mating move found!")
      ]
     [cl/s#
     ;;(cl/log "no Mating move found")
     #_(cl/== pv-new [:no-mating-move])
     (cl/is pv-new pv-b #(conj % :no-mating-move))
     (cl/== MFlag 0)]
     )
   )

;% Mattprüfungen
(defn mated-in-n-moveso
  [color N MFlag pv-w pv-new]
  (cl/project [color]
              ; (cl/log (str "mated-in-n-moveso: " color N ))
              (cl/condu
                [(cl/all (cl/== N 1) 
                         (mate?o color))
                 (cl/log "mated-in-1")
                 (cl/is pv-new pv-w #(conj % [:mated-in-1]))
                 (cl/== MFlag 1)
                 ] 
    
                 [(cl/== N 1) 
                  #_(cl/log "........................... no mate")
                  (cl/is pv-new pv-w #(conj % [:no-mate]))
                  (cl/== MFlag 0)
                  ] 
                 
                 [(stalemate?o color)
                  (cl/log "........................... stalemate!")
                  (cl/is pv-new pv-w #(conj % [:stalemate]))
                  (cl/== MFlag 0)
                  ] 
                 [(cl/fresh [piece from to opp-color NewN fromS toS pieceS move-num ply move-id pv-b mflag-opp]
                            (legal-moveo color piece from to)
                            (make-moveo color, piece, from, to)
                            (get-move-ido move-id)
                            #_(log-move "Make-black-move " piece from to nil nil move-id)
                            (piece-to-stringo color piece pieceS)
                            (field-to-stringo from fromS)
                            (field-to-stringo to toS)
                            (get-move-countero move-num)
                            (get-ply-countero ply)
                            (cl/project [N pieceS fromS toS move-num ply move-id]
                                        #_(log-move "........................... Black refute:"
                                                   pieceS fromS toS move-num ply move-id)
                                        (oppcol color, opp-color) 
                                        (cl/is NewN N dec)
                                        (cle/ignoreo (mates-in-n-moveso opp-color NewN mflag-opp pv-w pv-b))
                                        (unmake-moveo color, piece, from, to)
                                        #_(cl/== MFlag 0)
                                        (cl/conda
                                         [(cl/== mflag-opp 0)
                                          (cl/log "........................... no mate!")
                                          (cl/is pv-new pv-b #(conj [[:refuse color piece from to move-num ply move-id]] %))
                                          (cl/== MFlag 0)]
                                         [(cl/== mflag-opp 1)
                                          (cl/is pv-new pv-b #(conj [[:move-b color piece from to move-num ply move-id]] %))
                                          (cl/log "........................... mate!")
                                          (cl/== MFlag 1)
                                          cl/u#
                                          ]
                                         )
                                        ))
                  ]
                 [(cl/== MFlag 1)
                 (cl/log "........................... mate")
                 (cl/is pv-new pv-w #(conj % :mated ))
                 ]
                 )))

;% Handler fuer Mattsuche
(defn search-mateo 
  [color N pv pv-new] 
  (cl/fresh [is-mated?]
            (init-move-ido)
            (set-node-countero 0)
            (set-move-countero 0)
            (set-ply-countero 0)
            (cle/ignoreo(mates-in-n-moveso color N is-mated? pv pv-new))
            (cl/== is-mated? 1)))

(defn mate-in
  [n]
  (time (doall (cl/run-db* @emil-db [q nodes pv pv-new]
                           (cl/== pv [])
                           (search-mateo :w n pv pv-new)
                           (get-node-countero nodes)
;                    (cl/project [nodes]
;                                (cl/log (str "Nodes visited: " nodes)))
                           (cl/== q "Found mate, nodes visited:, pv: ")))))


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
  ;(search-mateo :w 2)
           )

;% Teststellung #3: W Ke1, Dd1, S Kh2, #2 -> 1.Kf2!
(defn makeTest3
  []
  (clear-board-emil)
  (assert-emil [pos :w :k 15])
  (assert-emil [pos :w :q 14])
  (assert-emil [pos :b :k 28])
)  

(defn test3
  []
  (makeTest3)
  ;(search-mateo :w 2)
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
  ;(search-mateo :w 2)
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
  ;(search-mateo :w 3)
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
  ;(search-mateo :w 3)
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
  ;(search-mateo :w 1)
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
  ;(search-mateo :w 2)
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
  ;(search-mateo :w 2)
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
  ;(search-mateo :w 2)
  )

;;------------------------------------------------------------------
;; runs


(test7)

(show-pos)



(comment 
  
  (cl/run-db* @emil-db [q]
              (cle/set-flago :a 42)
              (cle/inc-flago :a)
              (cle/traceo-s-meta)
              (cle/dec-flago :a)
              (cle/dec-flago :a)
              (cle/get-flago :a q)) ;; = 41
  
  (def x (cl/run-db* @emil-db [q]
                     (make-moveo :w :r 18 28)
                     (pos :b :k q)
                     ))
  (defn print-step
    [] 
    (cl/run-db* emil-db [q]
                (cl/fresh [x y]
                          (stepo x y)
                          ;;;(cl/trace-lvars "stepo: " x y) 
                          (cl/project [x y]   
                                      (cl/log x y)) 
                          ))
    'done)
  
  (cl/run-db* @emil-db [q]
              (cl/fresh [x]
                        (direction x)
                        (cl/== q x) 
                        )) 
  
  (cl/run-db* @emil-db [q]
              (cl/fresh [x y]
                        (direction x) 
                        (oppcol x y )
                        (cl/== q [x y]) 
                        )) 
  
  (cl/run-db* @emil-db [q]
              (cl/fresh [x]
                        (cl/== x 78 ) 
                        (on-boardo x)
                        (cl/== q x) 
                        ))
  
  (cl/run-db* @emil-db [q]
              (cl/fresh [x]
                        (square-occupiedo x)
                        (cl/== q x) 
                        )) 
  
  (cl/run-db* @emil-db [q]
              (square-emptyo 44)
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (square-emptyo 45)
              (cl/== q true))
  
  ;; alle Züge wTh1
  (sort 
    (cl/run-db* @emil-db [q]
                (pseudo-moveo :w :r 81 q)
                ))
  
  ;; alle Züge weiss
  (sort (cl/run-db* @emil-db [q]
                    (cl/fresh [x y] 
                              (pseudo-moveo :w x y q))))
  
  (cl/run-db* @emil-db [q]
              (move-geno)
              (cl/== q 1) 
              )
  
  (doall (cl/run-db* @emil-db [q]
                     (cle/find-all sol 
                               (cl/fresh [piece from to]
                                         (pseudo-moveo :w piece from to)
                                         (cl/== sol [piece from to]))
                               q)))
  (doall (cl/run-db* @emil-db [q]
                     (cle/find-n-sols 4 sol 
                                  (cl/fresh [piece from to]
                                            (pseudo-moveo :w piece from to)
                                            (cl/== sol [piece from to]))
                                  q)))
  
  ;; alle Züge aller Figuren
  (pprint 
    (sort (cl/run-db* @emil-db [q]
                      (cl/fresh [x y z q1] 
                                (pseudo-moveo x y z q1)
                                (cl/== q [x y :from z :to q1])))))
  
  (doall (cl/run-db* @emil-db [q]
                     (cl/fresh [piece from to]
                               (pseudo-moveo :w piece from to)
                               (cl/== q [piece from to]))))
  
  
  (cl/run-db* @emil-db [q]
              (cle/asserto pos :w :q 66)
              (cl/log "asserted.") 
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (cle/retracto pos :w :q 66)
              (cl/log "retracted.") 
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (cle/asserto pos :w :q 66)
              (cl/log "asserted.") 
              (cle/traceo-s-meta)
              (cle/retracto pos :w :q 66)
              (cl/log "retracted.") 
              (cle/traceo-s-meta)
              (cl/== q true))
  
  (cl/run-db* @emil-db [q]
              (cle/retract-allo "pos" 3)
              (cl/log "retracted pos.") 
              (cle/traceo-s-meta)
              (cl/== q true))
  
  
  (cl/run-db* @emil-db [q]
              (cl/log "ply counter before is " (get-ply-countero)) 
              (cle/doo (incr-ply-countero))
              (cl/log "ply counter now is " (get-ply-countero)) 
              (cl/== q true))
  
  (def x (cl/run-db* @emil-db [q]
                     (make-moveo :w :r 11 18)
                     (cl/== q true) 
                     ))
  
  (def x (cl/run-db* @emil-db [q]
                     (in-checko :b)
                     (cl/== q true) 
                     ))
  
  (def x (cl/run-db* @emil-db [q]
                     (mates-in-n-moveso :w, 1, q)
                     (cl/== q 1) 
                     ))
  
  (time
    (doall
      (cl/run-db* @emil-db [q]
                  (search-mateo :w, 2)
                  (cl/== q 1))))
  
  (mate-in 2)
  
  )

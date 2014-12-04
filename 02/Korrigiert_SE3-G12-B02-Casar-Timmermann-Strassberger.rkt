#lang racket
#|
  Abgabe Blatt 2
  Gruppe:
  Michael Strassberger  6527713
  Alexander Timmermann  6524072
  Christian Casar       6214251
|#
;;; 1 Symbole und Werte, Umgebungen
(define miau 'Plueschi)
(define katze miau)
(define tiger 'miau)

(define (welcherNameGiltWo PersonA PersonB)
   (let ((PersonA 'Sam)
             (PersonC PersonA))
       PersonC))

(define xs1 '(0 2 3 miau katze))
(define xs2 (list miau katze))
(define xs3 (cons katze miau))

;; 1. miau
; -> 'Plueschi
; miau wurde das Symbol "'Plueschi" als Wert zugewiesen

;; 2. katze
; ->'Plueschi
; katze referenziert die Variable miau
;;;;;rein technisch ist miau auch ein Symbol, nur das es hier weiter ausgewertet wird bis zu seinem Wert

;; 3. tiger
; -> 'miau
; 'miau ist ein Symbol und wird nicht weiter ausgewertet

;; 4. (quote katze)
; -> 'katze
; katze wird druch quote zu einem Symbol umgewandelt

;; 5. (eval tiger)
; -> 'Plueschi
; tiger wird zunaechst zu 'miau ausgewertet. Die Funktion eval evaluiert anschliessend
; Symbol 'miau zu 'Plueschi.


;; 6. (eval katze)
; -> Plueschi: undefined; cannot reference an identifier before its definition
; eval versucht das von katze referenzierte Symbol 'Plueschi zu evaluieren, Plueschi
; ist aber nicht definiert.


;; 7. (eval 'tiger)
; -> 'miau
; eval wertet 'tiger aus und tiger ist 'miau zugewiesen.


;; 8. (welcherNameGiltWo 'harry 'potter)
; -> 'harry
; welcherNameGiltWo liefert als Rueckgabe den Wert von PersonC zurueck. PersonC
; wird der Wert des Parameters PersonA zugewiesen, da das im let-Block definierte 
; PersonA im let-Block selbst undefiniert ist.


;; 9. (cdddr xs1)
; -> '(miau katze)
; cdr liefert das zweite Element eines gegebenen Paars p zurueck. Ist das Paar eine
; Liste, werden alle Listenelemente bis auf das erste zurückgeliefert.
; cdddr ist eine rekursiv definierte Funktion und liefert (cdr (cdr (cdr p))). 
; Im Beispiel werden die letzten beiden Element der Liste ausgegeben.


;; 10. (cdr xs2)
; -> '(Plueschi)
; Da miau und katze Variablen sind, werden bei der Verwendung beide Variablen
; durch ihren Wert 'Plueschi ersetzt. cdr liefert wiederum das zweite Element der 
; Liste.


;; 11. (cdr xs3)
; -> 'Plueschi
; (cons a b) definiert ein Paar mit den Elementen a und b. Vor der Anwendung von cdr
; auf xs3 werden katze und miau zu 'Plueschi evaluiert.


;; 12. (eval (sqrt 3))
; -> 1.7320508075688772
; eval wertet das Ergebnis von sqrt 3 aus aus.
;;;;;Diese Ergebnis ist die Zahl 1.732... und Zahlen evaluieren zu sich selbst


;; 13. (eval '(welcherNameGiltWo 'tiger 'katze))
; -> 'tiger
; eval wertet seine Argumentliste '(welcherNameGiltWo 'tiger 'katze) aus und
; fuehrt dabei die Funktion welcherNameGiltWo mit den Paramtern 'tiger und 'katze
; aus.


;; 14. (eval (welcherNameGiltWo 'katze 'tiger ))
; -> 'Plueschi
; Zunächst wird welcherNameGiltWo ausgewertet und eval wertet dann das Symbol 'katze zu 'Plueschi aus

;;;;;
;9/9 Punkten
;;;;;

;;; 2.1 Die Fakultaet einer Zahl
(define (faculty number)
  (cond 
    [(> number 1) (* number (faculty (- number 1)))]
    [(= number 1) number])) ;;;;;(= number 0) 1 wäre schöner um auch faculty 0 berechnen zu können

;;;;;
;2/2 Punkten
;;;;;

;;; 2.2 Potezen von Rationalzahlen
(define (power r n)
  (cond
    [(= n 0) 1]
    [(even? n) (sqr (power r (/ n 2)))]
    [(odd?  n) (* (power r (- n 1)) r)]))

;;;;;
;3/3 Punkten
;;;;;

;;; 2.3 Die Eulerzahl
(define (euler)
  (eulern 454))

(define (eulern n)
   (truncate 
    (*
     (/
      (for/sum ([i (range 1 n)])  ;;;;;Ihr sollt rekursive Funktionen schreiben und keine for Iteration verwenden
        (cond
          [(= i 1) 1]
          [(> i 1) (/ i (faculty (- i 1)))]))
      2)
     (power 10 1001))))

(define (findLeastIterationEuler) ;;;;;Habt ihr das wirklich durchlaufen lassen um zu bestimmen, dass bei ca 454 n/(n-1)! kleiner als 1/10^1000 wird?
  (for/list ([i (range 1 1000)])
    (display (- 1000 i))
    (if (= (-
            (eulern (- 1000 i))
            (eulern (- (- 1000 i) 1))) 0) 
        (display " Yes")
        (display " No"))
    (display "\n")))

;;;;;
;Keine Rekursion angewandt
;0/6 Punkten
;;;;;

;;; 2.4 Pi
(define (my-pi)
  (truncate
   (*
    (*
     4
     (for/sum ([i (range 1 25000)])
       (/
        1
        (cond
          [(= i 1) 1]
          [(odd?  i) (+ 1  (* 2 (- i 1)))  ]
          [(even? i) (- -1 (* 2 (- i 1)))  ]))))
    (power 10 4))))

;;;;;
;Keine Rekursion angewandt
;1/4 Zusatzpunkten
;;;;;

(define (type-of el)
  (cond
    [(procedure? el) 'Procedure]
    [(vector? el) 'Vector]
    [(string? el) 'String]
    [(char? el) 'Char]
    [(number? el) 'Number]
    [(symbol? el) 'Symbol]
    [(list? el) 'List]
    [(pair? el) 'Pair]
    [(boolean? el) 'Boolean]))

;;;;;
;0,5/5 Punkten
;
;Gesamt 14,5/25 Punkten
;+1/4 Zusatzpunkten
;;;;;

#lang racket

#|
    Abgabe Blatt 8
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#

(require swindle/extra)
(require se3-bib/setkarten-module)

#| Aufgabe 1:
    1.1: Eine Funktion höherer Ordnung ist eine Funktion, die eine Funktion als 
    ==== Argument erhält und/oder eine Funktion als Rückgabewert ausgibt.

    1.2: * foldl: ja, nimmt eine Funktion als erstes Argument entgegen
    ==== * kopf-oder-schwanz: nein, keine Funktion als Argument oder Rückgabe
;;;;; es wird entweder die Funktion car oder die Funktion cdr zurück gegeben
         * pimento: ja, Funktion als erstes Argument
;;;;; und eine Funktion zurück gegeben
         * my-tan: nein, keine Funktion als Argument oder Rückgabe

    1.3: Der Rückgabewert von (pimento * 5) ist eine anonyme Funktion, bei der
    ==== f an die *-Funktion und arg1 an 5 gebunden ist. Beim Aufruf von
         ((pimento * 5) 7) wird für die anonyme Funktion in der lokalen Umgebung
         arg2 an den Wert 7 gebunden. Es wird logischerweise 35 zurückgegeben.

    1.4: 1. (foldl (curry * 2) 1 '(1 1 2 3))
    ====      --> 96. foldl "faltet" die Liste mit der gegebenen Funktion von
                  links nach rechts auf. Dabei wird als Funktion hier eine ge-
                  curriete Version der Multiplikation benutzt. Die Werte werden
                  also erst verdoppelt und dann mit dem Rest multipliziert.
;;;;; Rest??? besser Zwischenergebnis

         2. (map cons '(1 2 3 4) '(1 2 3 4))
              --> '((1 . 1) (2 . 2) (3 . 3) (4 . 4)). map bildet die zwei Listen
                  unter Zuhilfenahme der Funktion im 1. Argument, also der cons-
                  Funktion, auf eine neue Liste ab, woraus die Liste aus Paaren
                  entsteht.

         3. (filter pair? '((a b ) () 1 (())))
              --> '((a b) (())). filter benutzt die gegebene Funktion und durch-
                  sucht die Liste. (a b) und (()) gelten als Paare. Die Elemente,
                  für die die Funktion #t zurückgibt, werden als neue Liste zurück-
                  gegeben.

         4. (map (compose (curry + 33) (curry * 1.8)) '(5505 100 0 1 1000 -273.15))
              --> '(9942.0 213.0 33 34.8 1833.0 -458.66999999999996). map bildet
                  immer noch eine Liste auf eine andere Liste ab. Hier wird die
                  Funktion aus zwei gecurrieten Funktionen zusammengesetzt. compose
                  wendet die Funktionen in umgekehrter Reihenfolge an, daher werden
                  alle Elemente erst mit 1.8 multipliziert und dann zu 33 addiert.
|#

;;;;;
;Erläuterung der Closure und der Bindungen sind ein bisschen kurz ausgefallen, aber okay
;9/10 Punkten
;;;;;

; Aufgabe 2
; NB: In der Aufgabe wird ausdrücklich von "Ausdrücken" gesprochen, deshalb haben
;     wir keine Funktionen definiert. Man kann zum Testen dann einfach den ent-
;     sprechenden Ausdruck auskommentieren.
;; Testliste:
(define xs '(12 29 -30 15 7 2 -400 36))

;; 1. Absolutwerte
; (map abs xs)

;; 2. durch 3 teilbar
; (filter (compose zero? (curryr modulo 3)) xs)

;; 3. Summe aller geraden Zahlen über 10
; (foldl + 0 (filter (curry < 10) (filter even? xs)))

;; 4. Aufteilen anhand eines Prädikats. Als Beispiel: odd?
; (partition odd? xs)
;;;;;
;Ihr solltet selber einen Ausdruck schreiben
;;;;;

;;;;;
;6/6 Punkten
;+ 0/3 Zusatzpunkten
;;;;;

; 3 Spieltheorie: Das Kartenspiel SET!
; 3.1 Spielkartenauspraegung, Repraesentation
;; Es wuerde sich anbieten die Kartenelement (Zahl, Muster...) als einzelne
;; Listen darzustellen, da zur Generierung aller Karten aber amb verwendet wird
;; und amb nur wie erwuenscht arbeitet, wenn die zu betrachtenden Werte als einzelne 
;; Werte gegeben werden, verzichten wir hier auf die einzelnen Wertlisten, da sie im
;; spaeteren Verlauf nicht mehr gebraucht werden. (s. 'all-cards' in 3.2)

;; Eine Spielkarte wird als Liste der Elemente dargestellt, aus der sie besteht, also z.B.
;; '(1 waves outline red), so kann die Karte anschliessend leicht angezeigt werden, indem apply
;; und show-set-card verwendet wird.

;;;;;
;4/4 Punkten
;;;;;


; 3.2 81 Spielkarten und anzeige
;; Liste aller moeglichen Spielkarten. Mithilfe von amb-collect bzw. amb werden alle Kombinationen
;; von Elementen zu je einer Liste zusammengesetzt und schliesslich zu einer grossen  Listen zusammengefuehrt. 
(define all-cards 
  (amb-collect
   (let ((numbers (amb 1 2 3))
         (patterns (amb 'waves 'oval 'rectangle))
         (modes (amb 'outline  'solid  'hatched))
         (colors (amb 'red  'green  'blue)))
     (list numbers patterns modes colors))))

(define (zeige-karte karte) 
  (apply show-set-card karte))

;;;;;
;Nette Variante mit dem amb
;zur Übung wären aber die standardmässigen Funktionen höherer Ordnung sinnvoll gewesen
;8/10 Punkten
;;;;;


; 3.3 SET-Bestimmung
;; Hilfsfunktion, die alle Listenelemente auf Gleichheit prueft
(define (all-elem-equal? liste)
  (andmap (lambda (x) 
            (equal? x (car liste)))
          liste))
;; Hilfsfunktion, die prueft, ob alle Elemente einzigartig sind
(define (all-elem-unique? liste)
  (andmap (lambda (x) 
            (not (equal? x (car liste))))
          (cdr liste)))
;;;;;
;lambda lässt sich jeweils auch durch curry bzw. curry und compose ersetzen
;;;;;

;; Prueft, ob karte1-3 ein SET ergeben. False, wenn zweimal dieselbe Karten uebergeben wurde oder kein SET vorliegt
(define (is-a-set? karten)
  (let* ((numbers (list (car (first karten)) (car (second karten)) (car (third karten))))
        (patterns (list (cadr (first karten)) (cadr (second karten)) (cadr (third karten))))
        (modes (list (caddr (first karten)) (caddr (second karten)) (caddr (third karten))))
        (colors (list (cadddr (first karten)) (cadddr (second karten)) (cadddr (third karten))))
        (elements (list numbers patterns modes colors))
        (karte1 (first karten))
        (karte2 (second karten))
        (karte3 (third karten)))
    (cond [(or (equal? karte1 karte2) (equal? karte1 karte3) (equal? karte2 karte3) #f)] ; eine Karte doppel gegeben
          ;;;;; Fall ergibt #t? sollte dieser nicht #f ergeben?
          [(ormap (lambda (x) (equal? #f (all-elem-unique? x))) elements) #f] ; zwei Karten stimmen in min. einer Eigenschaft ueberein
          ;;;;; Karten müssen doch in Eigenschaften übereinstimmen damit alle Karten in der Eigenschaft übereinstimmen können
          [(apply = numbers) #t] ; alle Zahlen sind gleich
          [all-elem-equal? patterns #t] ; alle Formen sind gleich
          [all-elem-equal? modes #t] ; alle Fuellungen sind gleich
          [all-elem-equal? colors #t] ; alle Farben sind gleich
          ;;;;;Genügen jeweils nicht um die Anforderungen für ein Set zu erfüllen, alle anderen Eigenschaften müssen auf allen Karten noch gleich oder verschieden sein
          [else #f]))) ;;;;;Was ist mit dem Fall alle Karten sind in allen Eigenschaften verschieden -> Set


;; Tests:
;;;;; (define t-test '((2 red oval hatched) (2 red oval hatched) (2 red oval hatched)))
;;;;; Sollte das nicht false ergeben, weil ihr 3 mal die selbe Karte übergebt
;;; (define f-test '((2  red rectangle outline) (2  green rectangle outline) (1  green rectangle solid)))

;;;;;
;Sollte #t ergeben, ergibt #f
;(is-a-set? '((2 red oval hatched) (2 red rectangle hatched) (2 red wave hatched)))
;;;;;

;;; (is-a-set? t-test)
;;; (is-a-set? f-test)

;;;;;
;Der Ansatz, die einzelnen Eigenschaften aller 3 Karten in 4 Listen zusammen zufassen
;um darauf die Gleichheit/Unterschiedlichkeit überprüfen zu können, ist gut
;Set Anforderungen nicht sinnvoll nachvollzogen/umgesetzt
;3/10 Punkten
;
;Gesamt 30/40 Punkten
;+0/13 Zusatzpunkten
;;;;;

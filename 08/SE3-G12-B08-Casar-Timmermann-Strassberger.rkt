#lang racket
(require swindle/extra)
(require se3-bib/setkarten-module)
#|
    Abgabe Blatt 8
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#

;3 Spieltheorie: Das Kartenspiel SET!
;3.1 Spielkartenauspraegung, Repraesentation
;; Es wuerde sich anbieten die Kartenelement (Zahl, Muster...) als einzelne
;; Listen darzustellen, da zur Generierung aller Karten aber amb verwendet wird
;; und amb nur wie erwuenscht arbeitet, wenn die zu betrachtenden Werte als einzelne 
;; Werte gegeben werden, verzichten wir hier auf die einzelnen Wertlisten, da sie im
;; spaeteren Verlauf nicht mehr gebraucht werden. (s. 'all-cards' in 3.2)

;; Eine Spielkarte wird als Liste der Elemente dargestellt, aus der sie besteht, also z.B.
;; '(1 waves outline red), so kann die Karte anschliessend leicht angezeigt werden, indem apply
;; und show-set-card verwendet wird.


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


;3.3 SET-Bestimmung
; Hilfsfunktion, die alle Listenelemente auf Gleichheit prueft
(define (all-elem-equal? liste)
  (andmap (lambda (x) 
            (equal? x (car liste)))
          liste))
; Hilfsfunktion, die prueft, ob alle Elemente einzigartig sind
(define (all-elem-unique? liste)
  (andmap (lambda (x) 
            (not (equal? x (car liste))))
          (cdr liste)))

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
          [(ormap (lambda (x) (equal? #f (all-elem-unique? x))) elements) #f] ; zwei Karten stimmen in min. einer Eigenschaft ueberein
          [(apply = numbers) #t] ; alle Zahlen sind gleich
          [all-elem-equal? patterns #t] ; alle Formen sind gleich
          [all-elem-equal? modes #t] ; alle Fuellungen sind gleich
          [all-elem-equal? colors #t] ; alle Farben sind gleich
          [else #f])))


(define t-test '((2 red oval hatched) (2 red oval hatched) (2 red oval hatched)))
(define f-test '((2  red rectangle outline) (2  green rectangle outline) (1  green rectangle solid)))

(is-a-set? t-test)
(is-a-set? f-test)





#lang racket
(require se3-bib/butterfly-module-2013)
#|
    Abgabe Blatt 4
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251

Grundueberlegung:

Datenstrukturen:

Ein Genstrang aus allen Merkmalen wird durch eine Liste
in Reihenfolge der merkmale Liste dargestellt
--> '(blue star straight hexagon)
Ein Elternteil wird aus einen Tuple 2er Genstraenge dargestellt
--> '( (blue star straight hexagon) '(red star straight hexagon) )
Mutations Triple
--> Tuple aus den Merkmalen die zur Mutation fuehren
--> Mutations Ergebnis
--> Chance n von 100

Damit wir moeglichst einfach weitere Gen merkmale hinzufuegen
koennen arbeiten wir hier mit Listen mit der Laenge der anzahl
von Gen merkmalen.
Wir modellieren das Dominanz verhalten von Genen durch die
Position in den Merkmals listen, weswegen wir uns eine Hilfs
Funktion schreiben welche die Position von einen element einer
Liste zurueckgibt (Habe leider nichts eingebautes gefunden, weshalb
selber schreiben fuer mich schneller ging ;) )
Um eine bessere Testbarkeit fuer unser Programm zu haben lagern wir
wichtige Funktionen aus:
1. Das dominantere zweier Gene zurueckgeben anhand einer Liste
2. Generieren von einen Rezessiven Merkmal aus einen Dominanten
3. Erzeugen eines Rezessiven Genstrang aus einen Dominanten
4. Erzeugt aus einen Dominanten genstrang mithilfe von 3. ein Elternteil
5. Eine Funktion welche mithilfe eines Elternteils aus 4. und durch 1. ein Kind

Ein rekursiver Generator welcher n kinder mithilfe
einer geburtsfunktion Schmetterlinge erzeugt
(Wir haben hier schon die ueberlegung fuer Aufgabe 
2 einfliessen lassen um wenig Code Duplizierung zu haben)
|#

(define musterung  (list 'dots 'stripes 'star))
(define wingcolor  (list 'blue 'yellow 'red))
(define feelerform (list 'curved 'straight  'curly))
(define wingform   (list 'hexagon 'rhomb 'ellipse ))

(define merkmale   (list wingcolor musterung feelerform wingform))


(define colormutations  (list (list (cons 'red 'yellow ) 'orange 75 )
                              (list (cons 'red 'blue)    'purple 15 )
                              (list (cons 'blue 'yellow) 'green 30  )))
(define feelermutations '())
(define formmutations   '())
(define mustermutations '())

(define mutations (list colormutations mustermutations feelermutations formmutations))

(define wingcolorMutation (list 'purple 'blue 'green 'yellow 'red 'orange))

(define merkmaleMutation (list wingcolorMutation musterung feelerform wingform))

;;; Helper Functions
#|
Sucht den index eines Elements in einer Liste.
Gibt -1 zurueck falls das element nicht in der Liste ist.
ansonsten gibt es den Index zurueck
@param el Das zu suchende Symbol in der Liste
@param list Die liste in der el gesucht werden soll
@return wenn gefunden den index des el in der Liste
-1 wenn nichts gefunden worden ist
|#
(define (getPosOfElem el list)
  (let position ([pos 0])
    (cond
      [(not (list? list)) -1]
      [(= (length list) pos) -1]
      [(eq? el (list-ref list pos)) pos]
      [else (position (+ pos 1))])))

#|
Diese Funktion gibt die dominantere der beiden Merkmale zurueck
@param el1 Ein erste Symbol aus der Merkmal liste list
@param el2 Ein zweites Symbol aus der Merkmal liste
@param list Die Liste in der beide Symbole enthalten ist
@result das Symbol welches dominanter ist
@require list enthaelt el1 und el2
|#
(define (getDominant el1 el2 list)
  (cond
    [(< (getPosOfElem el1 list) (getPosOfElem el2 list)) el1]
    [else el2]))

#|
Generiert ein Rezessives Merkmal anhand der dominanz Reihenfolge.
Die Dominanz Reihenfolge ist durch den index in der Liste gegeben.
0 ist dabei das dominanteste
(lenght list) das rezessivste
@param el Das Dominanten Merkmal aus der liste
@param list Merkmal Liste die das el enthaelt
@return ein Rezessives Merkmal aus der Liste
@example
(getRezessive 'blue wingcolor) --> 'red
|#
(define (getRezessive el list)
  (cond
    [(not (list? list)) "NOT A LIST!"]
    [(not (symbol? el)) "NOT A SYMBOL!"]
    [else 
     (let ([pos (getPosOfElem el list)])
       (list-ref list (+ pos (random (- (length list) pos )))))]))
#|
Sucht fuer jedes Element in der Liste ein Rezessives Merkmal. 
Die Merkmale in der Liste muessen in der Reihenfolge von der Liste 'merkmale' sein,
damit die Funktion funktioniert. Dadurch ist ebendfalls gewaehrleistet das man einfach
neue Merkmale hinzufuegen kann.
@param list Ein GenStrang nach der Datenstruktur deklaration am Anfang
@return ein Genstrang der Rezessiven Merkmale
@example
(calculateRezessive (list 'blue 'dots 'curly 'ellipse))
--> '(blue star straight hexagon)
|#
(define (calculateRezessive list)
  (cond
    [(not (list? list)) -1]
    [(not (= (length merkmale) (length list))) -1]
    [else
      (let findRezessive ([el (car list)] 
                          [merkmal (car merkmale)]
                          [elTail (cdr list)]
                          [merkmalTail (cdr merkmale)])
        (cons
         (getRezessive el merkmal)
         (cond
           [(> (length elTail) 0)
            (findRezessive 
             (car elTail) 
             (car merkmalTail) 
             (cdr elTail) 
             (cdr merkmalTail))]
           [else '()])))]))

#|
Gibt einen Tuple aus Dominanten und Rezessiven Merkmalen zurueck
@param list eine Liste mit elementen aus den Merkmal datenstrukturen
sortiert nach der Reihenfolge in der merkmale liste
@return pair von einer Liste der Dominanten Merkamle und eine Liste der
Rezessiven Merkmale
@example
(getParentTuple (list 'blue 'dots 'curly 'ellipse))
--> '( ('blue 'dots 'curly 'ellipse) ('yellow 'star 'curly 'ellipse) )
|#
(define (getParentTuple list)
  (cons
   list
   (calculateRezessive list)))

#|
Erzeugt aus 2 Eltern tupeln ein Kind unter
beruecksichtigung der dominanz Reihenfolge
der Merkmale
@param parent1 Ein Tuple aus der Liste der Dominanten und der Liste
 der Rezessiven Merkmale
@param parent2 " - "
@return eine Liste aus den Sichtbaren Merkmalen eines aus parent1 und
parent2 entstandenen kindes
@example
(genChild 
  (getParentTuple (list 'blue 'dots 'curly 'ellipse))
  (getParentTuple (list 'blue 'dots 'curly 'ellipse)))
--> '(blue star straight hexagon)
|#
(define (genChild parent1 parent2)
  (let getMerkmal ([nr 0])
    (cons
     (getDominant
      (cond 
        [(= (random 2) 1) (list-ref (car parent1) nr)]
        [else (list-ref (cdr parent1) nr)])
      (cond 
        [(= (random 2) 1) (list-ref (car parent2) nr)]
        [else (list-ref (cdr parent2) nr)])
      (list-ref merkmale nr))
     (cond
       [(< (+ nr 1) (length merkmale)) (getMerkmal (+ nr 1))]
       [else '()]))))
;;;;;
;Die Funktion ließe sich auch mit Rekursion über die Listen umsetzen ohne ein counter über nr einzuführen
;so ist die Funktion mehr am imperativen Programmierstil anstatt dem funktionalen Programmierstil orientiert
;;;;;

#|
Wrapper funktion die eine Liste entgegen nimmt um
ein Schmetterling zu zeichen
@param list ein Genstrang
@return void
@display Zeichnet ein Schmetterling mithilfe der Merkmale
im Genstrang
@example
(showbutterfly (list 'blue 'dots 'curly 'ellipse))
--> Graphical Output
|#
(define (showbutterfly list)
  (show-butterfly
   (list-ref list 0)
   (list-ref list 1)
   (list-ref list 2)
   (list-ref list 3)))

;;; Hauptfunktion
#|
Genrates n children of parent1 and parant2
@param parent1 ist ein Elternteil Datenstruktur
@param parent2 ist ein Elternteil Datenstruktur
@param n Eine Natuerliche Zahlgroesser als 0
@param generator Eine funktion die aus 2 eltern tupeln ein kind erstellt
@return void
@displays Die Eltern und deren n kinder
@example
(genchilds 
  (list 'blue 'dots 'curly 'ellipse) 
  (list 'blue 'stripes 'straight 'hexagon) 
  20
  genChild)
--> Graphical Output of 22 Butterflies
|#
(define (genchilds parent1 parent2 n generator)
  (display (showbutterfly parent1))
  (display (showbutterfly parent2))
  (display "\n")
  (let singleChild ([nr 0]
                    [p1 (getParentTuple parent1)]
                    [p2 (getParentTuple parent2)])
    (display (showbutterfly (generator p1 p2)))
    (cond
      [(> (- n 1) nr) (singleChild (+ nr 1) p1 p2)])))
;;;;;
;Es wäre schöner die Erzeugung von beliebig vielen Kindern von der Darstellung zu trennen
;ist aber auch okay so
;;;;;


;;; Tests
(display "Runnning Tests\nDominanzverhalten:\n")
(eq? (getDominant 'blue 'red wingcolor) 'blue)
(eq? (getDominant 'yellow 'red wingcolor) 'yellow)
(eq? (getDominant 'yellow 'blue wingcolor) 'blue)
(display "Rezessiv Generator\n")
(let ([gen1 (getRezessive 'yellow wingcolor)])
  (cond
    [(eq? gen1 'yellow) #t]
    [(eq? gen1 'red) #t]
    [else #f]))
(let ([gen1 (getRezessive 'blue wingcolor)])
  (cond
    [(eq? gen1 'blue) #t]
    [(eq? gen1 'yellow) #t]
    [(eq? gen1 'red) #t]
    [else #f]))
(display "Generate 10 Children\n\n")
(genchilds (list 'red 'dots 'curly 'ellipse)
           (list 'yellow 'stripes 'straight 'hexagon)
           10
           genChild)


#|
Aufgabe 2 Mutation
|#

#|
Muttiert gen 1 und gen2 anhand von den Mutationen die als Liste uebergeben wurden
@param gen1 1.tes Gen, wird zurueckgegeben falls keine Mutation stattfindet
@param gen2 2.tes Gen
@param listmutations Liste im von Mutations tripeln
@return Mutiertes gen
@example
(mutate 'yellow 'red colormutations) --> 'orange
|#
(define (mutate gen1 gen2 listmutations)
  (cond
    [(not (symbol? gen1))        gen1]
    [(not (symbol? gen2))        gen1]
    [(not (list? listmutations)) gen1]
    [(= (length listmutations) 0)gen1]
    [else
     (let iteration ([list (car listmutations)]
                     [tail (cdr listmutations)]
                     [chance (+ (random 100) 1)])
       (let ([mutuple (car list)])
         (cond
           [(and (< chance (caddr list))
                 (eq? (car mutuple) gen1)
                 (eq? (cdr mutuple) gen2)) (cadr list)]
           [(and (< chance (caddr list))
                 (eq? (car mutuple) gen2)
                 (eq? (cdr mutuple) gen1)) (cadr list)]
           [(= (length tail) 0) gen1]
           [else (iteration (car tail) (cdr tail) chance)])))]))

;;; Angepasster genchild

#|
Erzeugt aus 2 Eltern tupeln ein Kind unter
beruecksichtigung der dominanz Reihenfolge
der Merkmale und von Mutationen die dabei auftreten koennen
@param parent1 Ein Tuple aus der Liste der Dominanten und der Liste
 der Rezessiven Merkmale
@param parent2 " - "
@return eine Liste aus den Sichtbaren Merkmalen eines aus parent1 und
parent2 entstandenen kindes
@example
(genChildMutate 
  (getParentTuple (list 'red 'dots 'curly 'ellipse))
  (getParentTuple (list 'yellow'dots 'curly 'ellipse)))
--> '(orange dots curly ellipse)
|#
(define (genChildMutate parent1 parent2)
  (let getMerkmal ([nr 0])
    (cons
     (let ([gen1 (cond 
                   [(= (random 2) 1) (list-ref (car parent1) nr)]
                   [else (list-ref (cdr parent1) nr)])]
           [gen2 (cond 
                   [(= (random 2) 1) (list-ref (car parent2) nr)]
                   [else (list-ref (cdr parent2) nr)])])
       (getDominant
        (mutate gen1 gen2 (list-ref mutations nr))
        (mutate gen2 gen1 (list-ref mutations nr))
        (list-ref merkmaleMutation nr)))
     (cond
       [(< (+ nr 1) (length merkmale)) (getMerkmal (+ nr 1))]
       [else '()]))))

;;; Beispiel Ausfuehrung
(display "\n\n\nMutations Generation von Kindern\n\n")
(genchilds (list 'red 'dots 'curly 'ellipse)
           (list 'yellow 'stripes 'straight 'hexagon)
           10
           genChildMutate)


;;;;;
;Schöne Trennung zwischen Datenstrukturen und Funktionen
;
;30/30 Punkten
;+6/6 Zusatzpunkten
;;;;;

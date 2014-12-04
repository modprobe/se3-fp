#lang racket
(require se3-bib/butterfly-module-2013)

;;; Data Structures

(define musterung  (list 'dots 'stripes 'star))
(define wingcolor  (list 'blue 'yellow 'red))
(define feelerform (list 'curved 'straight  'curly))
(define wingform   (list 'hexagon 'rhomb 'ellipse ))

(define merkmale   (list wingcolor musterung feelerform wingform))

;;; Helper Functions
#|
Sucht den index eines Elements in einer Liste.
Gibt -1 zurueck falls das element nicht in der Liste ist.
ansonsten gibt es den Index zurueck
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
|#
(define (getParentTuple list)
  (cons
   list
   (calculateRezessive list)))

#|
Erzeugt aus 2 Eltern tupeln ein Kind unter
beruecksichtigung der dominanz Reihenfolge
der Merkmale
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

(define (showbutterfly list)
  (show-butterfly
   (list-ref list 0)
   (list-ref list 1)
   (list-ref list 2)
   (list-ref list 3)))
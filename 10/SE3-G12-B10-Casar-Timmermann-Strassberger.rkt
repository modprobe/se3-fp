#lang racket
#|
    Abgabe Blatt 10
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#
;;; Aufgabe 1.1
(define (every p? xs)
  (cond [(= (length xs) 0) #f]
        [else
         (let innerloop ([head (car xs)]
                         [tail (cdr xs)])
           (and (p? head) (cond
                            [(= (length tail) 0) #t]
                            [else 
                             (innerloop (car tail) (cdr tail))])))]))

(define (some p? xs)
  (cond [(= (length xs) 0) #f]
        [(p? (car xs)) (car xs)]
        [else (some p? (cdr xs))]))

;;; Aufgabe 1.2

(define m '(1 2 3 4))
(define symmetrisch '( (1 . 2) (3 . 4) (4 . 3) (2 . 1)))
(define asymmetrisch '( (1 . 2) (2 . 3) (3 . 4) (4 . 1)))
(define reflexsiv '( (2 . 2) (1 . 1) (4 . 4) (3 . 3)))
(define transitiv '( (1 . 2) (2 . 3) (1 . 3)))

(define (symmetrisch? relation)
  (every 
   (lambda (el)
     (let ([symel (cons (cdr el) (car el))])
       (equal? (some 
        (curry equal? symel) 
        relation) symel)))
     relation))

(define (asymmetrisch? relation)
  (every 
   (lambda (el)
     (let ([symel (cons (cdr el) (car el))])
       (equal? (some 
        (curry equal? symel) 
        relation) #f)))
     relation))

(define (reflexiv? set relation)
  (every
   (lambda (el)
     (not (equal?
           (some (curry equal? (cons el el)) relation) #f)))
   set))

(define (transitiv? relation)
  (every
   (lambda (firstEl)
     (some
      (lambda (secondEl)       
        (if (= (cdr firstEl) (car secondEl))
            (let ([transEl (cons (car firstEl) (cdr secondEl))])
              (equal? 
               (some (curry equal? transEl) relation)
               transEl))
            #t))
      relation))
   relation))

(define (aequi? set relation)
  (and
   (symmetrisch? relation)
   (reflexiv? set relation)
   (transitiv? relation)))

(define (ord? relation)
  (and
   (asymmetrisch? relation)
   (transitiv? relation)))

;;; Aufgabe 2

(define (Kreuzprodukt set1 set2)
  (let innerloop ([set1head (car set1)]
                  [set1tail (cdr set1)])
    (append
      (let 2innerloop ([set2head (car set2)]
                       [set2tail (cdr set2)])
        (cons
         (append 
          (if (list? set1head)
              set1head
              (list set1head))
          (if (list? set2head)
              set2head
              (list set2head)))
         (cond [(equal? set2tail '())'()]
               [else (2innerloop (car set2tail) (cdr set2tail))])))
      (cond [(equal? set1tail '())'()]
           [else (innerloop (car set1tail) (cdr set1tail))]))))

(define (Produkt listofsets)
  (cond [(< (length listofsets) 2) (car listofsets)]
        [else
         (Kreuzprodukt (car listofsets) (cadr listofsets))]))

;;; Aufgabe 3.1
#|
(a) zunaechst evaluiert (- 2 3) --> -1
=> (min 2 -1) --> -1
=> (max -1) --> -1
(b) '(+ ,(- 2 4) 2)
da wir mit ' Racket sagen das er den ausdruck nicht evaluieren soll
bekommen wir ein Symbol
(c) (car '(Alle meine Entchen) --> 'Alle
da Alle das erste Element von der Liste ist.
(d) (auf (dem See))
da wir mit cdr auf einer Liste eine neue Liste ohne dem 
ersten Element bekommen
(e) '(Listen sind einfach)
Wenn wir mit cons ein element und eine Liste verbinden
erhalten wir eine Liste mit den elementen von der liste
und dem neuen
(f) '(Paare . auch)
Normales cons verhalten
(g) #t
Beide Ausdruecke erstellen eine Liste mit denselben elementen
Equal? prueft nur ob in beiden listen dieselben elemente vorhanden sind
(h) 
eq? prueft auf Objekt gleichheit
(i) '(1 8 27)
Map fuehrt auf allen elementen der Liste die Prozedur aus
(j) '(1 3 5)
filter erstellt eine neue liste in der alle Elemente der liste
enthalten sind welche bei aufruf der Prozedur ein #t zurueckliefern
(k) 2
mit (curry min 6) erstellen wir uns eine Anonyme funktion diese fuehren
wir dann mit dem Argument 2 aus was dann zu den aufruf (min 6 2) fuehrt
(l) #t
(curry = 2) erstellt eine anonyme funktion diese rufen wir dann mit 2 auf
was zu folgenden aufruf fuehrt (= 2 2)
|#

;;; Aufgabe 3.3
#|
(a) (+ (* 3 4) (* 5 6))
(b) (sqrt (- 1 (sin (sin x))))
|#

;;; Aufgabe 3.4
#|
(a) (define (c a b) (sqrt (+ (* a a) (* b b))))
(b) (define (mytan a) (/ (sin a) (sqrt (- 1 (sin (sin x))))))
|#
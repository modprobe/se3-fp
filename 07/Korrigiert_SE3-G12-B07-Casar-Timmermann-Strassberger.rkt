#lang racket

#|
    Abgabe Blatt 7
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#

; 1 Generieren von Listen
;; allgemein rekursiv
(define (range-allg n acc step)
  (if
   (= n 0)
   '()
   (cons acc (range-allg (- n 1) (+ acc step) step))))

;; endrekursiv
(define (range-end output n acc step)
  (if
  (= n 0)
  (reverse output)
  (range-end (cons acc output)  (- n 1) (+ acc step) step)))
;;;;;
;Bei euch ist also output der eigentliche Akkumulator für die Endrekursion
;denn der Akkumulator hält ja normalerweise das Zwischenergebnis der Funktion
;euer acc hält mehr den aktuellen Wert für das aktuelle Listenelement
;;;;;

;; mit Funktionen hoeherer Ordnung
(define (range-high n start step)
  (build-list n (lambda (x) (+ start (* step x)))))
                                            

(define (range interval n)
  ;(range-allg n (car interval) (/ (- (cdr interval) (car interval)) n)))
  ;(range-end '() n (car interval)  (/ (- (cdr interval) (car interval)) n)))
  (range-high n (car interval) (/ (- (cdr interval) (car interval)) n)))
;; test
;; (range '(0 . 10) 5) ; -> '(0 2 4 6 8)

;;;;;
;Normalerweise würde wohl erwartet werden, dass die geschriebenen Funktion auf den Argumenten, die in der Aufgabe vorgegeben waren, arbeiten
;Jedoch ist das für diese Aufgabenstellung zumindest für die rekursiven Varianten problematisch. Deswegen ist bei dieser Aufgabe eure Lösung okay.
;Ansonsten möglichst wenig eigene Argumente einführen und vor-initialisieren (außer dem Akkumulator bei endrekursion)
;zumindest die Variante mit Funktionen höherer Ordnung, wäre auch mit den selben Argumenten lösbar
;9/10 Punkten
;;;;;

; 2 Funktions-Plotter
;; 2.1 Funktionen und Werte
(define (function->points f interval n)
  (map (lambda (x) (cons x (f x))) (range interval n)))

;;; Test
;;;(function->points sqr '(0 . 10) 5)

;;;;;
;3/3 Punkten
;;;;;

;; 2.2 Wertebereich
(define (rescale1d pointlist interval)
  (let ([plistmin (apply min pointlist)]
        [plistmax (apply max pointlist)]
        [intmin (car interval)]
        [intmax (cdr interval)]
        )
    (map (lambda (x) (+ intmin (* (/ (- x plistmin) (- plistmax plistmin)) (- intmax intmin)))) pointlist)))

;;; Test
;;; (rescale1d '(0 2 4 6 8) '(10 . 50))


(define (rescale2d pointlist intx inty)
  (map cons (rescale1d (map car pointlist) intx)
       (rescale1d (map cdr pointlist) inty)))

;;; Test
;;; (rescale2d '((0 . 0) (2 . 4) (4 . 16) (6 . 36) (8 . 64)) '(10 . 50) '(5 . 25))

;;;;;
;8/8 Punkten
;;;;;

;; 2.3 Grafische Darstellung 1
(require 2htdp/image)
(define (draw-points pointlist)
  (let ([background (empty-scene 800 600)])
  (foldl (lambda (punkt bild)
           (place-image
            (ellipse 5 5 "solid" "blue") (car punkt) (cdr punkt) bild)) ; 1x1 ist kaum zu erkennen
         background pointlist)))

;;;;;
;Schön, dass ihr auch diesen Teil mit Funktionen höherer Ordnung umgesetzt habt
;4/4 Punkten
;;;;;

;; 2.4 Grafische Darstellung 2
;;; wir skalieren die ausgerechneten Punkte der Funktion auf 800x600 hoch, zeichnen das Bild und drehen es dann
(define (plot-function f interval n)
  (flip-vertical ; sonst steht die Funktion auf dem Kopf
   (draw-points
    (rescale2d
     (function->points f interval n) '(0 . 800) '(0 . 600)))))

;;; Test
;;; (plot-function exp '(0 . 10) 300)

;;;;;
;6/6 Punkten
;;;;;

;; 2.5 Oszillograph
(require 2htdp/universe)

(define (live-plot-function f interval n t)
  (let* [(pointlist (rescale2d (function->points f interval n) '(0 . 800) '(0 . 600))) ; da wir die Liste der Punkte benötigen, können wir plot-function nicht direkt verwenden
         (tpos (list-ref pointlist (modulo t n)))]
    (place-image
     (ellipse 10 10 "solid" "red") (car tpos) (- 600 (cdr tpos)) (plot-function f interval n))))

(define (oszillograph function interval n)
  (animate (curry live-plot-function function interval n)))

;;; Test
;;; (oszillograph exp '(0 . 10) 300)
;;; (oszillograph sin (cons 0 (* 2 pi)) 300)

;;;;;
;4/4 Punkten
;
;Gesamt 34/35 Punkten
;;;;;
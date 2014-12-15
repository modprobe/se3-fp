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

;; mit Funktionen hoeherer Ordnung
(define (range-high n start step)
  (build-list n (lambda (x) (+ start (* step x)))))
                                            

(define (range interval n)
  ;(range-allg n (car interval) (/ (- (cdr interval) (car interval)) n)))
  ;(range-end '() n (car interval)  (/ (- (cdr interval) (car interval)) n)))
  (range-high n (car interval) (/ (- (cdr interval) (car interval)) n)))
;; test
(range '(0 . 10) 5) ; -> '(0 2 4 6 8)

; 2 Funktions-Plotter
;; 2.1 Funktionen und Werte
(define (function->points function interval n)
  (map (lambda (x) (cons x (function x))) (range interval n)))
(function->points sqr '(0 . 10) 5)

;; 2.3 Grafische Darstellung 1
(require 2htdp/image)
(define background (empty-scene 800 600))
(define (draw-points pointlist)
  (foldl (lambda (punkt bild)
           (place-image
            (ellipse 3 3 "solid" "blue") (car punkt) (cdr punkt) bild)) ; 1x1 ist kaum zu erkennen
         background pointlist))
; (draw-points (function->points sqr '(0 . 10) 5))
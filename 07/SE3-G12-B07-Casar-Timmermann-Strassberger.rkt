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

;;; Test
;;; (function->points sqr '(0 . 10) 5)

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

;; 2.3 Grafische Darstellung 1
(require 2htdp/image)
(define (draw-points pointlist)
  (let ([background (empty-scene 800 600)])
  (foldl (lambda (punkt bild)
           (place-image
            (ellipse 5 5 "solid" "blue") (car punkt) (cdr punkt) bild)) ; 1x1 ist kaum zu erkennen
         background pointlist)))

;; 2.4 Grafische Darstellung 2
;;; wir skalieren die ausgerechneten Punkte der Funktion auf 800x600 hoch, zeichnen das Bild und drehen es dann
(define (plot-function f interval n)
  (flip-vertical ; sonst steht die Funktion auf dem Kopf
   (draw-points
    (rescale2d
     (function->points f interval n) '(0 . 800) '(0 . 600)))))
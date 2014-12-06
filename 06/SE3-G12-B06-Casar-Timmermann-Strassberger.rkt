#lang racket

#|
    Abgabe Blatt 6
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#
(require 2htdp/image)

; Bildelemente
;;; Stern- und Eishimmel
(define nightsky (rectangle 1200 500 "solid" "black"))
(define snow (rectangle 1200 300 "solid" "white"))
(define window (empty-scene 1200 800))
(define sky-star (star 10 "solid" "yellow"))

;; geschmueckter Weihnachtsbaum
(define trunk (rectangle 10 20 "solid" "brown"))
(define lowest-level (add-curve (ellipse 70 20 "solid" "green")
                                10 5 15 0
                                65 5 15 1
                                "red"))
(define lower-level (add-curve (ellipse 50 20 "solid" "green")
                               5 5 5 0
                               50 5 10 1
                               "blue"))
(define middle-level (add-curve (ellipse 30 20 "solid" "green")
                                5 5 5 0
                                30 5 15 1
                                "gold"))
(define top-level (ellipse 10 20 "solid" "green"))
(define crown (star 5 "solid" "gold"))

(define x-mas-tree (above crown
                          top-level
                          middle-level
                          lower-level
                          lowest-level
                          trunk))

;; ein Eisstern
;;; Grundfigur und inneres Muster
(define back (regular-polygon 35 12 "solid" "lightblue") )
(define inner (ellipse 10 90 "solid" "white"))
(define inners (list (rotate 25 inner)
                     (rotate 90 inner)
                     (rotate 150 inner)))

(define pos (list (cons 65 65)
                  (cons 65 65)
                  (cons 65 65)))

;;; die aeusseren Dreiecke des Sterns
(define outer-tri (place-image (rhombus 20 40 "solid" "white")  
                               17 35 
                               (isosceles-triangle 60 35 "solid" "lightblue")))

;;; die auesseren Staebe des Sterns
(define stick (above (rhombus 20 60 "solid" "lightblue") 
                     (rectangle 15 60 "solid" "lightblue")))
(define side-tri (rotate 90 (triangle/saa 60  50 20 "solid" "lightblue")))
(define outer-stick (overlay/align/offset "left" "top" 
                                          (overlay/align/offset "left" "top" 
                                                                stick  
                                                                17 19 
                                                                (flip-vertical side-tri))
                                          -40 20
                                          (flip-vertical(flip-horizontal side-tri))))

; setzt den inneren Teil des Sterns zusammen
(define (inner-icestar muster posi base)
  (if 
   (= 1 (length muster))
   (place-image (car muster) (caar posi) (cdar posi) base)
   (place-image (car muster) (caar posi) (cdar posi) (inner-icestar (cdr muster) (cdr posi) base))))

;;; Koordinaten fuer aeussere Musterung
(define outer-pos (list (cons -100 38)
                        (cons -87 -15)
                        (cons -3 5)
                        (cons -25 -100)
                        (cons 83 -145)
                        (cons -5 -165)
                        (cons -15 -182)
                        (cons -84 -163)
                        (cons -119 -140)
                        (cons -131 -97)
                        (cons -119 15)
                        (cons -84 36)))

;;; fuegt Staebe und Dreiecke an das Grundmuster an
(define outer-muster (list outer-stick
                           (rotate 30 outer-tri)
                           (rotate 55 outer-stick)
                           (rotate 90 outer-tri)
                           (rotate 125 outer-stick)
                           (rotate 150 outer-tri)
                           (rotate 180 outer-stick)
                           (rotate 210 outer-tri)
                           (rotate 235 outer-stick)
                           (rotate 270 outer-tri)
                           (rotate 310 outer-stick)
                           (rotate 330 outer-tri)
                           (inner-icestar inners pos back)))

;;; setzt den kompletten Eisstern zusammen
(define (icestar muster posi)
  (if
   (= 1 (length posi) )
   (overlay/xy (car muster)
               (caar posi) (cdar posi)
               (cadr muster))
   (overlay/xy (car muster)
               (caar posi) (cdar posi)
               (icestar (cdr muster) (cdr posi)))))



;;; generiert zufaellig verteilte Koordinaten fuer Himmelobjekte und die Baeume
(define (make-coords x y coord-list)
  (cond [(>= x 1200) (append coord-list (list (cons x (+ (random 400) y))))]
        [else (append coord-list (list (cons x (+ (random 400) y)))
                      (make-coords (+ 50 x) y coord-list))]))

;;; erzeuge Koordinaten fuer alle Bildobjekte
(define star-coords (make-coords 0 0 '()))
(define ice-coords (make-coords 0 0 '()))
(define tree-coords (make-coords 0 50 '()))

; platizert Objekte auf Himmel und Boden
(define (place-objects muster posi base)
  (if 
   (= 1 (length posi))
   (place-image muster (caar posi) (cdar posi) base)
   (place-image muster (caar posi) (cdar posi) (place-objects muster (cdr posi) base))))

;;; Baut das Bild zusammen
(define sky-complete  (place-objects (scale 0.1 (icestar outer-muster outer-pos)) 
                                     ice-coords 
                                     (place-objects sky-star star-coords nightsky)))
(define sky-window (place-image
                    sky-complete
                    600 250  window))
(place-image (place-objects x-mas-tree tree-coords snow)
             600 649
             sky-window)



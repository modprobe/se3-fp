#lang racket

#|
  Abgabe Blatt 1
  Gruppe:
  Michael Strassberger  6527713
  Alexander Timmermann  6524072
  Christian Casar       6214251
|#
;;; Aufgabe 1.1 bogenmass und grad
(define (degrees->radians degrees) 
  (/
   (* 2 pi degrees)
   360))

(define (radians->degrees radians) 
  (/ 
   (* 360 radians) 
   (* 2 pi)))

;;;;;
;4/4 Punkten
;;;;;

;;; Aufgabe 1.2 umkehrfunktion acos
;;; hilfsfunktion zur umwandlung von cosinus nach sinus 
(define (cos->sin cos)
  (sqrt 
   (- 
    1 
    (expt cos 2))))

(define (my-acos cos)
  (atan 
   (/
    (cos->sin cos)
    cos)))

;;;;;
;3/3 Punkten
;;;;;

;;; Aufgabe 1.3 kilometer und seemeilen
(define (nm->km nm)
  (* nm 1.852))

;;;;;
;1/1 Punkten
;;;;;

;;; Aufgabe 2.1
;; 'grosskreisentfernung' wird hier als Hilfsfunktion für die Großkreisentfernung definiert.
(define (grosskreisentfernung latA lonA latB lonB)
  (my-acos (+
             (*
               (sin (degrees->radians latA))
               (sin (degrees->radians latB)))
             (*
               (cos (degrees->radians latA))
               (cos (degrees->radians latB))
               (cos (degrees->radians (- lonA lonB)))))))

(define (distanzAB latA lonA latB lonB)
  (nm->km
    (* 60 (radians->degrees (grosskreisentfernung latA lonA latB lonB)))))

;;;;;
;11/11 Punkten
;;;;;

;;; Aufgabe 2.2 Anfangskurs
;; 'kurswinkel' wird als Hilfsfunktion definiert um Redundanz zu vermeiden
(define (kurswinkel latA lonA latB lonB)
  (radians->degrees
      (my-acos ;;;;;Verwendung der my-acos hier problematisch. Zwar ist die oben vorgenommene Umformung richtig
               ;;;;;jedoch ist aufgrund der bereitgestellten Formeln die Funktion nicht so einfach vollständig anwendbar als acos Funktion
        (/
          (-
            (sin (degrees->radians latB))
            (*
              (cos (grosskreisentfernung latA lonA latB lonB))
              (sin (degrees->radians latA))))
          (*
            (cos (degrees->radians latA))
            (sin (grosskreisentfernung latA lonA latB lonB)))))))

(define (anfangskurs latA lonA latB lonB)
  (if (< lonA lonB)
    (kurswinkel latA lonA latB lonB)
    (- 360 (kurswinkel latA lonA latB lonB))))

;;;;;
;Die hier definierten Funktionen sind richtig und unter Verwendung der Standardfunktion (acos x)
;stimmt alles. Dementsprechend volle Punktzahl
;4/4 Zusatzpunkten

;;; Aufgabe 2.3 Himmelsrichtungen
(define (grad->himmelsrichtung grad)
  (let ([normgrad (modulo grad 360)]) ;;;;;Modulo erwartet Integer. Kommazahlen sind als Eingabe nicht möglich
    (display "NormGrad:" )            ;;;;;Vorher runden wäre ein möglicher Ausweg
    (display normgrad )
    (display "\n")
    (display "Himmelsrichtung:")
    (cond 
      [(> normgrad 348.75) "N"]
      [(> normgrad 326.25) "NNW"]
      [(> normgrad 303.75) "NW"]
      [(> normgrad 281.25) "WNW"]
      [(> normgrad 258.75) "W"]
      [(> normgrad 236.25) "WSW"]
      [(> normgrad 213.75) "SW"]
      [(> normgrad 191.25) "SSW"]
      [(> normgrad 168.75) "S"]
      [(> normgrad 145.25) "SSO"]
      [(> normgrad 123.75) "SO"]
      [(> normgrad 101.25) "OSO"]
      [(> normgrad 78.75) "O"]
      [(> normgrad 56.25) "ONO"]
      [(> normgrad 33.75) "NO"]
      [(> normgrad 11.25) "NNO"]
      [(> normgrad 0) "N"]
      [else "Nur 0 bis 360 Grad"])))    ;;;;;Wird einzig für 0 Grad benötigt und das sollte eigentlich in "N" enthalten sein

(define (himmelsrichtung->grad richtung)
  (cond
    [(string=? richtung "N")   0.0]
    [(string=? richtung "NNO") 22.5]
    [(string=? richtung "NO")  45.0]
    [(string=? richtung "ONO") 67.5]
    [(string=? richtung "O")   90.0]
    [(string=? richtung "OSO") 112.5]
    [(string=? richtung "SO")  135.0]
    [(string=? richtung "SSO") 157.5]
    [(string=? richtung "S")   180.0]
    [(string=? richtung "SSW") 202.5]
    [(string=? richtung "SW")  225.0]
    [(string=? richtung "WSW") 247.5]
    [(string=? richtung "W")   270.0]
    [(string=? richtung "WNW") 292.5]
    [(string=? richtung "NW")  315.0]
    [(string=? richtung "NNW") 337.5]
    [else "Keine gueltige Himmelsrichtung"]))

;;;;;
;6/6 Punkten
;
;Gesamt 25/25
;+ 4/4 Zusatzpunken
;;;;;

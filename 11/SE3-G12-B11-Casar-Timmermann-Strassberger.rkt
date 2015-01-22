#lang racket
#|
    Abgabe Blatt 11
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#

(require se3-bib/prolog/prologInScheme)
;1.1 Unifikation

;1 ?Haus = Informatikum, ?Farbe = weiss; Gleiche Stelligkeit
;2 unifizieren nicht, weil einerseits keine Variable vorhanden und andererseits Dame != Koenig
;3 ?Farbe = Dame
;4 Wenn ich mich nicht vergucke, das gleiche wie 3.
;5 ?andere= (k Herz Koenig) (k Kreuz Dame); gleiche Stelligkeit, da ?andere dem cdr der zweiten Liste entspricht
;6 ?farbe = Pik, ?wert = As, unifizieren, weil gleiche Stelligkeit und die Namen der Praedikate uebereinstimmen
;7 ?farbe= Pik, ?wert2 = As, ?wert = As; wie 6 und zusaetzlich gilt die Koreferenz zwischen ?wert und ?wert in dem zweiten Ausdruck

;1.2 Anfragen
;(ausleihe Signatur Lesernummer)
(<-(ausleihe "K 110" 100))
(<-(ausleihe "p 30" 102))
(<-(ausleihe "P 32" 104))
(<-(ausleihe "P 50" 104))

; (vorbestellung Signatur Lesernummer )
(<- (vorbestellung "K 110" 104) )
(<- (vorbestellung "K 110" 102 ))
(<- (vorbestellung "P 30" 100 ))
(<- (vorbestellung "P 30" 104 ))
; (leser Name Vorname Lesernummer Geburtsjahr)
(<- (leser Neugierig Nena 100 1989))
(<- (leser Linux Leo 102 1990))
(<- (leser Luator Eva 104 1988))


;1. Buch mit Signatur K 100 ausgeliehen?
;(?- (ausleihe "K 110" ?ausgeliehen_an)
;      (leser ?ausleiher_nachn ?ausleiher_vorn ?ausgeliehen_an ?))

;2. Welche Lesernummer hat Leo Linux?
;(?- (leser Linux Leo ?hat_lesenr ?))

;3. Welcher Leser hat Buch mit Signatur P 30 vorbestellt?
; (?- (vorbestellung "P 30" ?vorbesteller)
;      (leser ?vorbesteller_nname ?vorbesteller_vname ?vorbesteller ?))

;4. Welche ueber 60-Jaehrigen haben ein Buch ausgeliehen
;(?- (ausleihe ? ?ausleiher)
;      (leser ?ausleiher_ue60_nname ?ausleiher_ue60_vname ?ausleiher ?alter)
;      (test (> 1955 ?alter)))

;5. Welcher Leser hat mehr als ein Buch ausgeliehen?
; Wenn ein Leser mehr als ein Buch ausgeliehen hat, muss eine Lesernummer in ausleihe fuer unterschiedliche Signaturen vorkommen
;(?- (ausleihe ?sigA ?leser)
;      (ausleihe ?sigB ?leser)
;      (leser ?leser_mit_mehr_als_einem_buch_nname ?leser_mit_mehr_als_einem_buch_vname ?leser ?)
;      (test (not (equal? ?sigA ?sigB))))

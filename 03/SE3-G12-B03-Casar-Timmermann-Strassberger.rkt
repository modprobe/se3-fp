#lang racket
#|
  Abgabe Blatt 2
  Gruppe:
  Michael Strassberger  6527713
  Alexander Timmermann  6524072
  Christian Casar       6214251
|#

; Aufgabe 1: Die internationale Buchstabiertafel
;; Aufgabe 1.1: Datenstruktur für die Tafel
#|
  Wir verwenden ale Datenstruktur einen Hash zur Abbildung der einzelnen
  Buchstaben auf die Zielstrings. Durch den Hash wird die Lesbarkeit maximiert
  und die Zugriffszeit dabei auf einem "erträglichen" Maß gehalten (siehe dazu
  auch die Racket-Dokumentation zu Zugriffszeiten eines Hashes). Alternativ könnten
  wir auch einen Vector mit den Chars als Index nehmen, wo wir von konstanter Zugriffs-
  zeit ausgehen könnten. Darunter litte die Lesbarkeit aber erheblich.
|#

(define translatormatrix (hash #\A "Alpha "
                               #\B "Bravo "
                               #\C "Charlie "
                               #\D "Delta "
                               #\E "Echo "
                               #\F "Foxtrot "
                               #\G "Golf "
                               #\H "Hotel "
                               #\I "India "
                               #\J "Juliet "
                               #\K "Kilo "
                               #\L "Lima "
                               #\M "Mike "
                               #\N "November "
                               #\O "Oscar "
                               #\P "Papa "
                               #\Q "Quebec "
                               #\R "Romeo "
                               #\S "Sierra "
                               #\T "Tango "
                               #\U "Uniform "
                               #\V "Victor "
                               #\W "Whiskey "
                               #\X "X-Ray "
                               #\Y "Yankee "
                               #\Z "Zulu "
                               #\space #\space))

(define (encode-char-spell char)
  (let ([enc (char-upcase char)])
  (if (hash-has-key? translatormatrix enc)
      (hash-ref translatormatrix enc)
      (error "unknown char"))))

;; Buchstabieren eines Textes
(define (translate-text-spell text)
  (when (or
          (not (string? text))
          (= 
            0
            (string-length text)))
    (error "Only non-empty strings allowed!"))
  (display
    (encode-char-spell
      (car
        (string->list
          (substring text 0 1)))))
  (if (= 
        1 
        (string-length text ))
    (display "\n")
    (translate-text-spell
      (substring text 
                 1 
                 (string-length text)))))

; Aufgabe 2 Das internationale Flaggenalphabet
(require se3-bib/flaggen-module)

;; Datenstruktur fuer das Flaggenalphabet
#| 
  Verwende einen Hash zur Abbildung der einzelnen Buchstaben auf die 
  entsprechenden Flaggen. Subjektiv macht ein Hash die Datenstruktur 
  einfacher zu lesen statt die einzelnen Chars über ihren Zahlenwert
  in einem Vector zu indizieren. Laut Racket-Doku ist der Hash-Zugriff 
  offiziel in O(log N), in der Praxis soll man aber konstante 
  Zugriffszeit annehmen koennen ("log N can be treated reasonably 
  as a constant."). Space hinzugefuegt, damit Leerzeichen im Text erkannt werden.
|#  
(define flagdic (hash #\A A 
                   #\B B 
                   #\C C 
                   #\D D 
                   #\E E
                   #\F F
                   #\G G
                   #\H H
                   #\I I
                   #\J J
                   #\K K
                   #\L L
                   #\M M
                   #\N N
                   #\O O
                   #\P P
                   #\Q Q
                   #\R R
                   #\S S
                   #\T T
                   #\U U
                   #\V V
                   #\W W
                   #\X X
                   #\Y Y
                   #\Z Z
                   #\space #\space))

;; Eine Codierungsfunktion
(define (encode-char-flag char)
  (let ([enc (char-upcase char)])
  (if (hash-has-key? flagdic enc)
      (hash-ref flagdic enc)
      (error "unknown char"))))

;; Buchstabieren eines Textes
(define (translate-text-flag text)
  (when (or
         (not (string? text))
         (= 
          0
          (string-length text)))
    (error "Only non-empty strings allowed!"))
  (display
   (encode-char-flag
    (car
     (string->list
      (substring text 0 1)))))
  (if (= 
       1 
       (string-length text ))
      (display "\n")
      (translate-text-flag
       (substring text 
                  1 
                  (string-length text)))))


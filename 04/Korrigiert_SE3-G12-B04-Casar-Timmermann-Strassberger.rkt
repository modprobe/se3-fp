#lang racket
#|
    Abgabe Blatt 4
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#

;; AUFGABE 1
#|
    1. (max (min 2 (- 2 5)) 0)
    --> 0

    2. ’(+ (- 2 13) 11)
    --> ’(+ (- 2 13) 11)

    3. (cadr ’(Alle Jahre wieder))
    --> 'Jahre

    4. (cddr '(kommt (das Weihnachtfest)))
    --> '() ;;;;;Erläuterung?

    5. (cdadr '(kommt (das . Weihnachtfest)))
    --> 'Weihnachtsfest ;;;;;Erläuterung?

    6. (cons 'Listen '(ganz einfach und))
    --> '(Listen ganz einfach und)

    7. (cons 'Paare 'auch)
    --> '(Paare . auch) ;;;;;Erläuterung im Vergleich zum vorherigen Ausdruck

    8. (equal? (list ’Racket ’Prolog ’Java) ’(Racket Prolog Java))
    --> #t (True) ;;;;;Erläuterung?

    9. (eq? (list 'Racket 'Prolog 'Java) (cons 'Racket '(Prolog Java)))
    --> #f (False) ;;;;;Erläuterung?
|#

;;;;;
;Ein paar Erläuterungen sind schon notwendig damit erkennbar ist ob ihr auch verstanden habt
;warum diese Ausdrücke zu genau diesen Werten evaluieren 
;3/6 Punkten 

;; AUFGABE 2
; Aufgabe 2.1: Grammatik in BNF
#|
    <notmeldung> ::= <maydaymal3><ueberschrift><notfallort><notfallart><hilfeleistung><peilzeichen><unterschrift><over>
    <maydaymal3> ::= <mayday> <mayday> <mayday>
    <mayday := "MAYDAY "
    <ueberschrift> ::= <hierist> <schiffnamemal3> <rufzeichen> <zusammenfassung>
    <zusammenfassung> ::= "MAYDAY "<schiffname>" ICH BUCHSTABIERE "<snamechars>" "<rufzeichen>
    <snamechars> ::= <string>
    <schiffnamemal3>::= <schiffname> <schiffname> <schiffname>
    <schiffname> ::= <wort>
    <notfallort> ::= <standort><zeitpunkt>
    <notfallart> ::= <string>
    <hilfeleistung> ::= <string>
    <peilzeichen> ::= "ICH SENDE DEN TRÄGER --"
    <unterschrift> ::= <wort>" "<rufzeichen>
    <rufzeichen> ::= "RUFZEICHEN" <wort> <wort> <wort> <wort>
    <standort> ::= "NOTALLPOSITION "<string>
    <zeitpunkt> ::= "NOTFALLZEIT "<zeit>" UTC"
    <zeit> ::= <stunde><minute>
    <stunde> ::= 0<ziffer> | 1<ziffer> | 2<stundenziffer>
    <minute> ::= 0<ziffer> | 1<ziffer> | 2<ziffer> | 3<ziffer> | 4<ziffer> | 5<ziffer>
    <ziffer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    <stundenziffer> ::= "0" | "1" | "2" | "3"
    <string> ::= <wort> | <wort>" "<string>
    <wort> ::= <buchstabe> | <buchstabe><wort>
    <over> ::= "OVER"
|#

;;;;;
;4/4 Punkten
;;;;;

; Aufgabe 2.2: Die Generatorfunktion
(define translatormatrix (hash #\A "ALPHA "
                               #\B "BRAVO "
                               #\C "CHARLIE "
                               #\D "DELTA "
                               #\E "ECHO "
                               #\F "FOXTROT "
                               #\G "GOLF "
                               #\H "HOTEL "
                               #\I "INDIA "
                               #\J "JULIET "
                               #\K "KILO "
                               #\L "LIMA "
                               #\M "MIKE "
                               #\N "NOVEMBER "
                               #\O "OSCAR "
                               #\P "PAPA "
                               #\Q "QUEBEC "
                               #\R "ROMEO "
                               #\S "SIERRA "
                               #\T "TANGO "
                               #\U "UNIFORM "
                               #\V "VICTOR "
                               #\W "WHISKEY "
                               #\X "X-RAY "
                               #\Y "YANKEE "
                               #\Z "ZULU "
                               #\0 "NADAZERO "
                               #\1 "UNAONE "
                               #\2 "BISSOTWO "
                               #\3 "TERRATHREE "
                               #\4 "KARTEFOUR "
                               #\5 "PANTAFIVE "
                               #\6 "SOSISIX "
                               #\7 "SETTESEVEN "
                               #\8 "OKTOEIGHT "
                               #\9 "NOVENINE "
                               #\, "DECIMAL "
                               #\. "STOP "
                               #\space #\space))

(define (encode-char char)
  (let ([enc (char-upcase char)])
    (if (hash-has-key? translatormatrix enc)
      (hash-ref translatormatrix enc)
      (error "unknown char"))))

(define (spell-helper list)
  (if (= 0 (length list))
    ""
    (string-append
      (encode-char (car list))
      (spell-helper (cdr list)))))

(define (spell text)
  (spell-helper (string->list text)))

(define (string-repeat n str)
  (string-append* (make-list n str)))

(define space " ")
(define newline "\n")

(define (notfallmeldung schiffsname rufzeichen position notfallzeit notfallart weitere-angaben)
  (string-append  (string-repeat 3 "MAYDAY ") newline
                  "HIER IST " newline
                  (string-repeat 3 (string-append schiffsname " ")) (spell rufzeichen) newline
                  "MAYDAY " schiffsname " ICH BUCHSTABIERE " (spell schiffsname) newline
                  "RUFZEICHEN " (spell rufzeichen) newline
                  "NOTFALLPOSITION " position newline
                  "NOTFALLZEIT " notfallzeit newline
                  notfallart newline
                  weitere-angaben newline
                  "ICH SENDE DEN TRÄGER ---" newline
                  schiffsname space (spell rufzeichen) newline
                  "OVER" newline))

;;;;;
;Das ist keine Umsetzung eurer Grammatik sondern eine feste Notmeldungsstruktur in einer Funktion
;Es fehlen also eigene Funktionen für <ueberschrift> <hierist> <schiffnamemal3> <rufzeichen> <zusammenfassung> und so weiter
;aus denen sich eine Notmeldung zusammensetzt
;2/7 Punkten
;;;;;

; Aufgabe 2.3: Der Test
; BABETTE
(display "\nBABETTE:\n--------\n")
(display (notfallmeldung "BABETTE" "DEJY" "UNGEFAEHR 10 SM NORDOESTLICH LEUCHTTURM KIEL" "1000 UTC" "SCHWERER WASSEREINBRUCH WIR SINKEN"
                         "KEINE VERLETZTEN\nVIEW MANN GEHEN IN DIE RETTUNGSINSEL\nSCHNELLE HILFE ERFORDERLICH"))

; AMIRA
(display "\nAMIRA:\n------\n")
(display (notfallmeldung "AMIRA" "AMRY" "53°56’N, 006°31’E" "1640 UTC" "KENTERUNG IN SCHWERER SEE, SCHIFF SINKT" 
                         "15 MANN AN BORD, SCHIFF IST 15 METER LANG, ROTER RUMPF"))

;;;;;
;Darstellung ist okay
;3/3 Punkten
;;;;;

; 3. Funktionen vs Spezialformen
;; Innere und aeussere Reduktion
#|
Innere und aeussere Reduktion beschreiben das Verfahren wie Funktionen
ausgewertet werden. Bei der inneren Reduktion wird die Funktion und 
ihre Argumente von innen nach außen ausgewertet. Also zunaechst die
innersten Terme (etwa die Argumente der Funktion) und auf diese Werte
wird dann die jeweils naechste Ebene der Funktion angewandt.
Die aeussere Reduktion wertet eine Funktion von aussen nach innen aus. 
Bei einer Funktion wird z.B. zuerst ihr Funkionsrumpf ausgewertet und
erst dann nacheinander die Argumente
|#

(define (hoch3 x) (* x x x))
(display "\nAUFGABE 3: hoch3\n----------------\n")
(hoch3 (* 3 (+ 1 (hoch3 2))))

;; innere Reduktion: 
;; (hoch3 (* 3 (+ 1 (hoch3 2))))
;; (hoch3 (* 3 (+ 1 (* 2 2 2))))
;; (hoch3 (* 3 (+ 1 8)))
;; (hoch3 (* 3 9))
;; (hoch3 27)
;; (* 27 27 27)
;; 19683

;; aeussere Reduktion
;; (hoch3 (* 3 (+ 1 (hoch3 2))))
;; (* (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))
;; (* 27 (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2)))
;; (* 27 27 (* 3 (+ 1 (hoch3 2)))
;; (* 27 27 27)
;; 19683
;;;;;um den linken Ausdruck (* 3 (+ 1 (hoch3 2))) weiter reduzieren zu können (3 * term kann so nicht ausgewertet werden)
;;;;;werden dann die nötigen inneren Terme reduziert
;;;;;also eigentlich nicht ein Schritt von (* 3 (+ 1 (hoch3 2))) zu 27


;; Reduktionsstrategie in Racket
;; Racket verwendet im Allgemeinen die innere Reduktion. Fuer special form expressions
;; wie define, if, etc. werden abhaengig vom Ausdruck spezielle Auswertungformen benutzt.
;; Bei einem if wird zum Beispiel zunaechst die Bedingung ausgewertet und dann entsprechend
;; der if oder else Zweig

;; new-if
(define (new-if condition? then-clause else-clause)
  (cond (condition? then-clause)
        (else else-clause)))


(define (faculty product counter max-count)
  (new-if (> counter max-count)
          product
          (faculty (* counter product)
                   (+ counter 1)
                   max-count)))

;; Wenn (faculty 1 1 5) aufgerufen wird, entsteht ein Speicherueberlauf. Der Ueberlauf
;; kommt zustande, da die new-if Funktion zwar durchaus entsprechend der condition?
;; korrekt verzweigt, aber da new-if eine normale Funktion ist verwendet Racket hier
;; inner Reduktion so dass bevor new-if ausgewertet wird, zunaechst die drei Argumente
;; ausgewertet werden  und eine endlos Rekursion durch den faculty-Aufruf in der else-clause
;; entsteht.
;; Mit dem special form if passiert diese Ueberlauf nicht, da zunaechst die gegebene Bedingung
;; geprueft wird bevor der if- oder else-Zweig durchlaufen wird. Fuer eine korrekte Rekursion 
;; ist eine Auswertung durch inner Reduktion nicht geeignet.

;;;;;
;10/10 Punkten
;;;;;

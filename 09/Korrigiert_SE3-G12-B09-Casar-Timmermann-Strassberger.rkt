#lang swindle

#|
    Abgabe Blatt 9
    Gruppe:
        Michael Strassberger  6527713
        Alexander Timmermann  6524072
        Christian Casar       6214251
|#
(require swindle/misc)
(require swindle/setf)

;  1 CLOS und generische Funktionen

;; 1.1 Definition von Klassen
;; ==========================
;; Klassen entsprechend der Beschreibung vom Aufgabenzettel. Als Typ immer Strings
;; gefordert, um die cite-Methode zu vereinfachen.

(defclass* veroeffentlichung () 
  (autoren
   :reader autoren
   :initarg :aut
   :type <list>)
  (jahr
   :reader jahr
   :initarg :jr
   :type <string>)
  (titel
   :reader titel
   :initarg :tl
   :type <string>)
  (id
   :initarg :id
   :reader id
   :type <string>
   )
  
  :printer #t
)


(defclass* buch (veroeffentlichung)
   (verlag
   :reader verlag
   :initarg :verl
   :type <string>)
  
   (verlagsort
   :reader ort
   :initarg :ort
   :type <string>)

  
   (reihe
   :reader reihe
   :initarg :rh
   :type <string>)

  
   (serienummer
   :reader seriennr
   :initarg :snr
   :type <string>))

(defclass* sammelband (buch veroeffentlichung) ;;;;;Warum erbt sammelband nochmal extra direkt von veroeffentlichung?
   (herausgeber                                
   :reader herausgeber
   :initarg :hgb
   :type <string>)
  
   (seite
   :reader seite
   :initarg :s
   :type <string>))

(defclass* zeitschriftenartikel (veroeffentlichung)
   (zeitschirft
   :reader zschrift
   :initarg :zt
   :type <string>)
  
   (heftnr
   :reader heftnr
   :initarg :hnr
   :type <string>)
  
   (bandnr
   :reader band
   :initarg :bnr
   :type <string>)
  
   (monat
   :reader monat
   :initarg :mon
   :type <string>))

(define Nessie1790 (make buch 
      :verl "Minority-Verlag"
      :aut '("Nessie")
      :jr "1790"
      :tl "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
      :id  "Nessie1790"
      :ort "Inverness" 
      :rh "Die besondere Biographie"
      :snr "2"))

(define Prefect1979 (make sammelband
      :id "Prefect1979"
      :verl "Galactic Press"
      :aut '("Prefect, F.")
      :rh "Travel in Style"
      :jr "1979"
      :snr "5"
      :tl "Mostly harmless - some observations concerning the third planet of the solar sytem"
      :ort "Vega-System, 3rd planet"
      :s "420"
      :hgb "Adams, D., editor, The Hitchhiker's Guide to the Galaxy"))

(define Wells3200 (make zeitschriftenartikel
      :id "Wells3200"
      :aut '("Wells, H. G.")
      :jr "3200"
      :tl "Zeitmaschinen leicht gemacht"
      :zt "Heimwerkerpraxis für Anfänger"
      :hnr "550"
      :bnr "3")) 

;;;;;
;10/10 Punkten
;;;;;

;; 1.2 Generische Funktionen und Methoden
;; ======================================
;; Implementierung orientiert sich sehr stark an den gegebenen Beispielen. Da
;; teilweise nicht fuer alle slots Informationen gegeben sind bzw. keine genauere
;; Stuktur fuer den allgemeinen Zitataufbau angegben ist.
;;;;;
;okay Begründung akzeptiert
;;;;;

(defgeneric* cite ((vo veroeffentlichung)))

(defmethod cite ((vo buch))
  (string-append (car (autoren vo))
                 " (" (jahr vo) "). "
                 (titel vo) ", "
                 "Band " (seriennr vo) " der Reihe: " (reihe vo)". "
                 (verlag vo) ", " (ort vo) "."))

(defmethod cite ((vo sammelband))
  (string-append (car (autoren vo))
                 " (" (jahr vo) "). "
                 (titel vo) "."
                 " In " (herausgeber vo) ", " 
                 "volume " (seriennr vo) " of " (reihe vo) ". "
                 (verlag vo) ", " (ort vo) ", 1337 edition, "
                 "p. " (seite vo) "."))

(defmethod cite ((vo zeitschriftenartikel))
  (string-append (car (autoren vo))
                 " (" (jahr vo) "). "
                 (titel vo) ", "
                 (zschrift vo) ", "
                 (heftnr vo) "(" (band vo) ")." ))


(cite Nessie1790)
(cite Prefect1979)
(cite Wells3200)

;;;;;
;Es wäre trotzdem sinnvoll eine cite Methode für eine allgemeine Veröffentlichung zu implementieren
;diese könnte beispielsweise über supercalls (call-next-method) direkt im string-append eingebunden werden
;so müssen die Angaben zu autor, jahr und titel nicht in jeder Methode einzeln aufgeführt werden
;4/5 Punkten
;;;;;

;1.3 Ergaenzungsmethoden
#|
    Ergaenzungsmethoden sind eine Alternative zu super-calls bei der Vererbung von Methoden. Es werden Methoden bereit gestellt,
    die den Aufruf der aus der Oberklasse geerbten Methoden um spezifischere Funktionalitaet ergaenzen.
    CLOS bietet hier :before, :after und :around, die entsprechend ihrer Namen entweder vor, nach oder sowohl vor als auch nach
    Aufruf der geerbten Methode klassenspezifische Ergaenzungen ausfuehren.
    Vorteile fuer diese Alternative sind, dass die geerbte Methode nicht ueberladen werden muss, sondern einfach nur an den 
    entsprechenden Stellen ergaenzt wird. Ausserdem koennen die Ergaenzungen sicherstellen, dass die geerbten Methoden korrekt
    ausgefuehrt werden, indem etwaige klassenspezifische Aenderungen am Objekt beachtet werden.
    
    In diesem Programm koennten die Ergaenzungsmethoden benutzt werden, um fuer die veroeffentlichungs
    Klasse eine allgemeine cite-Methode zu implementieren. Diese Methode die allgemeinen Klasseninformationen ausgeben
    zu lassen und spaeter von den spezifischeren Klasen mithilfe von :after ihre spezifischeren
    Informationen anhaengen zu lassen. Damit dies korrekt funktioniert, muesste aber der Aufbau des Literaturangabe so angepasst
    werden, dass die einzelnen Angaben von unspezifisch nach spezifisch aufeinander folgen.

;;;;;
;Neben der Strukturierung der Angaben (inbesondere bei Sammelbänden, bei Büchern und Zeitschriftenartikeln sollte :after ausreichen)
;erzeugt die Rückgabe als Strings weitere Probleme
;In dieser Form würde nur der String der letzten aufgerufen cite Funktion in der Vererbung zur Ausgabe führen
;der String muss entweder jeweils durch display direkt ausgegeben werden, oder Zwischengespeichert und manipuliert werden.
;4/5 Punkten
;;;;;
|#

;  2 CLOS und Vererbung

;; Generische Methoden als Accessors
;; =================================
;; dass man nur eine Methode implementieren musste, haben wir erst gelesen, als 
;; wir fertig waren…

;;; Ein Amphibienfahrzeug kann sich in mehreren Medien bewegen. Diese sollten also
;;; als Liste zurückgegeben werden.
(defgeneric medium ((fahrzeug))
    :combination generic-list-combination)

;;; Logischerweise kann ein Fahrzeug nicht in allen Medien die gleiche Maximal-
;;; geschwindigkeit haben (bspw. das Amphibienflugzeug). Wenn also in einem Medium eine
;;; niedrigere Maximalgeschwindigkeit gilt, limitiert das logischerweise die
;;; komplette Maximalgeschwindigkeit.
(defgeneric max-velocity ((fahrzeug))
    :combination generic-min-combination)
;;;;;
;warum ist die limitierung logisch
;Maximalgeschwindigkeit könnte auch als höchste Geschwindigkeit des Fahrzeuges
;unter idealen Umständen gesehen werden
;;;;;


;;; siehe max-velocity. Das gleiche Prinzip gilt hier.
(defgeneric max-load ((fahrzeug))
    :combination generic-min-combination)

;;; Näher an der Realität wäre vermutlich ein Mittelwert, darauf wird aber aus
;;; Gründen der Komplexität verzichtet.
(defgeneric consumption ((fahrzeug))
    :combination generic-max-combination)


;;; Auch hier nimmt man aus Gründen der Fahrgastsicherheit™ lieber den kleineren
;;; Wert.
(defgeneric max-passengers ((fahrzeug))
    :combination generic-min-combination)

;;;;;
;2.2
;5/5 Punkten
;;;;;

;; Klassendefinitionen
;; ===================

(defclass* fahrzeug ())

(defclass* landfahrzeug (fahrzeug)
    (medium_land            :initvalue 'Land
                            :accessor medium)
)

(defclass* wasserfahrzeug (fahrzeug)
    (medium_wasser          :initvalue 'Wasser
                            :accessor medium)
    (max-velocity_wasser    :reader max-velocity
                            :initarg :max-velocity_water)
    (max-load_wasser        :reader max-load
                            :initarg :max-load_water)
    (consumption_wasser     :reader consumption
                            :initarg :consumption_water)
    (max-passengers_wasser  :reader max-passengers
                            :initarg :max-passengers_water)
)

(defclass* luftfahrzeug (fahrzeug)
    (medium_luft          :initvalue 'Luft
                          :accessor medium)
    (max-velocity_luft    :reader max-velocity
                          :initarg :max-velocity_air)
    (max-load_luft        :reader max-load
                          :initarg :max-load_air)
    (consumption_luft     :reader consumption
                          :initarg :consumption_air)
    (max-passengers_luft  :reader max-passengers
                          :initarg :max-passengers_air)
)

(defclass* schienenfahrzeug (landfahrzeug)
    (medium_schiene          :initvalue 'Schiene
                             :accessor medium)
    (max-velocity_schiene    :reader max-velocity
                             :initarg :max-velocity_rail)
    (max-load_schiene        :reader max-load
                             :initarg :max-load_rail)
    (consumption_schiene     :reader consumption
                             :initarg :consumption_rail)
    (max-passengers_schiene  :reader max-passengers
                             :initarg :max-passengers_rail)
)

(defclass* strassenfahrzeug (landfahrzeug)
    (medium_strasse          :initvalue 'Strasse
                             :accessor medium)
    (max-velocity_strasse    :reader max-velocity
                             :initarg :max-velocity_road)
    (max-load_strasse        :reader max-load
                             :initarg :max-load_road)
    (consumption_strasse     :reader consumption
                             :initarg :consumption_road)
    (max-passengers_strasse  :reader max-passengers
                             :initarg :max-passengers_road)
)

(defclass* amphibienfahrzeug (strassenfahrzeug wasserfahrzeug))

(defclass* amphibienflugzeug (strassenfahrzeug wasserfahrzeug luftfahrzeug))

(defclass* zweiwegefahrzeug (schienenfahrzeug strassenfahrzeug))

(defclass* zeitzug (schienenfahrzeug luftfahrzeug))

;;; einfach nur der Vollständigkeit halber…
(defclass* aeromobil (strassenfahrzeug luftfahrzeug))

;;;;;
;5/5 Punkten
;;;;;

;; Beispielobjekte
;; ===============
;; Die Zahlen sind teils sehr umständlich ausgerechnet, teils aus der Wikipedia
;; und teils auch einfach ausgedacht ;)

;;; Ein Terrapin Mk. 1 der British Army aus dem 2. Weltkrieg
(define terrapin-mark1
  (make amphibienfahrzeug
        :max-velocity_road 24 :max-velocity_water 8
        :max-load_road 4000 :max-load_water 4000
        :consumption_road 20 :consumption_water 20
        :max-passengers_road 2 :max-passengers_water 2))

;;; Ein Bombardier 415 'Superscooper', ein Amphibienflugzeug das als Lösch-
;;; flugzeug genutzt wird.
(define superscooper
  (make amphibienflugzeug
        :max-velocity_air 359 :max-velocity_road 150 :max-velocity_water 130
        :max-load_air 3000 :max-load_road 3000 :max-load_water 2900
        :consumption_air 2.375 :consumption_road 2.8 :consumption_water 2.6
        :max-passengers_air 2 :max-passengers_road 2 :max-passengers_water 2))

;;; Ein konvertierter Toyota Land Cruiser von Aries Rail aus Wangara, WA, Australia.
(define ariesHyrailLandcruiser
  (make zweiwegefahrzeug
        :max-velocity_rail 140 :max-velocity_road 180
        :max-load_rail 200 :max-load_road 300
        :consumption_rail 10 :consumption_road 8
        :max-passengers_rail 5 :max-passengers_road 5))

;;; The one and only.
(define julesVerneTrain
  (make zeitzug
        :max-velocity_rail 140 :max-velocity_air 180
        :max-load_rail 9999 :max-load_air 8888
        :consumption_rail 1.21 :consumption_air 1.21 ; in Gigawatts
        :max-passengers_rail 11 :max-passengers_air 11))

;;; Der 200-Bus aus Doctor Who (4x15 'Planet of the Dead')
(define the200Bus
  (make aeromobil
        :max-velocity_road 80 :max-velocity_air 200
        :max-load_road 1000 :max-load_air 200
        :consumption_road 20 :consumption_air 0
        :max-passengers_road 80 :max-passengers_air 6))

;; Beispielaufrufe
;; ===============

;(max-load terrapin-mark1)
;(medium superscooper)
;(consumption julesVerneTrain)
;(medium the200Bus)
;(max-passengers the200Bus)
;(max-load ariesHyrailLandcruiser)

;; Erklärung
;; =========
#|
    Die Klassenpräzedenzliste gibt bei Vererbungen eine Art Reihenfolge an,
    welche Klasse eine Andere spezifiziert. So kann bei Konflikten in der Ver-
    erbung (z.B. bei gleichen Methodennamen) eine Ordnung vorgegeben werden und
    die richtige Methode aufgerufen werden.

    Die generischen Methoden, die wir hier geschrieben haben, machen sich diese
    Klassenpräzedenzliste zunutze, indem sie die Methode für _alle_ Klassen in
    der Liste in der korrekten, in der KPL vorgegeben Reihenfolge aufrufen und die 
    Ergebnisse anhand einer vorgegebenen Ordnung verarbeiten. Als Beispiel kann 
    hier folgender Aufruf von (medium) dienen:

        => (medium superscooper)
        '(Strasse Land Wasser Luft)

    Wie man aus der Signatur von "amphibienflugzeug" erkennen kann, erbt die Klasse
    von "strassenfahrzeug", "wasserfahrzeug" und "luftfahrzeug". Es wird also erst
    die "medium"-Funktion des Straßenfahrzeugs aufgerufen, was das erste Element der
    Liste erklärt. Danach wird jedoch die "medium"-Funktion der Oberklasse aufgerufen,
    in diesem Fall "landfahrzeug". So kommt das zweite Element der Liste zustande.
    Die Ergebnisse werden in eine Liste zusammengefasst, weil wir einen "eingebauten"
    Kombinator benutzen.

    Die restlichen generischen Methoden arbeiten analog, mit dem Unterschied dass
    eine andere Kombinationsfunktion gewählt wurde. Da von den numerischen Werten
    immer ein Minimum bzw. Maximum bestimmt werden kann, benutzen wir die ent-
    sprechende Kombinationsfunktion dort.
|#

;;;;;
;10/10 Punkten
;Gute Erläuterung der Klassenpräzedenzliste
;
;Gesamt 38/40 Punkten
;;;;;

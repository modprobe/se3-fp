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

(defclass* sammelband (buch veroeffentlichung)
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

;; 1.2 Generische Funktionen und Methoden
;; Implementierung orientiert sich sehr stark an den gegebenen Beispielen. Da
;; teilweise nicht fuer alle slots Informationen gegeben sind bzw. keine genauere
;; Stuktur fuer den allgemeinen Zitataufbau angegben ist.

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

;1.3 Ergaenzungsmehtoden
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
|#

;  2 CLOS und Vererbung
;; 2.1 Definition von Klassen

(defclass* fahrzeug ()

)

(defclass* landfahrzeug (fahrzeug)

)

(defclass* wasserfahrzeug (fahrzeug)

)

(defclass* luftfahrzeug (fahrzeug)

)

(defclass* schienenfahrzeug (landfahrzeug)

)

(defclass* strassenfahrzeug (landfahrzeug)

)

(defclass* amphibienfahrzeug (landfahrzeug wasserfahrzeug)

)

(defclass* amphibienflugzeug (landfahrzeug wasserfahrzeug luftfahrzeug)

)

(defclass* zweiwegefahrzeug (schienenfahrzeug strassenfahrzeug)

)

(defclass* zeitzug (schienenfahrzeug luftfahrzeug)

)

;; pentru Mocanu
;; 2022.12.12-12.32 - mod window to go back to previous layer after window is created



(setq textStyle "STYLE2")



(setq textHeight 0.2)
(setq rowHeight (* textHeight 2.5)) 
(setq padding (/ (- rowHeight textHeight) 2))
(setq rowLength (* rowHeight 18.82))
(setq textContentCamera "Camera")
(setq textContentBalcon "Balcon")
(setq headerHeight (* rowHeight 0.88))

;; position for vertical lines
(setq offsetLinieNumeCamera (* rowHeight 4.3)) 
(setq offsetLinieSuprafata (* rowHeight 11.44)) 

;; position for text
;; id camera (1,2,3...)
(setq offsetTextIdCamera (* rowHeight 2))
;; Camera,Camera...
(setq offsetTextNumeCamera (* rowHeight 5.14))
;; text suprafata propriu-zisa
(setq offsetTextSuprafata (* rowHeight 14.8))


;; inner things from camera (text and circle)
(setq innerTextHeight (/ rowHeight 1.6))
(setq innerSmallCircleRadius (/ rowHeight 2))
(setq innerLargeCircleRadius (/ rowHeight 1.6))

(setq globalVarLeft (list 0.0 0.0))
(setq globalVarRight (list 10.0 0.0))

(setq suprafataUtila 0)
(setq suprafataTotala 0)

(setq layerName "Tabel")
(setq layerColor 7)

(setq autoExecutant (* rowHeight 0.24))
(setq textExecutant "Executant: PFA Mocanu Corneliu")
(setq autoSemnaturaStampila (* rowHeight 0.32))
(setq autoDetails (* rowHeight 3.125))
;; Data: 11.11.2022
(setq autoDateTop (* rowHeight 12.78))
(setq autoDateBottom (* rowHeight 8.62))
(setq autoReceptionat (* rowHeight 0.24))

(setq footerSerieAutorizat "SERIA CT NR.116, CATEGORIILE B/C")
(setq footerSemnaturaStampila "(semnatura, stampila)")
(setq textSemnaturaParafa "(semnatura,parafa)")
(setq footerReceptionat "Receptionat,")
(setq footerDateTopBottom "Data")
(setq footerDateMiddle "Data: 11.11.2022")


(defun c:xxx()
    (load "tabelUsaGeam_002.lsp")
    ;; (alert "Loaded")
    (princ "\nReload successful")
    (princ)
)




(defun getAreaText(ent coo / textLocation w lst counterArea alinarea x1 y1 x2 y2 tempOne finalArea)
    (setq textLocation (list (car coo)(- (cadr coo) (+ textHeight padding))))
    (setq w nil)
    (setq lst nil)
    (setq w (entget ent))

    (foreach x w
      (if (=(car x) 10)
        (setq lst (append lst (list (cdr x))))
      )
    )

    (setq counterArea 0)
    (setq alinarea 0)
    (while (< counterArea (length lst))
        (if ( = counterArea (- (length lst) 1))
            (progn
                (setq x1 (car (nth 0 lst)))
                (setq y1 (cadr (nth 0 lst)))
            )
            (progn
              (setq x1 (car (nth (+ counterArea 1) lst)))
              (setq y1 (cadr (nth (+ counterArea 1) lst)))
            )
        )
        (setq x2 (car (nth counterArea lst)))
        (setq y2 (cadr (nth counterArea lst)))
        (setq tempOne (- (* x1 y2) (* x2 y1)))
        (setq alinarea (+ alinarea tempOne))
        (setq counterArea (1+ counterArea))
    )
    (setq finalArea (atof (rtos (abs (/ alinarea 2)) 2 1)))
    (entmake (list (cons 0 "text")(cons 10 textLocation)(cons 1 (rtos finalArea 2 1))(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))
    (setq suprafataUtila ( + suprafataUtila finalArea))
)


;; create text inside each room by selecting the position of text
(defun makeTextAndCircle(roomNumber name / cRadius circleLocation textLoc createText)
  (setq textLoc (getPoint (strcat "<---------- Insereaza textul pentru " name " " roomNumber ". ->")))
  (terpri)
  (if (> (atoi roomNumber) 9)
      (progn
        (setq cRadius innerLargeCircleRadius)
        (setq circleLocation (list (+ (car textLoc) (/ innerLargeCircleRadius 1.3)) (+ (cadr textLoc) (/ innerLargeCircleRadius 2.1))))
      )
      (progn
        (setq cRadius innerSmallCircleRadius)
        (setq circleLocation (list (+ (car textLoc) (/ innerSmallCircleRadius 2)) (+ (cadr textLoc) (/ innerSmallCircleRadius 1.673))))
      )
  )
  (setq createText (entmake (list (cons 0 "text")(cons 1 roomNumber)(cons 10 textLoc)(cons 40 innerTextHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle))))
  (entmake (list (cons 0 "circle")(cons 10 circleLocation)(cons 40 cRadius)(cons 62 layerColor)(cons 8 layerName)))
)



(defun createHeader(x / two strt1 plr_1 plr_2 plr_3 plr_4 plr_text1 plr_text2 text1 text2 text3 text4 text5 text6 plr_text3 plr_text4 plr_text5 plr_text6 plr_text7 plr_text8 plr_text9 plr_text10 plr_text11 plr_text12 )
      ;; margin lines of header (top left, top middle, top right)
      (setq topLeftCorner (polar x (angtof "0") headerHeight))

      ;end coo of one, start of strt
      ;; most top horizontal line 
      (setq two (entmake (list (cons 0 "line")(cons 10 topLeftCorner)(cons 11 (polar topLeftCorner (angtof "90") rowLength) )(cons 62 layerColor)(cons 8 layerName))))
      (setq strt1 (cdr (caddr two)))
      ;create separators between id name and area (vertical lines)
      (setq plr_1 (polar x (angtof "90") offsetLinieNumeCamera))
      (setq plr_2 (polar plr_1 (angtof "0") headerHeight))
      (entmake (list (cons 0 "line")(cons 10 plr_1)(cons 11 plr_2)(cons 62 layerColor)(cons 8 layerName)))
      (setq plr_3 (polar x (angtof "90") offsetLinieSuprafata))
      (setq plr_4 (polar plr_3 (angtof "0") headerHeight))
      (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 plr_4)(cons 62 layerColor)(cons 8 layerName)))

      ; add text to the boxes created
      (setq plr_text1 (polar x (angtof "90") (* rowHeight 0.24)))
      (setq plr_text2 (polar plr_text1 (angtof "0") padding))
      (setq text1 (entmake (list (cons 0 "text")(cons 10 plr_text2)(cons 1 "Nr.incapere")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle))))
      (setq plr_text5 (polar x (angtof "90") (* rowHeight 4.88)))
      (setq plr_text6 (polar plr_text5 (angtof "0") padding))
      (setq text3 (entmake (list (cons 0 "text")(cons 10 plr_text6)(cons 1 "Denumire incapere")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle))))
      (setq plr_text9 (polar x (angtof "90") (* rowHeight 11.68)))
      (setq plr_text10 (polar plr_text9 (angtof "0") padding))
      (setq text5 (entmake (list (cons 0 "text")(cons 10 plr_text10)(cons 1 "Suprafata utila (mp)")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle))))
)



(defun c:ww (/ ent angleUnits angleDirection angleBase counter py temp checkEnt newLocation plrLineNumeCamera plrLineSuprafata plrRightVertical plr_endCameraBottomRight unimportant)
      (setq suprafataUtila 0)
      (setq suprafataTotala 0)
      ; get type of angle units (0 decimal degrees,1 deg/min/sec,2 grads,3 radians, 4 surveryors units)
      (setq angleUnits (getvar "aunits"))
      ; get angle direction -> 0 counter clockwise, 1 clockwise
      (setq angleDirection (getvar "angdir"))
      ; north
      (setq angleBase (getvar "angbase"))
      ; reset angles
      (setvar "aunits" 0)
      (setvar "angdir" 1)
      (setvar "angbase" (/ pi 2))
      (terpri)
      (setq counter (getint "<---------- Primul numar de camera: (ENTER incepe de la 1): "))
      (if (= counter nil)
          (setq counter 1)
        );if 
      (terpri)
      (setq start (getPoint "<---------- Alege unde plasezi tabelul -> "))
      
      (terpri)
      ;; get Y of start point
      (setq py (cadr start))
      ;; create temporary variable to increment from that point
      (setq temp py)
      
      ; function to create the header (send the initial coordinates)
      (createHeader start)
      
     (while  (setq ent (car (entsel (strcat "<---------- Selecteaza conturul camerei " (rtos counter 2 0) " -> (Sau apasa clic dreapta pentru a crea balcoanele.)"))))
          (terpri)
          (setq checkEnt (cdr (cadr (entget ent))))
          (if (= checkEnt "LWPOLYLINE")
                  (progn
                    (createTableRow start temp textContentCamera "camera")
                    (setq newLocation (list (+ offsetTextSuprafata (car start)) temp))
                    (setq temp (- temp rowHeight))
                    (getAreaText ent newLocation)
                    (makeTextAndCircle (rtos counter 2 0) "camera")
                    (setq counter (1+ counter))
                  )
                  (alert "Nu ai selectat o polilinie.")
            ); if
          
     );while

     ; start point of right vertical line
      (setq plrLineNumeCamera (polar start (angtof "90") offsetLinieNumeCamera ))
      (setq plrLineSuprafata (polar start (angtof "90") offsetLinieSuprafata ))
      (setq plrRightVertical (polar start (angtof "90") rowLength ))

      (setq newLocation (list (car start) temp))
      
      
      ;; end of line COO
      (setq plr_endCameraBottomRight (polar newLocation (angtof "90") rowLength ))
      

      ;last bottom line
      (setq unimportant (entmake (list (cons 0 "line")(cons 10 newLocation)(cons 11 plr_endCameraBottomRight)(cons 62 layerColor)(cons 8 layerName))))
      (entmake (list (cons 0 "line") (cons 10 plrLineNumeCamera)(cons 11 (polar newLocation (angtof "90") offsetLinieNumeCamera ))(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line") (cons 10 plrLineSuprafata)(cons 11 (polar newLocation (angtof "90") offsetLinieSuprafata ))(cons 62 layerColor)(cons 8 layerName)))
      (createTotalUtilaTable (cadr unimportant))
      (balc counter unimportant)
      (setq suprafataUtila 0)
      (setq suprafataTotala 0)

     ;; create left vertical line
     (entmake (list (cons 0 "line")(cons 10 left4_5)(cons 11 topLeftCorner)(cons 62 layerColor)(cons 8 layerName)))
     ;; create top right point and then create line
     (setq topRight (polar topLeftCorner (angtof "90") rowLength))
     ;; create right vertical line
     (entmake (list (cons 0 "line")(cons 10 topRight)(cons 11 right4_5)(cons 62 layerColor)(cons 8 layerName)))



      ;; put back old angles
      (setvar "aunits" angleUnits)
      (setvar "angdir" angleDirection)
      (setvar "angbase" angleBase)
      ;end program
      (princ)
);ww



(defun balc(counter x / ent checkEnt tmp start py temp rightTemp newLocation)
      (setq tmp (cdr (cadr x)))
      (setq start (polar tmp (angtof "180") rowHeight))
      (setq py (cadr start))
      (setq temp py)
      (setq rightTemp (polar start (angtof "90") rowLength))
      (setq globalVarLeft start)
      (setq globalVarRight rightTemp)

      (while (setq ent (car (entsel (strcat "\n<---------- Selecteaza conturul balconului " (rtos counter 2 0) " -> (Sau apasa clic dreapta pentru a termina.)"))))
          (setq checkEnt (cdr (cadr (entget ent))))
          (if (= checkEnt "LWPOLYLINE")
            (progn
              (terpri)
              (createTableRow start temp textContentBalcon "balcon")
              (setq newLocation (list (+ offsetTextSuprafata (car tmp)) temp))
              (setq temp (- temp rowHeight))
              (getAreaText ent newLocation)
              (makeTextAndCircle (rtos counter 2 0) "balconul")
              (setq counter (1+ counter))
            )
            (alert "Nu ai selectat o POLILINIE.")
          
          )
          
     );while
     (createFooter start)
)


(defun createFooter(start / plr_2 plr_3 plr_4 plr_5 plr_6 plr_7 plr_8 plr_9 plr_10 )
      ;; start este stanga sus la primu balcon
      (setq plr_2 (polar start (angtof "90") rowLength));; primu balcon dreapta sus
      (setq plr_3 (polar globalVarLeft (angtof "180") rowHeight)) ;; ultimu balcon stanga jos - rowHeight
      (setq plr_4 (polar globalVarRight (angtof "180") rowHeight)) ;; ultimu balcon dreapta jos - rowHeight
      (setq plr_5 (polar plr_3 (angtof "90") (* rowHeight 3.44)))
      (setq plr_6 (polar plr_5 (angtof "0") padding))
      (setq plr_7 (polar globalVarLeft (angtof "90") offsetLinieNumeCamera)) ;; bottom
      (setq plr_8 (polar globalVarLeft (angtof "90") offsetLinieSuprafata)) ;; bottom 
      (setq plr_9 (polar start (angtof "90") offsetLinieNumeCamera)) ;; top 
      (setq plr_10 (polar start (angtof "90") offsetLinieSuprafata)) ;; top 


      (entmake (list (cons 0 "line")(cons 10 plr_7)(cons 11 plr_9)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_8)(cons 11 plr_10)(cons 62 layerColor)(cons 8 layerName)))
      ;; bottom line under suprafata totala
      (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 plr_4)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "text")(cons 10 plr_6)(cons 1 (strcat "Suprafata totala = " (rtos suprafataUtila 2 1)  " mp"))(cons 62 layerColor)(cons 8 layerName)(cons 40 textHeight)(cons 7 textStyle)))
      (createAuthorized)
)

(defun createAuthorized(/ leftStart rightStart left1_2 right1_2 left2_3 right2_3 left3_4 right3_4 middleTop middleBottom plr_executant plr_semnaturaStampila plr_detailsAutorizat plr_receptionat plr_dataSus plr_dataMiddle plr_dataBottom)
      ;; globalVarLeft -> stanga sus la suprafata totala 
      ;; stanga sus la executant
      (setq leftStart (polar globalVarLeft (angtof "180") rowHeight))
      ;; dreapta sus la executant
      (setq rightStart (polar leftStart (angtof "90") rowLength))
      ;; stanga jos la executant
      (setq left1_2 (polar leftStart (angtof "180") rowHeight))
      ;; dreapta jos la executant
      (setq right1_2 (polar left1_2 (angtof "90") rowLength))
      (setq left2_3 (polar left1_2 (angtof "180") (* rowHeight 7.88)))
      (setq right2_3 (polar left2_3 (angtof "90") rowLength))
      (setq left3_4 (polar left2_3 (angtof "180") rowHeight))
      (setq right3_4 (polar left3_4 (angtof "90") rowLength))
      (setq left4_5 (polar left3_4 (angtof "180") (* rowHeight 5.36)))
      (setq right4_5 (polar left4_5 (angtof "90") rowLength))
      ;; line horizontal (left to right full) under Executant: PFA Mocanu Corneliu
      (entmake (list (cons 0 "line")(cons 10 left1_2)(cons 11 right1_2)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left2_3)(cons 11 right2_3)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left3_4)(cons 11 right3_4)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left4_5)(cons 11 right4_5)(cons 62 layerColor)(cons 8 layerName)))
     
      (setq middleTop (polar leftStart (angtof "90") (* rowHeight 11.92)))
      (setq middleBottom (polar middleTop (angtof "180") rowHeight))
      (setq middleBottomReceptionat1 (polar left3_4 (angtof "90") (* rowHeight 10.50) ))
      (setq middleBottomReceptionat2 (polar middleBottomReceptionat1 (angtof "0") rowHeight ))
      ;; linie intre Executant si Data: 11.11.2022
      (entmake (list (cons 0 "line")(cons 10 middleTop)(cons 11 middleBottom)(cons 62 layerColor)(cons 8 layerName)))
      ;; linie verticala dupa Data de pe randu cu Receptionat
      (entmake (list (cons 0 "line")(cons 10 middleBottomReceptionat1)(cons 11 middleBottomReceptionat2)(cons 62 layerColor)(cons 8 layerName)))
     
      ; point executant
      (setq plr_executant (polar (list (car leftStart)(- (cadr leftStart) padding textHeight)) (angtof "90") autoExecutant))
      (entmake (list (cons 0 "text")(cons 10 plr_executant)(cons 1 textExecutant)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

      (setq plr_semnaturaStampila (polar (list (car left1_2)(- (cadr left1_2) (+ textHeight padding))) (angtof "90") autoSemnaturaStampila))
      (entmake (list (cons 0 "text")(cons 10 plr_semnaturaStampila)(cons 1 footerSemnaturaStampila)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

      (setq plr_receptionat (polar (list (car left3_4)(+ (cadr left3_4) padding )) (angtof "90") autoReceptionat))
      (entmake (list (cons 0 "text")(cons 10 plr_receptionat)(cons 1 footerReceptionat)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

     ;; data sus Data: 11.11.2022
      (setq plr_dataSus (polar (list (car left1_2)(+ (cadr left1_2) padding )) (angtof "90") autoDateTop))
      (entmake (list (cons 0 "text")(cons 10 plr_dataSus)(cons 1 footerDateMiddle)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

      (setq plr_dataBottom (polar (list (car left3_4)(+ (cadr left3_4) padding )) (angtof "90") autoDateBottom))
      (entmake (list (cons 0 "text")(cons 10 plr_dataBottom)(cons 1 footerDateTopBottom)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

      (setq plr_subReceptionat (polar left3_4 (angtof "90") (* rowHeight 0.24)))
      (entmake (list (cons 0 "text")(cons 10 (list (car plr_subReceptionat)(- (cadr plr_subReceptionat) (+ textHeight padding))))(cons 1 textSemnaturaParafa)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))

)




(defun createTableRow (start temp text identifier / newCoo plr bottomLine bottomLineEnd newLocCameraId newLocCameraNume )
    (setq newCoo (list (car start) temp))
    (setq plr (polar newCoo (angtof "90") rowLength ))
    (setq bottomLine (polar newcoo (angtof "180") rowHeight))
    (setq bottomLineEnd (polar bottomLine (angtof "90") rowLength))
    (setq newLocCameraId   (list (+ offsetTextIdCamera    (car newCoo) )(- (cadr newCoo) (- rowHeight padding)) ))
    (setq newLocCameraNume (list (+ offsetTextNumeCamera  (car newCoo) )(- (cadr newCoo) (- rowHeight padding))))
    (if (= identifier "balcon")
        (progn
          ;; line horizontal between balcon
          (entmake (list (cons 0 "line")(cons 10 bottomLine)(cons 11 bottomLineEnd )(cons 62 layerColor)(cons 8 layerName)))
          (setq globalVarLeft bottomLine)
          (setq globalVarRight bottomLineEnd)
        )
        ;; line horizontal between camera
        (entmake (list (cons 0 "line")(cons 10 newCoo)(cons 11 plr )(cons 62 layerColor)(cons 8 layerName)))
    )
    (entmake (list (cons 0 "text")(cons 1 (rtos counter 2 0))(cons 10 newLocCameraId )  (cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))
    (entmake (list (cons 0 "text")(cons 1 text)              (cons 10 newLocCameraNume )(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle)))
)



;; tabel Suprafata totala utila = 0.0 mp
(defun createTotalUtilaTable(x / start plr_1 plr_2 plr_3 plr_4 plr_5 verticalBottomLineLeft horizontalBottomLine1 horizontalBottomLine2 textSuprafataTotala)
    (setq start (cdr x))
    (setq plr_1 (polar start (angtof "180") rowHeight ))
    (setq plr_2 (polar plr_1 (angtof "90") rowLength ))
    ;; linie sub Suprafata utila = ....
    (setq horizontalBottomLine1 (entmake (list (cons 0 "line")(cons 10 plr_1)(cons 11 plr_2)(cons 62 layerColor)(cons 8 layerName))))
    (setq plr_4 (polar plr_1 (angtof "90") (* rowHeight 3.44)))
    (setq plr_5 (polar plr_4 (angtof "0") padding))
    (setq textSuprafataTotala (entmake (list (cons 0 "text")(cons 10 plr_5)(cons 1 (strcat "Suprafata utila = " (rtos suprafataUtila 2 1)  " mp"))(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)(cons 7 textStyle))))
    (princ)
)



;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================




(setq layerNameUsiGeam "UsiGeamuri")
(setq layerColorUsiGeam 3)


(defun getManualDistance(/ dist finalDist)
     (setq dist (getdist "Alege distanta din desen sau introdu manual >>> "))
     (terpri)
     (cond 
          (
               (<= dist 0)
               (progn
                    (princ "Distanta trebuie sa fie mai mare ca zero\n")
                    (terpri)
                    (getManualDistance)
               )
          )
          (
               (= dist nil)
               (progn
                    (princ "Trebuie sa alegi o valoare\n")
                    (terpri)
                    (getManualDistance)
               )
          )
          (
               (> dist 0)
               (setq finalDist (rtos dist))
          )
     )
)



(defun c:gg(/ a b startBaza endBaza offsetDirection finalDistance lst offsetLine startSecond endSecond )
     ;; Create layer for doors and windows if not exists
     (setq currentLayer (getvar "clayer"))
     (command "_.Layer" "_Make" layerNameUsiGeam "_Color" layerColorUsiGeam "" "LType" "Continuous" "" "")
     
     (setq a (car (entsel "\nSelecteaza linia:\n")))
     (command "_.chprop" "_si" a "_color" "ByLayer" "")
     (command "_.chprop" "_si" a "_layer" layerNameUsiGeam "")
     (setq b (cdr (entget a)))
     ; coo baza start
     (setq startBaza (cdr (assoc 10 b)))
     ; coo baza end
     (setq endBaza (cdr (assoc 11 b)))

     (setq offsetDirection (getPoint "Directia de offset >\n"))
     (setq finalDistance (getManualDistance))

     (command "offset" finalDistance a offsetDirection "")

     (setq lst (entlast))
     (setq offsetLine (cdr (entget lst)))
     ;;;;;;; coo secund start
     (setq startSecond (cdr (assoc 10 offsetLine)))
     ;;;;;;; coo secund end
     (setq endSecond (cdr (assoc 11 offsetLine)))

     (entmake (list (cons 0 "line")(cons 10 startBaza)(cons 11 startSecond)(cons 8 layerNameUsiGeam)))
     (entmake (list (cons 0 "line")(cons 10 endBaza)(cons 11 endSecond)(cons 8 layerNameUsiGeam)))
     (command "offset" (/ (atof finalDistance) 2) a offsetDirection "")

     ;;;;;;;;; comenteaza randul de mai jos daca vrei sa lasi si linia opusa
     (command "erase" lst "")
     (setvar "clayer" currentLayer)
     (princ)
)






;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================
;;; ===========================================================





;;; USA


(setq layerNameUsiGeam "UsiGeamuri")
(setq layerColorUsiGeam 3)
(setq unghiUsa 90)


; base angles
(setq unghiPoz unghiUsa)
(setq unghiNeg (* unghiUsa -1))


(defun alin(/ lst endSecond startSecond offsetLine exteriorLine offSetDist offsetDirection ent start1 end1 leftBar rightBar leftMiddle rightMiddle)
     (command "_.Layer" "_Make" layerNameUsiGeam "_Color" layerColorUsiGeam "" "LType" "Continuous" "" "")
     (setq exteriorLine (car (entsel "\nSelecteaza linia usii exterioare: ")))
     (changeEntProperties exteriorLine)
     (terpri)
     (setq offsetDirection (getPoint "Directia de offset a usii >\n"))
     (terpri)
     (setq offSetDist (getdist "Grosime perete (mouse sau introdu manual): "))
     (terpri)

     (setq halfOffsetDist (/ offsetDist 2))

     (command "offset" halfOffsetDist exteriorLine offsetDirection "")
     (command "offset" halfOffsetDist exteriorLine offsetDirection "")
     (command "offset" offsetDist exteriorLine offsetDirection "")
     

     ;; original line that was selected (exterior)
     (setq ent (cdr (entget exteriorLine)))
     (setq start1 (cdr (assoc 10 ent)))
     (setq end1 (cdr (assoc 11 ent)))

     ;==============================
     (setq lst (entlast))
     (setq offsetLine (cdr (entget lst)))
     (setq startSecond (cdr (assoc 10 offsetLine)))
     (setq endSecond (cdr (assoc 11 offsetLine)))

     ;==============================

     ;; cross small lines
     (entmake (list (cons 0 "line")(cons 10 start1)(cons 11 startSecond)(cons 8 "0")))
     (entmake (list (cons 0 "line")(cons 10 end1)(cons 11 endSecond)(cons 8 "0")))

     ;; erase first selected line
     (eraseObj exteriorLine)
     (eraseObj lst)
     (princ)
)

;;; DELETE THIS FUNCTION - IS JUST FOR TESTING POINTS
(defun createCircle(x clr)
     (entmake (list (cons 0 "circle")(cons 10 x)(cons 40 0.1)(cons 62 clr)))
)

(defun changeEntProperties(ent)
     (command "_.chprop" "_si" ent "_color" "ByLayer" "")
     (command "_.chprop" "_si" ent "_layer" layerNameUsiGeam "")
)

(defun eraseObj(ent)
     (command "erase" ent "")
)

(defun c:xxd()
     (setq originalLayer (getvar "clayer"))
     (door unghiNeg "dreapta")
     (setvar "clayer" originalLayer)
     (princ)
)


(defun c:xxs()
     (setq originalLayer (getvar "clayer"))
     (door unghiPoz "stanga")
     (setvar "clayer" originalLayer)
     (princ)
)


; usa la stanga
(defun door(doorDirection LR / *error* a b angleType angleDirection startpoint1 endpoint1 s1x s1y pnt pntx pnty edata startpoint2 endpoint2 dist)
     (setq *error* xx:Error)

     (setq angleType (getvar "aunits"))
     (setq angleDirection (getvar "angdir"))
     (setq angleBase (getvar "angbase"))
     (setvar "aunits" 0)
     (setvar "angdir" 0)
     (setvar "angbase" (/ pi 2))
     (alin)

     (setq a (car (entsel "\nSelecteaza linia usii ce va fi rotita: ")))
     
     ; check if selected object is a line or not
     (if (eq (cdr(assoc 0 (entget a)))"LINE")
          (progn 
               (setq b (entget a))
               (changeEntProperties a)
               ; coo xy of start point (x y)
               (setq startpoint1 (cdr (assoc 10 b)))
               ; coo xy of end point (x y)
               (setq endpoint1 (cdr (assoc 11 b)))
               
               ; x of start point
               (setq s1x (car startpoint1))
               ; y of start point
               (setq s1y (cadr startpoint1))
               ; get origin of rotation
               (setq pnt (getpoint "\nSelecteaza punctul de rotire: \n"))
               
               ; x of origin point
               (setq pntx (car pnt))
               ; y of origin point
               (setq pnty (cadr pnt))
               (command "rotate" a "" pnt doorDirection)

               (setq edata (entget a))
               (setq startpoint2 (cdr (assoc 10 edata)))
               (setq endpoint2 (cdr (assoc 11 edata)))
               (setq dist (distance startpoint2 endpoint2))

               (if (= LR "stanga")
                    (progn
                         (if (and (= pnty s1y) (= pntx s1x))
                              (command "arc" endpoint1 "e" endpoint2 "r" dist)
                              (command "arc" startpoint1 "e" startpoint2 "r" dist)
                         );if

                    )
                    (progn
                         (if (and (= pnty s1y) (= pntx s1x))
                              (command "arc" endpoint2 "e" endpoint1 "r" dist)
                              (command "arc" startpoint2 "e" startpoint1 "r" dist)
                         );if
                    )
               )

               (setvar "aunits" angleType)
               (setvar "angdir" angleDirection)
               
          );progn
          ((alert "Nu ai selectat o linie.\nProgramul se inchide, incearca din nou."))
     );if line

     ;; reset angle directions back to what they were
     (setvar "aunits" angleType)
     (setvar "angdir" angleDirection)
     (setvar "angbase" angleBase)
     (princ)
)
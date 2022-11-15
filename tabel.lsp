



(setq textHeight 0.25)
(setq rowHeight (* textHeight 1.28)) ; 0.32
(setq padding (/ (- rowHeight textHeight) 2)) ; 0.2048
(setq rowLength (* rowHeight 43.75))  ; 14
(setq textContentCamera "Camera")
(setq textContentBalcon "Balcon")
(setq headerHeight (* rowHeight 2.5)) ; 0.8

;; position for vertical lines
(setq offsetLinieNumeCamera (* rowHeight 8.515))  ; 2.7248
(setq offsetLinieSuprafata (* rowHeight 25.39))   ; 8.1248
;; position for text
(setq offsetTextIdCamera (* rowHeight 3.515))     ; 1.1248
(setq offsetTextNumeCamera (* rowHeight 13.828))  ; 4.42496
(setq offsetTextSuprafata (* rowHeight 32.578))   ; 10.42496


;; inner things
(setq innerTextHeight (/ rowHeight 1.6))        ; 0.2
(setq innerSmallCircleRadius (/ rowHeight 2))   ; 0.16
(setq innerLargeCircleRadius (/ rowHeight 1.6)) ; 0.2

(setq globalVarLeft (list 0.0 0.0))
(setq globalVarRight (list 10.0 0.0))

(setq suprafataUtila 0)
(setq suprafataTotala 0)

(setq layerName "Tabel")
(setq layerColor 7)

(setq autoExecutant (* rowHeight 8.594))    ; 2.75
(setq autoName (* rowHeight 5.234))         ; 1.67
(setq autoDetails (* rowHeight 3.125))      ; 1
(setq autoDateTop (* rowHeight 35.938))     ; 11.50
(setq autoDateMiddle (* rowHeight 31.484))  ; 10.07
(setq autoDateBottom autoDateTop)
(setq autoReceptionat (* rowHeight 8.359))  ; 2.67

(setq footerSerieAutorizat "SERIA CT NR.116, CATEGORIILE B/C")
(setq footerNumeAutorizat "RUSU BOGDAN-ADRIAN")
(setq footerReceptionat "Receptionat,")
(setq footerDateTopBottom "Data")
(setq footerDateMiddle "Data: 11.11.2022")






(defun c:xxx()
    (load "tabel.lsp")
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
    (entmake (list (cons 0 "text")(cons 10 textLocation)(cons 1 (rtos finalArea 2 1))(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))
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
  ;; (setq innerTextLocation (list (- (car textLoc) (/ cRadius 2))(- (cadr textLoc) (/ cRadius 2))))
  (setq createText (entmake (list (cons 0 "text")(cons 1 roomNumber)(cons 10 textLoc)(cons 40 innerTextHeight)(cons 62 layerColor)(cons 8 layerName))))
  (entmake (list (cons 0 "circle")(cons 10 circleLocation)(cons 40 cRadius)(cons 62 layerColor)(cons 8 layerName)))
)



(defun createHeader(x / topLeftCorner one two strt1 three plr_1 plr_2 plr_3 plr_4 plr_text1 plr_text2 text1 text2 text3 text4 text5 text6 plr_text3 plr_text4 plr_text5 plr_text6 plr_text7 plr_text8 plr_text9 plr_text10 plr_text11 plr_text12 )
      ;; margin lines of header (top left, top middle, top right)
      (setq topLeftCorner (polar x (angtof "0") headerHeight))
      (setq one (entmake (list (cons 0 "line")(cons 10 x)(cons 11 topLeftCorner )(cons 62 layerColor)(cons 8 layerName))))
      ;end coo of one, start of strt
      (setq strt (cdr (caddr one)))
      (setq two (entmake (list (cons 0 "line")(cons 10 strt)(cons 11 (polar strt (angtof "90") rowLength) )(cons 62 layerColor)(cons 8 layerName))))
      (setq strt1 (cdr (caddr two)))
      (setq three (entmake (list (cons 0 "line")(cons 10 strt1)(cons 11 (polar strt1 (angtof "180") headerHeight))(cons 62 layerColor)(cons 8 layerName))))
      ;create separators between id name and area (vertical lines)
      (setq plr_1 (polar x (angtof "90") offsetLinieNumeCamera))
      (setq plr_2 (polar plr_1 (angtof "0") headerHeight))
      (entmake (list (cons 0 "line")(cons 10 plr_1)(cons 11 plr_2)(cons 62 layerColor)(cons 8 layerName)))
      (setq plr_3 (polar x (angtof "90") offsetLinieSuprafata))
      (setq plr_4 (polar plr_3 (angtof "0") headerHeight))
      (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 plr_4)(cons 62 layerColor)(cons 8 layerName)))

      ; add text to the boxes created
      (setq plr_text1 (polar x (angtof "90") (* rowHeight 1.641)))
      (setq plr_text2 (polar plr_text1 (angtof "0") (* rowHeight 0.391)))
      (setq text1 (entmake (list (cons 0 "text")(cons 10 plr_text2)(cons 1 "incapere")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
      (setq plr_text3 (polar x (angtof "90") (* rowHeight 2.968)))
      (setq plr_text4 (polar plr_text3 (angtof "0") (* rowHeight 1.401)))
      (setq text2 (entmake (list (cons 0 "text")(cons 10 plr_text4)(cons 1 "Nr.")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
      (setq plr_text5 (polar x (angtof "90") (* rowHeight 13.671)))
      (setq plr_text6 (polar plr_text5 (angtof "0") (* rowHeight 1.401)))
      (setq text3 (entmake (list (cons 0 "text")(cons 10 plr_text6)(cons 1 "Denumire")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
      (setq plr_text7 (polar x (angtof "90") (* rowHeight 14.06)))
      (setq plr_text8 (polar plr_text7 (angtof "0") (* rowHeight 0.391)))
      (setq text4 (entmake (list (cons 0 "text")(cons 10 plr_text8)(cons 1 "incapere")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
      (setq plr_text9 (polar x (angtof "90") (* rowHeight 30.625)))
      (setq plr_text10 (polar plr_text9 (angtof "0") (* rowHeight 1.401)))
      (setq text5 (entmake (list (cons 0 "text")(cons 10 plr_text10)(cons 1 "Suprafata utila")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
      (setq plr_text11 (polar x (angtof "90") (* rowHeight 32.813)))
      (setq plr_text12 (polar plr_text11 (angtof "0") (* rowHeight 0.391)))
      (setq text6 (entmake (list (cons 0 "text")(cons 10 plr_text12)(cons 1 "[mp]")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
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
      ;left vertical line
      (entmake (list (cons 0 "line") (cons 10 start)(cons 11 newLocation)(cons 62 layerColor)(cons 8 layerName)))
      ;right vertical line
      (entmake (list (cons 0 "line") (cons 10 plr_endCameraBottomRight)(cons 11 plrRightVertical)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line") (cons 10 plrLineNumeCamera)(cons 11 (polar newLocation (angtof "90") offsetLinieNumeCamera ))(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line") (cons 10 plrLineSuprafata)(cons 11 (polar newLocation (angtof "90") offsetLinieSuprafata ))(cons 62 layerColor)(cons 8 layerName)))
      (createTotalUtilaTable (cadr unimportant))
      (balc counter unimportant)
      (setq suprafataUtila 0)
      (setq suprafataTotala 0)
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
      (setq plr_5 (polar plr_3 (angtof "90") offsetTextNumeCamera))
      (setq plr_6 (polar plr_5 (angtof "0") padding))
      (setq plr_7 (polar globalVarLeft (angtof "90") offsetLinieNumeCamera)) ;; bottom
      (setq plr_8 (polar globalVarLeft (angtof "90") offsetLinieSuprafata)) ;; bottom 
      (setq plr_9 (polar start (angtof "90") offsetLinieNumeCamera)) ;; top 
      (setq plr_10 (polar start (angtof "90") offsetLinieSuprafata)) ;; top 


      ;;;;;; top left
      (entmake (list (cons 0 "line")(cons 10 start)(cons 11 globalVarLeft)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_2)(cons 11 globalVarRight)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_7)(cons 11 plr_9)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_8)(cons 11 plr_10)(cons 62 layerColor)(cons 8 layerName)))
      
      (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 plr_4)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 globalVarLeft)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 plr_4)(cons 11 globalVarRight)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "text")(cons 10 plr_6)(cons 1 (strcat "Suprafata Totala = " (rtos suprafataUtila 2 1)  " mp"))(cons 62 layerColor)(cons 8 layerName)(cons 40 textHeight)))
      (createAuthorized)
)

(defun createAuthorized(/ leftStart rightStart left1_2 right1_2 left2_3 right2_3 left3_4 right3_4 left4_5 right4_5 middleTop middleBottom plr_executant plr_numeAutorizat plr_detailsAutorizat plr_receptionat plr_dataSus plr_dataMiddle plr_dataBottom)
      ;; globalVarLeft -> stanga sus la suprafata totala 
      ;; stanga sus la executant
      (setq leftStart (polar globalVarLeft (angtof "180") rowHeight))
      ;; dreapta sus la executant
      (setq rightStart (polar leftStart (angtof "90") rowLength))
      ;; stanga jos la executant
      (setq left1_2 (polar leftStart (angtof "180") rowHeight))
      ;; dreapta jos la executant
      (setq right1_2 (polar left1_2 (angtof "90") rowLength))
      (setq left2_3 (polar left1_2 (angtof "180") (* rowHeight 11.25)))
      (setq right2_3 (polar left2_3 (angtof "90") rowLength))
      (setq left3_4 (polar left2_3 (angtof "180") (* rowHeight 1.328)))
      (setq right3_4 (polar left3_4 (angtof "90") rowLength))
      (setq left4_5 (polar left3_4 (angtof "180") (* rowHeight 10.78)))
      (setq right4_5 (polar left4_5 (angtof "90") rowLength))
      (entmake (list (cons 0 "line")(cons 10 left1_2)(cons 11 right1_2)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left2_3)(cons 11 right2_3)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left3_4)(cons 11 right3_4)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 left4_5)(cons 11 right4_5)(cons 62 layerColor)(cons 8 layerName)))

      (setq middleTop (polar leftStart (angtof "90") (* rowHeight 30.31)))
      (setq middleBottom (polar left4_5 (angtof "90") (* rowHeight 30.31)))
      (entmake (list (cons 0 "line")(cons 10 leftStart)(cons 11 left4_5)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 middleTop)(cons 11 middleBottom)(cons 62 layerColor)(cons 8 layerName)))
      (entmake (list (cons 0 "line")(cons 10 rightStart)(cons 11 right4_5)(cons 62 layerColor)(cons 8 layerName)))

      ; point executant
      (setq plr_executant (polar (list (car leftStart)(- (cadr leftStart) padding textHeight)) (angtof "90") autoExecutant))
      (entmake (list (cons 0 "text")(cons 10 plr_executant)(cons 1 "Executant")(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_numeAutorizat (polar (list (car left1_2)(- (cadr left1_2) (* padding 3) textHeight)) (angtof "90") autoName))
      (entmake (list (cons 0 "text")(cons 10 plr_numeAutorizat)(cons 1 footerNumeAutorizat)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_detailsAutorizat (polar (list (car left1_2)(- (cadr left1_2) (* padding 6) (* textHeight 2))) (angtof "90") autoDetails))
      (entmake (list (cons 0 "text")(cons 10 plr_detailsAutorizat)(cons 1 footerSerieAutorizat)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_receptionat (polar (list (car left3_4)(+ (cadr left3_4) (* padding 2.5) )) (angtof "90") autoReceptionat))
      (entmake (list (cons 0 "text")(cons 10 plr_receptionat)(cons 1 footerReceptionat)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_dataSus (polar (list (car left1_2)(+ (cadr left1_2) padding )) (angtof "90") autoDateTop))
      (entmake (list (cons 0 "text")(cons 10 plr_dataSus)(cons 1 footerDateTopBottom)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_dataMiddle (polar (list (car left2_3)(+ (cadr left2_3) (* padding 2) )) (angtof "90") autoDateMiddle))
      (entmake (list (cons 0 "text")(cons 10 plr_dataMiddle)(cons 1 footerDateMiddle)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

      (setq plr_dataBottom (polar (list (car left3_4)(+ (cadr left3_4) (* padding 2.5) )) (angtof "90") autoDateBottom))
      (entmake (list (cons 0 "text")(cons 10 plr_dataBottom)(cons 1 footerDateTopBottom)(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))

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
          (entmake (list (cons 0 "line")(cons 10 bottomLine)(cons 11 bottomLineEnd )(cons 62 layerColor)(cons 8 layerName)))
          (setq globalVarLeft bottomLine)
          (setq globalVarRight bottomLineEnd)
        )
        (entmake (list (cons 0 "line")(cons 10 newCoo)(cons 11 plr )(cons 62 layerColor)(cons 8 layerName)))
    )
    (entmake (list (cons 0 "text")(cons 1 (rtos counter 2 0))(cons 10 newLocCameraId )  (cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))
    (entmake (list (cons 0 "text")(cons 1 text)              (cons 10 newLocCameraNume )(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName)))
)



;; tabel Suprafata totala utila = 0.0 mp
(defun createTotalUtilaTable(x / start plr_1 plr_2 plr_3 plr_4 plr_5 verticalBottomLineLeft horizontalBottomLine1 horizontalBottomLine2 textSuprafataTotala)
    (setq start (cdr x))
    (setq plr_1 (polar start (angtof "180") rowHeight ))
    (setq verticalBottomLineLeft (entmake (list (cons 0 "line")(cons 10 start)(cons 11 plr_1)(cons 62 layerColor)(cons 8 layerName))))
    (setq plr_2 (polar plr_1 (angtof "90") rowLength ))
    (setq horizontalBottomLine1 (entmake (list (cons 0 "line")(cons 10 plr_1)(cons 11 plr_2)(cons 62 layerColor)(cons 8 layerName))))
    (setq plr_3 (polar start (angtof "90") rowLength ))
    (setq horizontalBottomLine2 (entmake (list (cons 0 "line")(cons 10 plr_3)(cons 11 plr_2)(cons 62 layerColor)(cons 8 layerName))))
    (setq plr_4 (polar plr_1 (angtof "90") offsetTextNumeCamera))
    (setq plr_5 (polar plr_4 (angtof "0") padding))
    (setq textSuprafataTotala (entmake (list (cons 0 "text")(cons 10 plr_5)(cons 1 (strcat "Suprafata Utila = " (rtos suprafataUtila 2 1)  " mp"))(cons 40 textHeight)(cons 62 layerColor)(cons 8 layerName))))
    (princ)
)


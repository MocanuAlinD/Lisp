;;08-11-2022 13.33

;;;; Program realizare geamuri automate pentru autocad
;;;; Explicatie:
;;;; 1. Alegeti linia pentru offset
;;;; 2. Alegeti directia de offset (clic pe partea in care se doreste offset pt fereastra)
;;;; 3. Alege daca introduci manual distanta de offset sau o iei cu mousel. ( poti alege doar 1 sau 2 )
;;;; 4. 



(defun getManualDistance(x / dist finalDist)
     (cond 
          (
               (= x 1)
               (progn 
                    (setq dist (getreal "Introdu distanta >>> "))
                    (cond 
                         (
                              (<= dist 0)
                              (progn
                                   (princ "Distanta trebuie sa fie mai mare ca zero\n")
                                   (getManualDistance 1)
                              )
                         )
                         (
                              (= dist nil)
                              (progn
                                   (princ "Trebuie sa introduci o valoare. Nu poate fi gol\n")
                                   (getManualDistance 1)
                              )
                         )
                         (
                              (> dist 0)
                              (setq finalDist (rtos dist))
                         )
                    )
               );progn
          )
          (
               (= x 2)
               (progn 
                    (setq dist (getdist "Alege distanta din desen >>> "))
                    (cond 
                         (
                              (<= dist 0)
                              (progn
                                   (princ "Distanta trebuie sa fie mai mare ca zero\n")
                                   (getManualDistance 2)
                              )
                         )
                         (
                              (= dist nil)
                              (progn
                                   (princ "Trebuie sa alegi o valoare\n")
                                   (getManualDistance 2)
                              )
                         )
                         (
                              (> dist 0)
                              (setq finalDist (rtos dist))
                         )
                    )
               );progn
          )
     )
)


(defun getSomeDistance (/ dist alegere)
     (setq alegere (getint "----->  1 - Introdu manual.\n----->  2 - Alege cu mouseul.\n"))
     (if (or (= alegere 1) (= alegere 2))
         (progn 
               (cond 
                    (
                         (= alegere 1)
                         (setq dist (getManualDistance 1))
                    )
                    (
                         (= alegere 2)
                         (setq dist (getManualDistance 2))
                    )
               );cond
         ) ;progn
         (getSomeDistance) 
     );if
)



(defun c:gg(/ a b startBaza endBaza offsetDirection finalDistance lst offsetLine startSecond endSecond )

     (setq a (car (entsel "\nSelecteaza linia:\n")))
     (setq b (cdr (entget a)))
     ; coo baza start
     (setq startBaza (cdr (assoc 10 b)))
     ; coo baza end
     (setq endBaza (cdr (assoc 11 b)))

     (setq offsetDirection (getPoint "Directia de offset >\n"))
     (setq finalDistance (getSomeDistance))

     (command "offset" finalDistance a offsetDirection "")

     (setq lst (entlast))
     (setq offsetLine (cdr (entget lst)))
     ;;;;;;; coo secund start
     (setq startSecond (cdr (assoc 10 offsetLine)))
     ;;;;;;; coo secund end
     (setq endSecond (cdr (assoc 11 offsetLine)))

     (command "line" startBaza startSecond "")
     (command "line" endBaza endSecond "")
     (command "offset" (/ (atof finalDistance) 2) a offsetDirection "")

     ;;;;;;;;; comenteaza randul de mai jos daca vrei sa lasi si linia opusa
     (command "erase" lst "")
     (princ)
)

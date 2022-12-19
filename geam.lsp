
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
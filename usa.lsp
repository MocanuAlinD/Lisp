
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


;(alert "'xxd' pentru usa la dreapta.\n'xxs' pentru usa la stanga.\n\nDeschide fisierul pentru mai multe detalii.")

; base angles
(setq unghiPoz 90)
(setq unghiNeg -90)

; usa la dreapta
(defun c:xxd(/ *error*      a b angleType angleDirection edata startpoint2 endpoint2 dist startpoint1 endpoint1 s1x s1y pnt pntx pnty)
     (setq *error* xx:Error)

     (setq angleType (getvar "aunits"))
     (setq angleDirection (getvar "angdir"))
     (setvar "aunits" 0)
     (setvar "angdir" 0)

     (setq a (car (entsel "\nSelecteaza linia: ")))

     ; check if selected object is a line or not
    (if (eq (cdr(assoc 0 (entget a)))"LINE")
          (progn 
               (setq b (entget a))
               (subst (cons 8 "Usa_Geam")(assoc 8 b) b)
               (princ b)
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
               (command "rotate" a "" pnt unghiNeg)
               
               ;===================================================================

               (setq edata (entget a))
               (setq startpoint2 (cdr (assoc 10 edata)))
               (setq endpoint2 (cdr (assoc 11 edata)))
               (setq dist (distance startpoint2 endpoint2))
               (if (and (= pnty s1y) (= pntx s1x))
                    (command "arc" endpoint2 "e" endpoint1 "r" dist)
                    (command "arc" startpoint2 "e" startpoint1 "r" dist)
               );if)

               ;; comenteaza 2 randuri mai jos daca nu vrei sa pastreze linia originala de la usa
               ;; Adauga ";" in fata randului de jos
               (command "line" startpoint1 endpoint1 end)
          );progn
          ((alert "Nu ai selectat o linie.\nProgramul se inchide, incearca din nou."))
     );if line
     (setvar "aunits" angleType)
     (setvar "angdir" angleDirection)
     (princ)
)






; usa la stanga
(defun c:xxs(/ *error* a b angleType angleDirection startpoint1 endpoint1 s1x s1y pnt pntx pnty edata startpoint2 endpoint2 dist)
     (setq *error* xx:Error)
     
     (setq angleType (getvar "aunits"))
     (setq angleDirection (getvar "angdir"))
     (setvar "aunits" 0)
     (setvar "angdir" 0)
     (setq a (car (entsel "\nSelecteaza linia: ")))
     ; check if selected object is a line or not
     (if (eq (cdr(assoc 0 (entget a)))"LINE")
          (progn 
               (setq b (entget a))
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
               (command "rotate" a "" pnt unghiPoz)
               
               ;===================================================================

               (setq edata (entget a))
               (setq startpoint2 (cdr (assoc 10 edata)))
               (setq endpoint2 (cdr (assoc 11 edata)))
               (setq dist (distance startpoint2 endpoint2))
               (if (and (= pnty s1y) (= pntx s1x))
                    (command "arc" endpoint1 "e" endpoint2 "r" dist)
                    (command "arc" startpoint1 "e" startpoint2 "r" dist)
               );if)

               (command "line" startpoint1 endpoint1 end)
          );progn
          ((alert "Nu ai selectat o linie.\nProgramul se inchide, incearca din nou."))
     );if line

     ;; reset angle directions back to what they were
     (setvar "aunits" angleType)
     (setvar "angdir" angleDirection)
     (princ)
)
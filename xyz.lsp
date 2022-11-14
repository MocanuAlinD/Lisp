;; pune ";" la inceputul randului urmator ca sa nu mai primesti mesajul de la inceput cu comanda. (ori poti sterge tot randul de jos)
(alert "Comanda este: >>> xyz <<< ")

;; distante dintre randuri si distantele dintre nr pct, x, y si z sunt in functie de marimea scrisului. Il micsorezi sau cresti, proportiile raman aceleasi
(setq marimeText 1)
(setq nrZecimaleCota 2)
(setq nrZecimaleCoo 3)
;; pune mai mic de 1.5 ca sa creasca spatiul intre randuri, sau il cresti ca sa micsorezi spatiu dintre randuri
(setq spatiu (+ marimeText ( / marimeText 1.5)))
(setq inStangaDreapta (* marimeText 1.2))
(setq inJosSus (* marimeText -0.4))
(setq offsetCooDreaptaX (* marimeText 3.16))
(setq offsetCooDreaptaY (* marimeText 14))
(setq offsetCooDreaptaZ (* marimeText 25))


(defun c:xxx()
    (load "xyz.lsp")
    (princ "Loaded")
    (princ)
)



(defun c:xyz(/ ent getType counter)
        (setq counter (getint "Primul punct este: (ENTER incepe de la 1) "))
        (terpri)
        (if (= counter nil)
          (setq counter 1)
        );if 
        (setq getInitialPoint (getpoint "Alege punctul de plecare: "))
        (terpri)
        (setq offsetText 0)
        (while (setq ent (car (entsel "<----------  Alege un punct: (clic dreapta pentru a termina) ")))
            (terpri)
            (setq xyz (entget ent))
            (setq testPoint (cdr (cadr xyz)))
            (terpri)


            (if (= testPoint "POINT")
                (progn
                    (setq getCoo (cdr(assoc 10 xyz)))
                    (setq x (cadr getCoo))
                    (setq y (car getCoo))
                    (setq z (caddr getCoo))
                    (setq xOriginal (car getInitialPoint))
                    (setq yOriginal (cadr getInitialPoint))
                    (entmake (list (cons 0 "text")(cons 1 (rtos counter 2 0))(cons 40 marimeText)(cons 10 (list ( + y inStangaDreapta) (+ x inJosSus)  ))(cons 8 "Nr Punct")))
                    (entmake (list (cons 0 "text")(cons 1 (rtos counter 2 0))(cons 40 marimeText)(cons 10 (list xOriginal (- yOriginal offsetText)))(cons 8 "Tabel")))
                    (entmake (list (cons 0 "text")(cons 1 (rtos x 2 nrZecimaleCoo))(cons 40 marimeText)(cons 10 (list (+ xOriginal offsetCooDreaptaX) (- yOriginal offsetText)))(cons 8 "Tabel")))
                    (entmake (list (cons 0 "text")(cons 1 (rtos y 2 nrZecimaleCoo))(cons 40 marimeText)(cons 10 (list (+ xOriginal offsetCooDreaptaY) (- yOriginal offsetText)))(cons 8 "Tabel")))
                    (entmake (list (cons 0 "text")(cons 1 (rtos z 2 nrZecimaleCota))(cons 40 marimeText)(cons 10 (list (+ xOriginal offsetCooDreaptaZ) (- yOriginal offsetText)))(cons 8 "Tabel")))
                    (setq offsetText (+ offsetText spatiu))
                    (setq counter (1+ counter))
                    (princ)
                );progn
                (princ "Nu ai ales un punct")
            );if


            
        );while
        (princ "Ai terminat.")
        (princ)

)

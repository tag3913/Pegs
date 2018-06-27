(load "help.l")

;simulate the play of the game
(defmethod play (&aux play avail move)
   (setf play ())
   (setf avail '(nw n ne w c e sw s se))
   (dolist (player '(x o x o x o x o x))
      (cond
         ((eq player 'x)
            (setf move (select avail))
            (setf avail (remove move avail))
            (setf play (snoc move play))
         )
         ((eq player 'o)
            (setf move (select avail))
            (setf avail (remove move avail))
            (setf play (snoc move play))
         )
      )
   )
   play
)

(setf positions '(NW N NE W C E SW S SE))

(defmethod stats ((n number) (demo t) &aux w l d p result)
   (if demo (format t "Begin Gathering Statistics ...~%"))
   (setf w 0 l 0 d 0)
   (dotimes ( i n )
      (setf p (play))
      (if demo (format t "~A~%" p))
      (if demo (visualize p))
      (setf result (analyze p))
      (if demo (format t "~A~%" result))
      (cond
         ((eq result 'w) (setf w (+ w 1)))
         ((eq result 'l) (setf l (+ l 1)))
         ((eq result 'd) (setf d (+ d 1)))
      )
   )
   (setf results (mapcar #'probability (list w l d) (list n n n)))
   (if demo (format t "End Gathering Statistics~%"))
   (mapcar #'list '(w l d) results)
)

(defmethod probability ((special integer) (total integer))
   (/ (float special) (float total))
)

(defmethod demo (&aux p)
   (setf p (play))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)

(defmethod visualize ((play list) )
   (dotimes (i 9)
      (cond
         ((find (nth i positions) play)
            (cond
               ((even (position (nth i positions) play))
                  (format t "X ")
               )
               (t
                  (format t "O ")
               )
            )
         )
         (t
            (format t "-- ")
         )
      )
      (if (or (= 2 i) (= 5 i))
         (terpri)
      )
   )
)

(defmethod analyze ((play list) &aux True-result  Xresult Oresult Xlist Olist Xwin Owin)
   (setf True-result 'T)
   (setf Xresult 'T)
   (setf Oresult 'T)
   (setf Xlist '())
   (setf Olist '())
   (setf wins '((NW N NE) (W C E) (SW S SE) (NW W SW) (N C S) (NE E SE) (NW C SE) (NE C SW)))
   (dotimes (i 9)
      (cond
         ((even i)
            (setf Xlist (snoc (nth i play) Xlist))
         )
         (t
            (setf Olist (snoc (nth i play) Olist))
         )
      )
   )
   (dotimes (i 8)
      (if (sublist Xlist (nth i wins))
         (progn
            (setf Xresult 'W)
            (setf Xwin (nth i wins))
         )
      )
      (if (sublist Olist (nth i wins))
         (progn
            (setf Oresult 'W)
            (setf Owin (nth i wins))
         )
      )

   )
   (if (equal Xresult 'W)
      (setf True-result Xresult)
   )
   (if (equal Oresult 'W)
      (setf True-result 'L)
   )
   (if (and (equal Xresult 'W) (equal Oresult 'W))
      (progn
         (if (< (farthest play Xwin) (farthest play Owin))
            (setf True-result 'W)
            (setf True-result 'L)
         )
      )
   )
   True-result
)

;TASK3 - modeling human and random players

; The root of the player hierarchy
;

(defclass player ()
   ((name :accessor player-name :initarg :name :initform 'emanon))
)

; A random machine player is a player
;

(defclass random-machine-player (player) ())

( defmethod display ((p random-machine-player))
   (format t "RANDOM MACHINE PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
)

; A human player is a player

( defclass human-player (player) ())

( defmethod display ((p human-player))
   (format t "HUMAN PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
   nil
)

; A move making method for a random machine player
;
(defmethod make-move ((p random-machine-player) (report t) &aux move)
   (if report (format t "BEGIN RANDOM PLAYER MOVE ...~%"))
   (setf move (select *avail*))
   (if report (format t "randomly selecting ~A for my move~%" move))
   (setf *avail* (remove move *avail*))
   (if report (format t "END RANDOM PLAYER MOVE ...~%"))
   move
)

; A move making method for a human player
;
(defmethod make-move ((p human-player) (report t) &aux move)
   (if report (format t "BEGIN HUMAN PLAYER MOVE ...~%"))
   (format t "Please select a move from ~A~%" *avail*)
   (setf move (read))
   (cond 
      ((not (member move *avail*))
         (make-move p)
      )
      (t
         (setf *avail* (remove move *avail*))
         move
      )
   )
   (if report (format t "END HUMAN PLAYER MOVE~%"))
   move
)

; A generic play method. The *play-so-far* variable will be used by the heuristic machine
;

(defmethod generic-play ((x player) (o player) (report t) &aux move)
   (setf *avail* '(nw n ne w c e sw s se))
   (setf *play-so-far* ())
   (dolist (player '(x o x o x o x o x))
      (if (or report (equal (type-of o) 'human-player-machine))
         (visualize *play-so-far*)
      )
      (cond 
         ((eq player 'x)
            (setf move (make-move x report))
         )
         ((eq player 'o)
            (setf move (make-move o report))
         )
      )
      (setf *play-so-far* (snoc move *play-so-far*))
      (if (game-over-p *play-so-far*) (return nil))
   )
   *play-so-far*
)

;Predicate to determine if the game is not over or not
;
(defmethod game-over-p((play list))
   (cond 
      ((line-p (odd play)) 'w)
      ((line-p (even play)) 'l)
      ((= (length play) 9) 'd)
      (t nil)
   )
)

(defmethod odd ((l list))
   (cond
      ((null l) ())
      ((null (cdr l)) (list (car l)))
      (t (cons (car l) (odd (cddr l))))
   )
)

(defmethod even ((l list))
   (cond
      ((null l) ())
      ((null (cdr l)) ())
      (t (cons (cadr l) (even (cddr l))))
   )
)

(defmethod line-p ((l list))
   (cond
      ((< (length l) 3)
         nil
      )
      ((= (length l) 3)
         (line (first l) (second l) (third l))
      )
      ((= (length l) 4)
         (or 
            (line (first l) (second l) (third l))
            (line (first l) (second l) (fourth l))
            (line (first l) (third l) (fourth l))
            (line (second l) (third l) (fourth l))
         )
      )
      ((= (length l) 5)
         (or 
            (line (first l) (second l) (third l))
            (line (first l) (second l) (fourth l))
            (line (first l) (second l) (fifth l))
            (line (first l) (third l) (fourth l))
            (line (first l) (third l) (fifth l))
            (line (first l) (fourth l) (fifth l))
            (line (second l) (third l) (fourth l))
            (line (second l) (fourth l) (fifth l))
            (line (third l) (fourth l) (fifth l))
         )
      )
   )
)

(defmethod line (a b c)
	(and (eq a b) (eq b c) (eq a c))
)

;two random machine players play one game
;
(defmethod demo-random-random (&aux p x o)
   (setf x (make-instance 'random-machine-player))
   (setf o (make-instance 'random-machine-player))
   (setf p (generic-play x o t))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)

;A random machine player plays one game against a human player
;
(defmethod demo-random-human (&aux p x o)
   (setf x (make-instance 'random-machine-player))
   (setf o (make-instance 'human-player))
   (setf p (generic-play x o t))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)


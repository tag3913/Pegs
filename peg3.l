
(load "help.l")

;TASK 1 ---------------------------------------------------------------
;
;Create game board and pieces
;

(setf style 3)
(setf *random-state* (make-random-state t))

;triangular-board - returns list for triangular board
;
(setf positions '(A B C D E F G H I J K L M N O))

(setf random-initial-spot (random 15))

(defmethod triangular-board ()

   (setf table (MAKE-HASH-TABLE))
   (dotimes (i 15)
      (setf (gethash (nth i positions) table) "+")
      (if (= random-initial-spot i)
         (setf (gethash (nth i positions) table) "o")
      )
   )
   table
)


;Board class - holds all board info being the key value pairs and style
;
(defclass board ()
   (
      (style :accessor board-style :initarg :style :initform style)
      (setup :accessor board-setup :initarg :setup :initform (triangular-board))
   )
)

;initialize - creates board object, this will be where boards can change------

(defun display ((b board))
   (format t "    ~A~%" (gethash 'A (board-setup b)))
   (format t "   ~A" (gethash 'B (board-setup b)))
   (format t " ~A~%" (gethash 'C (board-setup b)))
   (format t "  ~A" (gethash 'D (board-setup b)))
   (format t " ~A" (gethash 'E (board-setup b)))
   (format t " ~A~%" (gethash 'F (board-setup b)))
   (format t " ~A" (gethash 'G (board-setup b)))
   (format t " ~A" (gethash 'H (board-setup b)))
   (format t " ~A" (gethash 'I (board-setup b)))
   (format t " ~A~%" (gethash 'J (board-setup b)))
   (format t "~A" (gethash 'K (board-setup b)))
   (format t " ~A" (gethash 'L (board-setup b)))
   (format t " ~A" (gethash 'M (board-setup b)))
   (format t " ~A" (gethash 'N (board-setup b)))
   (format t " ~A" (gethash 'O (board-setup b)))
   (terpri)
)

(defun display-reference-board ()
   (format t "    A~%")
   (format t "   B")
   (format t " C~%")
   (format t "  D")
   (format t " E" )
   (format t " F~%")
   (format t " G")
   (format t " H")
   (format t " I")
   (format t " J~%")
   (format t "K")
   (format t " L")
   (format t " M")
   (format t " N")
   (format t " O")
   (terpri)
)


;TASK 2---------------------------------------------------------------------

;Make move function to lists of possible spaces for each peg to move
(defmethod possible-move (select-peg &aux moves)
   (cond
      ((equal select-peg 'A)
         (setf moves '(D F)))
      ((equal select-peg 'B)
         (setf moves '(I G)))
      ((equal select-peg 'C)
         (setf moves '(H J)))
      ((equal select-peg 'D)
         (setf moves '(A M K F)))
      ((equal select-peg 'E)
         (setf moves '(N L)))
      ((equal select-peg 'F)
         (setf moves '(A O D M)))
      ((equal select-peg 'G)
         (setf moves '(B I)))
      ((equal select-peg 'H)
         (setf moves '(J C)))
      ((equal select-peg 'I)
         (setf moves '(B G)))
      ((equal select-peg 'J)
         (setf moves '(C H)))
      ((equal select-peg 'K)
         (setf moves '(M D)))
      ((equal select-peg 'L)
         (setf moves '(E N)))
      ((equal select-peg 'M)
         (setf moves '(D F K O)))
      ((equal select-peg 'N)
         (setf moves '(L E)))
      ((equal select-peg 'O)
         (setf moves '(F M)))
   )
   moves
)


;between - finds the peg between the two positions
;
(defmethod between (peg-list &aux between-slot)
   (cond
      ((contains peg-list '((A D) (D A)))
         (setf between-slot 'B)
      )
      ((contains peg-list '((A F) (F A)))
         (setf between-slot 'C)
      )
      ((contains peg-list '((B G) (G B)))
         (setf between-slot 'D)
      )
      ((contains peg-list '((B I) (I B) (C H) (H C) (D F) (F D)))
         (setf between-slot 'E)
      )
      ((contains peg-list '((C J) (J C)))
         (setf between-slot 'F)
      )
      ((contains peg-list '((D K) (K D)))
         (setf between-slot 'G)
      )
      ((contains peg-list '((D M) (M D) (E L) (L E) (G I) (I G)))
         (setf between-slot 'H)
      )
      ((contains peg-list '((E N) (N E) (F M) (M F) (H J) (J H)))
         (setf between-slot 'I)
      )
      ((contains peg-list '((F O) (O F)))
         (setf between-slot 'J)
      )
      ((contains peg-list '((K M) (M K)))
         (setf between-slot 'L)
      )
      ((contains peg-list '((L N) (N L)))
         (setf between-slot 'M)
      )
      ((contains peg-list '((M O) (O M)))
         (setf between-slot 'N)
      )
   )
   between-slot
)

;makes a move on the board
;
(defmethod make-move ((b board) select-peg to-slot)
    (if (and (equal (gethash select-peg (board-setup b)) "+") (equal (gethash to-slot (board-setup b)) "o"))
       (progn
           (cond
               ((and (member to-slot (possible-move select-peg)) (equal (gethash (between (list select-peg to-slot)) (board-setup b)) "+"))
                 (setf (gethash (between (list select-peg to-slot)) (board-setup b)) "o")
                 (setf (gethash to-slot (board-setup b)) "+")
                 (setf (gethash select-peg (board-setup b)) "o")
               )
           )
       )
    )
)

;TASK 3--------------------------------------------------------
;
;Make display moves function to show human players the moves in the 
;current game that they can make.
;Allows the player to select from the available moves to progess in the game
;terminates when no moves are available or the player chooses to quit

(defmethod generate-available-moves ((b board) &aux to-slots)
   (setf moves '())
   (dotimes (open-num 15)
      (if (equal (gethash (nth open-num positions) (board-setup b)) "+")
         (progn
            (setf select-peg (nth open-num positions))
            (setf to-slots (possible-move select-peg))
            (dotimes (closed-num (length to-slots))
               (setf to-slot (nth closed-num to-slots))
               (setf between-slot (between (list select-peg to-slot)))
               (if (and (equal (gethash to-slot (board-setup b)) "o") (equal (gethash between-slot (board-setup b)) "+"))
                  (setf moves (cons (list select-peg to-slot) moves))
               )   
            )
         )
      )
   )
   moves
)


(defun display-moves ((b board))
   (setf moves (generate-available-moves b))
   (setf counter 0)
   (dotimes (i (length moves))
      (format t "~A: " counter)
      (format t "Move ~A" (car (nth i moves)))
      (format t " to ~A" (rac (nth i moves)))
      (setf counter (+ counter 1))
      (terpri)
   )  
)


(defun play (&aux board moves)
   (format t "WELCOME TO PEG SOLITAIRE~%")
   (format t "Use the reference board below to make moves~%")
   (format t "Enter 'quit' to exit the game~%")
   (display-reference-board)
   (setf board (make-instance 'board))
   (run board)
) 

(defmethod run ((b board) &aux moves choice)
   (setf moves (generate-available-moves b))
   (cond
      ((null moves)
         (format t "Finished!~%")
         (format t "Final board:~%")
         (display b)
      )
      (t
         (display b)
         (terpri)
         (format t "CURRENT BOARD:")
         (terpri)
         (display-moves b)
         (setf choice (read))
         (if (equal choice 'quit)
            (format t "Thanks for playing!")
            (progn
               (setf selected-move (nth choice moves))
               (make-move b (car selected-move) (rac selected-move))
               (run b)
            )
         )
      )
   )
)




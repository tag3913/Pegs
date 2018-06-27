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

(defmethod initialize ()
   (setf board (make-instance 'board))
   (display board)
)

(defmethod display ((b board))
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
 
;TASK 2---------------------------------------------------------------------

;Make move function

;create lists of possible spaces for each peg to move
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
    (display b)
)



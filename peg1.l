;TASK 1 ---------------------------------------------------------------
;
;Create game board and pieces
;

(setf style 3)

;triangular-board - returns list for triangular board
; 

(defmethod triangular-board ()
   (setf table (MAKE-HASH-TABLE))
   (setf (gethash 'A table) "o")
   (setf (gethash 'B table) "+")
   (setf (gethash 'C table) "+")
   (setf (gethash 'D table) "+")
   (setf (gethash 'E table) "+")
   (setf (gethash 'F table) "+")
   (setf (gethash 'G table) "+")
   (setf (gethash 'H table) "+")
   (setf (gethash 'I table) "+")
   (setf (gethash 'J table) "+")
   (setf (gethash 'K table) "+")
   (setf (gethash 'L table) "+")
   (setf (gethash 'M table) "+")
   (setf (gethash 'N table) "+")
   (setf (gethash 'O table) "+")
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
    

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
(setf reverse-initial-spot (random 15))

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

;TASK 4--------------------------------------------------------------------

;Node class
(defclass node ()
   (
      (configuration :accessor node-configuration :initarg :configuration)
      (edges :accessor node-edges :initarg :edges :initform '())
   )
)

;Edge class
(defclass edge ()
   (
      (toNode :accessor edge-toNode :initarg :toNode)
      (fromNode :accessor edge-fromNode :initarg :fromNode)
      (action :accessor edge-action :initarg :action)
      (weight :accessor edge-weight :initarg :weight :initform 15)
   )
)

;Creates an edge
(defmethod generate-edge ((toNode node) (fromNode node) action &aux return-edge)
   (setf return-edge (make-instance 'edge))
   (setf (edge-toNode return-edge) toNode)
   (setf (edge-fromNode return-edge) fromNode)
   (setf (edge-action return-edge) action)
   return-edge
)
;Creates a node
(defmethod generate-node (config &aux return-node)

   (setf new-table (MAKE-HASH-TABLE))
   (dotimes (i 15)
      (setf (gethash (nth i positions) new-table) (gethash (nth i positions) config))
   )

   (setf return-node (make-instance 'node))
   (setf (node-configuration return-node) new-table)
   return-node
)

;Adds edge to Node
(defmethod add-edge ((selected-node node) (selected-edge edge))
   (setf (node-edges selected-node) (cons selected-edge (node-edges selected-node)))
   selected-node
)

(defun play-with-memory (&aux board moves)
   (format t "WELCOME TO PEG SOLITAIRE~%")
   (format t "Use the reference board below to make moves~%")
   (format t "Enter 'quit' to exit the game~%")
   (display-reference-board)
   (setf board (make-instance 'board))
   (setf nodes '())
   (setf nodes (snoc (generate-node (board-setup board)) nodes))
   (setf edges '())
   (run-with-memory board nodes edges)
)

;Plays the game while generating nodes and edges
(defmethod run-with-memory ((b board) nodes edges &aux moves choice initial-node)
   (setf moves (generate-available-moves b))
   (cond
      ((null moves)
         (format t "Finished!~%")
         (format t "Final board:~%")
         (display b)
         (print-list nodes)
         (print-list edges)
      )
      (t
         (display b)
         (terpri)
         (terpri)
         (display-moves b)
         (setf choice (read))
         (if (equal choice 'quit)
            (format t "Thanks for playing!")
            (progn
               (setf selected-move (nth choice moves))
               (make-move b (car selected-move) (rac selected-move))
               (setf new-node (generate-node (board-setup b)))
               (setf new-edge (generate-edge new-node (nth (- (length nodes) 1) nodes) selected-move))
               (setf new-node (add-edge new-node new-edge))
               (setf edges (snoc new-edge edges))
               (setf nodes (snoc new-node nodes))
               (run-with-memory b nodes edges)
            )
         )
      )
   )
)

(defmethod print-list (lst)
   (dotimes (i (length lst))
      (print (nth i lst))
      (terpri)
   )
)

;TASK 5---------------------------------------------------------------------------------

(setf nodes '())
(setf edges '())

(defun auto-play (&aux board moves)
   (setf board (make-instance 'board))
   (setf nodes (snoc (generate-node (board-setup board)) nodes))
   (auto-run board)
)

;has computer randomly play game
(defmethod auto-run ((b board) &aux moves choice new-node new-edge)
   (setf moves (generate-available-moves b))
   (cond
      ((null moves)
         (format t "Final board:~%")
         (display b)
         (terpri)
         (format t "Final Peg Count: ~A" (final-peg-count b))
         (terpri)
      )
      (t
         (display b)
         (terpri)
         (terpri)
         (display-moves b)
         (setf choice (random (length moves)))
         (format t "Selected Move: ~A" choice)
         (terpri) 
         (setf selected-move (nth choice moves))
         (make-move b (car selected-move) (rac selected-move))
         (setf new-node (generate-node (board-setup b)))
         (setf new-edge (generate-edge new-node (nth (- (length nodes) 1)  nodes) selected-move))
         (setf (nth (- (length nodes) 1) nodes) (add-edge (nth (- (length nodes) 1) nodes) new-edge))
         (setf edges (snoc new-edge edges))
         (setf nodes (snoc new-node nodes))
         (auto-run b)
      )
   )
)

(defmethod final-peg-count ((b board) &aux count)
   (setf count 0)
   (dotimes (i 15)
      (if(equal (gethash (nth i positions) (board-setup b)) "+")
         (setf count (+ count 1))
      )
   )
   count
)


;TASK6-------------------------------------------------------
;reverse play
;This task is meant to create a machine that plays the game in 
;reverse order to begin generating the graph from the completed
;board.

(defclass reverse-board ()
   (
      (style :accessor board-style :initarg :style :initform style)
      (setup :accessor board-setup :initarg :setup :initform (reverse-triangular-board))
   )
)


(defmethod reverse-triangular-board ()

   (setf table (MAKE-HASH-TABLE))
   (dotimes (i 15)
      (setf (gethash (nth i positions) table) "o")
      (if (= reverse-initial-spot i)
         (setf (gethash (nth i positions) table) "+")
      )
   )
   table
)

(defun reverse-display-moves ((b reverse-board))
   (setf moves (reverse-generate-available-moves b))
   (setf counter 0)
   (dotimes (i (length moves))
      (format t "~A: " counter)
      (format t "Move ~A" (car (nth i moves)))
      (format t " to ~A" (rac (nth i moves)))
      (setf counter (+ counter 1))
      (terpri)
   )
)

(defmethod reverse-generate-available-moves ((b reverse-board) &aux to-slots)
   (setf moves '())
   (dotimes (open-num 15)
      (if (equal (gethash (nth open-num positions) (board-setup b)) "+")
         (progn
            (setf select-peg (nth open-num positions))
            (setf to-slots (possible-move select-peg))
            (dotimes (closed-num (length to-slots))
               (setf to-slot (nth closed-num to-slots))
               (setf between-slot (between (list select-peg to-slot)))
               (if (and (equal (gethash to-slot (board-setup b)) "o") (equal (gethash between-slot (board-setup b)) "o"))
                  (setf moves (cons (list select-peg to-slot) moves))
               )
            )
         )
      )
   )
   moves
)



(defmethod make-reverse-move ((b reverse-board) select-peg to-slot)
    (if (and (equal (gethash select-peg (board-setup b)) "+") (equal (gethash to-slot (board-setup b)) "o"))
       (progn
           (cond
               ((and (member to-slot (possible-move select-peg)) (equal (gethash (between (list select-peg to-slot)) (board-setup b)) "o"))
                 (setf (gethash (between (list select-peg to-slot)) (board-setup b)) "+")
                 (setf (gethash to-slot (board-setup b)) "+")
                 (setf (gethash select-peg (board-setup b)) "o")
               )
           )
       )
    )
)
 
(defun reverse-auto-play (&aux board moves)
   (setf board (make-instance 'reverse-board))
   (reverse-auto-run board)
)

;has computer randomly play game in reverse
(defmethod reverse-auto-run ((b reverse-board) &aux moves choice new-node new-edge)
   (setf moves (reverse-generate-available-moves b))
   (cond
      ((null moves)
         (format t "Final board:~%")
         (display b)
         (terpri)
      )
      (t
         (display b)
         (terpri)
         (terpri)
         (reverse-display-moves b)
         (setf choice (random (length moves)))
         (format t "Selected Move: ~A" choice)
         (terpri)
         (setf selected-move (nth choice moves))
         (make-reverse-move b (car selected-move) (rac selected-move))
         (reverse-auto-run b)
      )
   )
)

;TASK7-------------------------------------------------------

;Testing methods 
(defmethod display-graph-demo()
   (cond
      ((null nodes)
         (format t "Graph is empty")
         (terpri)
      )
      (t
         (dotimes (i (length nodes))
            (terpri)
            (setf select-node (nth i nodes))
            (format t "Node : ~A" (node-configuration select-node))
            (terpri)
            (setf node-list (node-edges select-node))
            (dotimes (e (length node-list))
               (setf select-edge (nth e (node-edges select-node)))
               (format t "Edges : ~A, Weight : ~A" (edge-action select-edge) (edge-weight select-edge))
               (terpri)
            )
         )
      )
   )
)

(defmethod trace-through-demo()
   (setf node (car nodes))
   (cond
      ((null edges)
         (format t "empty nodes")
      )
      (t
         (dotimes (i (length nodes))
            (format t "Node : ~A -> " (node-configuration node)) 
            (if (not (null (car (node-edges node))))
               (format t "Edge : ~A -> " (edge-action (car (node-edges node))))
            )
            (terpri)
            (if (not (null (car (node-edges node))))
               (setf node (edge-toNode (car (node-edges node))))
            )
         )
      )
   )   
)
(defmethod auto-play-trials (&aux board reverse-board initial-node reverse-initial-node)
   (setf board (make-instance 'board))
   (setf reverse-board (make-instance 'reverse-board))
   (setf initial-node (generate-node (board-setup board)))
   (setf reverse-initial-node (generate-node (board-setup reverse-board)))

   (cond
      ((equal (existing-node initial-node) 'false)
         (setf nodes (snoc initial-node nodes))
      )
      (t
         (setf initial-node (existing-node initial-node))
      )
   )

   (cond
      ((equal (existing-node reverse-initial-node) 'false)
         (setf nodes (snoc reverse-initial-node nodes))
      )
      (t
         (setf reverse-initial-node (existing-node reverse-initial-node))
      )
   )

   (auto-run-trials board initial-node)
   (reverse-auto-run-trials reverse-board reverse-initial-node)
)

;has computer randomly play game and generate graph of moves
(defmethod auto-run-trials ((b board) (before-node node) &aux moves choice new-node new-edge update-edges)
   (setf moves (generate-available-moves b))
   (cond
      ((null moves)
         (format t "Final board:~%")
         (display b)
         (terpri)
         (format t "Final Peg Count: ~A" (final-peg-count b))
         (terpri)
         (update-weights (final-peg-count b))
      )
      (t
         (display b)
         (terpri)
         (terpri)
         (display-moves b)
         (setf choice (random (length moves)))
         (format t "Selected Move: ~A" choice)
         (terpri)
         (setf selected-move (nth choice moves))
         (make-move b (car selected-move) (rac selected-move))
         (setf new-node (generate-node (board-setup b)))
         (cond 
            ((equal (existing-node new-node) 'false)
               (setf new-edge (generate-edge new-node before-node selected-move))
               (setf nodes (snoc new-node nodes))
               (setf before-node (add-edge before-node new-edge))
               (setf edges (snoc new-edge edges))
               (setf *update-edges* (snoc new-edge *update-edges*))
            )
            (t
                (setf new-node (existing-node new-node))
                (setf new-edge (generate-edge new-node before-node selected-move))
                (if (equal (existing-edge new-edge) 'false)
                   (progn
                      (setf before-node (add-edge before-node new-edge))
                      (setf edges (snoc new-edge edges))
                   )
                )
                (setf *update-edges* (snoc new-edge *update-edges*))
            )
         )
         (auto-run-trials b new-node)
      )
   )
)


;has computer randomly play game and generate graph of moves in reverse
(defmethod reverse-auto-run-trials ((b reverse-board) (before-node node) &aux moves choice new-node new-edge)
   (setf moves (reverse-generate-available-moves b))
   (cond
      ((null moves)
         (format t "Final board:~%")
         (display b)
         (terpri)
         (update-weights 1)
      )
      (t
         (display b)
         (terpri)
         (terpri)
         (reverse-display-moves b)
         (setf choice (random (length moves)))
         (format t "Selected Move: ~A" choice)
         (terpri)
         (setf selected-move (nth choice moves))
         (make-reverse-move b (car selected-move) (rac selected-move))
         (setf new-node (generate-node (board-setup b)))
         (cond 
            ((equal (existing-node new-node) 'false)
               (setf new-edge (generate-edge before-node new-node (flip-list selected-move)))
               (setf nodes (snoc new-node nodes))
               (setf new-node (add-edge new-node new-edge))
               (setf edges (snoc new-edge edges))
            )
            (t
                (setf new-node (existing-node new-node))
                (setf new-edge (generate-edge before-node new-node (flip-list selected-move)))
                (if (equal (existing-edge new-edge) 'false)
                   (progn
                      (setf new-node (add-edge new-node new-edge))
                      (setf edges (snoc new-edge edges))
                   )
                )
            )
         )
         (setf *update-edges* (snoc new-edge *update-edges*))
         (reverse-auto-run-trials b new-node)
      )
   )
)


;determines if node is already in graph
(defmethod existing-node ((n node) &aux return)
   (setf return 'false)
   (dotimes (i (length nodes))
      (if (hash-equal (node-configuration n) (node-configuration (nth i nodes)))
         (setf return (nth i nodes))
      )
   )
   return
)

(defmethod existing-edge ((e edge) &aux return)
   (setf return 'false)
   (dotimes (i (length edges))
      (if (and (equal (edge-action e) (edge-action (nth i edges))) (hash-equal (node-configuration (edge-fromNode e)) (node-configuration (edge-fromNode (nth i edges)))))
         (setf return (nth i edges))
      )
   )
   return
)

(defmethod flip-list (lst &aux return-lst)
   (setf return-lst '())
   (setf return-lst (snoc (rac lst) return-lst))
   (setf return-lst (snoc (car lst) return-lst))
   return-lst 
)

(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x hash2) y)
                                   (throw 'flag nil)))
                             hash1)
              (throw 'flag t)
       )
   )
)

;TASK 8---------------------------------------------------------------

(setf *update-edges* '())

;method to change weights of edges, if the final score is less than the previous weight
(defmethod update-weights (final-score)
   (dotimes (edge (length *update-edges*))
      (if (> (edge-weight (nth edge *update-edges*)) final-score)
         (setf (edge-weight (existing-edge (nth edge *update-edges*))) final-score)
      )
   )
   (setf *update-edges* '())
)

;TASK 9----------------------------------------------------------------

;method to check for similar boards of different configurations
(defmethod left-check-rotation ((b board) &aux return)
   (setf return 'false)
   (setf left-board (left-rotate-triangle b))
   (setf left-rotate-node (generate-node (board-setup left-board)))
   (if (not (equal (existing-node left-rotate-node) 'false))
      (setf return (existing-node left-rotate-node))
   )  
   return
)

(defmethod right-check-rotation ((b board) &aux return)
   (setf return 'false)
   (setf right-board (right-rotate-triangle b))
   (setf right-rotate-node (generate-node (board-setup right-board)))

   (if (not (equal (existing-node right-rotate-node) 'false))
      (setf return (existing-node right-rotate-node))
   )
   return
)


(defmethod reverse-left-check-rotation ((b reverse-board) &aux return)
   (setf return 'false)
   (setf left-board (reverse-left-rotate-triangle b))
   (setf left-rotate-node (generate-node (board-setup left-board)))

   (if (not (equal (existing-node left-rotate-node) 'false))
      (setf return (existing-node left-rotate-node))
   )
   return
)

(defmethod reverse-right-check-rotation ((b reverse-board) &aux return)
   (setf return 'false)
   (setf right-board (reverse-right-rotate-triangle b))
   (setf right-rotate-node (generate-node (board-setup right-board)))

   (if (not (equal (existing-node right-rotate-node) 'false))
      (setf return (existing-node right-rotate-node))
   )
   return
)


(setf *rotations* nil)

(defun create-rotations-triangle()
   (setf *rotations* (MAKE-HASH-TABLE))
   (setf (gethash 'A *rotations*) '(K O))
   (setf (gethash 'B *rotations*) '(L J))
   (setf (gethash 'C *rotations*) '(G N))
   (setf (gethash 'D *rotations*) '(M F))
   (setf (gethash 'E *rotations*) '(H I))
   (setf (gethash 'F *rotations*) '(D M))
   (setf (gethash 'G *rotations*) '(N C))
   (setf (gethash 'H *rotations*) '(I E))
   (setf (gethash 'I *rotations*) '(E H))
   (setf (gethash 'J *rotations*) '(B L))
   (setf (gethash 'K *rotations*) '(O A))
   (setf (gethash 'L *rotations*) '(J B))
   (setf (gethash 'M *rotations*) '(F D))
   (setf (gethash 'N *rotations*) '(C G))
   (setf (gethash 'O *rotations*) '(A K))
)


(defmethod convert-move ((e edge) rotation &aux return-move)
   (setf return-move '())
   (if (equal rotation 'left)
      (progn
         (setf return-move (snoc (car (gethash (car (edge-action e)) *rotations*)) return-move))
         (setf return-move (snoc (car (gethash (rac (edge-action e)) *rotations*)) return-move))
      )
   )
   (if (equal rotation 'right)
      (progn
         (setf return-move (snoc (rac (gethash (car (edge-action e)) *rotations*)) return-move))
         (setf return-move (snoc (rac (gethash (rac (edge-action e)) *rotations*)) return-move))
      )
   )
   return-move
)

(defmethod left-rotate-triangle ((b board) &aux return-board)
  (setf return-board (make-instance 'board))
   (dotimes (i 15)
      (setf (gethash (nth i positions) (board-setup return-board)) (gethash (car (gethash (nth i positions) *rotations*)) (board-setup b)))
   )
   return-board
)

(defmethod right-rotate-triangle ((b board) &aux return-board)
   (setf return-board (make-instance 'board))
   (dotimes (i 15)
      (setf (gethash (nth i positions) (board-setup return-board)) (gethash (rac (gethash (nth i positions) *rotations*)) (board-setup b)))
   )
   return-board
)

(defmethod reverse-left-rotate-triangle ((b reverse-board) &aux return-board)
  (setf return-board (make-instance 'reverse-board))
   (dotimes (i 15)
      (setf (gethash (nth i positions) (board-setup return-board)) (gethash (car (gethash (nth i positions) *rotations*)) (board-setup b)))
   )
   return-board
)

(defmethod reverse-right-rotate-triangle ((b reverse-board) &aux return-board)
   (setf return-board (make-instance 'reverse-board))
   (dotimes (i 15)
      (setf (gethash (nth i positions) (board-setup return-board)) (gethash (rac (gethash (nth i positions) *rotations*)) (board-setup b)))
   )
   return-board
)


;TASK 10-----------------------------------------------------------------

(defmethod auto-play-trials-final (&aux board reverse-board initial-node reverse-initial-node)
   (create-rotations-triangle)
   (setf board (make-instance 'board))
   (setf reverse-board (make-instance 'reverse-board))
   (setf initial-node (generate-node (board-setup board)))
   (setf reverse-initial-node (generate-node (board-setup reverse-board)))

   (cond
      ((and (and (equal (existing-node initial-node) 'false) (equal (right-check-rotation board) 'false)) (equal (left-check-rotation board) 'false))
         (setf nodes (snoc initial-node nodes))
      )
      ((not (equal (existing-node initial-node) 'false))
         (setf initial-node (existing-node initial-node))
      )
   )

   (cond
      ((and (and (equal (existing-node reverse-initial-node) 'false) (equal (reverse-right-check-rotation reverse-board) 'false)) (equal (reverse-left-check-rotation reverse-board) 'false))
         (setf nodes (snoc reverse-initial-node nodes))
      )
      ((not (equal (existing-node reverse-initial-node) 'false))
         (setf reverse-initial-node (existing-node reverse-initial-node))
      )
   )

   (auto-run-trials-final board initial-node)
   (reverse-auto-run-trials-final reverse-board reverse-initial-node)
)

(setf *best-move* '())

;has computer randomly play game and generate graph of moves
(defmethod auto-run-trials-final ((b board) (before-node node) &aux moves choice new-node new-edge update-edges)
   (setf moves (generate-available-moves b))
   (setf before-board (make-instance 'board))
   (setf (board-setup before-board) (node-configuration before-node))
   (cond
      ((null moves)
         ;(format t "Final board:~%")
         ;(display b)
         ;(terpri)
         ;(format t "Final Peg Count: ~A" (final-peg-count b))
         ;(terpri)
         (update-weights (final-peg-count b))
      )
      (t
         ;(display b)
         ;(terpri)
         ;(terpri)
         ;(display-moves b)
         (setf choice (random (length moves)))
         ;(format t "Selected Move: ~A" choice)
         ;(terpri)
         (setf selected-move (nth choice moves))
         (if (not (null *best-move*))
            (setf selected-move *best-move*)
         )
         (make-move b (car selected-move) (rac selected-move))
         (setf *best-move* '())
         (setf new-node (generate-node (board-setup b)))
         (if (not (equal (existing-node new-node) 'false))
            (progn
               (if (not (null (best-move (existing-node new-node)))) 
                  (progn
                     (format t "~A" (edge-weight (best-move (existing-node new-node))))
                     (if (equal (edge-weight (best-move (existing-node new-node))) 1)
                        (progn
                           (setf *best-move* (edge-action (best-move (existing-node new-node))))
                           (format t "GOT IT")
                        )
                     )
                  )
               )
            )
         )
         (cond 
            ((not (equal (left-check-rotation before-board) 'false))
               (setf new-edge (generate-edge new-node before-node selected-move))
               (setf left-change-move (convert-move new-edge 'right))
               (if (equal (left-check-rotation b) 'false)       
                   (progn
                      (setf left-rotated-node (generate-node (board-setup (left-rotate-triangle b))))
                      (setf rotated-edge (generate-edge left-rotated-node (left-check-rotation before-board) left-change-move))
                      (setf (node-edges (left-check-rotation before-board)) (snoc rotated-edge (node-edges (left-check-rotation before-board))))
                      (setf nodes (snoc left-rotated-node nodes))
                      (setf *update-edges* (snoc rotated-edge *update-edges*))
                      (setf edges (snoc rotated-edge edges))
                   )
               )
            )
            ((not (equal (right-check-rotation before-board) 'false))
               (setf new-edge (generate-edge new-node before-node selected-move))
               (setf right-change-move (convert-move new-edge 'left))
               (if (equal (right-check-rotation b) 'false)
                   (progn
                      (setf right-rotated-node (generate-node (board-setup (right-rotate-triangle b))))
                      (setf rotated-edge (generate-edge right-rotated-node (right-check-rotation before-board) right-change-move))
                      (setf (node-edges (right-check-rotation before-board)) (snoc rotated-edge (node-edges (right-check-rotation before-board))))
                      (setf nodes (snoc right-rotated-node nodes))
                      (setf *update-edges* (snoc rotated-edge *update-edges*))
                      (setf edges (snoc rotated-edge edges))
                   )
               )
            )
            ((equal (existing-node new-node) 'false)
               (setf new-edge (generate-edge new-node before-node selected-move))
               (setf nodes (snoc new-node nodes))
               (setf before-node (add-edge before-node new-edge))
               (setf edges (snoc new-edge edges))
            )
            (t
               (setf new-node (existing-node new-node))
               (setf new-edge (generate-edge new-node before-node selected-move))
               (if (equal (existing-edge new-edge) 'false)
                  (progn
                     (setf before-node (add-edge before-node new-edge))
                     (setf edges (snoc new-edge edges))
                  )
               )
            )
         )
         (setf *update-edges* (snoc (existing-edge new-edge) *update-edges*))
         (auto-run-trials-final b new-node)
      )
   )
)


;has computer randomly play game and generate graph of moves in reverse
(defmethod reverse-auto-run-trials-final ((b reverse-board) (before-node node) &aux moves choice new-node new-edge)
   (setf moves (reverse-generate-available-moves b))
   (setf before-board (make-instance 'reverse-board))
   (setf (board-setup before-board) (node-configuration before-node))
   (cond
      ((null moves)
         ;(format t "Final board:~%")
         ;(display b)
         ;(terpri)
         (update-weights 1)
      )
      (t
         ;(display b)
         ;(terpri)
         ;(terpri)
         ;(reverse-display-moves b)
         (setf choice (random (length moves)))
         ;(format t "Selected Move: ~A" choice)
         ;(terpri)
         (setf selected-move (nth choice moves))
         (make-reverse-move b (car selected-move) (rac selected-move))
         (setf new-node (generate-node (board-setup b)))
         (cond
            ((not (equal (reverse-left-check-rotation before-board) 'false))
               (setf new-edge (generate-edge before-node new-node selected-move))
               (setf left-change-move (convert-move new-edge 'right))
               (if (equal (reverse-left-check-rotation b) 'false)
                   (progn
                      (setf left-rotated-node (generate-node (board-setup (reverse-left-rotate-triangle b))))
                      (setf rotated-edge (generate-edge (reverse-left-check-rotation before-board) left-rotated-node (flip-list left-change-move)))
                      (setf (node-edges left-rotated-node) (snoc rotated-edge (node-edges left-rotated-node)))
                      (setf nodes (snoc left-rotated-node nodes))
                      (setf *update-edges* (snoc rotated-edge *update-edges*))
                      (setf edges (snoc rotated-edge edges))
                   )
               )
            )
            ((not (equal (reverse-right-check-rotation before-board) 'false))
               (setf new-edge (generate-edge before-node new-node selected-move))
               (setf right-change-move (convert-move new-edge 'left))
               (if (equal (reverse-right-check-rotation b) 'false)
                   (progn
                      (setf right-rotated-node (generate-node (board-setup (reverse-right-rotate-triangle b))))
                      (setf rotated-edge (generate-edge (reverse-right-check-rotation before-board) right-rotated-node (flip-list right-change-move)))
                      (setf (node-edges right-rotated-node) (snoc rotated-edge (node-edges right-rotated-node)))
                      (setf nodes (snoc right-rotated-node nodes))
                      (setf *update-edges* (snoc rotated-edge *update-edges*))
                      (setf edges (snoc rotated-edge edges))
                   )
               )
            ) 
            ((equal (existing-node new-node) 'false)
               (setf new-edge (generate-edge before-node new-node (flip-list selected-move)))
               (setf nodes (snoc new-node nodes))
               (setf new-node (add-edge new-node new-edge))
               (setf edges (snoc new-edge edges))
            )
            (t
                (setf new-node (existing-node new-node))
                (setf new-edge (generate-edge before-node new-node (flip-list selected-move)))
                (if (equal (existing-edge new-edge) 'false)
                   (progn
                      (setf new-node (add-edge new-node new-edge))
                      (setf edges (snoc new-edge edges))
                   )
                )
            )
         )
         (setf *update-edges* (snoc new-edge *update-edges*))
         (reverse-auto-run-trials-final b new-node)
      )
   )
)


(defmethod run-trials (times initial-spot reverse-spot)
   (setf random-initial-spot initial-spot)
   (setf reverse-initial-spot reverse-spot)
   (dotimes (i times)
      (auto-play-trials-final)
   )
)

(defmethod best-move ((n node) &aux return-edge)
   (setf return-edge (car (node-edges n)))
   (dotimes (i (length (node-edges n)))
      (if (< (edge-weight (nth i (node-edges n))) (edge-weight return-edge))
         (setf return-edge (nth i (node-edges n)))
      )   
   )
   return-edge
)

(defmethod find-solution (initial-spot &aux node found-node)
   (setf random-initial-spot initial-spot)
   (setf board (make-instance 'board))
   (setf node (generate-node (board-setup board)))
   (setf found-node (existing-node node))
   (display board)
   (if (not (equal found-node 'false))
      (display-path found-node board)
   )
)

(defmethod display-path ((n node) (b board) &aux best-edge next-node)
   (setf best-edge (best-move n))
   (cond
      ((null best-edge)
         (terpri)
         (format t "complete")
      )
      (t
         (format t "~A" (edge-action best-edge))
         (terpri)
         (make-move b (car (edge-action best-edge)) (rac (edge-action best-edge)))
         (display b)
         (setf next-node (edge-toNode best-edge))
         (display-path next-node b)
      )
   )
)

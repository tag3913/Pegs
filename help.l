
(defmethod snoc (elem lst)
   (cond
      ((null lst)
         (list elem)
      )
      (t
         (cons (car lst) (snoc elem (cdr lst)))
      )
   )
)

(defmethod select (lst)
   (nth (random (length lst)) lst)
)

(defmethod even (x)
   (= (rem x 2) 0)
)

(defmethod sublist (lst1 lst2)
   (cond
      ((null lst2)
         t
      )
      ((find (car lst2) lst1)
         (sublist lst1 (cdr lst2))
      )
      (t
         nil
      )
   )
)

(defun rac (lst)
   (cond
      ((singleton-p lst)
         (car lst)
      )
      (T
         (rac(cdr lst))
      )
   )
)

(defun singleton-p (lst)
   (cond
      ((null lst)
        NIL
      )
      ((null(cdr lst))
        T
      )
      (T
        NIL
      )
   )
)

(defun farthest (lst1 lst2 &aux value)
   (setf value 0)
   (dotimes (i (length lst2))
      (if (> (position (nth i lst2) lst1) value)
         (setf value (position (nth i lst2) lst1))
      )
   )
   value
)


(defun includes (liste1 liste2) 
   (cond 
      ((null liste1) 
         t
      ) 
      ((member (car liste1) liste2)
          (includes (cdr liste1) liste2)
      ) 
      (t 
         nil
      )
   )
)          

(defun compare (lst1 lst2)
   (if (and (includes lst1 lst2) (includes lst2 lst1))
      t
      nil
   )
)

(defun contains (lst1 lst2)             
   (cond 
      ((null lst2)
         nil
      )
      ((equal lst1 (car lst2))
         t
      )
      (t
         (contains lst1 (cdr lst2))
      )
   )
)

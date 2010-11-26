(defun group (lst num)
  (let ((out) (group))
    (dotimes (i (length lst) out)
      (push-to-end (nth i lst) group)
      (when (eq (+ 1 (mod i num)) num)
	(push-to-end group out)
	(setf group nil)))))
      
(defmacro make-board (&rest args)
  (if (not args) 
      `(make-array '(6 7) :initial-element nil)
      `(make-array '(6 7) :initial-contents (reverse (group (list ,@(mapcar (lambda (x) `',x) args)) 7)))))
		   
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

(defun print-board (board)
  (format t "~{~8A~}~%" (list 0 1 2 3 4 5 6))
  (dotimes (i (array-dimension board 0))
    (dotimes (j (array-dimension board 1))
      (format t "~8A" (aref board (- (array-dimension board 0) i 1) j)))
    (format t "~%")))

(defun get-row (board row)
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (dotimes (i (array-dimension board 1) out)
      (setf (aref out i) (aref board row i)))))

(defun get-col (board col)
  (let ((out (make-array (array-dimension board 0) :initial-element nil)))
    (dotimes (i (array-dimension board 0) out)
      (setf (aref out i) (aref board i col)))))
    
(defun next (arr)
  (dotimes (i (array-dimension arr 0) nil)
    (if (not (aref arr i))
	(return-from next i))))

(defun place (board col chip &optional (verbose t))
  (awhen (next (get-col board col))
    (setf (aref board it col) chip)
    (if verbose
	(print-board board))))

(defun find4-hor (board chip)
  (let ((out 0))
    (dotimes (row (array-dimension board 0) out)
      (dotimes (col (- (array-dimension board 1) 3))
	(if (and (eq (aref board row col)
		     (aref board row (+ col 1)))
		 (eq (aref board row (+ col 2))
		     (aref board row (+ col 3)))
		 (eq (aref board row col)
		     (aref board row (+ col 3)))
		 (eq (aref board row col)
		     chip))
	    (incf out))))))

(defun find4-ver (board chip)
  (let ((out 0))
    (dotimes (col (array-dimension board 1) out)
      (dotimes (row (- (array-dimension board 0) 3))
	(if (and (eq (aref board row col)
		     (aref board (+ row 1) col))
		 (eq (aref board (+ row 2) col)
		     (aref board (+ row 3) col))
		 (eq (aref board row col)
		     (aref board (+ row 3) col))
		 (eq (aref board row col)
		     chip))
	    (incf out))))))

(defun find4-dia-right (board chip)
  (let ((out 0))
    (dotimes (col (- (array-dimension board 1) 3) out)
      (dotimes (row (- (array-dimension board 0) 3))
	(if (and (eq (aref board row col)
		     (aref board (+ row 1) (+ col 1)))
		 (eq (aref board (+ row 2) (+ col 2))
		     (aref board (+ row 3) (+ col 3)))
		 (eq (aref board row col)
		     (aref board (+ row 3) (+ col 3)))
		 (eq (aref board row col)
		     chip))
	    (incf out))))))

(defun find4-dia-left (board chip)
  (let ((out 0))
    (dotimes (col (- (array-dimension board 1) 3) out)
      (dotimes (row (- (array-dimension board 0) 3))
	(let ((col* (- (array-dimension board 1) col 1)))
	  (if (and (eq (aref board row col*)
		       (aref board (+ row 1) (- col* 1)))
		   (eq (aref board (+ row 2) (- col* 2))
		       (aref board (+ row 3) (- col* 3)))
		   (eq (aref board row col*)
		       (aref board (+ row 3) (- col* 3)))
		   (eq (aref board row col*)
		       chip))
	      (incf out)))))))

(defun find4-dia (board chip)
  (+ (find4-dia-right board chip)
     (find4-dia-left board chip)))

(defun find4 (board chip)
  (let ((sum
	 (+ (find4-hor board chip)
	    (find4-ver board chip)
	    (find4-dia board chip))))
    (if (> sum 0)
	sum)))

(defun find3 (board chip)
  (let ((out (make-array (array-dimension board 1) :initial-element 0)))
    (dotimes (i (array-dimension board 1) out)
      (let ((board (copy-array board)))
	(place board i chip nil)
	(aif (find4 board chip)
	     (setf (aref out i) it))))))

(defun find2 (board chip)
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (dotimes (i (array-dimension board 1) out)
      (let ((board (copy-array board)))
	(place board i chip nil)
	(setf (aref out i) (reduce #'+ (find3 board chip)))))))

(defvar *b1*)
(setf *b1* (make-board
	    nil nil nil nil nil nil nil
	    nil nil nil nil nil nil nil
	    nil nil nil nil nil nil nil
	    nil nil nil nil nil nil red
	    nil nil nil nil nil nil red
	    red nil nil red red red red))
(assert (eq (find4 *b1* 'red) 1))
(assert (equalp (find3 *b1* 'red) #(1 1 2 1 1 1 2)))

(defvar *b2*)
(setf *b2* (make-board
	    nil nil nil  nil  nil nil nil
	    nil nil nil  nil  nil nil nil
	    nil nil nil  nil  nil nil nil
	    nil nil nil  nil  blue nil nil
	    nil nil red  blue blue nil nil
	    nil red blue blue blue nil nil))
(assert (not (find4 *b2* 'red)))
(assert (equalp (find3 *b2* 'red) #(0 0 0 0 0 0 0)))
(assert (equalp (find2 *b2* 'red) #(0 0 0 1 1 0 0)))
(assert (not (find4 *b2* 'blue)))
(assert (equalp (find3 *b2* 'blue) #(0 0 0 0 1 1 0)))

(defvar *b3*)
(setf *b3* (make-board
	    nil nil nil nil nil nil nil
	    nil nil nil nil nil nil nil
	    red nil nil blu nil nil nil
	    nil red blu blu nil nil nil
	    nil blu red blu nil nil nil
	    blu blu blu red nil nil nil))
(assert (eq (find4 *b3* 'red) 1))
(assert (eq (find4 *b3* 'blu) 1))


	    
(defun board-rank (board good-chip bad-chip &optional (weight-win-loss))
  (let ((rank 0))
    ;if bad-chip wins, return loss
    (aif (find4 board bad-chip)
	 (if (not weight-win-loss)
	     (return-from board-rank 'loss)
	     (incf rank (* -200 it))))
    ;if good-chip wins, return win
    (aif (find4 board good-chip)
	 (if (not weight-win-loss)
	     (return-from board-rank 'win)
	     (incf rank (* 200 it))))
    ;otherwise, add all the goods, and subtract all the bads
    (incf rank
	  (+ (* (reduce #'+ (find3 board good-chip)) 20)
	     (* (reduce #'+ (find2 board good-chip)) 2)
	     (* (reduce #'+ (find3 board bad-chip)) -20)
	     (* (reduce #'+ (find2 board bad-chip)) -2)))))

(defun get-possible-moves (board)
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (dotimes (i (array-dimension board 1) out)
      (if (not (aref board (- (array-dimension board 0) 1) i))
	  (setf (aref out i) t)))))

(defmacro! do-possible-moves ((varsym board% &optional ret) &body body)
  `(let ,(remove-if-not #'consp (list ret))
     (let* ((,g!board ,board%)
	    (,g!moves (get-possible-moves ,g!board)))
       (dotimes (,varsym (array-dimension ,g!board 1) ,(if (consp ret) (car ret) ret))
	 (when (aref ,g!moves ,varsym)
	   ,@body)))))

(defun get-move (board good-chip bad-chip)
  ;check for win; if can win, then play there
  (do-possible-moves (i board)
    (let ((board (copy-array board)))
      (place board i good-chip nil)
      (if (eq 'win (board-rank board good-chip bad-chip))
	  (return-from get-move i))))
  ;block opponent if can lose
  (do-possible-moves (i board)
    (let ((board (copy-array board)))
      (place board i bad-chip nil)
      (if (eq 'loss (board-rank board good-chip bad-chip))
	  (return-from get-move i))))
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (do-possible-moves (i board)
      (let ((board (copy-array board)))
	(place board i good-chip nil)
	(setf (aref out i)
	      (do-possible-moves (i board (sum 0))
		(let ((board (copy-array board)))
		  (place board i bad-chip nil)
		  (incf sum
			(do-possible-moves (i board (sum 0))
			  (let ((board (copy-array board)))
			    (place board i good-chip nil)
			    (incf sum (board-rank board good-chip bad-chip t))))))))))
    (format t "board rank: ~a~%" out)
    (let ((move (cons nil -1)))
      (dotimes (i (array-dimension board 1) (cdr move))
	(awhen (aref out i)
	  (if (eq (cdr move) -1)
	      (setf move (cons it i))
	      (when (> it (car move))
		(setf move (cons it i)))))))))
  	   
(defvar *board* (make-array '(6 7) :initial-element nil))

(defmacro! with-time (time &body body)
  "executes body in 'time' seconds; time is intended to be longer than it should take to execute body"
  `(let* ((,g!start (get-internal-real-time))
	  (,g!finish (+ (* ,time internal-time-units-per-second) ,g!start))
	  (,g!time-to-sleep))
     ,@body
     (setf ,g!time-to-sleep (/ (- ,g!finish (get-internal-real-time)) internal-time-units-per-second))
     (if (< ,g!time-to-sleep 0)
	 (format t "lagging behind by ~a seconds~%" (coerce (abs ,g!time-to-sleep) 'double-float))
	 (sleep ,g!time-to-sleep))))

(defun connect4 ()
  "go for it.. connect4!"
  (while t
    (with-time (/ 1 60)
      (if (listen) (format t "~a~%" (attempt (eval (read))))))))




(defmacro! acond (&rest clauses)
 "works just like cond, but stores the 
  value of each condition as 'it', which is accessable in the code
  following the condition"
    (if clauses
	(let ((cl1 (car clauses)))
	  `(let ((,g!sym ,(car cl1)))
	     (if ,g!sym
		 (let ((it ,g!sym)) 
		   (declare (ignorable it)) 
		   ,@(cdr cl1))
		 (acond ,@(cdr clauses)))))))

;this macro has been tweaked from the one in server.lisp
(defmacro! with-time (time &body body)
  "executes body in 'time' seconds; time is intended to be longer than it should take to execute body"
  `(let* ((,g!start (get-internal-real-time))
	  (,g!finish (+ (* ,time internal-time-units-per-second) ,g!start))
	  (,g!time-to-sleep))
     ,@body
     (setf ,g!time-to-sleep (/ (- ,g!finish (get-internal-real-time)) internal-time-units-per-second))
     (if (< ,g!time-to-sleep 0)
	 (format nil "lagging behind by ~a seconds~%" (coerce (abs ,g!time-to-sleep) 'double-float))
	 (sleep ,g!time-to-sleep))))

(defmacro! return-time (&body body)
  `(let* ((,g!start (get-internal-real-time)))
     ,@body
     (/ (- (get-internal-real-time) ,g!start) internal-time-units-per-second)))

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
  (acond ((next (get-col board col))
	  (setf (aref board it col) chip)
	  (if verbose (print-board board))
	  t)
	 (t
	  nil)))

(defun attempt-place (board col chip &optional (verbose t))
  (attempt
   (progn
     (if (not (find col (list 0 1 2 3 4 5 6)))
	 (return-from attempt-place nil))
     (if (not (place board col chip verbose))
	 (return-from attempt-place nil))
     t)))

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
	    
(defpun toggle () ((chip))
  (format t "toggling from ~a~%" chip)
  (if (eq chip 'bad-chip)
      (setf chip 'good-chip)
      (setf chip 'bad-chip))
  chip)

(defmacro search-expander (depth)
  (if (eq depth 0)
      `(board-rank board good-chip bad-chip t)
      `(do-possible-moves (i board (sum 0))
	 (let ((board (copy-array board)))
	   (place board i ,(toggle) nil)
	   (incf sum
		 (cond
		   ((or (find4 board good-chip)
			(find4 board bad-chip))
		    (board-rank board good-chip bad-chip t))
		   (t
		    (search-expander ,(- depth 1)))))))))

(defmacro! get-move (o!board o!good-chip o!bad-chip)
  `(let ((move)
	 (cnt 0)
	 (maxTime (eval-object (get-matching-line (get-pandoric 'args 'configFileWdLST) "maxTime="))))
     (while (and
	     (fboundp (symb `get-move- cnt))
	     (< (return-time
		 (format t "searching at depth ~a~%" cnt)
		 (setf move (funcall (symb 'get-move- cnt) ,g!board ,g!good-chip ,g!bad-chip)))
	       maxTime))
       (incf cnt))
     move))
   
(defmacro get-move-builder (depth)
  `(defun ,(symb 'get-move- depth) (board good-chip bad-chip)
     ;check for win; if can win, then play there
     (do-possible-moves (i board)
       (let ((board (copy-array board)))
	 (place board i good-chip nil)
	 (if (eq 'win (board-rank board good-chip bad-chip))
	     (return-from ,(symb 'get-move- depth) i))))
     ;block opponent if can lose
     (do-possible-moves (i board)
       (let ((board (copy-array board)))
	 (place board i bad-chip nil)
	 (if (eq 'loss (board-rank board good-chip bad-chip))
	     (return-from ,(symb 'get-move- depth) i))))
     (let ((out (make-array (array-dimension board 1) :initial-element nil)))
       (do-possible-moves (i board)
	 (let ((board (copy-array board)))
	   (place board i good-chip nil)
	   (setf (aref out i)
		 (search-expander ,depth))))
       (format t "board rank: ~a~%" out)
       (let ((move (cons nil -1)))
	 (dotimes (i (array-dimension board 1) (cdr move))
	   (awhen (aref out i)
	     (if (eq (cdr move) -1)
		 (setf move (cons it i))
		 (when (> it (car move))
		   (setf move (cons it i))))))))))

(defmacro build-get-moves (depth)
  (format t "building at depth ~a~%" depth)
  (setf (get-pandoric 'toggle 'chip) 'good-chip)
  (if (eq depth 0)
      `(get-move-builder ,depth)
      `(progn
	 (get-move-builder ,depth)
	 (build-get-moves ,(- depth 1)))))

(build-get-moves 50)

(defun finished-p (board)
  (do-possible-moves (i board t)
    (return-from finished-p nil)))

(defmacro make-game (&optional (player1-human-player-p t) (player2-human-player-p t))
  `(let* ((board (make-array '(6 7) :initial-element  nil))
	  (player1 (make-player :human-player-p ,player1-human-player-p :chip 'red))
	  (player2 (make-player :human-player-p ,player2-human-player-p :chip 'yellow))
	  (current player1)
	  (toggle (lambda () (if (eq current player1) player2 player1))))
     (defpun game () (board player1 player2 current toggle)
       (cond ((finished-p board)
	      (quit))
	     ((find4 board (get-pandoric player1 'chip))
	      (format t "~a wins!~%" (get-pandoric player1 'chip))
	      (quit))
	     ((find4 board (get-pandoric player2 'chip))
	      (format t "~a wins!~%" (get-pandoric player2 'chip))
	      (quit))
	     (t
	      (funcall current)
	      (setf current (funcall toggle)))))))

(defmacro make-player (&key (human-player-p t) (chip))
  `(plambda () ((chip ,chip)
		(human-player-p ,human-player-p))
     (if human-player-p
	 (while (not (attempt-place
		      (get-pandoric 'game 'board)
		      (progn
			(format t "enter move for ~a:~%" chip)
			(attempt (eval (read))))
		      chip)))
	 (place (get-pandoric 'game 'board)
		(get-move board chip (get-pandoric (funcall (get-pandoric 'game 'toggle)) 'chip))
		chip))))

(defun connect4 ()
  "go for it.. connect4!"
  (make-game (progn
	       (format t "player1 human-player-p?~%")
	       (eval (read)))
	     (progn
	       (format t "player2 human-player-p?~%")
	       (eval (read))))
  (print-board (get-pandoric 'game 'board))
  (while t
    (with-time (/ 1 60)
      (funcall 'game))))

(defun compile-connect4 ()
  (compile-file "connect4.lisp" :output-file "connect4.fasl"))

#|(defmacro output-in-color (str color)
  `(progn
     ;(script "tput setaf 0; tput setab 7")
     (script (format nil "tput setaf ~a" ,color))
     (format t "~a~%" ,str)
     (script "tput op")))|#

 #| (script
   (format nil "tput setaf 0;tput setab 7; echo \"`tput setaf ~a` ~a `tput setaf 7`\"; tput op"
	   color
	   str)))|#


#|(defun connect4 ()
  (while t
    (format t "~a~%" (attempt (eval (read))))))|#



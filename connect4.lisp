(defun group (source n)
  "paul graham's group"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro! acond (&rest clauses)
  "paul graham's acond, and then tweaked a bit"
  (if clauses
      (let ((cl1 (car clauses)))
	`(let ((,g!sym ,(car cl1)))
	   (if ,g!sym
	       (let ((it ,g!sym)) 
		 (declare (ignorable it)) 
		 ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(defmacro! dotimes-reverse ((i n &optional result) &body body)
  "works like dotimes, except that i is bound in reverse (from highest to zero)"
  `(let ((,i)
	 (,g!n ,n))
     (dotimes (,g!iForward ,g!n ,result)
       (setf ,i (- ,g!n ,g!iForward 1))
       ,@body)))

(defun copy-array (array)
  "copies an array; found code in the wild; seems to work fine"
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

;///////////////////////////////////
;printing colored text to the terminal
(defun get-color (val)
  (cond ((eq val 'red) 1)
	((eq val 'yellow) 3)
	((eq val 'green) 2)
	(t 0)))

(defmacro in-color (str color)
  `(format nil "`tput setaf ~a`~a`tput setaf 7`" (get-color ,color) ,str))

(defun in-colors (words)
  (let ((out))
    (dolist (word words)
      (push-to-end (in-color (car word) (cdr word)) out))
    (make-sentence out :spaceDesignator "")))

(defun print-colored-line (words)
  "prints a line of colored words to the terminal
   each word is a dotted pair; car is the word (as a string); cdr is the color (as a symb)
   words is a list of word dotted pairs"
  (let ((lns (format nil 
		     (script
		      (fast-concatenate 
		       (format nil "echo \"~a\"; " (in-colors words))
		       "tput op")))))
    (dolist (ln (get-lines lns))
      (format t "~a " ln))
    (format t "~%")))
;end printing colored text to the terminal
;///////////////////////////////////

(defmacro! return-time (&body body)
  "evaluates body, and returns time (in seconds) it took to evaluate body"
  `(let ((,g!start (get-internal-real-time)))
     ,@body
     (/ (- (get-internal-real-time) ,g!start) internal-time-units-per-second)))

;this macro has been tweaked from the one in server.lisp
(defmacro! with-time (time &body body)
  "executes body in 'time' seconds; time is intended to be longer than it should take to execute body"
  `(let ((,g!time-to-sleep))
     (setf ,g!time-to-sleep (- ,time (return-time ,@body)))
     (if (< ,g!time-to-sleep 0)
	 (format nil "lagging behind by ~a seconds~%" (coerce (abs ,g!time-to-sleep) 'double-float))
	 (sleep ,g!time-to-sleep))))

(defmacro make-board (&rest args)
  "creates a connect4 board; creates a nil board if args is empty; otherwise populates board with args"
  (if (not args) 
      `(make-array '(6 7) :initial-element nil)
      `(make-array '(6 7) :initial-contents (reverse (group ',args 7)))))

(defun print-board (board)
  "prints board to terminal in color"
  (format t "~{~8A~}~%" (list 0 1 2 3 4 5 6))
  (dotimes-reverse (i (array-dimension board 0))
    (let ((line))
      (dotimes (j (array-dimension board 1))
	(push-to-end (cons
		      (format nil "~8A" (aref board i j))
		      (aref board i j))
		     line))
      (print-colored-line line))
    (format t "~%")))
		   
(defun get-row (board row)
  "gets fresh row slice of board"
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (dotimes (i (array-dimension board 1) out)
      (setf (aref out i) (aref board row i)))))

(defun get-col (board col)
  "gets fresh col slice of board"
  (let ((out (make-array (array-dimension board 0) :initial-element nil)))
    (dotimes (i (array-dimension board 0) out)
      (setf (aref out i) (aref board i col)))))
    
(defun next (arr)
  "gets row after highest chip on column arr"
  (dotimes (i (array-dimension arr 0) nil)
    (if (not (aref arr i))
	(return-from next i))))

(defun curr (arr)
  "gets row of highest chip on column arr"
  (dotimes-reverse (i (array-dimension arr 0) nil)
    (if (aref arr i)
	(return-from curr i))))

(defun place (board col chip &optional (verbose t))
  "places chip on column col on board"
  (awhen (next (get-col board col))
    (setf (aref board it col) chip)
    (if verbose (print-board board))
    t))

(defun attempt-place (board col chip &optional (verbose t))
  "attempts to place chip on board
   if an invalid move, board will not be changed and nil is returned"
  (attempt
   (progn
     (if (not (find col (list 0 1 2 3 4 5 6)))
	 (return-from attempt-place nil))
     (if (not (place board col chip verbose))
	 (return-from attempt-place nil))
     t)))

(defun unplace (board col &optional (chip) (verbose t))
  "takes off highest chip on column col on board"
  (awhen (curr (get-col board col))
    (if chip (assert (eq chip (aref board it col))))
    (setf (aref board it col) nil)
    (if verbose (print-board board))
    t))

(defmacro! with-placement ((chip col board) &body body)
  "body is evaluated in an environment where chip has been placed on col on board"
  `(let ((,g!chip ,chip)
	 (,g!col ,col))
     (place ,board ,g!col ,g!chip nil)
     ,@body
     (unplace ,board ,g!col ,g!chip nil)))

(defun get-possible-moves (board)
  (let ((out (make-array (array-dimension board 1) :initial-element nil)))
    (dotimes (i (array-dimension board 1) out)
      (if (not (aref board (- (array-dimension board 0) 1) i))
	  (setf (aref out i) t)))))

(defmacro! do-possible-moves ((varsym board &optional ret) &body body)
  "a constrained dotimes; will only evaluate body for i's that are columns on board that are possible moves"
  `(let ((,g!moves (get-possible-moves ,board)))
     (dotimes (,varsym (array-dimension ,board 1) ,ret)
       (when (aref ,g!moves ,varsym)
	 ,@body))))

(defmacro with-possible-placements ((chip varsym board &optional ret) &body body)
  "a macro combination of above macros
   for each possible move, evaluate body in an environment where that move is made 
   varsym is visible to body"
  `(do-possible-moves (,varsym ,board ,ret)
     (with-placement (,chip ,varsym ,board)
       ,@body)))

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
    (with-possible-placements (chip i board out)
      (aif (find4 board chip)
	   (setf (aref out i) it)))))

(defun find2 (board chip)
  (let ((out (make-array (array-dimension board 1) :initial-element 0)))
    (with-possible-placements (chip i board out)
      (setf (aref out i) (reduce #'+ (find3 board chip))))))
	    
(defun board-rank (board good-chip bad-chip &optional (depth 0) (weight-win-loss))
  (let ((rank 0))
    ;if bad-chip wins, return loss
    (awhen (find4 board bad-chip)
      (if (not weight-win-loss)
	  (return-from board-rank 'loss)
	  (incf rank (* -20000 it))))
    ;if good-chip wins, return win
    (awhen (find4 board good-chip)
      (if (not weight-win-loss)
	  (return-from board-rank 'win)
	  (incf rank (* 20000 it))))
    ;otherwise, add all the goods, and subtract all the bads
    (incf rank
	  (+ (* (reduce #'+ (find3 board good-chip)) 20)
	     (* (reduce #'+ (find2 board good-chip)) 2)
	     (* (reduce #'+ (find3 board bad-chip)) -20)
	     (* (reduce #'+ (find2 board bad-chip)) -2)))
    ;attentuate by the current depth of the board
    (/ rank (+ 1 depth))))

(defun toggle (chip good-chip bad-chip)
  ;(format t "toggling from ~a between ~a & ~a~%" chip good-chip bad-chip)
  (assert (or (eq chip good-chip) (eq chip bad-chip)))
  (assert (not (eq good-chip bad-chip)))
  (if (eq chip bad-chip)
      good-chip
      bad-chip))

(defmacro search-expander (chip depth maxDepth)
  "recursive macro that generates code that returns the rank of the board
   depth defines how many moves to look ahead when determining the quality of the board
   board must be defined, and there are all sorts of other intentional variable capture/injections here"
  (if (eq depth 0)
      `(board-rank board good-chip bad-chip ,(- maxDepth depth) t)
      `(let ((sum 0))
	 (with-possible-placements (,(toggle chip 'good-chip 'bad-chip) i board sum)
	   (incf sum (cond ((or (find4 board good-chip) (find4 board bad-chip))
			    (board-rank board good-chip bad-chip ,(- maxDepth depth) t))
			   (t
			    (search-expander ,(toggle chip 'good-chip 'bad-chip) ,(- depth 1) ,maxDepth))))))))

(defun find-guarantee (board good-chip bad-chip chip depth)
  "helper function for search-expander-2; recurses on depth, which is a list of 'any and 'all
   'any looks to find at least one 'win or 'loss when making the next move
   'all looks to find where no matter what the next move is, 'win or 'loss is returned
   This allows you to say things like: 
   [1] if the next move is a win -> nil
   [2] if the next move allows the opponent to win -> (list 'any)
   [3] if the next move means that for all opponent moves afterward, there is a way to win on the next move (list 'all 'any)
   [4] if the next move means that, there is an opponent's move that, for all of our moves afterward, there is a way for the opponent to win (list 'any 'all 'any)
   [5] etc..."
  (cond	((eq 'win (board-rank board good-chip bad-chip)) 'win)
	((eq 'loss (board-rank board good-chip bad-chip)) 'loss)
	((null depth) nil)
	(t (let ((out))
	     (with-possible-placements ((toggle chip good-chip bad-chip) i board)
	       (push (find-guarantee board good-chip bad-chip (toggle chip good-chip bad-chip) (rest depth)) out))
	     (cond ((eq (car depth) 'any)
		    (cond ((null out) nil)
			  ((member 'win out) 'win)
			  ((member 'loss out) 'loss)
			  (t nil)))
		   ((eq (car depth) 'all)
		    (cond ((null out) nil)
			  ((null (remove-if (lambda (x) (eq x 'win)) out)) 'win)
			  ((null (remove-if (lambda (x) (eq x 'loss)) out)) 'loss)
			  (t nil)))
		   (t (error "shouldn't have gotten here")))))))

(defmacro search-expander-2 (depth-lst depth cnt win-loss)
  "recursive macro that generates code that finds moves that guarantee either wins or losses
   if a guaranteed win/loss is found, then it updates 'out', that is already initialized
   all sorts of intentional variable capture/injections here too"
  (let ((next-any-all (aif depth-lst (toggle (car it) 'any 'all) 'any)))
    (format t "~a with depth-lst ~a~%" win-loss depth-lst)
    (if (not (eq cnt depth))
	`(cond ((eq (find-guarantee board good-chip bad-chip good-chip ',depth-lst) ',win-loss)
		,(cond ((eq win-loss 'win)
			`(progn
			   (format t "count ~a, col ~a: playing for the win~%" ,cnt i)
			   (setf (nth i out) (list 'win ,cnt i))))
		       ((eq win-loss 'loss)
			`(progn
			   (format t "count ~a, col ~a: not playing b/c opponent can win~%" ,cnt i)
			   (setf (nth i out) (list 'loss ,cnt i))))
		       (t (error "shouldn't have gotten here"))))
	       (t (search-expander-2 ,(cons next-any-all depth-lst) ,depth ,(+ cnt 1) ,(toggle win-loss 'win 'loss)))))))

(defmacro get-move-builder (depth)
  "defines a get-move function that looks up to depth future moves to determine the next move"
  (let ((fun (symb 'get-move- depth)))
    `(defun ,fun (board good-chip bad-chip)
       (let ((out (make-list (array-dimension board 1) :initial-element nil))
	     (move (cons nil -1)))
	 (with-possible-placements (good-chip i board)
	   (setf (nth i out) t))
	 (with-possible-placements (good-chip i board)
	   (search-expander-2 nil ,depth -1 win))
	 (format t "board absolutes: ~a~%" out)
         ;if there is a win -> take the one with the shortest count
	 (aif (remove-if-not (lambda (x) (and (consp x) (eq (first x) 'win))) out)
	      (return-from ,fun (third (car (sort it (lambda (x y) (< (second x) (second y))))))))
         ;if there are all bad moves -> take the one with the longest count
	 (if (not (member t out))
	     (let ((it (remove-if-not (lambda (x) (and (consp x) (eq (first x) 'loss))) out)))
	       (return-from ,fun (third (car (sort it (lambda (x y) (> (second x) (second y)))))))))
         ;change all bad moves to nil
	 (setf out (mapcar (lambda (x) (if (consp x) nil x)) out))
	 ;if there is only one move -> do it
	 (if (eq (length (remove-if-not (lambda (x) (eq x t)) out)) 1)
	     (return-from ,fun (- (length out) (length (member t out)))))
	 (format t "board absolutes: ~a~%" out)
         ;otherwise, look into the future...
	 (with-possible-placements (good-chip i board)
	   (when (nth i out)
	     (setf (nth i out)
		   (search-expander good-chip ,depth ,depth))))
	 (format t "board rank: ")
	 (dotimes (i (array-dimension board 1) (format t "~%"))
	   (if (numberp (nth i out))
	       (format t "~,5f " (coerce (nth i out) 'double-float))
	       (format t "~a " (nth i out))))
         ;and play at the best location
	 (do-possible-moves (i board)
	   (if (eq (cdr move) -1)
	       (setf move (cons (nth i out) i))
	       (awhen (nth i out)
		 (when (or (null (car move))
			   (> it (car move)))
		   (setf move (cons it i))))))
	 (values (cdr move) out)))))

(defmacro build-get-moves (depth)
  (format t "building at depth ~a~%" depth)
  `(progn
     (get-move-builder ,depth)
     ,(when (> depth 0)
	    `(build-get-moves ,(- depth 1)))))

(build-get-moves 25)

(defun get-move (board good-chip bad-chip)
  "wrapper function that calls get-moves functions
   will start with get-move-0, then if that call is returned
   fast enough, will call get-move-1, and keep incrementing the 
   depth until a call takes too long; then stops, and returns
   the move found from calling that last get-moves function"
  (let ((move) (last) (mv) (lt) 
	(cnt 0)
	(maxTime (eval-object (get-matching-line (get-pandoric 'args 'configFileWdLST) "maxTime="))))
    (while (and
	    (fboundp (symb `get-move- cnt))
	    (< (return-time
		(format t "searching at depth ~a~%" cnt)
		(multiple-value-setq (mv lt) (funcall (symb 'get-move- cnt) board good-chip bad-chip))
		(push mv move)
		(push lt last))
	       maxTime)
	    (if (> (length last) 10)
		(not (equalp (first last) (second last)))
		t))
      (incf cnt))
    (first move)))
   
(defun finished-p (board)
  (do-possible-moves (i board t)
    (return-from finished-p nil)))

(defmacro make-game (&optional (player1-human-player-p t) (player2-human-player-p t))
  "defines a game object that is a lexical pandoric closure
   calling the closure makes the next player make a move on the board
   the game is advanced by repeatedly calling this closure"
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
  "defines a player object that is a lexical pandoric closure
   calling the closure makes the player make a move on the board"
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
      

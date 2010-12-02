(deftest test-find4 ()
  (let ((b1 (make-board
	     nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil
	     nil nil nil nil nil nil red
	     nil nil nil nil nil nil red
	     red nil nil red red red red)))
    (check (eq (find4 b1 'red) 1))
    (check (equalp (find3 b1 'red) #(1 1 2 1 1 1 2)))))

(deftest test-find3 ()
  (let ((b2 (make-board
	     nil nil nil  nil  nil nil nil
	     nil nil nil  nil  nil nil nil
	     nil nil nil  nil  nil nil nil
	     nil nil nil  nil  yellow nil nil
	     nil nil red  yellow yellow nil nil
	     nil red yellow yellow yellow nil nil)))
    (check (not (find4 b2 'red)))
    (check (equalp (find3 b2 'red) #(0 0 0 0 0 0 0)))
    (check (equalp (find2 b2 'red) #(0 0 0 1 1 0 0)))
    (check (not (find4 b2 'yellow)))
    (check (equalp (find3 b2 'yellow) #(0 0 0 0 1 1 0)))))

(deftest test-find4-2 ()
  (let ((b3 (make-board
	     nil nil nil nil nil nil nil
	     nil nil nil nil nil nil nil
	     red nil nil yellow nil nil nil
	     nil red yellow yellow nil nil nil
	     nil yellow red yellow nil nil nil
	     yellow yellow yellow red nil nil nil)))
    (check (eq (find4 b3 'red) 1))
    (check (eq (find4 b3 'yellow) 1))))

(defun test-connect4 ()
  (let ((result
	 (runtests 
	  (test-find4)
	  (test-find3)
	  (test-find4-2))))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))
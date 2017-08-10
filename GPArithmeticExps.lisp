(defvar x 0)
(defvar y 0)
(defvar z 0)
(defvar population '())
(defvar errors '())     ;used for fitness. the lower the error, the higher the fitness
(defvar randList '())   ;random list of index postions for parents in population
(defvar kid '())
(defvar p1 '())
(defvar p2 '())
(defvar tgt1 '())
(defvar tgt2 '())
(defvar minList '())   ;used for report summary in output
(defvar maxList '())   ;used for report summary in output
(defvar opset (list '+ '- '* 'div1))
(defvar numset (list -9.0 -8.0 -7.0 -6.0 -5.0 -4.0 -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
(defvar varset (list 'x 'y 'z))
(defvar pop-size 20)
(defvar gen-count 100)
(defvar test-data (list '(1 0 2 2)
			'(0 -2 1 -16)
			'(9 8 -6 72)
			'(9 -7 5 113)
			'(-8 7 3 150)
			'(-4 -5 -3 58)
			'(5 4 -5 20)
			'(6 -4 6 41)
			'(-5 3 -7 -24)
			'(-6 -5 9 -18) ))

;;;The function div1 generates fail-safe for the division of zero as the denominator
;;;Arguments: none
;;;Returns: 1 if zero denominator or result of division
(defun div1(num1 num2)
  (if (= num2 0)
      1
      (/ num1 num2)
      ))

;;;The function get-random-op generates a random operator
;;;Arguments: none
;;;Returns: One of the operators in opset
(defun get-random-op ()
  (let (opset)
    (setq opset (list '+ '- '* 'div1))
    (nth (random (length opset)) opset )
    ))

;;;The function get-random-number generates a random number from -9 to 9
;;;Arguments: none
;;;Returns: One of the numbers in range -9 to 9
(defun get-random-number ()
  (let (numset)
    (setq numset (list -9.0 -8.0 -7.0 -6.0 -5.0 -4.0 -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0    ))
    (nth (random (length numset)) numset )
    ))

;;;The function get-random-variable generates a random variable x, y, or z
;;;Arguments: none
;;;Returns: One of the variables x, y, or z
(defun get-random-variable ()
  (let (varset)
    (setq varset (list 'x 'y 'z ))
    (nth (random (length varset)) varset)
    ))


;;;The function make-expression generates a random expression using the form
;;;containing operators and at least two arguments
;;;Arguments: none
;;;Returns: A random expression
(defun make-expression()
  (let (op arg1 arg2 choice)
    (setq op (get-random-op))
    (setq choice (random 11))
	  (if (< choice 4) ;choice is 0, 1, 2 or 3
	      (setq arg1 (get-random-number))
	      (if (< choice 8)
		  (setq arg1 (get-random-variable)) ;choice is 4,5,6 or 7
		  (setq arg1 (make-expression))  ;choice is 8
		  )
	      )
    (setq choice (random 11))
	  (if (< choice 4) ;choice is 0, 1, 2 or 3
	      (setq arg2 (get-random-number))
	      (if (< choice 8)
		  (setq arg2 (get-random-variable)) ;choice is 4,5,6 or 7
		  (setq arg2 (make-expression))  ;choice is 8
		  )
	      )

	  (list op arg1 arg2)))


;;;The function test-expression tests if the function make-expression works correctly
;;;Arguments: Test data values for x, y, and z
;;;Returns: A random expression
(defun test-expression(xval yval zval)
  (setq x xval)
  (setq y yval)
  (setq z zval)
  (eval (write (make-expression)))
  )

;;;The function make-population generates a population of expression based on pop-size
;;;Arguments: none
;;;Returns: A population with pop-size amount of expressions
(defun make-population ()
  (let (exp)
    (setq population '())
    (loop while (< (length population) pop-size) ; (n pop-size)
       do
	 
	  (setq exp (make-expression))
	  (if (null (member exp population))
	      (setq population (append population (list exp)))
	      )
	 )    
    )
  population
  )

;;;The function evaluate-expression calculates the result of an expression given some test data
;;;Arguments: An expression
;;;Returns: A population with pop-size amount of expression
(defun evaluate-expression(exp)
;;(print func)
  (let (estimate answer sum error)
    (setq sum 0)
    (dolist (item test-data)
      (setq x (nth 0 item))
      (setq y (nth 1 item))
      (setq z (nth 2 item))
      (setq answer (nth 3 item))
      (setq estimate (eval exp))
      (setq error (abs (- estimate answer)))
      (setq sum (+ sum error))
     ; (format t "~S  estimate: ~F   abs error: ~F~%" item  estimate error)
      )
    sum
    ))

;;;The function evaluate-all calculates the fitness of all of the expressions in the population
;;;Arguments: None
;;;Returns: A list of fitness values of each expression in population
(defun evaluate-all ()
  (setq errors '())
  (Let ( error)
    (dolist (exp population)
     ; (format t "function being evaluated: ~S~%" exp)
      (setq error (evaluate-expression exp))
      (setq errors (append errors (list error)))
      ))
  )

;;;The function find-best-expression finds the expression with the lowest error/highest fitness
;;;Arguments: None
;;;Returns: A list of the fittest expression and its fitness value
(defun find-best-expression()
  (let (index min len min-index)
    (setq len (length errors))
    (setq index 0)
    (setq min-index 0)
    (setq min (nth 0 errors))
    (loop while (< index len) 
       do
	 (if (< (nth index errors) min)
	     (progn
	       (setq min (nth index errors))
	       (setq min-index index))
	     )
	 (setq index (1+ index))
	 )
    (list (nth min-index population) min)
    ))


;;;The function find-worst-expression finds the expression with the highest error/lowest fitness
;;;Arguments: None
;;;Returns: A list of the most unfit expression and its fitness value
(defun find-worst-expression()
  (let (index max len max-index)
    (setq len (length errors))
    (setq index 0)
    (setq max-index 0)
    (setq max (nth 0 errors))
    (loop while (< index len) 
       do
	 (if (> (nth index errors) max)
	     (progn
	       (setq max (nth index errors))
	       (setq max-index index))
	     )
	 (setq index (1+ index))
	 )
    (list (nth max-index population) max)
    ))

;;;The function find-ave-error calculates the average of all the fitness values
;;;Arguments: None
;;;Returns: The average of all fitness values
(defun find-ave-error()
  (let (ave len index)
    (setq len (length errors))
    (setq index 0)
    (setq ave 0)
    (loop while ( < index len)
	 do
	 (setq ave (+ (nth index errors)))
	 (setq index (1+ index))
    )
    (list ave)
  )
)

;;;The function purge-half gets rid of the unfit half of the population
;;;Arguments: None
;;;Returns: A list of the upper half fittest expressions 
(defun purge-half()
(let (temp median middle index)
  (setq temp (copy-seq errors))
  (setq temp (sort temp #'< ))
  (setq middle (floor pop-size 2))

  (if (= (mod pop-size 2) 0) ;pop-size is even
      (setq median (/ (+ (nth middle temp) (nth (- middle 1) temp)) 2 ))
      (setq median (nth middle temp))
      )
;  (format t "~%INSIDE PURGE: ~%")
;  (format t "sorted errors        : ~S~%" temp)
;  (format t "Median               : ~F~%"  median)
;  (format t "errors before change : ~S~%" errors)

  (setq index 0)
  (loop while (and (< index (length errors)) (not (= (/ pop-size 2) (length population)))) 
     do
       (if (>= (nth index errors) median)
	   (progn
	    ; (format t "removing ~S from ~S and ~S from ~S~%" (nth index errors) errors (nth index population) population)
	     (setq errors (remove (nth index errors) errors :count 1)) ;remove errors item
	     (setq population (remove (nth index population) population :count 1)) ;remove population item	   
	     )
	   (setq index (+ index 1))
	   )
       )
  )
)

;;;The function test-purge tests if purge-half function works correctly
;;;Arguments: None
;;;Returns: A list of the upper half fittest expressions 
(defun test-purge()
  (setq errors '())
  (setq population '())
  (make-population)
  (evaluate-all)
 ;;(print 'before)
  ;;(print errors)
  ;;(print population)

  (purge-half)

  ;;(print 'after)
  ;;(print errors)
  ;;(print population)
)

;;################################################################################################################################
;;;The function new-find-node finds a valid segment of an expression
;;;Arguments: An expression
;;;Returns: A valid segment of an expression
(defun new-find-node(exp)
 (let (choice piece)
   (setq choice (random 2))
   (setq piece (nth (+ 1 choice) exp))
   (if (not (listp piece))
	piece
	(progn 
	  (if (= (length piece) 1)
	      piece
	      (progn
		(setq choice (random 2))
		(if (= choice 0)
		    piece
		    (new-find-node piece))
		)
	      )
	  )
	)
   )
 )


;;;The function find-mutation-point finds the smallest piece of an expression to mutate
;;;Arguments: An expression
;;;Returns: Smallest piece of an expression to mutate
(defun find-mutation-point(exp)
 (let (choice piece)
   (setq choice (random 3))
   (setq piece (nth choice exp))
   (if (not (listp piece))
	piece
	(progn 
	  (if (= (length piece) 1)
	      piece
	      (progn
		piece
		(find-mutation-point piece)
		)
	      )
	  )
	)
   )
 )


;;;The function make-mutation places the piece of mutation into the expression
;;;Arguments: An expression
;;;Returns: A mutated expression
(defun make-mutation(exp)
  (let (mutee replace choice)
    (setq mutee (find-mutation-point exp))
    ;;debug
    ;;(format t "mutee: ~S ~%" mutee)
    (let* ((ops '(+ - * div1)))
      (if (member mutee ops)
	  (setq replace (get-random-op))
	  (progn 
	    (setq choice (random 2))
	    (if (eq choice 0)
	       	(setq replace (get-random-number))
		(setq replace (get-random-variable))
	    )
          )
      )
      ;;debug
      ;;(format t "replace: ~S ~%" replace)
    )
    (new-make-kid exp mutee replace)
  )
)


;;The function mutate determines a random expression to mutate in the population
;;;Arguments: None
;;;Returns: None
(defun mutate ()
  (let (choice)
    (setq choice (random pop-size))
    (setf (nth choice population) (make-mutation (nth choice population)))
  ) 
)


;;The function mutate-exp determines when to mutate 1% of the time
;;;Arguments: None
;;;Returns: None
(defun mutate-exp()
  (let (choice count)
    (setq count 0)
    (dotimes (n pop-size)
      (setq choice (random 50))
      (if (eq choice 7)
	  (progn
	    ;(format t "Surprise! A mutation occurred.~%")
	    (setf (nth n population) (make-mutation (nth n population)))
	    (setq count (+ count 1))
	    )
	  )
      )
    count
    )
  )


;;The function make-new-kid generates children to put back in population
;;;Arguments: an expression, the target segment, the new segment to swap
;;;Returns: Two children to each parent pairs which mate
(defun new-make-kid (rmom rtgt rnew)
(let (lp rp mp) 
  ;;(print rmom)
  ;;(print rtgt)
  ;;(print rnew)
  (cond 
    ( (and (eq rmom rtgt) rnew))
    ( (not (listp rmom)) rmom)
    ( (eq (nth 0 rmom) rtgt) (list rnew (nth 1 rmom)  (nth 2 rmom)))
    ( (eq (nth 1 rmom) rtgt) (list (nth 0 rmom) rnew (nth 2 rmom)))
    ( (eq (nth 2 rmom) rtgt) (list (nth 0 rmom) (nth 1 rmom) rnew))
    (t (progn
	(setq lp (car rmom))
	(setq mp (new-make-kid (nth 1 rmom) rtgt rnew))
	(setq rp (new-make-kid (nth 2 rmom) rtgt rnew))
	;;(format t "lp: ~S~%" lp)
	;;(format t "mp: ~S~%" mp)
	;;(format t "rp: ~S~%" rp)
	(list lp mp rp)
	))
       )
    )
  )


;;The function new-crossover swaps the segments in generating children
;;;Arguments: Two expressions
;;;Returns: Population with new kids
(defun new-crossover (par1 par2)
  (let (piece1 piece2 kid1 kid2)
    (setq piece1 (new-find-node par1))
    (setq piece2 (new-find-node par2))
    (setq kid1 (new-make-kid par1 piece1 piece2))
    (setq kid2 (new-make-kid par2 piece2 piece1))
  
    ;(format t "parent1: ~S ~%" par1)
    ;(format t "parent2: ~S ~%" par2)

    ;(format t "piece1: ~S ~%" piece1)
    ;(format t "piece2: ~S ~%" piece2)

    ;(format t "kid1: ~S ~%" kid1)
    ;(format t "kid2: ~S ~%" kid2)
    (setq population (append population (list kid1)))
    (setq population (append population (list kid2)))
    
    )
  )

;;The function rand-list generates a random order of parents to mate
;;;Arguments: None
;;;Returns: List of random order of parents to mate
(defun rand-list()
  (let (randNum)
  (setq randList '())
  (loop while (< (length randList) (length population))
     do (setq randNum (random (length population)))
     (if (not (member randNum randList :test 'equal))
	 (setq randList (append randList (list randNum)))))
  ;;(nth (random (length randList)) randList)
))


;;The function crossover-all picks two random parents and mates them
;;;Arguments: None
;;;Returns: None
(defun crossover-all()
  ;;randomly draw 2 survivors from "pool" to mate
  (let (index p1Pos p2Pos)

    (setq index 0)
    (rand-list)

    ;;debug 
    ;(format t "randList: ~S~%" randList)
    (dotimes (n (floor (length population) 2)) ;;loop until all parents mate
       ;;retrieve 2 random index positions
       (setq p1Pos (nth index randList)) ;;remember to put "do" here
       (setq p2Pos (nth (+ index 1) randList))
       (setq index (+ index 1))

       ;;retrieve 2 random parents from population pool
       (setq p1  (nth p1Pos population))
       (setq p2 (nth p2Pos population))
      
       ;;Mate parents
       (new-crossover p1 p2)
    )
))


;;The function test-crossover tests if crossover works correctly
;;;Arguments: None
;;;Returns: None
(defun test-crossover()
  (setq errors '())
  (setq population '())
  (make-population)
  (evaluate-all)
  (print 'before)
  (print errors)
  (print population)

  (purge-half)

  (print 'after)
  (print errors)
  (print population)
  (crossover-all)
  (print population)
)


;;The function genetic-prog calls on all the other functions to implement genetic programming
;;;Arguments: None
;;;Returns: Report Summary for each generation and a Final report summary showing best and worst
;;;         expressions and their fitness along with the final population
(defun genetic-prog()
  (setq minList '())
  (setq maxList '())
  (let (count aveError)
    (make-population)
    (dotimes (n gen-count)

      (format t "~%~%*************************  GENERATION ~D ************************* ~%" (+ n 1))
      ;;(format t "Population (start)              : ~S~%" population)
      ;;(format t "Population size : ~S~%" (length population))
      (evaluate-all)
      (find-ave-error)
      (setq aveError (find-ave-error))
      (format t "Best fitness: ~S~%" (cdr (find-best-expression)))
      (format t "Average fitness: ~S~%" aveError)
      (format t "Worst fitness: ~S~%" (cdr (find-worst-expression)))

      (if (eq n 0)
	  (progn
	    (setq maxList (append maxList (list (find-worst-expression))))
	    (setq minList (append minList (list (find-best-expression))))
	  )
      )
      (if (eq n 1)
	  (progn
	    (setq maxList (append maxList (list (find-worst-expression))))
	    (setq minList (append minList (list (find-best-expression))))
	  )
      )
      (if (eq n 98)
	  (progn
	    (setq maxList (append maxList (list (find-worst-expression))))
	    (setq minList (append minList (list (find-best-expression))))
	  )
      )
     (if (eq n 99)
	  (progn
	    (setq maxList (append maxList (list (find-worst-expression))))
	    (setq minList (append minList (list (find-best-expression))))
	  )
      )

      ;(print errors)
      (purge-half)
      ;;(format t "Population size : ~S~%" (length population))
      ;(format t "Population (after purge): ~S~%" population)
      (crossover-all)
      ;(format t "Population (after crossover): ~S~%" population)
      ;;(format t "Population size : ~S~%" (length population))

      (setq count (mutate-exp))
      (format t "Population (after ~D mutations): ~S~%" count  population)
      ;;(format t "Population size : ~S~%" (length population))
      )

    (format t "~%~%***************** Report Summary ***************** ~%")
    (format t "~%****** First Generation ****** ~%")
    (format t "Best Expression: ~S~%"  (nth 0 (nth 0 minList)))
    (format t "Best Fitness: ~S~%"  (nth 1 (nth 0 minList)))
    (format t "Worst Expression: ~S~%"  (nth 0 (nth 0 maxList)))
    (format t "Worst Fitness: ~S~%"  (nth 1 (nth 0 maxList)))
    (format t "~%****** Second Generation ****** ~%")
    (format t "Best Expression: ~S~%"  (nth 0 (nth 1 minList)))
    (format t "Best Fitness: ~S~%"  (nth 1 (nth 1 minList)))
    (format t "Worst Expression: ~S~%"  (nth 0 (nth 1 maxList)))
    (format t "Worst Fitness: ~S~%"  (nth 1 (nth 1 maxList)))
    (format t "~%****** Second to Last Generation ****** ~%")
    (format t "Best Expression: ~S~%"  (nth 0 (nth 2 minList)))
    (format t "Best Fitness: ~S~%"  (nth 1 (nth 2 minList)))
    (format t "Worst Expression: ~S~%"  (nth 0 (nth 2 maxList)))
    (format t "Worst Fitness: ~S~%"  (nth 1 (nth 2 maxList)))
    (format t "~%****** Last Generation ****** ~%")
    (format t "Best Expression: ~S~%"  (nth 0 (nth 3 minList)))
    (format t "Best Fitness: ~S~%"  (nth 1 (nth 3 minList)))
    (format t "Worst Expression: ~S~%"  (nth 0 (nth 3 maxList)))
    (format t "Worst Fitness: ~S~%"  (nth 1 (nth 3 maxList)))

    (format t "~%~%Final Population: ~S~%" population)
    )
  T
  )

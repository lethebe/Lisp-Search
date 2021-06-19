;Jeromy Lethebe
;Program conducts a depth-first search of the farmer,
;wolf, goat, cabbage problem and by using a rescursive call
; tree to generate and test possible states until a goal 
;state is reached or search is exuasted Program is run using
;(run start goal) syntax where start and goal are
;lists ex. (run '(w w w w) '(e e e e)). Item one in each 
;lists refers to the farmer and e represents east and w 
;represents west; item two is the wolf, three the goat, and 
;four the cabbage.

;used to create a state in a list. first item refers
;to which side the farer is on, second the wolf,
;third the goat, and fourth the cabbage. ex:
; (w w w w) or (e w w e )
(defun createState (farmer wolf goat cabbage) (list farmer wolf goat cabbage))

;returns the opposite side for  given input side
(defun getOppositeSide (side) 
	(cond ((equal 'e side) 'w)
			((equal 'w side) 'e)))

;checks that a move is a legal, wolf can't be left
; with the goat, and the goat can't be left with the
;cabbage 
(defun isSafe (state)
	(cond ((and 
				(not (equal (getFarmerSide state)(getWolfSide state)))
				(equal (getGoatSide state) (getWolfSide state))) nil)
			
			((and (equal (getGoatSide state)(getCabbageSide state))
				(not (equal (getFarmerSide state) (getGoatSide state))))nil)

			(t state)))


; returns the side the farmer is on for  given state
(defun getFarmerSide (state) (first state))

;returns the side the wolf is on for a given state
(defun getWolfSide (state) (nth 1 state))

;returns the side the goat is on for a given state
(defun getGoatSide (state) (nth 2 state))

;returns the side the cabbage is on for a given 
;state
(defun getCabbageSide (state) (nth 3 state))

;creates a state from a given state with the farmer
;moving alone 
(defun FarmerMovesAlone (state)
	(isSafe (createState (getOppositeSide (getFarmerSide state))
			(getWolfSide state)
			(getGoatSide state)
			(getCabbageSide state)
			)))

;creates a state from a given state with the farmer
;and wolf moving across
(defun FarmerMovesWolf (state)
	(cond ((equal (getFarmerSide state) (getWolfSide state))
		   (isSafe (createState (getOppositeSide (getFarmerSide state))
				   (getOppositeSide (getWolfSide state))
				   (getGoatSide state)
				   (getCabbageSide state))))
	      (t nil)))

;creates a state from a given state with the farmer
;and goat moving across
(defun farmerMovesGoat (state)
   (cond ((equal (getFarmerSide state) (getGoatSide state))
          (isSafe (createState (getOppositeSide (getFarmerSide state))
                  (getWolfSide state)
                  (getOppositeSide (getGoatSide state))
                  (getCabbageSide state)))) 
          (t nil)))

;creates a state from a given state with the farmer
;and cabbage moving across 
(defun farmerMovesCabbage (state)
   (cond ((equal (getFarmerSide state) (getCabbageSide state))
           (isSafe (createState (getOppositeSide (getFarmerSide state))
                   (getWolfSide state)
                   (getGoatSide state)
                   (getOppositeSide (getCabbageSide state)))))   
         (t nil)))


;used to run the search, startState and goalState
;are entered as lists. ex: (run '(w w w w) '(e e e e))
(defun run (startState goalState) 
	(declare (special *path*))
	(setq *path* "No solution")
	;calls searchSolution
	(searchSolution startState goalState nil)
	;prints result
    (format t "The solution path is: ~%~%~S" *path*))
	
		 
;actual search function used to do a depth-first
;search. start state, goal state and closedList are passed
;to each recursive call. closedList will be the path to
;each state and will eventually be the solution, if
;a solution exists. Search works by recursively trying
;to apply subsequent legal moves until the goal state is 
;found or search is exhuasted.  
(defun searchSolution (startState goalState closedList)
	;check for null
	(cond ((null startState) nil)
         ;check for goal
		 ((equal startState goalState) (setq *path* (reverse (cons startState closedList))))
         ;check for membership in path
		 ((not (member startState closedList :test #'equal))
              ;recursive tree that recursively applies
			  ;legal moves until exhaustion, or a goal
			  ;state is found. 
			  (or (searchSolution (farmerMovesAlone startState) goalState (cons startState closedList))
                  (searchSolution (farmerMovesWolf startState) goalState (cons startState closedList))
                  (searchSolution (farmerMovesGoat startState) goalState (cons startState closedList))
                  (searchSolution (farmerMovesCabbage startState) goalState (cons startState closedList))))))

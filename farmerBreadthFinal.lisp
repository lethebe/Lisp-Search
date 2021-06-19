;Jeromy Lethebe
;Program conducts a breadth first search of the farmer,
;wolf, goat, cabbage problem and uses  list to store 
;child-parent pairs so that the list can be parsed through
;if a goal state is reached to obtain the path. Program is
;run using (run start goal) syntax where start and goal are
;lists ex. (run '(w w w w) '(e e e e)). Item one in each 
;lists refers to the farmer and e represents east and w 
;represents west; item two is the wolf, three the goat, and 
;four the cabbage.

;function used run the search, states are expressed 
;in list form like '(e e e e)
(defun run (start goal) 
	;calls search solution for the search
	(searchSolution start goal)
	;calls findPath for finding the path, if the goal
	;is not reached "No Solution" will be output
	(cond ((equal (first *parentList*) goal) (findpath goal))
				(t (setq *solutionPath* "No Solution")))
	;prints the path to the shell
	(format t "The solution path is: ~%~%~S" *solutionPath*))
	
;used as part of sublist to slice a list
(defun getItems (numList num) 
	(if (> num 0)
		(cons (car numList) (getItems (cdr numList) (- num 1)))
		'()))

;used to create a slice of a list from start to
;counter number of items (uses base 1 indexing)
(defun sublist (numList start counter)
	(if (> start 1)
		(sublist (cdr numList) (- start 1) counter)
		(getItems numList counter)))

;used to parse through *solutionPath* to find the path since breadth first is more complicated
(defun findPath (state) 
	(declare (special *solutionPath*)
	         (special *index*)
		 (special *parentState*)
		 (special *parentList*))
	;if state == startState stop
	(cond ((equal state *startState*) nil)
	(t (setq *index* (position state *parentList* :test #'equal))
			;check to make sure index isn't a parent
			(cond ((equal (mod *index* 2) 1) 
				(setq *parentList* (sublist *parentList* (+ *index* 2) (- (length *parentList*) (+ *index* 1))))
				(setq *index* (position state *parentList* :test #'equal))))
			;write parent to *solutionPath* splice
			;*parentList*
			(setq *solutionPath* (cons (nth (+ *index* 1) *parentList*) *solutionPath* ))
			(setq *parentState* (nth (+ *index* 1) *parentList*))
			(setq *parentList* (sublist *parentList* (+ *index* 3) (- (length *parentList*)(+ *index* 2))))
			; recursive call on next state in the 
			;path
			(findPath *parentState*))))

;used to create a state of form '(w w e e) first
;item is farmer, second is wolf, third is goat
;fourth is cabbage and e refers to east w refers to 
;west.
(defun createState (farmer wolf goat cabbage) (list farmer wolf goat cabbage))

;used to return the opposite side for a given
;input side
(defun getOppositeSide (side) 
	(cond ((equal 'e side) 'w)
			((equal 'w side) 'e)))

;used to check if a given state is valid
(defun isSafe (state)
	(cond   ((and 
				(not (equal (getFarmerSide state)(getWolfSide state)))
				(equal (getGoatSide state) (getWolfSide state))) nil)
			
			((and (equal (getGoatSide state)(getCabbageSide state))
				(not (equal (getFarmerSide state) (getGoatSide state))))nil)

			(t state)))


;return the side the farmer is on
(defun getFarmerSide (state) (first state))

;return the side the wolf is on
(defun getWolfSide (state) (nth 1 state))

;return the side the goat is on
(defun getGoatSide (state) (nth 2 state))

;return the side the cabbage is on
(defun getCabbageSide (state) (nth 3 state))

;generates a state where only the farmer switches 
;sides, the move must be safe however 
(defun FarmerMovesAlone (state)
	(isSafe (createState (getOppositeSide (getFarmerSide state))
			(getWolfSide state)
			(getGoatSide state)
			(getCabbageSide state)
			)))

;generates a state where the farmer and wolf move
;to the opposite side safely and legally
(defun FarmerMovesWolf (state)
	(cond ((equal (getFarmerSide state) (getWolfSide state))
		   (isSafe (createState (getOppositeSide (getFarmerSide state))
				   (getOppositeSide (getWolfSide state))
				   (getGoatSide state)
				   (getCabbageSide state))))
	      (t nil)))

;generates a state where the farmer and goat move
;to the opposite side, must be safe and legal move.
(defun farmerMovesGoat (state)
   (cond ((equal (getFarmerSide state) (getGoatSide state))
          (isSafe (createState (getOppositeSide (getFarmerSide state))
                  (getWolfSide state)
                  (getOppositeSide (getGoatSide state))
                  (getCabbageSide state)))) 
          (t nil)))

;generates a state where the farmer and cabbage move
;to the opposite side, must be safe and legal move.
(defun farmerMovesCabbage (state)
   (cond ((equal (getFarmerSide state) (getCabbageSide state))
           (isSafe (createState (getOppositeSide (getFarmerSide state))
                   (getWolfSide state)
                   (getGoatSide state)
                   (getOppositeSide (getCabbageSide state)))))   
         (t nil)))


; searches for a solution state using breadth-first
;search, by using *closedStates* and *openStates* 
;to keep a queue going. child parent pairs are 
;written to *parentList* so that the solution path
;found can be parsed from it using findPath.
(defun searchSolution (startState goalState)
	(declare (special *openStates* )
			 (special *closedStates*)
                         (special *parentList*)
			 (special *goalState*)
			 (special *startState*)
			 (special *solutionPath*)
			 (special *moveList*))
	(setq *goalState* goalState)
	(setq *solutionPath* (list goalState))
	(setq *startState* startState)
	(setq *openStates* (list startState))
        (setq *parentList* (list startState '(0 0 0 0)))
	(setq *closedStates* nil)
	(setq *moveList* '(FarmerMovesAlone FarmerMovesWolf FarmerMovesGoat FarmerMovesCabbage))
	(search1 (list startState)))

(defun search1 (path)
  (declare (special *openStates*)
           (special *closedStates*)
           (special *goalState*)
           (special *moveList*))
  (cond ((null *openStates*) nil)
        (t (let ((state (car *openStates*)))
             (cond ((equal state *goalState*) path)
                   (t (setq *closedStates* (cons state *closedStates*))
                      (setq *openStates* 
                            (append (cdr *openStates*)
                                    (generatePossibleStates state *moveList*)))
                      (search1 (cons state path))))))))

;used to genrate the possible states that can 
;legally be reached from a given state. each state
;is checked against *openStates* and 
;*closedStates*  so that no duplicates/cycles
;occur
(defun generatePossibleStates (state moveList)
  (declare (special *openStates*)
           (special *parentList*)
           (special *closedstates*))
  (cond ((null moveList) nil)
        (t (let ((child (funcall (car moveList) state))
                 (rest (generatePossibleStates state (cdr moveList))))
             (cond ((null child) rest)
                   ((member child rest :test #'equal) rest)
                   ((member child *openStates* :test #'equal) rest)
                   ((member child *closedStates* :test #'equal) rest)
                   (t (setq *parentList* (cons state *parentList*))(setq *parentList* (cons child *parentList*))(cons child rest)))))))
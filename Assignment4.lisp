https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder

;;; Due MIDNIGHT the evening of SUNDAY, NOVEMBER 18


;;; STATE SPACE SEARCH

;;; WHAT YOU MUST DO
;;;
;;; Your job is to implement the following functions:
;;;
;;; GENERAL-SEARCH  	(general search function)
;;; GOAL-P		(goal testing predicate)
;;; BFS-ENQUEUER	(enqueues in breadth-first order)
;;; DFS-ENQUEUER	(enqueues in depth-first order)
;;; MANHATTAN-ENQUEUER	(enqueues by manhattan distance)
;;; NUM-OUT-ENQUEUER	(enqueues by number of tiles out of place)
;;;
;;; You may write auxillary functions if necessary.  Make sure you
;;; implement true A* search under the assumption of monotonicity
;;; (don't maintain the pointers). I VERY STRONGLY URGE YOU TO COMPILE.
;;;
;;; You then will run your four enqueueing functions on the five 
;;; examples at the end of this file and report, in a neatly-organized
;;; table, the number of iterations it took to solve each example.
;;; If it's taking more than 20000 iterations, you may simply state
;;; that the example FAILED for the function.  In every case except
;;; for depth-first search possibly, you should find the solution with
;;; the smallest number of moves, if you find a solution at all.  The
;;; minimal number of moves is already indicated next to the
;;; example.
;;;
;;; Additionally, you should also provide the actual solutions that
;;; MANHATTAN-ENQUEUER discovered for the first four examples, and
;;; state why you believe the fifth example is (or isn't) difficult
;;; for MANHATTAN-ENQUEUER.
;;;
;;; Once you have implemented these functions, it's easy to run them.
;;; Here's the manhattan enqueuer being run on example #6, with the
;;; result being printed out in a pleasing fashion.
;;;
;;; (setf s (make-initial-state '(1 8 9 3 2 4 6 5 7)))
;;; (print-solution (general-search s #'goal-p #'manhattan-enqueuer))
;;;
;;; Mail the following to the TA, zipped/gzipped as usual, as an attachment:
;;;
;;; 1. Your completed functions and any auxillary functions and data
;;;    you wrote to support them.
;;; 2. The number of iterations required for each of the 4 functions
;;;    on each of the 5 examples.
;;; 3. The actual printed out solutions for MANHATTAN-ENQUEUER on the
;;;    first four examples.
;;; 4. A 500 word report detailing how you implemented your functions,
;;;    why you thought various techniques did better than others,
;;;    and your explanation for why the fifth example is easy or hard
;;;    for MANHATTAN-ENQUEUER.


;;; WHAT'S PROVIDED
;;;
;;; Accompanying this file are two other files: "utilities.lisp" and
;;; "queue.lisp".  utilities.lisp must be loaded first, then queue.lisp.
;;;  queue.lisp is what you need to take a look at: it's an implementation
;;; of three kinds of queues: LIFO stacks, FIFO queues, and priority queues.
;;; You will find it useful.
;;;
;;; In the homework file (this one), are many functions and macros :-) which 
;;; operate on 8-puzzles.  They should be pretty self-explanatory.  One
;;; function you won't use in your code, but might find useful to test with,
;;; is CREATE-RANDOM-STATE, which makes lots of random moves on an 8-puzzle
;;; to randomize it.  Most of the macros are there just for function-inlining,
;;; except for the two provided FOREACH-... macros.  See if you can understand
;;; them.


;;; EXTRA CREDIT
;;;
;;; Worried about your course score so far?  Here's a chance for some extra 
;;; credit.
;;;
;;; Extra credit counts after the class has been ranked and assigned grades,
;;; so you getting extra credit doesn't affect the grades of other students
;;; relative to yourself.  Do at least one of:
;;;
;;; 1. Change to a different puzzle.  2x2x2 rubik's cube, peg solitaire, a 
;;;    "Dad's Puzzle" (piano-mover's puzzle).  You need to provide a somewhat
;;;    intelligent new heuristic.
;;;
;;; 2. Implement a combination of A* and IDS, called IDA*
;;;     You can find information on IDA* on Wikipedia at
;;;     http://en.wikipedia.org/wiki/IDA* 
;;;
;;; 3. Implement a simple constraint-satisfaction backtracking search or
;;; MIN-CONFLICTS constraint-satisfaction search.  Then formulate some
;;; simple constraint satisfaction problems (cryptarithmetic, N-Queens,
;;; or Sudoku if you're feeling ambitious) both as constraint satisfaction
;;; and as state-space search.  Then compare the constraint satisfaction
;;; search with state-space search on these methods.


;;; To submit extra credit, put your additional changes into a separate 
;;; subdirectory called "extracredit".  Include a README in that directory
;;; indicating what you did.  Be specific.  Help us understand.

;;; FUN ADDITIONAL STUFF YOU MIGHT TRY
;;;
;;; 1. Implement simple best-first search (that is, f = h).
;;;    Does it always find minimum solutions? 
;;; 2. Change it to a 15-puzzle (4x4).
;;; 3. There is an inefficiency in my representation: you have to call
;;;    (depth _state_) rather than the state already knowing its depth.
;;;    That multiplies the complexity by an additional O(lg n).  You might
;;;    try storing the depth in the state somehow and see how much faster
;;;    that gets you.


;;; THE NUM-OUT AND MANHATTAN HEURISTICS
;;;
;;; The NUM-OUT heuristic is simply the total number of tiles out of place 
;;; (not including the blank space).
;;;
;;; The MANHATTAN heuristic is the sum, over each tile (not including the
;;; blank), of the manhattan distance of that tile from where it's supposed
;;; to be.  Manhattan distance between two points <x,y> and <x2,y2> is equal
;;; to |x-x2| + |y-y2|, that is, it's the difference along the x dimension
;;; plus the difference along the y dimension.
;;;
;;; Example:
;;;
;;; In the following puzzle:
;;;
;;; 1 8 3
;;; 9 6 5     (9 is the blank -- ignore it)
;;; 7 4 2
;;;
;;; NUM-OUT = (#8 out of place) + (#6 out of place) + (#5 out of place) + 
;;;           (#4 out of place) + (#2 out of place) = 5
;;;
;;; MANHATTAN:   TILE     X OUT    Y OUT
;;;              #1       0        0
;;;              #2       1        2
;;;              #3       0        0
;;;              #4       1        1
;;;              #5       1        0
;;;              #6       1        0
;;;              #7       0        0
;;;              #8       0        2
;;;      Total            4   +    5     =  9
;;;
;;; Both heuristics are both admissable and, I believe, monotonic.  Which
;;; one is better?



;;; THE 8-PUZZLE REPRESENTATION
;;; 
;;; 8-puzzles are simple-vectors of integers with 10 slots.
;;; Slots 0...8 are the positions in the puzzle in row-major
;;; order, filled
;;; with numbers representing the tile that's in that slot
;;; (9 is the empty space).  slot 9 additionally says where
;;; the empty space is located.  Of course slot 9 is unnecessary,
;;; but it makes the puzzle much more efficient than wandering through
;;; the array each time looking for a 9.  So for example,
;;; the following puzzle:
;;;
;;; 2 7 4
;;; 9 8 3
;;; 1 5 6
;;;
;;; ...is stored in a simple-vector with the following values:
;;;
;;; #(2 7 4 9 8 3 1 5 6 3)
;;;
;;; ...the last item (3) says that the empty space, represented
;;; as a 9, is located in slot 3 of the array.
;;;
;;; One way that you could do state-based search is to treat a PUZZLE
;;; as a STATE.  But we're not going to do that.  We want to keep a
;;; history around so we know *how* we got to that puzzle situation.
;;; So a STATE will be defined as CONS cell whose CAR points to the
;;; puzzle, and whose CDR points to the previous state.  Our initial
;;; state's CDR points to nil. The nice thing about doing it this way
;;; is that when you get to the goal state, it appears to be just
;;; a list of states all the way back to the initial state.
;;;
;;; As such the functions below are very carefully named as operating
;;; on STATES or on PUZZLES.  Don't mix them up!


;;; THE ALGORITHM

;;; The search algorithm we'll use is a slight modification of the one
;;; given in class.  If you go through it you'll realize it's basically
;;; the same thing, with the following changes:
;;;
;;; 1. No maximum depth.  We keep around a history list, so the depth
;;;    isn't, erm, technically necessary.  :-)
;;; 2. Maximum number of iterations before we bag it and quit
;;; 3. Our enqueueing function evaluates the states' F(s) values and
;;;    enqueues them, all in one swoop
;;; 4. Keep in mind that in a heuristic version of the search,
;;;    the enqueuing function enqueues by the g(s) + h(s),
;;;    NOT just by the h(s).

;;; Here we go:


;;; GeneralSearch(InitialState, GoalTest, EnqueueingFunction, MaxIterations)
;;;   make new empty queue
;;;   make new empty history
;;;   iterations <- 0
;;;   state <- InitialState
;;;
;;;   EnqueuingFunction(queue,state)
;;;   add state to history
;;;
;;;   loop:
;;;     iterations++
;;;     if (iterations > MaxIterations or queue is empty) return 'FAILURE
;;;     state <- dequeue(queue)
;;;     if GoalTest(state)
;;;        print out number of iterations
;;;        return state
;;;     else for each child of the state
;;;        if the child is not in the history or in the queue
;;;           EnqueuingFunction(queue,child [state])
;;;           add child [puzzle] to history



(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 9 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
    "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited states.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."

	;; IMPLEMENT ME

;; hints: The history list ought to contain PUZZLES, not states.
;; However, the queue ought to contain STATES, which in this case consist of
;; conses consisting of the PUZZLE as the car, and the puzzle's parent's cons as the cdr.
;; You should use #'equalp to test for equality between puzzles in the history list.
;; You should also add the puzzle to the history list at exactly the same time
;; you add its corresponding state to the queue.

)

(defun goal-p (state)
    "Returns T if state is a goal state, else NIL.  Our goal test."

	;; IMPLEMENT ME
)

(defun dfs-enqueuer (state queue)
    "Enqueues in depth-first order"

	;; IMPLEMENT ME
)

(defun bfs-enqueuer (state queue)
    "Enqueues in breadth-first order"

	;; IMPLEMENT ME
)                                         

(defun manhattan-enqueuer (state queue)
    "Enqueues by manhattan distance"

	;; IMPLEMENT ME
)

(defun num-out-enqueuer (state queue)
    "Enqueues by number of tiles out of place"

	;; IMPLEMENT ME
)

#|
;;; The five test examples.

;;; Solves in 4 moves:
(setf s (make-initial-state '(
9 2 3
1 4 6
7 5 8)))

;;; Solves in 8 moves:
(setf s (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

;;; Solves in 16 moves:
(setf s (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

;;; Solves in 24 moves:
(setf s (make-initial-state '(
1 8 9
3 2 4
6 5 7)))

;;; easy or hard to solve?  Why?
(setf s (make-initial-state '(
9 2 3
4 5 6
7 8 1)))

|#

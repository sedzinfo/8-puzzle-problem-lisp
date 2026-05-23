;;;; =============================================================
;;;; 8-Puzzle Solver
;;;; Searches: BFS, DFS, DFS-bounded, BFS-path, BFS-graph,
;;;;           Best-first, A*
;;;;
;;;; Puzzle representation: list of 9 elements, blank = 'b
;;;; Example: grid  1 2 3     => (1 2 3 4 b 5 6 7 8)
;;;;                4 _ 5
;;;;                6 7 8
;;;; =============================================================

;;; ---------------------------------------------------------------
;;; GLOBAL PARAMETERS
;;; ---------------------------------------------------------------

(defparameter n 0)               ; exploration trial counter (reset before each search)
(defparameter *depth-limit* 15)  ; default depth limit for DFS-bounded
(defparameter *dfs-max-trials* 200000) ; safety cap for DFS tree exploration
(defparameter *8-puzzle-rules* nil)

;;; ---------------------------------------------------------------
;;; MOVE OPERATORS
;;; ---------------------------------------------------------------

(defun can-blank-move-up (puzzle)
  (> (position 'b puzzle) 2))

(defun move-blank-up (puzzle)
  (let* ((new-puzzle (copy-list puzzle))
         (pos (position 'b puzzle))
         (new-pos (- pos 3)))
    (setf (nth pos new-puzzle) (nth new-pos puzzle))
    (setf (nth new-pos new-puzzle) 'b)
    new-puzzle))

(defun can-blank-move-down (puzzle)
  (< (position 'b puzzle) 6))

(defun move-blank-down (puzzle)
  (let* ((new-puzzle (copy-list puzzle))
         (pos (position 'b puzzle))
         (new-pos (+ pos 3)))
    (setf (nth pos new-puzzle) (nth new-pos puzzle))
    (setf (nth new-pos new-puzzle) 'b)
    new-puzzle))

(defun can-blank-move-left (puzzle)
  (let ((pos (position 'b puzzle)))
    (and (/= pos 0) (/= pos 3) (/= pos 6))))

(defun move-blank-left (puzzle)
  (let* ((new-puzzle (copy-list puzzle))
         (pos (position 'b puzzle))
         (new-pos (- pos 1)))
    (setf (nth pos new-puzzle) (nth new-pos puzzle))
    (setf (nth new-pos new-puzzle) 'b)
    new-puzzle))

(defun can-blank-move-right (puzzle)
  (let ((pos (position 'b puzzle)))
    (and (/= pos 2) (/= pos 5) (/= pos 8))))

(defun move-blank-right (puzzle)
  (let* ((new-puzzle (copy-list puzzle))
         (pos (position 'b puzzle))
         (new-pos (+ pos 1)))
    (setf (nth pos new-puzzle) (nth new-pos puzzle))
    (setf (nth new-pos new-puzzle) 'b)
    new-puzzle))

;;; ---------------------------------------------------------------
;;; RULE STRUCTURE & RULE SET
;;; ---------------------------------------------------------------

(defstruct rule precondition action)

(setf *8-puzzle-rules*
      (list (make-rule :precondition #'can-blank-move-left  :action #'move-blank-left)
            (make-rule :precondition #'can-blank-move-right :action #'move-blank-right)
            (make-rule :precondition #'can-blank-move-up    :action #'move-blank-up)
            (make-rule :precondition #'can-blank-move-down  :action #'move-blank-down)))

;;; ---------------------------------------------------------------
;;; CORE PUZZLE HELPERS
;;; ---------------------------------------------------------------

(defun equal-node (node1 node2)
  (equal node1 node2))

(defun applicable-p (rule state)
  (funcall (rule-precondition rule) state))

(defun apply-operator (rule state)
  (funcall (rule-action rule) state))

(defun puzz-children (node rules)
  (cond
    ((null rules) nil)
    ((applicable-p (car rules) node)
     (cons (apply-operator (car rules) node)
           (puzz-children node (cdr rules))))
    (t (puzz-children node (cdr rules)))))

(defun children-of (node)
  (puzz-children node *8-puzzle-rules*))

;;; ---------------------------------------------------------------
;;; DISPLAY
;;; ---------------------------------------------------------------

(defun tile-string (tile)
  (if (eq tile 'b) "_" (format nil "~A" tile)))

(defun print-puzzle-square (puzzle)
  (format t "~%+---+---+---+~%")
  (dotimes (row 3)
    (let ((start (* row 3)))
      (format t "|~2A |~2A |~2A |~%"
              (tile-string (nth start puzzle))
              (tile-string (nth (+ start 1) puzzle))
              (tile-string (nth (+ start 2) puzzle)))))
  (format t "+---+---+---+~%"))

(defun print-solution (path)
  "Prints each state in a solution path as a 3x3 grid with a step counter."
  (if (or (null path) (eq path 'fail))
    (format t "~%No solution found.~%")
    (progn
      (format t "~%========== SOLUTION (~A steps) ==========" (1- (length path)))
      (loop for state in path
            for step from 0
            do (format t "~%--- Step ~A ---" step)
               (print-puzzle-square state))
      (format t "~%==========================================~%"))))

;;; ---------------------------------------------------------------
;;; PATH STRUCTURE & COMPARATOR
;;; ---------------------------------------------------------------

(defstruct path-structure
  (nodes nil)
  (cost  0))

(defun better-path-p (path1 path2)
  (< (path-structure-cost path1) (path-structure-cost path2)))

;;; ---------------------------------------------------------------
;;; HEURISTIC: number of misplaced tiles
;;; ---------------------------------------------------------------

(defun h (state goal)
  "Returns the number of tiles not in their goal position."
  (cond
    ((null state) 0)
    ((equal-node state goal) 0)
    ((equal (car state) (car goal)) (h (cdr state) (cdr goal)))
    (t (+ 1 (h (cdr state) (cdr goal))))))

;;; ---------------------------------------------------------------
;;; PATH BUILDING UTILITIES
;;; ---------------------------------------------------------------

(defun make-new-paths (children parent)
  "Prepends each child to parent path; no cycle removal."
  (cond
    ((null children) nil)
    ((null parent)   nil)
    (t (cons (cons (car children) parent)
             (make-new-paths (cdr children) parent)))))

(defun make-new-paths-rcp (children parent)
  "Prepends each child to parent path; removes cycles."
  (cond
    ((null children) nil)
    ((member (car children) parent :test #'equal-node)
     (make-new-paths-rcp (cdr children) parent))
    (t (cons (cons (car children) parent)
             (make-new-paths-rcp (cdr children) parent)))))

(defun make-new-paths-rcpmsac (children parent)
  "Removes cycles; returns path-structures with cost = path length."
  (cond
    ((null children) nil)
    ((member (car children) parent :test #'equal-node)
     (make-new-paths-rcpmsac (cdr children) parent))
    (t (let ((new-path (cons (car children) parent)))
         (cons (make-path-structure :nodes new-path :cost (list-length new-path))
               (make-new-paths-rcpmsac (cdr children) parent))))))

(defun make-new-paths-rcpmsach (children parent goal)
  "Removes cycles; returns path-structures with cost = path length + h(state, goal)."
  (cond
    ((null children) nil)
    ((member (car children) parent :test #'equal-node)
     (make-new-paths-rcpmsach (cdr children) parent goal))
    (t (let ((new-path (cons (car children) parent)))
         (cons (make-path-structure :nodes new-path
                                    :cost  (+ (length new-path) (h (car children) goal)))
               (make-new-paths-rcpmsach (cdr children) parent goal))))))

;;; ---------------------------------------------------------------
;;; SEARCH ALGORITHMS
;;; ---------------------------------------------------------------

;;; BFS tree search (no path tracking)
;;;   nodeq  = list of states to explore, e.g. '((1 2 3 4 b 5 6 7 8))
;;;   goal   = target state,              e.g. '(1 2 3 4 5 6 7 8 b)
;;;   returns SUCCESS or FAIL
(defun breadth-first-search (nodeq goal)
  (cond
    ((null nodeq) 'fail)
    ((equal-node (car nodeq) goal) 'success)
    (t
     (format t "~%[BFS trial ~A]" (setf n (+ n 1)))
     (print-puzzle-square (car nodeq))
     (breadth-first-search
      (append (cdr nodeq) (children-of (car nodeq)))
      goal))))

;;; DFS tree search (no path tracking)
;;;   nodeq  = list of states to explore, e.g. '((1 2 3 4 b 5 6 7 8))
;;;   returns SUCCESS or FAIL
(defun depth-first-search (nodeq goal &optional (seen (make-hash-table :test 'equal)))
  (cond
    ((null nodeq) 'fail)
    ((> n *dfs-max-trials*) '(stopped-by-trial-limit))
    (t
     (let ((node (car nodeq)))
       (cond
         ((gethash node seen)
          (depth-first-search (cdr nodeq) goal seen))
         ((equal-node node goal) 'success)
         (t
          (setf (gethash node seen) t)
          (format t "~%[DFS trial ~A]" (setf n (+ n 1)))
          (print-puzzle-square node)
          (depth-first-search
           (append
            (remove-if (lambda (child) (gethash child seen))
                       (children-of node))
            (cdr nodeq))
           goal
           seen)))))))

;;; DFS with exploration-count depth bound
;;;   resets n to 0 each call; stops when n exceeds *depth-limit*
(defun depth-first-search-bounded (nodeq goal)
  (cond
    ((null nodeq) 'fail)
    ((equal-node (car nodeq) goal) 'success)
    ((> n *depth-limit*) '(stopped-by-depth-limit))
    (t
     (format t "~%[DFS-bounded trial ~A]" (setf n (+ n 1)))
     (print-puzzle-square (car nodeq))
     (depth-first-search-bounded
      (append (children-of (car nodeq)) (cdr nodeq))
      goal))))

;;; BFS with path tracking (no cycle removal)
;;;   pathq = list of paths, each path is a list of states (head = current state)
;;;           e.g. '(((1 2 3 4 b 5 6 7 8)))
;;;   returns the solution path (reversed to start→goal) or FAIL
(defun breadth-first-path-search (pathq goal)
  (cond
    ((null (car pathq)) 'fail)
    ((equal-node (caar pathq) goal) (reverse (car pathq)))
    (t
     (format t "~%[BFS-path trial ~A]" (setf n (+ n 1)))
     (print-puzzle-square (caar pathq))
     (breadth-first-path-search
      (append (cdr pathq)
              (make-new-paths (children-of (caar pathq)) (car pathq)))
      goal))))

;;; BFS graph search (with cycle removal + path tracking)
;;;   pathq = list of paths, e.g. '(((1 2 3 4 b 5 6 7 8)))
;;;   returns the solution path (reversed to start→goal) or NIL
(defun breadth-first-graph-search (pathq goal)
  (unless (null pathq)
    (let ((current-path (car pathq)))
      (if (equal-node (car current-path) goal)
        (reverse current-path)
        (progn
          (format t "~%[BFS-graph trial ~A]" (setf n (+ n 1)))
          (print-puzzle-square (car current-path))
          (breadth-first-graph-search
           (append (cdr pathq)
                   (make-new-paths-rcp (children-of (car current-path)) current-path))
           goal))))))

;;; Best-first search (sorted by path cost = path length)
;;;   pathq = list of path-structures
;;;           e.g. (list (make-path-structure :nodes '((1 2 3 4 b 5 6 7 8)) :cost 0))
;;;   returns the solution path (reversed to start→goal) or NIL
(defun best-first-search (pathq goal)
  (unless (null pathq)
    (if (equal-node (car (path-structure-nodes (car pathq))) goal)
      (reverse (path-structure-nodes (car pathq)))
      (progn
        (format t "~%[Best-first trial ~A]" (setf n (+ n 1)))
        (print-puzzle-square (car (path-structure-nodes (car pathq))))
        (best-first-search
         (sort (append (remove (car pathq) pathq)
                       (make-new-paths-rcpmsac
                        (children-of (car (path-structure-nodes (car pathq))))
                        (path-structure-nodes (car pathq))))
               #'better-path-p)
         goal)))))

;;; A* search (sorted by f = g + h, where g = path length, h = misplaced tiles)
;;;   pathq = list of path-structures
;;;           e.g. (list (make-path-structure :nodes '((1 2 3 4 b 5 6 7 8)) :cost 0))
;;;   returns the solution path (reversed to start→goal) or NIL
(defun astar (pathq goal)
  (unless (null pathq)
    (if (equal-node (car (path-structure-nodes (car pathq))) goal)
      (reverse (path-structure-nodes (car pathq)))
      (progn
        (format t "~%[A* trial ~A]" (setf n (+ n 1)))
        (print-puzzle-square (car (path-structure-nodes (car pathq))))
        (astar
         (sort (append (remove (car pathq) pathq)
                       (make-new-paths-rcpmsach
                        (children-of (car (path-structure-nodes (car pathq))))
                        (path-structure-nodes (car pathq))
                        goal))
               #'better-path-p)
         goal)))))

;;; ---------------------------------------------------------------
;;; CONVENIENCE WRAPPERS
;;; Call these with just a start state and goal state (as flat lists).
;;; ---------------------------------------------------------------

(defun bfs (start goal-state)
  "BFS tree search.  Returns SUCCESS or FAIL.
   Example: (bfs '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))"
  (setf n 0)
  (time (breadth-first-search (list start) goal-state)))

(defun dfs (start goal-state)
  "DFS tree search.  Returns SUCCESS or FAIL.
   Example: (dfs '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))"
  (setf n 0)
  (time (depth-first-search (list start) goal-state (make-hash-table :test 'equal))))

(defun dfs-bounded (start goal-state &optional (limit 15))
  "DFS with an exploration-count bound (default 15).
   Example: (dfs-bounded '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 b 5 6 7 8))"
  (setf n 0 *depth-limit* limit)
  (time (depth-first-search-bounded (list start) goal-state)))

(defun bfps (start goal-state)
  "BFS path search.  Returns solution path from start to goal, or FAIL.
   Example: (bfps '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))"
  (setf n 0)
  (let ((result (time (breadth-first-path-search (list (list start)) goal-state))))
    (print-solution result)
    result))

(defun btgs (start goal-state)
  "BFS graph search (cycle removal).  Returns solution path, or NIL.
   Example: (btgs '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))"
  (setf n 0)
  (let ((result (time (breadth-first-graph-search (list (list start)) goal-state))))
    (print-solution result)
    result))

(defun best (start goal-state)
  "Best-first search (sorted by path cost).  Returns solution path, or NIL.
   Example: (best '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))"
  (setf n 0)
  (let ((result (time (best-first-search
                       (list (make-path-structure :nodes (list start) :cost 0))
                       goal-state))))
    (print-solution result)
    result))

(defun a* (start goal-state)
  "A* search (f = path length + misplaced tiles).  Returns solution path, or NIL.
   Example: (a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))"
  (setf n 0)
  (let ((result (time (astar
                       (list (make-path-structure :nodes (list start) :cost 0))
                       goal-state))))
    (print-solution result)
    result))

;;; ---------------------------------------------------------------
;;; SAMPLE TESTS
;;; Uncomment any block to run it.
;;; ---------------------------------------------------------------

(format t "~%~%=== 8-Puzzle Solver Ready ===")
(format t "~%Wrappers (start goal):  bfs  dfs  dfs-bounded  bfps  btgs  best  a*")
(format t "~%Example: (a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))~%")

#|
;;; --- BFS / DFS (return SUCCESS or FAIL) ---
(bfs '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))  ; 1 ply
(bfs '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 5 6 7 8))  ; 2 ply
(bfs '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))  ; 5 ply
(bfs '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 7 6 8 5))  ; 10 ply

(dfs '(1 2 3 4 5 b 6 7 8) '(1 2 3 4 b 5 6 7 8))
(dfs '(1 2 3 4 b 5 6 7 8) '(1 b 3 4 2 5 6 7 8))

;;; --- DFS with depth bound ---
(dfs-bounded '(1 2 3 4 5 6 b 7 8) '(1 2 3 4 b 5 6 7 8) 15)
(dfs-bounded '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 b 5 6 7 8) 15)

;;; --- BFS path / BFS graph (return solution path) ---
(bfps '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))  ; 5 ply
(bfps '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 b 7 6 8 5))  ; 8 ply

(btgs '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))  ; 5 ply
(btgs '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 7 6 8 5))  ; 10 ply

;;; --- Best-first & A* (return solution path) ---
(best '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))  ; 1 ply
(best '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 5 6 7 8))  ; 2 ply
(best '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))  ; 5 ply
(best '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 7 6 8 5))  ; 10 ply

(a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 b 4 5 6 7 8))    ; 1 ply
(a* '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 5 6 7 8))    ; 2 ply
(a* '(1 2 3 4 b 5 6 7 8) '(2 4 3 1 7 5 6 b 8))    ; 5 ply
(a* '(1 2 3 4 b 5 6 7 8) '(b 2 3 1 4 7 6 8 5))    ; 10 ply
(a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 8 b 6 5 7))    ; 15 ply
(a* '(1 2 3 4 b 5 6 7 8) '(4 1 3 2 b 8 6 5 7))    ; 20 ply
(a* '(1 2 3 4 b 5 6 7 8) '(1 2 3 4 5 6 7 8 b))    ; goal state
|#

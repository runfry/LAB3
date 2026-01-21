;;; ---------------------------------------------------------------
;;; LAB WORK 3
;;; Variant 6 (Algorithm 1): Selection Sort (Non-decreasing)
;;; ---------------------------------------------------------------

;;; ===============================================================
;;; 1. FUNCTIONAL APPROACH (Recursion, no assignment)
;;; ===============================================================

(defun get-min (lst cur-min)
  "Helper function: recursively finds the minimum element."
  (cond
    ((null lst) cur-min)
    ((< (car lst) cur-min) (get-min (cdr lst) (car lst)))
    (t (get-min (cdr lst) cur-min))))

(defun remove-first-occurrence (val lst)
  "Helper function: creates a new list without the first occurrence of val."
  (cond
    ((null lst) nil)
    ((= (car lst) val) (cdr lst)) ; Found element - skip it
    (t (cons (car lst) (remove-first-occurrence val (cdr lst))))))

(defun selection-sort-functional (lst)
  "Selection sort (functional): find min, add to head, recurse."
  (cond
    ((null lst) nil)
    (t
     (let ((min-val (get-min (cdr lst) (car lst)))) ; Find minimum
       (cons min-val 
             (selection-sort-functional 
              (remove-first-occurrence min-val lst))))))) ; Recurse for the rest

;;; ===============================================================
;;; 2. IMPERATIVE APPROACH (Loops, destructive changes)
;;; ===============================================================

(defun selection-sort-imperative (lst)
  "Selection sort (imperative): loops and destructive modification."
  ;; Create a copy to avoid modifying the original list
  (let ((result (copy-list lst))) 
    ;; Outer loop
    (do ((i result (cdr i))) 
        ((null i) result) ; Return result when done
      
      ;; Inner loop: find min in the unsorted part
      (let ((min-node i)) 
        (do ((j (cdr i) (cdr j))) 
            ((null j)) 
          (when (< (car j) (car min-node))
            (setf min-node j))) ; Update min-node reference
        
        ;; Swap values
        (rotatef (car i) (car min-node))))))

;;; ===============================================================
;;; 3. UNIT TESTS
;;; ===============================================================

(defun check-sort (name input expected)
  "Runs both sort implementations and checks the result."
  (format t "~%Testing: ~a~%" name)
  (format t "Input: ~a~%" input)
  
  ;; Check functional version
  (let ((res-func (selection-sort-functional input)))
    (format t "  Functional: ~:[FAILED~;passed~] => ~a~%" 
            (equal res-func expected) res-func))
  
  ;; Check imperative version
  (let ((res-imp (selection-sort-imperative input)))
    (format t "  Imperative: ~:[FAILED~;passed~] => ~a~%" 
            (equal res-imp expected) res-imp)))

(defun run-tests ()
  (format t "========== STARTING TESTS ==========~%")

  (check-sort "Test 1: Normal list"
              '(3 1 4 1 5 9 2 6)
              '(1 1 2 3 4 5 6 9))

  (check-sort "Test 2: Already sorted"
              '(1 2 3 4 5)
              '(1 2 3 4 5))

  (check-sort "Test 3: Reverse sorted"
              '(5 4 3 2 1)
              '(1 2 3 4 5))

  (check-sort "Test 4: Duplicates"
              '(4 2 4 2 4)
              '(2 2 4 4 4))

  (check-sort "Test 5: Single element"
              '(10)
              '(10))

  (check-sort "Test 6: Empty list"
              nil
              nil)
  
  (check-sort "Test 7: Negative numbers"
              '(-5 10 -3 0 2)
              '(-5 -3 0 2 10))

  (format t "~%========== TESTS FINISHED ==========~%"))

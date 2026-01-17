<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до програмування"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гуров Іван Олегович</p>
<p align="right"><b>Рік</b>: 2026</p>

## Загальне завдання

Реалізувати алгоритм сортування двома способами: функціонально (з використанням рекурсії без присвоювань) та імперативно (з використанням циклів та деструктивних операцій).

## Варіант 6

**Алгоритм:** Selection Sort (Сортування вибором)

**Порядок сортування:** За неспаданням (non-decreasing)

## Лістинг реалізації функціонального підходу

```lisp
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
    ((= (car lst) val) (cdr lst))
    (t (cons (car lst) (remove-first-occurrence val (cdr lst))))))

(defun selection-sort-functional (lst)
  "Selection sort (functional): find min, add to head, recurse."
  (cond
    ((null lst) nil)
    (t
     (let ((min-val (get-min (cdr lst) (car lst))))
       (cons min-val 
             (selection-sort-functional 
              (remove-first-occurrence min-val lst)))))))
```

### Опис алгоритму (функціональний підхід)

Функціональна реалізація сортування вибором працює наступним чином:

1. **get-min** - рекурсивно знаходить мінімальний елемент у списку
2. **remove-first-occurrence** - створює новий список без першого входження заданого значення
3. **selection-sort-functional** - основна функція, яка:
   - Знаходить мінімальний елемент у списку
   - Додає його на початок результату
   - Рекурсивно сортує залишок списку без цього елемента

Використовується чиста функціональна рекурсія без побічних ефектів та присвоювань.

## Лістинг реалізації imperативного підходу

```lisp
(defun selection-sort-imperative (lst)
  "Selection sort (imperative): loops and destructive modification."
  (let ((result (copy-list lst))) 
    (do ((i result (cdr i))) 
        ((null i) result)
      
      (let ((min-node i)) 
        (do ((j (cdr i) (cdr j))) 
            ((null j)) 
          (when (< (car j) (car min-node))
            (setf min-node j)))
        
        (rotatef (car i) (car min-node))))))
```

### Опис алгоритму (імперативний підхід)

Імперативна реалізація використовує класичний алгоритм сортування вибором:

1. Створюється копія вхідного списку для уникнення модифікації оригіналу
2. Зовнішній цикл **do** проходить по кожній позиції списку
3. Внутрішній цикл **do** знаходить мінімальний елемент у несортованій частині
4. **rotatef** міняє місцями поточний елемент з мінімальним
5. Використовується **setf** для оновлення посилання на мінімальний вузол

## Тестові набори

```lisp
(defun check-sort (name input expected)
  "Runs both sort implementations and checks the result."
  (format t "~%Testing: ~a~%" name)
  (format t "Input: ~a~%" input)
  
  (let ((res-func (selection-sort-functional input)))
    (format t "  Functional: ~:[FAILED~;passed~] => ~a~%" 
            (equal res-func expected) res-func))
  
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
```

## Тестування

```lisp
CL-USER> (run-tests)
========== STARTING TESTS ==========

Testing: Test 1: Normal list
Input: (3 1 4 1 5 9 2 6)
  Functional: passed => (1 1 2 3 4 5 6 9)
  Imperative: passed => (1 1 2 3 4 5 6 9)

Testing: Test 2: Already sorted
Input: (1 2 3 4 5)
  Functional: passed => (1 2 3 4 5)
  Imperative: passed => (1 2 3 4 5)

Testing: Test 3: Reverse sorted
Input: (5 4 3 2 1)
  Functional: passed => (1 2 3 4 5)
  Imperative: passed => (1 2 3 4 5)

Testing: Test 4: Duplicates
Input: (4 2 4 2 4)
  Functional: passed => (2 2 4 4 4)
  Imperative: passed => (2 2 4 4 4)

Testing: Test 5: Single element
Input: (10)
  Functional: passed => (10)
  Imperative: passed => (10)

Testing: Test 6: Empty list
Input: NIL
  Functional: passed => NIL
  Imperative: passed => NIL

Testing: Test 7: Negative numbers
Input: (-5 10 -3 0 2)
  Functional: passed => (-5 -3 0 2 10)
  Imperative: passed => (-5 -3 0 2 10)

========== TESTS FINISHED ==========
NIL
```

**Переваги імперативного підходу:**
- Ефективність використання пам'яті (in-place сортування)
- Зрозумілість для програмістів з імперативним досвідом

;Задание 7
;Напишите функцию, которая умножает на заданное число-аргумент все числа
;из заданного списка-аргумента, когда
;a) все элементы списка --- числа
;б) все элементы списка --- любые объекты

;a) C использованием функционалов

(defun mullall(k lst)
    (mapcar #'(lambda (x) (* k x)) lst)
)

;с использованием рекурсии

;cons

(defun mullall-r(k lst)
    (cond   ((null lst) nil)
            (t (cons (* k (car lst)) (mullall-r k (cdr lst))))
    )
)

;append

(defun mullall-r2(k lst rlst)
    (cond   ((null lst) rlst)
            (t (mullall-r2 k (cdr lst) (append rlst (cons (* k (car lst)) nil))))
    )
)

;б) все элементы списка --- любые объекты

; функционалы

(defun mullall-d(k lst)
    (mapcar 
        #'(lambda (x) 
                (cond ((numberp x)(* k x))
                      ((listp x) (mullall-d k x))
                      (t x)
                )
           ) lst)
)

;рекурсия

(defun mullall-dr(k lst)
    (cond ((null lst) nil)
          ((numberp (car lst)) (cons (* k (car lst)) (mullall-dr k (cdr lst))))
          ((listp (car lst)) (cons (mullall-dr k (car lst)) (mullall-dr k (cdr lst))))
          (t (cons (car lst) (mullall-dr k (cdr lst))))
    )
)

;Задание 8.Напишите функцию, select-between, которая из списка-аргумента, содержащего только числа, выбирает 
;только те, которые расположены между двумя указанными границами-аргументами и возвращает их в виде списка 
;(упорядоченного по возрастанию списка чисел (+ 2 балла)).


(defun is_between (left right a)
    (if (> a left) (< a right) (> a right))
)

(defun make_select_between (lst l r)
    (reduce 
        #'(lambda (res el) (if (is_between l r el) (append res (cons el Nil)) res)) 
        lst :initial-value nil
    ) 
)

(defun insert_elem (elem list)
    (cond 
        ((null list) (cons elem nil))
        ((< elem (car list)) (cons elem list))
        (t  (cons (car list) (insert_elem elem (cdr list))))
    )
)

(defun my_sort (list)
    (reduce 
        #'(lambda (sorted tmp)
            (insert_elem tmp sorted)
        ) list :initial-value nil
    )
)

(defun sorted_select_between(left right list)
    (my_sort (make_select_between list left right))
)

;Задание 2. Напишите функцию, которая уменьшает на 10 все числа из списка аргумента этой функции. 

(defun minus_ten(lst)
    (mapcar
        #'(lambda (x)
            (cond
                ((numberp x) (- x 10))
                ((listp x) (minus_ten x))
                (t x)
            )
        ) lst
    ) 
)

(defun minus_ten_r (lst)
    (cond
        ((null lst) nil)
        ((numberp (car lst)) (cons (- (car lst) 10) (minus_ten_r (cdr lst)) ))
        ((listp (car lst)) (cons (minus_ten_r (car lst)) (minus_ten_r (cdr lst)) ))
        (t (cons (car lst) (minus_ten_r (cdr lst)) ))
    )
)

;3. Написать функцию, которая возвращает первый аргумент списка -аргумента.
;который сам является непустым списком. 

(defun first_listarg_el (lst)
    (cond
        ((null lst) nil)
        ((and (listp (car lst)) (not (null (car lst)))) (caar lst))
        (t (first_listarg_el (cdr lst)))
    )
)

(defun first_listarg (lst)
    (car ( find-if (lambda (x) (and (listp x) (not (null x)))) lst
    ))
)

;сумма элементов смешанного структурированного списка
;рекурсия
(defun my_sum_r(lst res)
    (cond
        ((null lst) res)
        ((numberp (car lst))  (my_sum_r (cdr lst)(+ res (car lst))))
        ((listp (car lst)) (my_sum_r (cdr lst) (my_sum_r (car lst) res)))
        (t (my_sum_r (cdr lst) res))
    )
)

;функционал

(defun my_sum (lst)
    (reduce #'(lambda (res x)
        (cond
            ((numberp x) (+ res x))
            ((listp x) (+ (my_sum x) res))
            (t res)
        )) lst :initial-value 0)
)

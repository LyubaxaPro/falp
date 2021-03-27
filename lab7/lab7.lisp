;-------------------------------------------------
;1 задание

; 1 способ

(defun my_reverse(lst)
    (reduce
        #'(lambda (res tmp) 
            (cons tmp res)
        ) lst :initial-value nil
    )
)


(defun is_pol(lst)
    (equal lst (my_reverse lst))
)

; 2 способ рекурсия

(defun my_rev_rec(lst rlst)
    (cond ((null lst) rlst)
          (t (my_rev_rec (cdr lst) (cons (car lst) rlst)))
    )
)

;Задание 2.
;Написать предикат set-equal, который возвращает t, если два его множества-аргумента содержат одни и те же элементы, порядок которых не имеет значения.

;использование стандартных функций
(defun is_equal (seta setb)
    (and (subsetp seta setb) (subsetp setb seta))
)

; Использование функционалов

(defun is_in_set(el search_set)
    (reduce #'(lambda (a b) (or a b))
        (mapcar #'(lambda (x) (equal el x)) search_set)
    )   
)

(defun is_subset(seta setb) 
        (reduce #'(lambda (a b) (and a b))
            (mapcar #'(lambda (x) (is_in_set x setb))seta)
        )   
)

;стандартные функции
(defun is_equal (seta setb)
    (and (is_subset seta setb) (is_subset setb seta))
)

;рекурсия

(defun is_in_set_r (el search_set)
    (cond ((null search_set) nil)
          ((equal el (car search_set)) T)
          (t (is_in_set_r el (cdr search_set)))
    )
)

(defun is_subset_r (seta setb)
    (cond ((null seta) t)
          ((is_in_set_r (car seta) setb) (is_subset_r (cdr seta) setb))
          (t nil)
    )
)

(defun is_equal (seta setb)
    (and (is_subset_r seta setb) (is_subset_r setb seta))
)

;Задание 3.
;Напишите необходимые функции, которые обрабатывают таблицу и точечных пар:(страна, столица), и возвращают по стране - столицу, по столице - страну.

;Функционалы
(defun find_by_func(val lst)
    (find-if (lambda (x) (not (null x)))
        (mapcar #'(lambda (pair) 
            (cond ((equal (car pair) val) (cdr pair))
            ((equal (cdr pair) val) (car pair))
            ))lst)
    )
)

;Рекурсия

(defun find_by_func_r (val lst)
    (cond ((null lst) nil)
          ((equal (caar lst) val) (cdar lst))
          ((equal (cdar lst) val) (caar lst))
          (t (find_by_func_r val (cdr lst)))
    )
)

;4. Напишите функцию swap-first-last, которая переставляет в списке-аргументе первый и последний элементы.

(defun my_reverse(lst)
    (reduce
        #'(lambda (res tmp) 
            (cons tmp res)
        ) lst :initial-value nil
    )
)


(defun swap-first-last (list)
    (reduce 
        #'(lambda (result tmp)
            (cond ((equal (length result) (- (length list) 1)) 
                    (my_reverse (cons  (car list) result)))
                   ( t (cons tmp result ))
            )
        ) (cdr list)  :initial-value (last list) 
    ) 
)

;5. Напишите функцию swap-two-ellement, которая переставляет в списке- 
;аргументе два указанных своими порядковыми номерами элемента в этом списке.

;возвращает n-ый хвост
(defun my_nthcdr (n list)
    (reduce 
        #'(lambda (result tmp)
            (if (equal (length result) (- (length list) n))
                result 
                (cdr result)
            ) 
        ) list :initial-value list
    )
)

;возвращает n 
(defun my_nth (n list)
    (car (my_nthcdr n list))
)

(defun swap-two-ellement (left right lst)
    (reduce 
        #'(lambda (result tmp)
            (cond 
                (   
                    (equal (length result) left)  
                    (append result (list (my_nth right lst)))
                ) 

                (   
                    (equal (length result) right)   
                    (append result (list (my_nth left lst)))
                ) 

                (   
                    t 
                    (append result (list tmp))
                )
            )
        ) lst :initial-value nil
    )
)

;6. Напишите две функции, swap-to-left и swap-to-right, которые производят круговую перестановку в списке-аргументе влево и вправо, соответственно. 

(defun swap-to-left (lst)
    (reduce 
        #'(lambda (tmp result)
            (cons  tmp result)) 
     (cdr lst) :initial-value (list (car lst)) :from-end t
    )
)

(defun my_reverse(lst)
    (reduce
        #'(lambda (res tmp) 
            (cons tmp res)
        ) lst :initial-value nil
    )
)

(defun swap-to-right (list)
    (reduce 
        #'(lambda (result tmp)
            (cond ((equal (length result) (length list)) (my_reverse result))
                ( t (cons tmp result))
            )
        ) list :initial-value (last list) 
    )
)


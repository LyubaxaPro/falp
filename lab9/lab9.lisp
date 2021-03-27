;Задание 4. Написать функцию, которая выбирает из заданного списка только те числа, которые больше 1 и меньше 10. 

;рекурсия

(defun is_between (left right a)
    (if (> a left) (< a right) (> a right))
)

(defun getfromto_r (lst left right)
    ( cond 
        ((null lst) Nil)
        (
            (listp (car lst)) 
            (nconc 
                (getfromto_r (car lst) left right)
                (getfromto_r (cdr lst) left right))
        )
        (
            (and 
                (numberp (car lst))
                (is_between left right (car lst))
            )
            (cons (car lst) (getfromto_r (cdr lst) left right))
        )

        (t (getfromto_r (cdr lst) left right))
    )
)

;функционалы

(defun is_between (left right a)
    (if (> a left) (< a right) (> a right))
)

(defun make_select_between (lst left right)
    (reduce 
        #'(lambda (res el) 
            (cond
                ((and (numberp el) (is_between left right el)) (append res (cons el Nil)))
                ((listp el) (append res (make_select_between el left right)))
                (t res)
            ) 
           
        )lst :initial-value nil
    ) 
)

;Задание 5. Написать функцию, вычисляющую декартово произведение двух своих списков аргументов. 
;( Напомним, что А х В это множество всевозможных пар (a b), где а ), где а принадлежит А, принадлежит В.)

(defun decart (a b)
    (mapcan
        #'(lambda (x) 
            (mapcar #'(lambda (y) (list x y)) b )
        ) a 
    )
)

;рекурсия

(defun dec_rec(el lst)
    ( cond 
        ((null lst) Nil)
        (t (cons (cons el (cons (car lst) nil)) (dec_rec el (cdr lst))))
    )
)


(defun decart_rec(a b)
    ( cond 
        ( (null a) nil)
        (t (nconc (dec_rec (car a) b) (decart_rec (cdr a) b)))
    )
)

;7. Пусть list-of-list список, состоящий из списков. Написать функцию, которая
;вычисляет сумму длин всех элементов list-of-list, т.е. например для аргумента ((1 2) (3 4)) -> 4.

(defun sum_of_len_r(lst res)
    ( cond 
        ((null lst) res)
        ((listp (car lst)) (sum_of_len_r (cdr lst) (+ res (length (car lst)))))
        (t (sum_of_len_r (cdr lst) (+ res 1)))
    )
)

(defun sum_of_len(lst)
    (sum_of_len_r lst 0)
)

(defun sum_of_len_f (lst)
    (reduce
        #'(lambda (res el)
            (cond
                ((listp el) (+ res (length el)))
                (t (+ res 1))
            )
        )lst :initial-value 0
    ) 
)


;Используя рекурсию, написать функцию, которая по исходному списку строит список квадратов чисел смешанного структурированного списка.


(defun list_of_square (lst res)
    (cond
        ((null lst) nil)
        ((numberp (car lst)) (list_of_square (cdr lst) (cons (* (car lst) (car lst)) res)))
        ((listp (car lst)) (cons (list_of_square (car lst) res) (list_of_square (cdr lst) res)))
        (t (list_of_square (cdr lst) res))
    )
)


(defun make-square-lsts (lst)
    (cond 
        ((null lst) Nil)
        ((symbolp (car lst)) (make-square-lsts (cdr lst)))
        ((numberp (car lst)) (cons (* (car lst) (car lst)) (make-square-lsts (cdr lst)))) 
        (T (nconc (make-square-lsts (car lst)) (make-square-lsts (cdr lst))))
    )
)

(defun make_sq_l(lst) 
    (mapcan  
        #'(lambda (x) 
            (cond ((listp x) (make_sq_l x)) 
            ((numberp x) (* x x)) 
            (t x) 
            ) 
        ) 
        lst 
    ) 
)




(defun reg-add-help(lst res)
    ( cond ((null lst) res)
    ((numberp (car lst)) (reg-add-help (cdr lst) (+ res (car lst))))
    ((listp (car lst)) (reg-add-help (cdr lst) (reg-add-help (car lst) res)))
    (t (reg-add-help (cdr lst) res))
    )
)


(defun recnth(num lst)
    ( cond ((null lst) nil)
    ((equal num 0) (car lst))
    (t (recnth (- num 1) (cdr lst)))
    )
)


(defun alloddr_r(lst rst)
    ( cond  ((null rst) nil)
    ((null lst) rst)
    ((listp (car lst)) (alloddr_r (cdr lst) (alloddr_r (car lst) rst)))
    ((not(and (numberp (car lst)) (oddp (car lst)))) nil)
    (t (alloddr_r (cdr lst) rst))
    )
)

(defun alloddr(lst) (alloddr_r lst t))


(defun my_last(lst)
    ( cond ((null (cdr lst)) (car lst))
    (t (my_last (cdr lst)))
    )
)

(defun get_n_sum(lst n)
 ( cond
 ((or (equal n -1) (null lst) ) 0)
 (t (+ (car lst) (get_n_sum (cdr lst) (- n 1) )))
 )
)


(defun from_n_to_end(lst n)
    ( cond ((or (null lst) (and (equal n 0) (< (car lst) 0)) ) 0) 
    ((equal n 0) (+ (car lst) (from_n_to_end (cdr lst) n)) )
    (t (from_n_to_end (cdr lst) (- n 1)))
    )
)


(defun lastodd_help(lst res)
 ( cond ((null lst) res)
 ((oddp (car lst)) (lastodd_help (cdr lst) (car lst)))
 (t (lastodd_help (cdr lst) res))
 )
)
(defun lastodd(lst) (lastodd_help lst nil))


(defun squares_all(lst)
 ( cond ((null lst) nil)
 (t (cons (* (car lst) (car lst)) (squares_all (cdr lst))))
 )
)


(defun select-odd-h(lst res)
    ( cond ((null lst) (cdr res))
           ((and (numberp (car lst))(oddp (car lst)))
            (select-odd-h (cdr lst) (append res (list (car lst))))
            )
            ((listp (car lst))
            (select-odd-h (cdr lst)
            (append res (select-odd-h (car lst) '(nil)))
            )
            )
    (t (select-odd-h (cdr lst) res))
    )
)
(defun select-odd(lst) (select-odd-h lst '(nil)))



(defun select-odd(lst)
    (mapcan (lambda (x) (and (oddp x) (list x))) lst)
)


(defun select-odd(lst)
    (reduce 
        #'(lambda (x y) (if (oddp x) (cons x y) y)) 
        lst :initial-value Nil :from-end t
    )
)


(defun my_reverse(lst)
    (reduce
        #'(lambda (res tmp) 
            (cons tmp res)
        ) lst :initial-value nil
    )
)

(defun select-odd-rec(lst result) 
    (cond 
        ((null lst) (my_reverse result))
        (t (select-odd-rec (cdr lst) (if (oddp (car lst)) (cons (car lst) result) result))
        )
    )
)

(defun select-odd-3(lst) (select-odd-rec lst Nil))

(defun sum-all-odd-h(lst res)
    ( cond 
        ((null lst) res)
        (
            (and (numberp (car lst)) (oddp (car lst)))
            (sum-all-odd-h (cdr lst) (+ res (car lst)))
        )

        (
            (listp (car lst))
            (sum-all-odd-h (cdr lst)(sum-all-odd-h (car lst) res))
        )
        (t (sum-all-odd-h (cdr lst) res))
    )
)



(defun create-data (lst surname name patronymic salary age skill)
    (cons (list surname name patronymic salary age skill) lst)
)

(defun change-salary-all(lst param)
    (mapcar
        #'(lambda (x)
            (setf (cadddr x)(* param (cadddr x)))
        ) lst
    )
)

(defun change-salary-person (lst surname name patronymic param)
    (mapcar 
        #'(lambda (x)
            (if 
                (and 
                    (equal surname (car x))
                    (equal name (cadr x))
                    (equal patronymic (caddr x))
                )
                (setf (caddr x) (* param (cadddr x)))
            )
        ) lst

    )
)


(defun sum-salary (lst)
    (cond
        ((null lst) 0)
        (t (+ (car (cdddar lst)) (sum-salary (cdr lst))))
    )
)

(create-data '((A B C 40000 23 2)) 'Ol 'P 'Er 70000 24 6)

(setq table (create-data '((A B C 40000 23 2)) 'Ol 'P 'Er 70000 24 6))


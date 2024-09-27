#|Процедура, удаляющая соседние дубликаты.|#
;В этой процедуре мы использовали хвостовую рекурсию
;Сложность процедуры - O(n) 
(define (uniq xs)
  (if (null? xs)            
      '()                  
      (let loop ((current (car xs))   
                 (rest (cdr xs))      
                 (result '()))        
        (cond
          ((null? rest)           
           (reverse (cons current result))) 
          ((equal? current (car rest)) 
           (loop current (cdr rest) result))  
          (else                    
           (loop (car rest) (cdr rest) (cons current result)))))))

#|Процедура, удаляющая из списка все элементы,
подходящие под условие.|#
;В этой процедуре мы не использовали хвостовую рекурсию
;Сложность процедуры - O(n) 
(define (delete pred? xs)
  (cond
    ((null? xs) '())
    ((pred? (car xs)) (delete pred? (cdr xs)))
    (else (cons (car xs) (delete pred? (cdr xs))))))

#|Процедура, принимающая список коэффициентов
и переменную и вычисляющая значение полинома.|#
;В этой процедуре мы не использовали хвостовую рекурсию
;Сложность процедуры - O(n) 
(define (polynom coeffs x)
  (define (helper coeffs x power)
    (if (null? coeffs)
        0
        (+ (* (car coeffs) (expt x power))
           (helper (cdr coeffs) x (+ power 1)))))
  (helper (reverse coeffs) x 0))

#|Процедура, которая возвращает список, полученный
путём вставки элемента между элементами списка.|#
;В этой процедуре мы не использовали хвостовую рекурсию
;Сложность процедуры - O(n) 
(define (intersperse  e xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else (cons (car xs)
                (cons e
                      (intersperse e (cdr xs)))))))

#|Предикат, который возвращает истинну, если все
элементы списка xs удовлетворяет предуикату.|#
;В этой процедуре мы не использовали хвостовую рекурсию
;Сложность процедуры - O(n) 
(define (all? pred? xs)
  (or (null? xs)
      (and (pred? (car xs))
           (all? pred? (cdr xs)))))

#|Композиция функций (процедур) одного аргумента,
принимающая произвольное число процедур одного
аргумента и возвращающа процедуру, являющуюся
композицией этих процедур.|#
;В этой процедуре мы использовали хвостовую рекурсию
;Сложность процедуры - O(1) 
(define (o . functions)
  (lambda (n)
    (let recur ((funcs (reverse functions))
                (res n))
      (if (null? funcs)
          res
          (recur (cdr funcs) ((car funcs) res))))))
      

  

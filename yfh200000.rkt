#lang racket
(provide (all-defined-out)) 

;1. divisible-by-x?
; Returns a function that checks if a given number is divisible by x.
(define (divisible-by-x? x)
  ; Define a function that takes a number 'y' and returns true if 'y' is divisible by 'x'.
  (λ (y)                    
    (= (remainder y x) 0))) ; Returns true if 'y' is divisible by 'x'.

;2. function-4 
; Takes a procedure as input and applies it to the number 4.
(define function-4
   (λ (proc)
       (proc 4)))

;3. my-map 
; Maps a function 'f' over each element of the list 'lst'.
(define (my-map f lst)
  (if (null? lst)
      '() ; If the list is empty, return an empty list.
      ; Apply the function 'f' to the first element of 'lst' and recursively map 'f' over the rest of the list.
      (cons (f (car lst)) 
            (my-map f (cdr lst)))))

;4. pair-up 
; Pairs up corresponding elements of two lists into sublists.
(define (pair-up lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '() ; If either of the lists is empty, return an empty list.
      ; Pair up the first elements of both lists and recursively pair up the rest of the lists.
      (cons (list (car lst1) (car lst2))
            (pair-up (cdr lst1) (cdr lst2)))))

;5. classify
; Classifies elements of a list into two lists based on a given procedure.
(define (classify procedure lst)
  (define (classify-helper lst true-list false-list)
    (cond
      ((null? lst) (list (reverse true-list) (reverse false-list))) ; If the list is empty, return the classified lists.
      ((procedure (car lst))
       ; If the procedure returns true for the first element, add it to the true-list and continue recursively.
       (classify-helper (cdr lst) (cons (car lst) true-list) false-list))
      (else
       ; If the procedure returns false for the first element, add it to the false-list and continue recursively.
       (classify-helper (cdr lst) true-list (cons (car lst) false-list)))))
  ; Start the classification process with empty true and false lists.
  (classify-helper lst '() '()))

;6. luhn
; Implements the Luhn algorithm to validate credit card numbers.
(define (luhn n)
  ((lambda (luhn-loop)
     (luhn-loop n #t 0))
   (lambda (n odd? s)
     (begin
       (define (luhn-loop q odd? s)
         (if (zero? q)
             (zero? (modulo s 10))
             (begin
               ; Perform Luhn algorithm steps recursively.
               ((lambda (r)
                  (luhn-loop (quotient q 10)
                             (not odd?) ; Flip the odd/even flag
                             (+ s (quotient (* (if odd? 2 1) r) 10)
                                (remainder (* (if odd? 2 1) r) 10))))
                (remainder q 10)))))
       ; Start Luhn algorithm with initial parameters.
       (if (zero? n)
           (zero? (modulo s 10))
           (luhn-loop n #f 0))))))

;7. my-sorted?
; Checks if a list is sorted in non-decreasing order according to a given comparison function.
(define (my-sorted? cmp lst)
  (if (or (null? lst) 
          (null? (cdr lst)))  
      #t ; If the list has 0 or 1 elements, it is considered sorted.
      ; Check if each element is less than or equal to the next one according to the comparison function.
      (and (cmp (car lst) (cadr lst)) 
           (my-sorted? cmp (cdr lst)))))

;8. my-flatten
; Flattens a nested list structure into a single list.
(define (my-flatten l)
  (cond [(empty? l)      null] ; If the list is empty, return an empty list.
        [(not (list? l)) (list l)] ; If the element is not a list, return a list containing that element.
        [else
          ; Recursively flatten each sublist and concatenate them together.
          (append (my-flatten (first l)) 
                  (my-flatten (rest l)))]))

;9. upper-threshold
; Filters out elements from a list that are greater than or equal to a given threshold.
(define (upper-threshold lst threshold)
  (cond
    ((null? lst) '()) ; If the list is empty, return an empty list.
    ((< (car lst) threshold)
     ; If the first element is less than the threshold, keep it and recursively filter the rest of the list.
     (cons (car lst)
           (upper-threshold (cdr lst) threshold)))
    (else
     ; If the first element is greater than or equal to the threshold, skip it and recursively filter the rest.
     (upper-threshold (cdr lst) threshold))))

;10. my-list-ref
; Retrieves the element at a specified index from a list.
(define (my-list-ref lst n)
  (let ((result
         ; Use foldl to traverse the list while keeping track of the current index.
         (foldl (lambda (ele acc)
                  (cond
                    ((first acc) acc) ; If the result is already found, return it.
                    ((= n (second acc)) (list #t ele)) ; If the index matches, return the element.
                    (else (list #f (add1 (second acc)))))) ; Otherwise, continue with the next index.
                (list #f 0) ; Start with index 0 and no result found.
                lst)))
    (if (first result)
        (second result) ; If the result is found, return it.
        (error "ERROR: Index out of bounds")))) ; If the index is out of bounds, raise an error.

;11. deep-map
; Maps a function over each element of a nested list structure.
(define (deep-map f lst)
  (if (null? lst)
      '() ; If the list is empty, return an empty list.
      (if (list? (car lst))
          ; If the first element is a list, recursively apply 'deep-map' to it and the rest of the list.
          (cons (deep-map f (car lst))
                (deep-map f (cdr lst)))
          ; If the first element is not a list, apply the function 'f' to it and continue recursively.
          (cons (f (car lst))
                (deep-map f (cdr lst))))))


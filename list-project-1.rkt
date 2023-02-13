; Michael Granberry
; COMP 333 - Spring 2023
; List Project 1

#lang racket
#|
(rotate-left-1 x)
x: list
|#
(define (rotate-left-1 x)
  (cond
    ((empty? x) x)
    (else (append (cdr x) (list (car x))))
    )
  )

#|
(rotate-left-n x n)
x: list
n: number of left rotations
|#
(define (rotate-left-n x n)
  (cond
    ((= n 0) x)
    (else (append (rotate-left-n (cdr x) (- n 1)) (list (car x))))
    )
  )

#|
(count-items x)
x: list
|#
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items (cdr x))))
    )
  )

#|
(list-item-n x n)
x: list
n: item at index n
|#
(define (list-item-n x n)
  (cond
    ((= n 0) (car x))
    (else (list-item-n (cdr x) (- n 1)))
    )
  )

#|
(list-minus-item-n x n)
x: list
n: remove item at index n
|#
(define (list-minus-item-n x n)
  (cond
    ((= n 0) (cdr x))
    (else (append (list (car x)) (list-minus-item-n (cdr x) (- n 1))))
    )
  )

#|
(rotate-right-1 x)
x: list
|#
(define (rotate-right-1 x)
  (append (list (car (reverse-list x))) (list-minus-item-n x (- (count-items x) 1)))
  )

#|
(reverse-list x)
x: list
|#
(define (reverse-list x)
  (cond
    ((empty? x) '())
    (else (append (reverse-list (cdr x)) (list (car x))))
    )
  )

#|
(cons-to-all a x)
a: item being cons to x
x: list of lists
|#
(define (cons-to-all a x)
  (cond
    ((= (length x) 1) (list (cons a (car x))))
    (else (append (list (cons a (car x))) (cons-to-all a (cdr x))))
    )
  )

#|
(cons-to-all-map a x)
a: item being cons to x
x: list of lists
|#
(define (cons-to-all-map a x)
  (map (lambda (z) (cons a z)) x)
  )

#|
ph-1
x: list
n: index
|#
(define (ph-1 x n)
  (cons-to-all (list-item-n x n) (permute (list-minus-item-n x n)))
  )  

#|
ph-2
x: list
n: index
|#
(define (ph-2 x n)
  (cond 
    ((= n 0) (ph-1 x n))
    (else (ph-1 x (- n 1)))
    )
  )

#|
(permute x)
x: list
|#
(define (permute x)
  (cond
    ((empty? x) null)
    ((empty? (cdr x)) (list x))
    ((empty? (cddr x)) (list (reverse x)))
    (else (ph-2 x (- (length x) 1)))
    )
  )
    


;--------------------------------------------------------------
; Test cases (rotate-left-1 x)
(displayln "Test cases for (rotate-left-1 x)")
(rotate-left-1 '())
(rotate-left-1 '(a))
(rotate-left-1 '(a b c))

;--------------------------------------------------------------
(displayln "")
; Test cases (rotate-left-n x n)
(displayln "Test cases for (rotate-left-n x n)")
(rotate-left-n '(a b c) 0)
(rotate-left-n '(a b c d e) 2)
(rotate-left-n '(a b c d e) 5)

;--------------------------------------------------------------
(displayln "")
; Test cases (count-items x)
(displayln "Test cases for (count-items x)")
(count-items '())
(count-items '(a))
(count-items '(a b c d e))

;--------------------------------------------------------------
(displayln "")
; Test cases (list-item-n x n)
(displayln "Test cases for (list-item-n x n)")
(list-item-n '(a b c d e) 0)
(list-item-n '(a b c d e) 4)
(list-item-n '(a b c d e) 1)

;--------------------------------------------------------------
(displayln "")
; Test cases (list-minus-item-n x n)
(displayln "Test cases for (list-minus-item-n x n)")
(list-minus-item-n '(a b c d e) 0)
(list-minus-item-n '(a b c d e) 1)
(list-minus-item-n '(a b c d e) 2)
(list-minus-item-n '(a b c d e) 4)

;--------------------------------------------------------------
(displayln "")
; Test cases (rotate-right-1 x)
(displayln "Test cases for (rotate-right-1 x)")
(rotate-right-1 '(a b c d e))
(rotate-right-1 '(a))
(rotate-right-1 '(a b))
(rotate-right-1 '(a b c d e f g))

;--------------------------------------------------------------
(displayln "")
; Test cases (reverse-list x)
(displayln "Test cases for (reverse-list x)")
(reverse-list '(a))
(reverse-list '(a b))
(reverse-list '(a b c d e))

;--------------------------------------------------------------
(displayln "")
; Test cases (cons-to-all a x)
(displayln "Test cases for (cons-to-all a x)")
(cons-to-all 'a '((b c) (d e) (f g)))

;--------------------------------------------------------------
(displayln "")
; Test cases (cons-to-all-map a x)
(displayln "Test cases for (cons-to-all-map a x)")
(cons-to-all 'a '((b c) (d e) (f g)))

;--------------------------------------------------------------
(displayln "")
; Test cases (ph-1 x n)
(displayln "Test cases for (ph-1 x n)")
(ph-1 '(a b c d) 0)
(ph-1 '(a b c d) 1)
(ph-1 '(a b c d) 2)
(ph-1 '(a b c d) 3)

;--------------------------------------------------------------
(displayln "")
; Test cases (ph-2 x)
(displayln "Test cases for (ph-2 x)")
(ph-1 '(a b c d) 0)
(ph-1 '(a b c d) 1)
(ph-1 '(a b c d) 2)
(ph-1 '(a b c d) 3)

;--------------------------------------------------------------
(displayln "")
; Test cases (permute x)
(displayln "Test cases for (permute x)")
(permute '(a b))
(permute '(a b c))
(permute '(a b c d))
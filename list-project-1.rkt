#lang racket

; ---------------------------------------
; Michael Granberry
; COMP 333 - Spring 2023
; List Project 1
; ---------------------------------------

#|
(rotate-left-1 x)
x: list of elements
returns: this function returns a list x rotated once to the left
|#
(define (rotate-left-1 x)
  (cond
    ((empty? x) x)
    (else (append (cdr x) (list (car x))))
    )
  )

#|
(rotate-left-n x n)
x: list of elements
n: number of left rotations
returns: this function returns a list x rotated n times to the left
|#
(define (rotate-left-n x n)
  (cond
    ((= n 0) x)
    (else (rotate-left-n (rotate-left-1 x) (- n 1)))
    )
  )

#|
(count-items x)
x: list of elements
returns: this function returns the number of elements in list x
|#
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items (cdr x))))
    )
  )

#|
(list-item-n x n)
x: list of elements
n: item at index n
returns: this function returns the element at index n in list x
|#
(define (list-item-n x n)
  (cond
    ((= n 0) (car x))
    (else (list-item-n (cdr x) (- n 1)))
    )
  )

#|
(list-minus-item-n x n)
x: list of elements
n: remove item at index n
returns: this function returns list x with the element at index n removed
|#
(define (list-minus-item-n x n)
  (cond
    ((= n 0) (cdr x))
    (else (append (list (car x)) (list-minus-item-n (cdr x) (- n 1))))
    )
  )

#|
(rotate-right-1 x)
x: list of elements
returns: this function returns a list x rotated once to the right
|#
(define (rotate-right-1 x)
  (append (list (car (reverse-list x))) (list-minus-item-n x (- (count-items x) 1)))
  )

#|
(reverse-list x)
x: list of elements
returns: this function returns list x in reverse order
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
returns: this function returns a list of lists with a cons'd to each element in the list
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
returns: this function returns a list of lists with a cons'd to each element in the list
|#
(define (cons-to-all-map a x)
  (map (lambda (z) (cons a z)) x)
  )



#|
(permute x)
x: list of elements
returns: This function returns a list of lists of all possible permutations of list x
|#
(define (permute x)
  (cond
    ((empty? x) null)
    ((empty? (cdr x)) (list x))
    ((empty? (cddr x)) (list x (reverse x)))
    (else (ph-2 x (- (length x) 1)))
    )
  )

#|
ph-1
x: list of elements
n: index of list
returns: This function returns a lists of lists where element at index n is cons to
all possible permutations of list x without the nth element
|#
(define (ph-1 x n)
  (cons-to-all (list-item-n x n) (permute (list-minus-item-n x n)))
  )  

#|
ph-2
x: list of elements
n: index of list
returns: This function returns a list of lists where each permutation iteration is appended together
|#
(define (ph-2 x n)
  (cond 
    ((= n 0) (ph-1 x n))
    (else (append (ph-1 x n) (ph-2 x (- n 1))))
    )
  )


;--------------------------------------------------------------
; Test cases (rotate-left-1 x)
(displayln "Test cases for (rotate-left-1 x)")
(rotate-left-1 '()) ; '()
(rotate-left-1 '(a)) ;'(a)
(rotate-left-1 '(a b c)) ; '(b c a)

;--------------------------------------------------------------
(displayln "")
; Test cases (rotate-left-n x n)
(displayln "Test cases for (rotate-left-n x n)")
(rotate-left-n '(a b c) 0) ; '(a b c)
(rotate-left-n '(a b c d e) 2) ; '(c d e b a)
(rotate-left-n '(a b c d e) 5) ; '(a b c d e)

;--------------------------------------------------------------
(displayln "")
; Test cases (count-items x)
(displayln "Test cases for (count-items x)")
(count-items '()) ; 0
(count-items '(a)) ; 1
(count-items '(a b c d e)) ; 5

;--------------------------------------------------------------
(displayln "")
; Test cases (list-item-n x n)
(displayln "Test cases for (list-item-n x n)")
(list-item-n '(a b c d e) 0) ; 'a
(list-item-n '(a b c d e) 4) ; 'e
(list-item-n '(a b c d e) 1) ; 'b

;--------------------------------------------------------------
(displayln "")
; Test cases (list-minus-item-n x n)
(displayln "Test cases for (list-minus-item-n x n)")
(list-minus-item-n '(a b c d e) 0) ; '(b c d e)
(list-minus-item-n '(a b c d e) 1) ; '(a c d e)
(list-minus-item-n '(a b c d e) 2) ; '(a b d e)
(list-minus-item-n '(a b c d e) 4) ; '(a b c d)

;--------------------------------------------------------------
(displayln "")
; Test cases (rotate-right-1 x)
(displayln "Test cases for (rotate-right-1 x)")
(rotate-right-1 '(a b c d e)) ; '(e a b c d)
(rotate-right-1 '(a)) ; '(a)
(rotate-right-1 '(a b)) ; '(b a)
(rotate-right-1 '(a b c d e f g)) ; (g a b c d e f)

;--------------------------------------------------------------
(displayln "")
; Test cases (reverse-list x)
(displayln "Test cases for (reverse-list x)")
(reverse-list '(a)) ; '(a)
(reverse-list '(a b)) ; '(b a)
(reverse-list '(a b c d e)) ; '(e d c b a)

;--------------------------------------------------------------
(displayln "")
; Test cases (cons-to-all a x)
(displayln "Test cases for (cons-to-all a x)")
(cons-to-all 'a '((b c) (d e) (f g))) ; '((a b c) (a d e) (a f g))

;--------------------------------------------------------------
(displayln "")
; Test cases (cons-to-all-map a x)
(displayln "Test cases for (cons-to-all-map a x)")
(cons-to-all 'a '((b c) (d e) (f g))) ; '((a b c) (a d e) (a f g))

;--------------------------------------------------------------
(displayln "")
; Test cases (permute x)
(displayln "Test cases for (permute x)")
(permute '(a b)) ; '((a b) (b a))
(permute '(a b c)) ; '((c a b)(c b a)(b a c)(b c a)(a b c)(a c b))
(permute '(a b c d))
;'((d c a b) (d c b a) (d b a c) (d b c a) (d a b c)
; (d a c b) (c d a b) (c d b a) (c b a d) (c b d a)
; (c a b d) (c a d b) (b d a c) (b d c a) (b c a d)
; (b c d a) (b a c d) (b a d c) (a d b c) (a d c b)
; (a c b d) (a c d b) (a b c d) (a b d c))
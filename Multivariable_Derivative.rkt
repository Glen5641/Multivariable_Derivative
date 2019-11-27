#lang racket

#| Homework 5 Scheme Project |#
#| Solve the derivative for a multivariable function |#
#| Poly Add Sub Mul and Ders Functions |#

;Helper Function for Adding Polynomials
(define (add_helper x y)
  (if (null? x)
    y
    (if (null? y)
      x
      (cons
        (+ (car x) (car y))
        (add_helper (cdr x) (cdr y))))))

;Function for Adding Polynomials
(define (add l1 l2)
  (if (null? l1)
    l2
    (if (null? l2)
      l1
      (cons
        (add_helper (car l1) (car l2))
        (add (cdr l1) (cdr l2))))))

;Function for Inverting List to Negative
(define (invert l)
  (if (null? l)
    l
    (cons
        (- 0 (car l))
        (invert (cdr l)))))

;Helper Function for Subtracting Polynomials
(define (sub_helper x y)
  (if (null? x)
    (invert y)
    (if (null? y)
      x
      (cons
        (- (car x) (car y))
        (sub_helper (cdr x) (cdr y))))))

;Function for Subtracting Polynomials
(define (sub l1 l2)
  (if (null? l1)
    l2
    (if (null? l2)
      l1
      (cons
        (sub_helper (car l1) (car l2))
        (sub (cdr l1) (cdr l2))))))

;Function for Removing Zeros from List
(define (remz_helper x)
  (if (null? x)
    x
    (if (= (car x) 0)
      (remz_helper (cdr x))
      x)))

;Function for Removing Zeros from List of Lists
(define (remz l)
  (if (null? l)
    l
    (cons
      (remz_helper (car l))
      (remz (cdr l)))))

;Function for Removing empty lists from List of Lists
(define (reml l)
  (if (null? l)
    l
    (if (null? (car l))
      (reml (cdr l))
      l)))

;Function for Reversing List
(define (rev_helper x)
  (if (null? x)
    x
    (append
      (rev_helper (cdr x))
      (list (car x)))))

;Function for Reversing List of Lists
(define (rev l)
  (if (null? l)
    l
    (cons
      (rev_helper (car l))
      (rev (cdr l)))))

;Function for Obtaining Derivative of Polynomial
(define (der l x)
  (if (null? l)
    l
    (cons
      (* (car l) x)
      (der (cdr l) (+ x 1)))))

;Function for Obtaining Derivatives of Polynomial list
(define (poly_der l)
  (if (null? l)
    l
    (cons
      (cdr (der (car l) 0))
      (poly_der (cdr l)))))

;Function for Multiplying Single Variable over List
(define (mult l x)
  (if (null? l)
    l
    (cons
      (* (car l) x)
      (mult (cdr l) x))))

;Function for adding y zeros
(define (addz l x y)
  (if (null? l)
    l
    (if (>= x y)
      l
      (cons
        0
        (addz l (+ x 1) y)))))

;Function for multiplying 2 lists and adding zeros
(define (multl l1 l2 x)
  (if (null? l1)
    l1
    (cons
      (addz (mult l2 (car l1)) 0 x)
      (multl (cdr l1) l2 (+ x 1)))))

;Function for adding lists of lists
(define (addll l)
  (if (null? l)
    l
    (if (null? (cdr l))
      (car l)
      (addll (cons (add_helper (car l) (car (cdr l))) (cddr l))))))

;Function for Multiplying Single Variable Polynomials
(define (poly_multl l1 l2)
  (addll (multl l1 l2 0)))

;Function for adding lists of zeros
(define (addzl l x y)
  (if (null? l)
    l
    (if (>= x y)
      l
      (cons
        '(0)
        (addzl l (+ x 1) y)))))

;Function for multiplying list of lists and adds zero lists
(define (poly_multll l1 l2 x y)
  (if (null? l1)
    l1
    (if (null? l2)
      l2
      (cons
        (addzl (map (lambda (x)
            (poly_multl (car l1) x)) l2) x y)
        (poly_multll (cdr l1) l2 x (+ y 1))))))

;Function for adding lists of lists
(define (poly_addll l)
  (if (null? l)
    l
    (if (null? (cdr l))
      (car l)
      (poly_addll (cons (add (car l) (car (cdr l))) (cddr l))))))

;Function for multiplying multivariable polynomials
(define (poly_mult l1 l2)
  (poly_addll (poly_multll l1 l2 0 0)))

;Final Function for Adding Lists of Polynomials
(define (poly_add l1 l2)
  (rev_helper
    (reml
      (rev_helper
        (rev
          (remz
            (rev
              (add l1 l2))))))))

;Final Function for Subtracting Lists of Polynomials
(define (poly_sub l1 l2)
  (rev_helper
    (reml
      (rev_helper
        (rev
          (remz
            (rev
              (sub l1 l2))))))))

;Final Function for Obtaining Derivatives of Polynomial list
(define (poly_ders l)
  (rev_helper
    (reml
      (rev_helper
        (rev
          (remz
            (rev
              (poly_der l))))))))

;Final Function for Multiplying Lists of Polynomial
(define (poly_mul l1 l2)
  (rev_helper
    (reml
      (rev_helper
        (rev
          (remz
            (rev
              (poly_mult l1 l2))))))))

(poly_add '((1 2 3) (1 2 3) (1 2 3)) '((1 2 3) (1 2 3) (1 2 3)))
(poly_add '((1 -1) (1 2 3) () (3)) '((-1 1) (-1 2) (3)))
(poly_sub '((1 2 3) (1 2 3) (1 2 3)) '((1 2 3) (1 2 3) (1 2 3)))
(poly_sub '((1 -1) (1 2 3) () (3)) '((-1 1) (-1 2) (3)))
(poly_ders '((0 1 2 3 4) (1 2 3 4 5) (2 3 4 5 6)))
(poly_ders '((0 1 2) (1 2 3) (2 3 4)))
(poly_mul '((1 2 3) (1 2 3) (1 2 3)) '((1 2 3) (1 2 3) (1 2 3)))
(poly_mul '((1) (1 2 3) () (3)) '((-1) (-1 2) (3)))

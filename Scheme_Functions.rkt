#lang scheme

;Poly_addition function for poly_add
(define (poly_addition apol bpol)
  (if (null? apol)
      bpol
      (if (null? bpol)
          apol
          (cons (empty_list (adder (car apol) (car bpol)))
                (poly_addition (cdr apol) (cdr bpol))))))

;Addition Function
(define (adder apol bpol)
  ;Check if apol is null
  (if (null? apol)
      bpol
      ;Check if bpol is null
      (if (null? bpol)
          apol
          ;Append to beginning of list the sum of first two elements in apol and bpol
          ;Repeat until () is reached 
          (cons (+ (car apol)(car bpol)) (adder (cdr apol)(cdr bpol))))))

;Poly_subtract function for Poly_sub
(define (poly_subtract apol bpol)
  (if (null? apol)
      bpol
      (if (null? bpol)
          apol
          (cons (empty_list (subtractor (car apol) (car bpol)))
                (poly_subtract (cdr apol) (cdr bpol))))))

;Subtraction Function
(define (subtractor apol bpol)
  ;Check if apol is null
  (if (null? apol)
      bpol
      ;Check if bpol is null
      (if (null? bpol)
          apol
          ;Append to beginning of list the difference of the first two elements in apol
          ;and bpol. Repeat until () is reached
          (cons (- (car apol)(car bpol))(subtractor (cdr apol)(cdr bpol))))))

;Multiplier function that multiples apol by individual numbers in bpol
(define (multiplier apol n)
  (if (null? apol)
      apol
      (cons (* n (car apol)) (multiplier (cdr apol) n))))

;Exponent_correction function appends zeros to the beginning of apol depending
;on placement of number in bpol so that addition of the two lists is correct
(define (exponent_correction apol n)
  (if (= n 0)
      apol
      (exponent_correction (append '(0) apol) (- n 1))))

;Function that multiplies two lists together using multiplier
(define (poly_multiplier apol bpol)
  (if (null? bpol)
      '()
      (if (null? apol)
          '()
          (adder (exponent_correction (multiplier apol (car (reverse bpol))) (- (length bpol) 1))
                                    (poly_multiplier apol (reverse (cdr (reverse bpol))))))))

;Higher level function for multiplying two lists, so that it cans step into a list of lists
(define (poly_multix apol bpol)
  (if (null? bpol)
      '()
      (if (null? apol)
          '()
          (adder (poly_list_helper (poly_multiplier (car apol) (car (reverse bpol))) (- (length bpol) 1))
                    (poly_multix (cdr apol) (reverse (cdr (reverse bpol))))))))

;Poly_derivative function for Poly_derx
(define (poly_derivative apol)
  (if (null? apol)
      '()
      (cons (poly_derx_step (car apol)) (poly_derivative (cdr apol)))))

;Poly_derx_step Function
;Helps with organizing the recursions, but also starts the derivative process
(define (poly_derx_step apol)
  (if (null? apol)
      '()
      (if (null? (poly_derx_step_2 apol 0))
          '()
          (cdr (poly_derx_step_2 apol 0)))))

;Poly_derx_step_2 Function
;Helps poly_derx_step with actual derivation of numbers in list
(define (poly_derx_step_2 apol x)
  ;Checks if apol is null
  (if (null? apol)
      apol
      ;Multiplies first element in list by number of iterations (this represents the
      ;level of exponent on the x variable)
      (cons (* x (car apol)) (poly_derx_step_2 (cdr apol) (+ x 1)))))

;If list full of 0's, return an empty list
(define (empty_list cpol)
  (if (andmap (curry eq? 0) cpol)
      '() cpol))

;Helping poly_mul proper list placement based on y exponent
(define (poly_list_helper apol n)
  (if (= n 0)
      apol
      (poly_list_helper(append '() apol) (- n 1))))

;Function removes empty trailing lists
(define (remove_trailing_lists apol)
  (if (null? (car (reverse apol)))
      (remove_trailing_lists (reverse (cdr (reverse apol))))
      apol))

;Main Poly_add function 
(define (poly_add apol bpol)
  (if (null? apol)
      bpol
      (if (null? bpol)
          apol
          (remove_trailing_lists (poly_addition apol bpol)))))

;Main Poly_sub function
(define (poly_sub apol bpol)
  (if (null? apol)
      bpol
      (if (null? bpol)
          apol
          (remove_trailing_lists (poly_subtract apol bpol)))))

;Main Poly_mul function
(define (poly_mul apol bpol)
  (if (null? apol)
      '()
      (cons (poly_multix apol bpol) (poly_mul (cdr apol) bpol))))

;Main Poly_derx function
(define (poly_derx apol)
  (if (null? apol)
      '()
      (remove_trailing_lists (poly_derivative apol))))


(poly_add '(() (1 2 3) (3))  '((-1 1) (-1 2) (-3)))
(poly_sub '((1 1) (1 2 3 4) (3)) '((-1 1) (2 3 4) (3)))
(poly_mul '((1) (1 2 3) () (3))  '((-1) (-1 2) (3)))
(poly_derx '((1) (1 2 3) () (3)))

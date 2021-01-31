; Notes and code taken when viewing the following MIT lecture (1986):
; `Lecture 7B: Metacircular Evaluator, Parts 1 and 2`.
;
; - part 1 - https://www.youtube.com/watch?v=aAlR3cezPJg
; - part 2 - https://www.youtube.com/watch?v=QVEOq5k6Xi0
;
; The instructor is one of the following (not sure which):
;   - Harold Abelson
;   - Gerald Jay Sussman
;   - Julie Sussman

; Some shorthand:
;
; eval - evaluate
; exp  - expression
; env  - environment

; Some notation:
;
; ```
; (add a b c)
;   | |    |
;   |cdr   |
;     |    |
;     -----
;       | cdr
;
; (a b c (d e))
; ```
;
; car - the first element of the list
; cdr - the list, expect for the first element
;
; They can be combined like so:
;
; Between the first letter (`c`) and the last (`r`):
; - `a` is the "car of"
; - `d` is the "cdr of"
;
; So:
;
; `cadr` is "the car of the cdr"
; `cddr` is "the cdr of the cdr"
;
; See https://stackoverflow.com/a/13112481

(define eval
  (lambda (exp env)
          ; if its a number, ret the number
    (cond ((number ? exp) exp)

          ; if a symbol, look it up
          ((symbol ? exp) (lookup exp env))

          ; if it's a quote, return the quoted thing
          ((eq ? (car exp) 'quote) (cadr exp))
          
          ; if it's a lambda, return closures for the exp
          ((eq ? (car exp) 'lambda)
            (list 'closure (cdr exp) env))

          ; if a conditional, evaluate it
          ((eq ? (car exp) 'cond)
            (eval cond (cdr exp) env))
          
          ; default combination - evaluate the operator and apply it
          (else (apply (eval (car exp) env)
                       (eval (cdr exp) env))))))

; first 5 are special forms

; primitives
; lookup
; apply
; eval
; cond
; list
; evcond
; evlist

(define apply
  (lambda (proc args)
    (cond ((primitive ? proc)
           (apply-primop proc args))
          ((eq ? (car proc) 'closure)
            (eval (cadadr proc)
                  (bind (caadr proc)
                        args
                        (caddr proc))))
          ; might have to ask, is this compiled code?
          ; is this a fortran program to execute?
          ; note: there's a builtin assumption that this is lisp
          ;   there is an abstract version of this that is beyond the syntax,
          ;   as such, we could rewrite in algol
          (else error))))

(define evlist
  (lambda (l env)
    (cond ((eq ? l ()) '())
          (else
            (cons (eval (car l) env)
                  (evlist (cdr l) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq ? clauses '()) '())
          ((eq ? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false ? (eval (caar clauses) env))
           (evcond (cdr clauses) env))
          (else
            (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          end)))

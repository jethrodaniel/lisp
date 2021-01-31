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
  ; The universal function, i.e, `eval`.
  ;
  ; We need the help of a few things here:
  ; - evcond
  ; - evlist
  ; - lookup
  ; - bind
  ;
  (lambda (exp env)
          ; If it's a number, return the number.
          ;
          ; ```
          ; 3 -> 3
          ; ```
          ;
    (cond ((number ? exp) exp)

          ; If it's a symbol, look it up to get the value.
          ;
          ; ```
          ; x -> x
          ; ```
          ;
          ((symbol ? exp) (lookup exp env))

          ; If it's a quote, return the quoted entity.
          ;
          ; ```
          ; 'foo -> (quote foo) -> foo
          ; ```
          ;
          ; Note that `'literal` is simply syntactic sugar for
          ; `(quote literal)`, which is expanded "before" evaluating.
          ;
          ((eq ? (car exp) 'quote) (cadr exp))

          ; If it's a lambda, return a closure for the lambda within
          ; the current env.
          ;
          ; ```
          ; (lambda add (a b))
          ; (lambda x (+ x y)) ; -> (closure ((x)(+ x y)) <env>)
          ; ```
          ;
          ((eq ? (car exp) 'lambda)
            (list 'closure (cdr exp) env))

          ; If it's a conditional, evaluate it in the current env.
          ;
          ; ```
          ; (cond (test1 exp1)
          ;       (test2 exp2)
          ;       (testn exp3))
          ; ```
          ;
          ((eq ? (car exp) 'cond)
            (evcond (cdr exp) env))

          ; Otherwise, we need to evaluate the operator and apply it to
          ; it's arguments, if any.
          ;
          ; ```
          ; (+ 3 4)
          ; ```
          ;
          (else (apply (eval (car exp) env)
                       (evlist (cdr exp) env))))))

; The `kernel` is the group of basic primitives we need
;  - apply

(define apply
  ; Take a procedure and arguments, and calls the procedure with
  ; the arguements.
  ;
  ; We need the help of a few things here:
  ; - primitive
  ; - apply-primop
  ; - bind
  ;
  (lambda (proc args)
           ; If this is a primitive, just magically get the answer.
           ;
           ; Exactly "how" we know the procedure is a primitive is not known.
           ; Well, it's not essential, since we don't need any primitives
           ; anyway.
           ;
           ; ```
           ; (list a b)
           ; ```
           ;
    (cond ((primitive ? proc)
           ; This is magic that we won't explain - drop to machine language
           ; here.
           ;
           (apply-primop proc args))

          ; If it's a closure, then we have to do an `eval` of the body.
          ;
          ; The way we evalute the application of a procedure to its
          ; arguments is by evaulating the body of the procedure in the
          ; environment resulting from extending the environment of the
          ; procedure with the bindings of the formal parameters of the
          ; procedure to the arguments that were passed to it.
          ;
          ; ```
          ; (closure ((<args>) (<body>)) <env>)
          ; ```
          ;
          ((eq ? (car proc) 'closure)
            ; The `cadadr` of the procedure
            ;
            ; The body is the 2nd element of the 2nd element of the
            ; procedure, i.e, the `cadadr` of the procedure.
            ;
            (eval (cadadr proc)
                  ; The `caadr` of the procedure is the bound variable list,
                  ; i.e, the arguments to the procedure.
                  ;
                  ; So we need to create a new environment to eval the
                  ; conditional's body in, which we make by combining the
                  ; bound variables, the arguments, and the environment of
                  ; the closure.
                  ;
                  (bind (caadr proc)
                        args
                        (caddr proc))))

          ; Do the error handling you'd like here:
          ;
          ; Did you try to apply 1 to an argument?
          ;   Then you'll get an undefined procedure type.
          ;
          ; We might, for instance, need to determine,
          ; "is this compiled code?".
          ;
          (else error))))

(define evlist
  ; Evaluate a list.
  ;
  ; `l` is a list.
  ;
  (lambda (l env)
          ; If the list is an empty list, return the empty list.
          ;
    (cond ((eq ? l ()) '())
          ; Otherwise, we need to map the list to each item's evaluation.
          ;
          ; Refresher: `cons` "constructs" a list from 2 elements. So the
          ; following are identical:
          ;
          ; ```
          ; (cons 1 (cons 2 (cons 3 nil)))
          ; (list 1 2 3)
          ; (1 . (2 . (3 . nil)))
          ; (1 2 3)
          ; ```
          ;
          ; So we're "reconstructing" the list with it's evaluations.
          ;
          (else
            (cons (eval (car l) env)
                  (evlist (cdr l) env))))))

(define evcond
  ; To evaluate a conditional, we need to:
  ;
  ; 1. Test each clause, i.e, evaluate it's predicate
  ; 2. When we find a "true" predicate, we evaluate it's body
  ; 3. If we don't find a "true" predicate, we evaluate the `else` body
  ;
  (lambda (clauses env)
          ; If we have no clauses, return the empty list.
          ;
    (cond ((eq ? clauses '()) '())

          ; If we only have an `else` clause, evaluate it's body.
          ;
          ; `caar` is first clause, 1st element.
          ; `cadar` is first clause, 2nd element.
          ;
          ((eq ? (caar clauses) 'else)
           (eval (cadar clauses) env))

          ; If the first predicate of clauses is false, then we need to try
          ; the next clause, i.e, discard the first clause.
          ;
          ((false ? (eval (caar clauses) env))
           (evcond (cdr clauses) env))

          ; Otherwise, we had a "true" clause, so we want to evaluate it's
          ; body.
          ;
          (else
            (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          end)))

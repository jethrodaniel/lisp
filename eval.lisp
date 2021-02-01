; Notes and code taken when viewing the following MIT lecture (1986):
; `Lecture 7B: Metacircular Evaluator, Parts 1 and 2`.
;
; `Structure and Interpretation of Computer Programs`, by
;   Harold Abelson and Gerald Jay Sussman
;
; - part 1 - https://www.youtube.com/watch?v=aAlR3cezPJg
; - part 2 - https://www.youtube.com/watch?v=QVEOq5k6Xi0
;
; The lecturer is Gerald Jay Sussman.

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
  ; Associate some variables with values, i.e, make a new frame for an
  ; environment structure.
  ;
  ; An environment structure is represented as a list of frames.
  ;
  ; So bind adds the new elements to the environment frame in order to
  ; make a new environment.
  ;
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          env)))

(define pair-up
  ; Map a list of variables to their values.
  ;
  ; We handle the error cases of
  ; - values, but no vars
  ; - vars, but no values
  ;
  ; Otherwise, we construct the mapping by taking the first variable to
  ; value mapping, and `cons`ing it with the rest of the mappings, recursively.
  ;
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error TMA))))  ; too many arguments
      ((eq? vals '()) (error TFA)) ; too few arguments
      (else
        (cons (cons (car vars)
                    (car vals))
              (pair-up (cdr vars)
                       (cdr vals)))))))

(define lookup
  ; Lookup a symbol in a given environment.
  ;
  ;
  (lambda (sym env)
          ; If the environment's empty, we have an unbound variable.
          ;
    (cond ((eq? env '()) (error UBV)) ; unbound variable
          ; Otherwise, the environment must have a first frame, so we lookup the
          ; symbol in the first frame using `assq` - that may be empty if
          ; `assq` didn't match, and if so, we continue to loop through the
          ; environment until we find the mapping for our symbol.
          ;
          (else
            ((lambda (vcell)
               (cond ((eq? vcell '())
                      (lookup sym
                              (cdr env)))
                     (else (cdr vcell))))
             (assq sym (car env)))))))

(define assq
  ; Lookup a symbol in a list of pairs, return the first match, or an empty
  ; list if no match is found.
  ;
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (car alist))
           (car alist))
          (else
            (assq sym (cdr alist))))))

; The above implements the "kernel" of all languages.
;
; Cue the eval-apply M.C Escher sketch with the hands.
;

; Now let's evaluate an example by hand.
;
; e0 - some initial, global, environment
;
; This is a procedure of one argument x, which produces as a value a procedure
; of one argument y which adds x to y.
;
; We are applying this to 3, which should return 3, and 4, which returns 4,
; then adds them. That is, this is the long form of
;
; ```
; (+ 3 4)
; ```
;
; e0 looks like this - it's the initial values the machine is born with.
;
; ```
; +: ...
; -: ...
; car: ...
; cdr: ...
; ```
;
(eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) e0)

; Which reduces to:
;
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (evlist '(4) <e0>)

; Which reduces to:
;
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (cons (eval '4 <e0>)
             (evlist '() <e0>)))

; Which reduces to:
;
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (cons (4 '())))

; Which reduces to:
;
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (cons (4)))

; Which reduces to:
;
(apply (apply (eval '(lambda (x) (lambda (y) (+ x y))) <e0>)
              '(3))
       '(4))

; Which reduces to:
;
(apply (apply '(closure ((x) (x y) (+ x y))) <e0>)
              '(3))
       '(4))

; Which reduces to:
;
(apply (apply '(closure ((x) (x y) (+ x y))) <e0>)
              '(3)
       '(4))

; The `closure` introduces a `bind`, so we need to create a new environment
;
; ```
; e0 --> +-----------------------+
;        | car: ..  +: ..  *: .. |
;        | cdr: ..  -: ..  x: .. | <--+
;        +-----------------------+    |
;                                     |
;                                     +-------+
; e1 -------------------------------> | x = 3 | <--+
;              +--+ +--+------------> +-------+    |
;         +--> |  |-|  |                  +--------+
;         |    +--+ +--+                  |
;         |                           +-------+
;      lambda (y) = e2           +--> | x = 4 |
;                   |            |    +-------+
;                   |            |
;                   +------------+
; ```

(apply (eval '(lambda (y) (+ x y)) <e1>)
       '(4))

(apply '(closure ((y) (+ x y)) <e1>)
       '(4))

(eval '(+ x y) <e2>)

(apply (eval '+ <e2>)
       (evlist '(x y) <e2>))

(apply <+> '(3 4))

; eval produces a procedure and arguments for apply
;
; ```
;         +---- proc, args ----+
;         |                    |
; eval ---+                    +--> apply
;      <--+                    +---
;         |                    |
;         +---- exp, env  -----+
;
; ```
;
; `eval` may do some things on it's own (well, not use this loop)
; - evlist
; - evcond
;
; Similarly, `apply` may do some things outside of this loop
; - `apply-primop`
;
; But those aren't the general `eval-apply`.


(define exp
  ; Exponential function, using recursion.
  ;
  (lambda (x n)
    (cond ((= n 0) 1)
          (else
            (* x (exp x (- n 1)))))))

; First part of this lecture ends with an explanation of lambda calculus.

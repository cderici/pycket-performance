#lang racket

(require (for-syntax racket/syntax
                     syntax/parse
                     syntax/free-vars
                     ))

(provide lift-this define/unroll inline-this)

(define-syntax (lift-this stx)
  (syntax-parse stx
    #:literals (define lambda define/unroll unroll)
    [(_ (~and lam (lambda (arg:id ...) body ...)))
     (define exp (local-expand #'lam 'expression '()))
     (define frees (free-vars exp))
     
     (syntax-parse exp
       #:literals (#%plain-lambda)
       [(#%plain-lambda (larg ...) lbody ...)
        (with-syntax* ([(free-ids ...) frees]
                       [lifted-id (syntax-local-lift-expression
                                   #'(lambda (free-ids ...)
                                       (lambda (larg ...) lbody ...)))])
          #'(lifted-id free-ids ...))])]
    ;[(_ (do ([i:id init:expr ... step:expr] ...)
    ;      (stop?:expr finish:expr ...) body:expr ...))
     
    [(_ (~and def (define (f:id arg:id ...) body ...)))
     ;; expanding for free-vars (-it only accepts core forms-)
     (define exp (local-expand #'(letrec ([f (lambda (arg ...) body ...)]) f) 'expression '()))
     (define frees (free-vars exp))

     (syntax-parse exp
       #:literals (letrec-values)
       [(letrec-values (((b:id) (lambda (larg ...) lbody ...))) dummy:id)
        (with-syntax* ([(free-ids ...) frees]
                       [exp-to-be-lifted
                        #'(letrec ((g (lambda (larg ... free-ids ...)
                                        (let-syntax ([b (lambda (stx)
                                                          (syntax-parse stx
                                                            [_ #'(lambda (larg ...)
                                                                   (g larg ... free-ids ...))]))])
                                          lbody ...)))) g)]
                       [lifted_id (syntax-local-lift-expression #'exp-to-be-lifted)])
          #'(define-syntax (f styx)
              (syntax-parse styx
                [(_ actual-args:expr (... ...))
                 #'(lifted_id actual-args (... ...) free-ids ...)])))])]
    ;; heavy lifting
    [(_ (~and defunroll (define/unroll n:number (f:id arg:id ...) body ...)))
       #'(lift-this (define (f arg ...) (unroll n f (arg ...) body body )...))]))

(define-syntax (inline-this stx)
  (syntax-parse stx
    ;[(_ 0 (define/unroll n:number (f:id arg:id ...) body ...))
    [(_ (define/unroll n:number (f:id arg:id ...) body ...))
     #'(define-syntax (f stx)
       (syntax-parse stx
         [(_ act-args (... ...))
          #'(let-values ([(arg ...) (values act-args (... ...))])
              (unroll n f (arg ...) body body) ...)]))
     #;(with-syntax ([new-id (format-id #'f "~a-inlined" (syntax-e #'f))])
       (displayln #'new-id)
       #'(begin
           (define-syntax (new-f stx)
             (syntax-parse stx
               [(_ act-args (... ...))
                #'(let-values ([(arg ...) (values act-args (... ...))])
                    (unroll n f (arg ...) body body) ...)]))
           (define/unroll n (f arg ...) body ...)))
     ]))

(define-syntax (unroll stx)
  (syntax-parse stx
    #:datum-literals (lift-this)
    [(_ 0 fname:id (fvar:id ...) fbody:expr e:expr) #'e]
    [(_ _ _ (_ ...) _ (lift-this e:expr)) #'(lift-this e)]
    [(_ n:number fname:id (fvar:id ...) fbody:expr e:expr)
     (syntax-parse #'e
       #:literals (let define define/unroll set! lambda begin cond do)
       [(define (fname var ...) body ...) #'e]
       [(define var body:expr) #'e]
       [(set! var:id body:expr) #'(set! var (unroll n fname (fvar ...) fbody body))]
       [(begin) #'(void)] ;; <- (otherwise errors "begin: empty form not allowed in: (begin)"
       [(lambda (var:id ...) body:expr ...) #'(lambda (var ...) (unroll n fname (fvar ...) fbody body) ...)]
       [(define/unroll n (fname var ...) body ...)
        #'(define (fname var ...)
            (unroll n fname (var ...) body body) ...)]
       [(let loop ((var arg-exp) ...) body)
        #'((lambda (var ...)
           (let-syntax ([loop (lambda (stx)
                                (syntax-parse stx
                                  [(_ args (... ...)) #'((letrec ([loop (lambda (var ...)
                                                                    (unroll n loop (var ...) body body))])
                                                     loop) args (... ...))]))])
             (unroll n loop (var ...) body body)))
         arg-exp ...)]
       [(cond (condition truth:expr ...) ... (else els:expr))
        #'(cond ((unroll n fname (fvar ...) fbody condition) (unroll n fname (fvar ...) fbody truth) ...) ...
                (else (unroll n fname (fvar ...) fbody els)))]
       [(letlike ([newvar:id rhs:expr] ...) letbody ...)
        #'(letlike ([newvar (unroll n fname (fvar ...) fbody rhs)] ...)
                   (unroll n fname (fvar ...) fbody letbody) ...)]
       [(do ([i:id init:expr ... step:expr] ...)
          (stop?:expr finish:expr ...) body:expr ...)
        ;; we might have some uninitialized ids, let's find'em
        (define uninit-ids* (map (λ (s) (car (syntax->list s)))
                                 (filter (λ (s) (= 1 (length (syntax->list s))))
                                         (syntax->list #'((i init ...) ...)))))
        (with-syntax ([(uninit-ids ...) uninit-ids*]
                      [body #'(if stop?
                                  (begin (void) finish ...)
                                  (begin body ... (doloop step ...)))])
          #'((lift-this (lambda (i ...)
               (let-syntax ([doloop (lambda (stx)
                                      (syntax-parse stx
                                        [(_ args (... ...)) #'((letrec ([doloop (lambda (i ...)
                                                                                  (unroll n doloop (i ...) body body))])
                                                                 doloop) args (... ...))]))])
                 (unroll n doloop (i ...) body body)))) init ... ... uninit-ids ...)
          #;#'((letrec ([doloop (lambda (i ...)
                                (unroll n doloop (i ...) body body))])
            doloop) init ... ... uninit-ids ...))]
       [(f:id r:expr ...)
        (if (eq? (syntax->datum #'fname)
                 (syntax->datum #'f))
            (with-syntax ([new-n (- (syntax->datum #'n) 1)])
              #'(let ((fvar r) ...) (unroll new-n fname (fvar ...) fbody fbody)))
            #'(f (unroll n fname (fvar ...) fbody r) ...))]
       [(f:expr r:expr ...)
        #'((unroll n fname (fvar ...) fbody f) (unroll n fname (fvar ...) fbody r) ...)]
       [_ #'e])]))


(define-syntax (define/unroll stx)
  (syntax-parse stx
    [(_ n:number (fname:id var:id ...) body ...+)
     #'(define (fname var ...)
         (unroll n fname (var ...) body body) ...)]
    [(_ n:number (fname:id var:id ... . rest:id) body ...+)
     #'(define (fname var ... . rest)
         (unroll n fname (var ...) body body) ...)]))


#;(inline-this (define/unroll 3 (fact n)
  (let loop ((i n) (acc 0))
    (if (< i 2) 1
        (loop (- i 1) (* i acc))))))

#;(+ (fact 5) (fact 3))
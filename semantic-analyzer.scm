(define isRedundantApplications
  (lambda  (exp)
    (let ((applic (car exp))
          (proc   (lambda () (caadr exp)))
          (procArg (lambda () (cadadr exp))))

          (and (equal? applic 'applic) 
               (equal? (proc) 'lambda-simple) 
               (null? (procArg))
          )    
    )
  )
)

(define remove-applic-lambda-nil
  (lambda (parsedExp) 
      (cond 
        ;;in case its an empty list
        ((null? parsedExp)  '())
        ;;in case its a more complicated expression  
        ((list? (car parsedExp)) 
          (cons
          (remove-applic-lambda-nil (car parsedExp))
          (remove-applic-lambda-nil (cdr parsedExp))
          ))
        ;;in case it is an redundant Application
        ((isRedundantApplications parsedExp)  
          (let ((expr (car (cddadr parsedExp))))
            (remove-applic-lambda-nil expr)
          )
        )
        (else
          (cons (car parsedExp) (remove-applic-lambda-nil (cdr parsedExp)) )
          )
      )
  )
)

(define test
  (lambda ()
  (remove-applic-lambda-nil
    '(applic
    (lambda-simple
    (fact)
    (seq ((set (var fact) (box (var fact)))
    (box-set
    (var fact)
    (lambda-simple
    (n)
    (if3 (applic (var zero?) ((var n)))
    (const 1)
    (applic
    (var *)
    ((var n)
    (applic
    (box-get (var fact))
    ((applic (var -) ((var n) (const 1))))))))))
    (applic
    (lambda-simple () (applic (box-get (var fact)) ((const 5))))
    ()))))
    ((const #f))))
    
    )
  )

(define box-set
  ;; fill in the variable boxing details here
  )

(define pe->lex-pe
  ;; fill in the lexical addressing details here
  )

(define annotate-tc
  ;; fill in the tail-call annotation details here
  )


 
    
 
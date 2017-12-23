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


(define debugPrint
  (lambda (content)
  (display content)
  (newline)
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

  
(define checkForDuplicateParam
    (lambda (parameterList var)
        
        (and (member var parameterList) #t)
  )
)

;;return lambda type as a string if it is a lambda
;; return #f if it is not a lambda
(define getLambdaType
  (lambda (exp)
    (if (pair? exp)
      (if (or (equal? (car exp) 'lambda-simple)(equal? (car exp) 'lambda-opt))
          (car exp)  ;;it is a lambda expression return lambda type
          #f  ;;return false if it is not a lambda exp
      )
      #f
    )
  )
)
;;recive lambda type and the expression and return its parameterlist
;;(any*symbol=>list) 
(define getLambdaVars
  (lambda (lambdaType expr)
    
        (if (equal? lambdaType 'lambda-simple) 
        (cadr expr)
        (if (null? (cadr expr))
            (list (caddr expr))
            `(,@(cadr expr) ,(caddr expr))
        )
    )
  )
)



(define getLambdaBody
  (lambda (lambdaType expr)
    (if (equal? lambdaType 'lambda-simple) 
      (caddr expr)
      (cadddr expr)
    )
  )
)

(define isSetVar?
  (lambda (exp var)

    (cond
      ;;vase we finish search
      ((null? exp) #f)
      ;;this is a symbol and not a set exp
      ((symbol? exp) #f)
      ;;case first exp is a list
      ((list? (car exp)) 
        (or (isSetVar? (car exp) var) (isSetVar? (cdr exp) var)))
      ;;set of null
      
      ((and (equal? (car exp) 'set) (null? (cdr exp)) ) #f)
      ;;true case
      ((and (equal? (car exp) 'set) (equal? (cadadr exp) var )) #t)
      ;;nested lambda expression and it contain a parameter with same name
      ((getLambdaType (car exp))
        (let* 
            ((lambdaType (getLambdaType (car exp)))
            (lambdaBody (getLambdaBody lambdaType (car exp)))
            (lambdaVars (getLambdaVars lambdaType (car exp))))
            (if(checkForDuplicateParam lambdaVars var)
                #f
                (isSetVar? lambdaBody var)
            )
          )
      )
      (else
        (isSetVar? (cdr exp) var))
    )
  )  
)

(define isReadVar?
  (lambda (exp var)
    (cond
      ;;vase we finish search
      ((null? exp) #f)
      ;;this is a symbol and not a set exp
      ((symbol? exp) #f)
      ;;True Case
      ((equal? var (car exp)) #t)
      ;;it s more complicated expressiom
      ((list? (car exp))  
        (or (isReadVar? (car exp) var) (isReadVar? (cdr exp) var )))
      ;;case the st is null
      
      ((equal?  'set (car exp))
        (if (null? (cdr exp))
            #f
            (isReadVar? (caddr exp) var)
        )
      )
      (else
        (isReadVar? (cdr exp) var))
    
    )
  )  
)

(define isBoundVar?
  (lambda (exp var)

  ;;(debugPrint exp)
  (cond
    ;;vase we finish search
    ((null? exp) #f)

    ;;this is a symbol and not a set exp
    ((symbol? exp) #f)

    ((equal? (car exp) 'var) (equal? (cadr exp) var ))

    ((list? (car exp)) 
      (or (isBoundVar? (car exp) var) (isBoundVar? (cdr exp) var) ))
    
      ((getLambdaType (car exp))
        (let* 
          ((lambdaType (getLambdaType exp))
          (lambdaVars (getLambdaVars lambdaType exp))
          (lambdaBody (getLambdaBody lambdaType exp)))
          (if (checkForDuplicateParam lambdaVars var)
              #f
              (isBoundVar? lambdaBody var)
          )
        )
     )
     ((equal? (car exp) 'set)
          (if (null? (cdr exp))
            #f
            (isBoundVar? (cddr exp) var )
          )
     )
     (else
      (isBoundVar? (cdr exp) var )
      )
  
    )
  )  
)

(define needBoxing?
  (lambda (body var)
  
  (and (isSetVar? body var)
        
       (isReadVar? body var) 
        
      (isBoundVar? body var))
  )

)




(define boxVarLambdaBody
  (lambda (body)
    (lambda (var)
      (if (needBoxing? body var)
          var
         `() 
      )    
    )
  )
  
)


(define changeVarsInBody
  (lambda (exp var)
  

    (cond
      ((null? exp) '())

      ;;case when we found set exression
      ((and 
        (list?  exp)
        
        (equal? (car exp) 'set)
        
        (equal? (cadr exp) `(var ,var)) )
          
        `(box-set ,(cadr exp) ,@(changeVarsInBody (cddr exp) var)))
      ((getLambdaType (car exp))
      
        (let* 
            ((lambdaType (getLambdaType (car exp)))
            (lambdaVars (getLambdaVars lambdaType (car exp)))
            (lambdaBody (getLambdaBody lambdaType (car exp))))

            
            (if (checkForDuplicateParam lambdaVars var)
                exp
                (cons (cons lambdaType (cons lambdaVars (changeVarsInBody (list lambdaBody) var)))
                 (changeVarsInBody (cdr exp) var)))
        ))
       ((and (list? exp) 
              
              (equal? (car exp) `(var ,var)))
              
              (cons `(box-get ,(car exp)) (changeVarsInBody (cdr exp) var)))

      ((list? (car exp))
      
      (cons (changeVarsInBody (car exp) var) (changeVarsInBody (cdr exp) var)))
      (else
        
        (cons (car exp) (changeVarsInBody (cdr exp) var))
        )
    )  
  )
)


(define putVarInBox
  (lambda (expr var)
 

   (if (equal? 'seq (car expr))
   `(seq ((set (var ,var) (box (var ,var))) ,@(changeVarsInBody (cadr expr) var)))
   `(seq ((set (var ,var) (box (var ,var))) ,@(list (changeVarsInBody  expr var)))))
  )
)


(define boxVars 
  (lambda (lambdaBody lambdaVars)
  
    (if (null? lambdaVars)
          lambdaBody
    (boxVars (putVarInBox lambdaBody (car lambdaVars)) (cdr lambdaVars))
          
          
          
      )
    )
)
  



(define boxSetLambda
  (lambda (lambdaType expr)

  (let* 
      ((lambdaVars (getLambdaVars lambdaType expr))
      (lambdaBody (getLambdaBody lambdaType expr))
      (boxVarProc (boxVarLambdaBody lambdaBody))
      (boxedParams (filter (lambda (x) (not (equal? x '() ))) (map boxVarProc lambdaVars)))
      (boxedBody  (boxVars lambdaBody  boxedParams)))
            
      `(,lambdaType ,lambdaVars ,(box-set boxedBody)) 
    ;;`(,lambdaType ,lambdaVars ,boxedParams)

      )

  )
)



(define box-set
  (lambda (parsedExpr)
    (cond 
      ((null? parsedExpr) '())
      
      ((list? (car parsedExpr)) (cons (box-set (car parsedExpr))
                                      (box-set (cdr parsedExpr))))

      ((getLambdaType parsedExpr)
        (let
          ((lambdaType (getLambdaType parsedExpr)))
            (if lambdaType
                (boxSetLambda lambdaType parsedExpr)
                (cons (car parsedExpr) (box-set (cdr parsedExpr)))
            )
          )
        )
        (else
          (cons (car parsedExpr) (box-set (cdr parsedExpr)))
          )
    )
  )
)

(define pe->lex-pe
  ;; fill in the lexical addressing details here
  )

(define annotate-tc
  ;; fill in the tail-call annotation details here
  )


 
    
 
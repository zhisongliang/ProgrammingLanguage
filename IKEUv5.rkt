#lang typed/racket
(require typed/rackunit)

;; this macro defines a "tstruct" form that's just
;; like "struct" but automatically inserts the #:transparent
;; tag.
(define-syntax tstruct
  (syntax-rules ()
    [(_ name fields)
     (struct name fields #:transparent)]))

;;; -----------------------------------------------------
;;; Definitions:

;;; Syntax of IKEU5
;;; The concrete syntax of the IKEU5 language with these additional features can be captured with the following EBNF:

;;;   Expr	=               Num
;;;  	 	|	 	id
;;;  	 	|	 	String
;;;  	 	|	 	{if Expr then Expr else Expr}
;;;  	 	|	 	{let {[id = Expr] ...} Expr}
;;;  	 	|	 	{{id ...} : Expr}
;;;  	 	|	 	{Expr Expr ...}
;;; ... where an id is not let, =, if, then, else, or :.

;; <exprC> ::=
(define-type ExprC (U NumC IdC StrC IfC AppC LamC))
;; <numC-def> ::=
(tstruct NumC ([n : Real]))
;; <idC-def> ::=
(tstruct IdC ([s : Symbol]))
;; <strC-def> ::=
(tstruct StrC ([s : String]))
;; <ifC-def> ::=
(tstruct IfC ([if : ExprC] [then : ExprC] [else : ExprC]))
;; <app-def> ::=
(tstruct AppC ([fun : ExprC] [arg : (Listof ExprC)]))
;; <lamC-def> ::=  
(tstruct LamC ([arg : (Listof Symbol)] [body : ExprC]))

;; <value> ::=
(define-type Value (U Real Boolean String CloV PrimV))
;; <Closures-def> ::=
(tstruct CloV ([arg : (Listof Symbol)] [body : ExprC] [env : Env]))
;; <Primtive-Oprators-def> ::=
(tstruct PrimV ([op : Symbol]))

; <Binding-def> ::= (name : Symbol) (val : Value)
(tstruct Binding ((name : Symbol) (val : Value)))

; <Environment-def> ::= (Listof Binding) 
(define-type Env (Listof Binding))

; Defining out top environment with primitives
(define top-env (list (Binding '+ (PrimV '+))
                      (Binding '- (PrimV '-))
                      (Binding '* (PrimV '*))
                      (Binding '/ (PrimV '/))
                      (Binding '<= (PrimV '<=))
                      (Binding 'equal? (PrimV 'equal?))
                      (Binding 'true true)
                      (Binding 'false false)
                      (Binding 'error (PrimV 'error))))

; keywords table where id is not a keyword
(define keywords-table (set 'let '= 'if 'then 'else ':))

; lookup hash table for operators
(define op-table
  (hash '+ +
        '* *
        '- -
        '/ /
        '<= <=
        'equal? equal?))

;;; -----------------------------------------------------
;;; Functions:

; (: top-interp (Sexp -> String))
; top-level interpreter
; takes a S-expression and returns a string representation of the value of the expression
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; (: parse(Sexp -> ExprC))
; parse a S-expression into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s) 
     (cond 
       [(not (set-member? keywords-table s)) (IdC s)]
       [else (error "IKEU5 Id cannot be keyword ~e" s)])]
    [(? string? s) (StrC s)]
    [(list l ...)
     (match l
       ; IfC
       [(list 'if rest ...)
        (match rest
          [(list if 'then then 'else else) 
            (IfC (parse if) (parse then) (parse else))]
          [else (error "IKEU5 invalid if expression ~e" s)])]
       ; LamC
       [(list (list (? symbol? s) ...) ': body)
        (cond 
          [(checkDuplicateElem (cast s (Listof Symbol))) (error "IKEU5 duplicate function parameters ~e" s)]
          [else (LamC (cast s (Listof Symbol)) (parse body))])]
       ; let desurgaring {let {vars ...} body} 
       [(list 'let (list vars ...) body)
        (define varNames (getVarName vars))
        (define varVals (getVarVal vars))
        (cond
          [(checkDuplicateElem varNames) (error "IKEU5 duplicate function parameters ~e" s)]
          [else (AppC (LamC varNames (parse body)) varVals)])]
       ; AppC
       [else (AppC (parse (first l)) 
                   (map (lambda (x) (parse x)) (rest l)))])]))

; (: interp (ExprC Env -> Value))
; interpret an expression in a given environment
; and return the value of the expression
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) n]
    [(IdC s) (lookup-env env s)]
    [(StrC s) s]
    [(IfC if then else)
     (cond 
       [(boolean? (interp if env)) 
        (cond 
          [(interp if env) (interp then env)]
          [else (interp else env)])]
       [else (error "IKEU5 invalid if expression ~e" e)])]
    [(LamC arg body) (CloV arg body env)]
    [(AppC fun args)
     (match (interp fun env)
       [(CloV arg body cur-env)
        (interp body (ext-env cur-env (map (lambda (x) (interp (cast x ExprC) env)) args) arg))]
       [(PrimV op)
        (match args
          [(list l r) (interp-primV op (interp l env) (interp r env))]
          [else (error "IKEU5 user-error ~v" e)])]
       [else (error "IKEU5 invalid function ~e" e)])]))

; (: serialize(Value -> String))
; accepts any IKEU5 value, and return a string of its resprsentation
(define (serialize [value : Value]) : String
  (match value
    [(? real? ) (~v value)]
    [(? boolean? ) (if value "true" "false")]
    [(? string? ) (~v value)]
    [(? CloV? ) "#<procedure>"]
    [(? PrimV? ) "#<primop>"]))

;;;-----------------------------------------------------
;;; helper functions:

; (: getVarName((Listof Sexp) -> (Listof Symbol)))
; helper function for parse
; get the variable names from a let expression
; accepts vars which is a list of Sexp, returns a list of symbols
(define (getVarName [vars : (Listof Sexp)]) : (Listof Symbol)
  (match vars
    ['() '()]
    [(cons f r) 
      (match f
        [(list (? symbol? var) '= val)
          (cond 
            [(not (set-member? keywords-table var)) (cons var (getVarName r))] 
            [else (error "IKEU5 var cannot be keyword ~e" vars)])]
        [else (error "IKEU5 invalid let expression ~e" vars)])]))

; (: getVarVal((Listof Sexp) -> (Listof ExprC)))
; helper function for parse
; get the variable values from a let expression
; accepts vars which is a list of Sexp, returns a list of ExprC
(define (getVarVal [vars : (Listof Sexp)]) : (Listof ExprC)
  (match vars
    ['() '()]
    [(cons f r) 
      (match f
        [(list var '= val)
        (cons (parse val) (getVarVal r))]
        [else (error "IKEU5 invalid let expression ~e" vars)])]))

; (: checkDuplicateElem (Listof Symbol) -> Boolean)
; helper function for parse
; check if there are duplicate elements in a list, return true if there are duplicates, else return false
(define (checkDuplicateElem [lst : (Listof Symbol)]) : Boolean
  (let record ((restElem lst) (seen '()))
    (cond
      [(empty? restElem) #f]
      [(memq (car restElem) seen) #t]
      [else (record (cdr restElem) (cons (car restElem) seen))])))

; (: lookup-env(Env Symbol -> Value))
; lookup binding value in environment for a given symbol, return #f if not found
(define (lookup-env [env : Env] [sym : Symbol]) : Value
  (cond
    [(empty? env) (error "IKEU5 unbound variable ~e" sym)]
    [(equal? (Binding-name (first env)) sym) (Binding-val (first env))]
    [else (lookup-env (rest env) sym)]))

; (: interp-primV (Symbol Value Value -> Value))
; a helper function of interp to interpret primitive operators
; and return the value of the expression
(define (interp-primV [op : Symbol][l : Value][r : Value]) : Value
    (cond
      [(hash-has-key? op-table op)
       (cond
         [(equal? op 'equal?)
         (cond 
           [(and (boolean? l) (boolean? r)) (equal? l r)]
           [(and (string? l) (string? r)) (equal? l r)]
           [(and (real? l) (real? r)) (equal? l r)]
           [else #f])]
         [(and (real? l) (real? r))
          (cond 
            [(equal? op '/)
             (cond
               [(equal? 0 r) (error "IKEU5 cannot divide by zero ~e" r)]
               [else (/ l r)])]
            [else ((hash-ref op-table op) l r)])]
         [else (error "IKEU5 unsupported operation ~e" op)])]
       [else (error "IKEU5 unsupported operator ~e" op)]))

; (: ext-env (Env (Listof Value) (Listof Symbol) -> Env)
; a helper function of interp to extend the environment
; and return the extended environment
(define (ext-env [env : Env] [val : (Listof Value)] [arg : (Listof Symbol)]) : Env
  (cond 
    [(empty? val) env]
    [(equal? (length val) (length arg)) 
     (cons (Binding (first arg) (first val))
           (ext-env env (rest val) (rest arg)))]
    [else (error "IKEU5 number of arguments not match ~e" arg)]))

;;; -----------------------------------------------------
;;; Testcases:

;;; Testcases for top-interp
(check-equal? (top-interp '(() : 9)) "#<procedure>")

;;; Testcases for parse
(check-equal? (parse '{{a b c} : 3} ) (LamC '(a b c) (NumC 3)))
(check-equal? (parse '{let 
                        {[z = {+ 9 14}]
                         [y = 98]} 
                        {+ z y}})
              (AppC (LamC (list 'z 'y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{if {<= 1 2} then {+ 1 2} else {+ 1 3}})
              (IfC (AppC (IdC '<=) (list (NumC 1) (NumC 2)))
                   (AppC (IdC '+) (list (NumC 1) (NumC 2)))
                   (AppC (IdC '+) (list (NumC 1) (NumC 3)))))
(check-equal? (parse "hello") (StrC "hello"))
(check-exn (regexp (regexp-quote "IKEU5 Id cannot be keyword"))
           (lambda () (parse 'if)))
(check-exn (regexp (regexp-quote "IKEU5 invalid if expression"))
           (lambda () (parse '{if {<= 1 2} then {+ 1 2} notelse {+ 1 3}})))
(check-exn (regexp (regexp-quote "IKEU5 duplicate function parameters"))
           (lambda () (parse '{let {[y = {+ 9 14}] [y = 98] } {+ y y}})))
(check-exn (regexp (regexp-quote "IKEU5 duplicate function parameters"))
           (lambda () (parse '{{a a} : 3})))
(check-exn (regexp (regexp-quote "IKEU5 invalid if expression"))
           (lambda () (parse '{if ""})))
(check-exn (regexp (regexp-quote "IKEU5 var cannot be keyword"))
           (lambda () (parse '(let ((if = "")) "World"))))

;;; Testcases for interp
(check-equal? (interp (NumC 1) '()) 1)
(check-equal? (interp (IdC 'a) (list (Binding 'a 1))) 1)
(check-equal? (interp (StrC "a") top-env) "a")
(check-equal? (interp (IfC (IdC 'true) (IdC 'true) (IdC 'false)) top-env) true)
(check-equal? (interp (IfC (IdC 'false) (IdC 'true) (IdC 'false)) top-env) false)
(check-equal? (interp (LamC (list 'a) (AppC (IdC '+) (list (IdC 'a) (NumC 1))))
                      (list (Binding 'a 1))) (CloV '(a) (AppC (IdC '+) (list (IdC 'a) (NumC 1))) (list (Binding 'a 1))))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-env) 3)
(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 1) (NumC 2))) top-env) false)
(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 1) (NumC 1))) top-env) true)
(check-equal? (interp (AppC (IdC 'equal?) (list (StrC "a") (StrC "a"))) top-env) true)
(check-equal? (interp (AppC (IdC 'equal?) (list (StrC "a") (StrC "b"))) top-env) false)
(check-equal? (interp (AppC (IdC 'equal?) (list (IdC 'true) (IdC 'false))) top-env) false)
(check-equal? (interp (AppC (IdC 'equal?) (list (IdC 'true) (NumC 1))) top-env) false)
(check-equal? (interp (AppC (IdC '/) (list (NumC 1) (NumC 2))) top-env) 1/2)
(check-equal? (interp (AppC (LamC (list 'a) (AppC (IdC '+) (list (IdC 'a) (NumC 1)))) (list (NumC 1))) top-env) 2)

(check-exn (regexp (regexp-quote "IKEU5 cannot divide by zero ~e"))
           (lambda () (interp (AppC (IdC '/) (list (NumC 1) (NumC 0))) top-env)))
(check-exn (regexp (regexp-quote "IKEU5 unsupported operation ~e"))
           (lambda () (interp (AppC (IdC '+) (list (NumC 1) (IdC 'true))) top-env)))           
(check-exn (regexp (regexp-quote "IKEU5 invalid function ~e"))
           (lambda () (interp (AppC (IdC 'true) (list (NumC 1) (NumC 2))) top-env)))
(check-exn (regexp (regexp-quote "IKEU5 invalid if expression"))
           (lambda () (interp (IfC (IdC '+) (IdC 'false) (AppC (IdC '+) (list (NumC 1) (NumC 2)))) top-env)))
(check-exn (regexp (regexp-quote "IKEU5 user-error"))
           (lambda () (interp (AppC (IdC '+) (list (NumC 1) (NumC 2) (NumC 3))) top-env)))

;;; Testcases for lookup-env
(check-equal? (lookup-env top-env '+) (PrimV '+))
(check-exn (regexp (regexp-quote "IKEU5 unbound variable"))
           (lambda () (lookup-env top-env 'x)))

;;; Testcases for serialize
(check-equal? (serialize 1) "1")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize "hey") (~v "hey"))
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (PrimV 'equal?)) "#<primop>")
(check-equal? (serialize (CloV '(x) (IdC 'x) (list (Binding 'x 4)))) "#<procedure>")

;;;testcases for interp-primV
(check-exn (regexp (regexp-quote "IKEU5 unsupported operator ~e"))
           (lambda () (interp-primV 'plus 1 2)))

;;;testcases for ext-env
(check-exn (regexp (regexp-quote "IKEU5 number of arguments not match"))
           (lambda () (ext-env top-env (list 1 2) (list 'a 'b 'c))))

;;; testcases for getVarName, take list of sexp
(check-exn (regexp (regexp-quote "IKEU5 invalid let expression"))
           (lambda () (getVarName (list 'a '- 1))))


;;; testcases for getVarVal
(check-exn (regexp (regexp-quote "IKEU5 invalid let expression"))
           (lambda () (getVarVal (list 'a '- 2))))

;(printf "~v" (parse '{a {{b}} 4}))
;(AppC (IdC 'a) (list (AppC (AppC (IdC 'b) '()) '()) (NumC 4)))      

;;; (printf "~v" (parse '{{z} : 
;;;                         {{{x} : x} 
;;;                           {{{y} : y} 
;;;                             {let {[a = 9] 
;;;                                   [b = 7]} 
;;;                                 {let {[c = 8]} 
;;;                                   {+ 4 6}}}}}}))

(printf "~v" (top-interp '{let 
                        {[f = {[x y] : {+ x y}}]} 
                        {f 1 2}}))

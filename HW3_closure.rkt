#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Trenton Taylor
; u0872466
; HW3
; September 12, 2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (thunkV [dexp : Exp]
          [thunk-env : Env])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))


(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (unletE [n : Symbol]
          [body : Exp])
  (ifE [check : Exp]
       [true_exp : Exp]
       [false_exp : Exp])
  (eqE [l : Exp]
       [r : Exp])
  (trueE [t : Value])
  (falseE [f : Value])
  (delayE [d : Exp])
  (forceE [de : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `true s)
     (trueE (boolV #t))]
    [(s-exp-match? `false s)
     (falseE (boolV #f))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (eqE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{unlet SYMBOL ANY} s)
     (unletE (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{delay ANY} s)
     (delayE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{force ANY} s)
     (forceE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `{force {force {delay {delay 1}}}})
        (forceE (forceE (delayE (delayE (numE 1))))))
  (test (parse `{force 1})
        (forceE (numE 1)))
  (test (parse `{force {delay 1}})
        (forceE (delayE (numE 1))))
  (test (parse `{delay x})
        (delayE (idE 'x)))
  (test (parse `{delay {+ 1 true}})
        (delayE (plusE (numE 1) (trueE (boolV #t)))))
  (test (parse `{unlet x x})
        (unletE 'x (idE 'x)))
  (test (parse `{let {[x 1]}
                  {unlet x
                         x}})
        (letE 'x (numE 1) (unletE 'x (idE 'x))))
  (test (parse `{unlet y {+ x y}})
        (unletE 'y (plusE (idE 'x) (idE 'y))))
  (test (parse `{if true 1 2})
        (ifE (trueE (boolV #t)) (numE 1) (numE 2)))
  (test (parse `{if 1 2 3})
        (ifE (numE 1) (numE 2) (numE 3)))
  (test (parse `{if {= 1 1} 1 2})
        (ifE (eqE (numE 1) (numE 1)) (numE 1) (numE 2)))
  (test (parse `{= {+ 1 2} 3})
        (eqE (plusE (numE 1) (numE 2)) (numE 3)))
  (test (parse `{= 1 2})
        (eqE (numE 1) (numE 2)))
  (test (parse `{= true false})
        (eqE (trueE (boolV #t)) (falseE (boolV #f))))
  (test (parse `{+ 1 true})
        (plusE (numE 1) (trueE (boolV #t))))
  (test (parse `true)
        (trueE (boolV #t)))
  (test (parse `false)
        (falseE (boolV #f)))
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(trueE t) t]
    [(falseE f) f]
    [(eqE l r) (if (and (numV? (interp l env)) (numV? (interp r env)))
                   (boolV (eq? (numV-n (interp l env)) (numV-n (interp r env))))
                   (error 'interp "not a number"))]
    [(ifE c t f) (if (boolV? (interp c env))
                     (if (boolV-b (interp c env))
                         (interp t env)
                         (interp f env))
                     (error 'interp "not a boolean"))]
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]
    [(unletE n body) (interp body
                             (shorten-env n env))]
    [(delayE d) (thunkV d env)]
    [(forceE de) (let ([th (interp de env)])
                   (if (thunkV? th)
                     (interp (thunkV-dexp th) (thunkV-thunk-env th))
                     (error 'interp "not a thunk")))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]))

(module+ test
  (test/exn (interp (parse `{force 1})
                    mt-env)
            "not a thunk")
  (test (interp (parse `{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse `15)
                mt-env))
  (test (interp (parse `{force {delay {+ 1 2}}})
                mt-env)
        (numV 3))
  (test (interp (parse `{force {delay {delay 1}}})
                mt-env)
        (thunkV (numE 1) mt-env))
  (test (interp (parse `{force {delay 1}})
                mt-env)
        (numV 1))
  (test (interp (parse `{force {delay x}})
                (extend-env (bind 'x (numV 71)) mt-env))
        (numV 71))
  (test/exn (interp (parse `{force 1})
                mt-env)
        "not a thunk")
  (test (interp (parse `{delay {+ 1 true}})
                mt-env)
        (thunkV (plusE (numE 1) (trueE (boolV #t))) mt-env))
  (test (interp (parse `{delay x})
                (extend-env (bind 'x (numV 1)) mt-env))
        (thunkV (idE 'x) (list (bind 'x (numV 1)))))
  (test (interp (parse `{delay x})
                mt-env)
        (thunkV (idE 'x) mt-env))
  (test/exn (interp (parse `{let {[x 1]}
                             {unlet x
                              x}})
                    mt-env)
            "free variable")
  (test (interp (parse `{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env)
        (interp (parse `3) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env)
        (interp (parse `6) mt-env))
  (test (interp (parse `{let {[f {lambda {z}
                                   {let {[z 8]}
                                     {unlet z
                                       z}}}]}
                          {f 2}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse `9)
                mt-env))
  (test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse `10)
                mt-env))
  (test/exn (interp (parse `{if 1 2 3})
                    mt-env)
            "not a boolean")
  (test (interp (parse `{if true 1 0}) mt-env)
        (numV 1))
  (test (interp (parse `{if false 1 0}) mt-env)
        (numV 0))
  (test (interp (parse `{if {= 11 11} true false}) mt-env)
        (boolV #t))
  (test (interp (parse `{if {= 11 12} true false}) mt-env)
        (boolV #f))
  (test/exn (interp (parse `{if {+ 11 0} true false}) mt-env)
        "not a boolean")
  (test (interp (parse `{= {+ 1 2} {* 1 3}}) mt-env)
        (boolV #t))
  (test (interp (parse `{= 1 2}) mt-env)
        (boolV #f))
  (test (interp (parse `{= 1 1}) mt-env)
        (boolV #t))
  (test/exn (interp (parse `{= 1 true}) mt-env)
        "not a number")
  (test/exn (interp (parse `{= true false}) mt-env)
        "not a number")
  (test/exn (interp (parse `{+ 1 true}) mt-env)
            "not a number")
  (test (interp (parse `true) mt-env)
        (boolV #t))
  (test (interp (parse `false) mt-env)
        (boolV #f))
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; shorten-env -------------------------------------
(define (shorten-env [n : Symbol] [env : Env]) : Env
  (type-case (Listof Binding) env
    [empty mt-env]
    [(cons b rst-env) (if (symbol=? n (bind-name b))
                          rst-env
                          (rm-sym-env n (extend-env b mt-env) rst-env))]))

(define (rm-sym-env [n : Symbol] [new-env : Env] [cur-env : Env]) : Env
  (type-case (Listof Binding) cur-env
    [empty new-env]
    [(cons b rst-env) (if (symbol=? n (bind-name b))
                          (append (flip-env new-env) rst-env)
                          (rm-sym-env n (extend-env b new-env) rst-env))]))
; cons b and then shorthen the rest of the env

(define (flip-env [cur-env : Env]) : Env
  (type-case (Listof Binding) cur-env
    [empty cur-env]
    [(cons b rst-env) (flip (extend-env b mt-env) rst-env)]))

(define (flip [new-env : Env] [cur-env : Env]) : Env
  (type-case (Listof Binding) cur-env
    [empty new-env]
    [(cons b rst-env) (flip (extend-env b new-env) rst-env)]))
    


(module+ test
  (test (shorten-env 'y (extend-env (bind 'x (numV 1)) mt-env))
        (list (bind 'x (numV 1))))
  (test (shorten-env 'x mt-env)
        mt-env)
  (test (shorten-env 'x (extend-env (bind 'x (numV 1)) mt-env))
        mt-env)
  (test (shorten-env 'x (extend-env (bind 'x (numV 1))
                                    (extend-env (bind 'y (numV 2))
                                            (extend-env (bind 'z (numV 3))
                                                    mt-env))))
        (list (bind 'y (numV 2)) (bind 'z (numV 3))))
  (test (shorten-env 'y (extend-env (bind 'x (numV 1))
                                    (extend-env (bind 'y (numV 2))
                                            (extend-env (bind 'z (numV 3))
                                                    mt-env))))
        (list (bind 'x (numV 1)) (bind 'z (numV 3))))
  (test (shorten-env 'z (extend-env (bind 'x (numV 1))
                                    (extend-env (bind 'y (numV 2))
                                            (extend-env (bind 'z (numV 3))
                                                    mt-env))))
        (list (bind 'x (numV 1)) (bind 'y (numV 2))))

  ; flip
  (test (flip-env mt-env)
        mt-env)
  (test (flip-env (extend-env (bind 'x (numV 1))
                              (extend-env (bind 'y (numV 2))
                                          (extend-env (bind 'z (numV 3))
                                                      mt-env))))
        (list (bind 'z (numV 3)) (bind 'y (numV 2)) (bind 'x (numV 1))))
  )

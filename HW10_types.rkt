#|
CS 3520 Homework 10
Due: Wednesday, November 14th, 2018 11:59pm
Trenton Taylor
u0872466

Part 1 — true, false, =, and if
Start with typed-lambda.rkt. The implementation already includes a bool type,
but no expressions of bool type.

Add support for true, false, {= <Exp> <Exp>}, and {if <Exp> <Exp> <Exp>}
expressions, where = produces a boolean given two numbers, and if requires a
boolean expression for the test.

Examples:

  (test (interp (parse `{if true 4 5})
                mt-env)
         (numV 4))
  
  (test (interp (parse `{if false 4 5})
                mt-env)
         (numV 5))
  
  (test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
         (numV 5))
  
  (test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
         (boolT))
  
  (test (typecheck (parse `{if {= 1 {+ -1 2}}
                               {lambda {[x : num]} {+ x 1}}
                               {lambda {[y : num]} y}})
                   mt-env)
        ;; This result may need to be adjusted after part 3:
        (arrowT (numT) (numT)))
  
  (test/exn (typecheck (parse `{+ 1 {if true true false}})
                       mt-env)
            "no type")

Part 2 — Pairs
Implement {pair <Exp> <Exp>}, {fst <Exp>}, and {snd <Exp>} expressions,
as well as {<Type> * <Type>} types, as shown in video 10.

Examples (some of which depend on a choice of constructor and may not
apply directly to your implementation):

  (test (interp (parse `{pair 10 8})
                mt-env)
        ;; Your constructor might be different than pairV:
        (pairV (numV 10) (numV 8)))
  
  (test (interp (parse `{fst {pair 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{snd {pair 10 8}})
                mt-env)
        (numV 8))
  
  (test (interp (parse `{let {[p : (num * num) {pair 10 8}]}
                          {fst p}})
                mt-env)
        (numV 10))
  
  (test (typecheck (parse `{pair 10 8})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (crossT (numT) (numT)))
  
  (test (typecheck (parse `{fst {pair 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{+ 1 {snd {pair 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{lambda {[x : (num * bool)]}
                             {fst x}})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (arrowT (crossT (numT) (boolT)) (numT)))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {fst x}}
                            {pair 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {snd x}}
                            {pair 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse `{fst 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{lambda {[x : (num * bool)]}
                                 {if {fst x}
                                     1
                                     2}})
                       mt-env)
            "no type")

Part 3 — Functions that Accept Multiple Arguments, Yet Again
With pairs, functions can accept multiple arguments by accepting
paired values, but we can also add direct support for multiple arguments.

Change the interpreter to allow multiple function arguments and multiple
arguments at function calls. The grammar of the language is now as follows
(not counting the let sugar, whose syntax can remain limited to a single
binding):

  <Exp> = <Num>
         | true
         | false
         | {+ <Exp> <Exp>}
         | {* <Exp> <Exp>}
         | {= <Exp> <Exp>}
         | <Sym>
         | {if <Exp> <Exp> <Exp>}
         | {lambda {[<Sym> : <Type>]*} <Exp>}
         | {<Exp> <Exp>*}
         | {pair <Exp> <Exp>}
         | {fst <Exp>}
         | {snd <Exp>}
  
  <Type> = num
         | bool
         | (<Type> * <Type>)
         | (<Type>* -> <Type>)

Examples:

  (test (interp (parse `{{lambda {}
                           10}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{{lambda {[x : num] [y : num]} {+ x y}}
                         10
                         20})
                mt-env)
        (numV 30))
  
  
  (test (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                            10
                            false})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                                false
                                10})
                       mt-env)
            "no type")
|#


#lang plait


(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (closV [arg : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (pairV [f : Value]
         [s : Value]))

(define-type Exp
  (numE [n : Number])
  (trueE [t : Value])
  (falseE [f : Value])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (equalE [l : Exp]
          [r : Exp])
  (ifE [tst : Exp]
       [true_exp : Exp]
       [false_exp : Exp])
  (pairE [f : Exp]
         [s : Exp])
  (fstE [fe : Exp])
  (sndE [se : Exp])
  (lamE [n : (Listof Symbol)]
        [arg-type : (Listof Type)]
        [body : Exp])
  (appE [fun : Exp]
        [arg : (Listof Exp)]))

(define-type Type
  (numT)
  (boolT)
  (arrowT [arg : Type]
          [result : Type])
  (starT [fst : Type]
         [snd : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

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
     (equalE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s)))
             (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{pair ANY ANY} s)
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{fst ANY} s)
     (fstE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{snd ANY} s)
     (sndE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse-type (third bs))
                   (parse (third (s-exp->list s))))
             (parse (fourth bs))))]
    [(s-exp-match? `{lambda {[SYMBOL ... : ANY ...]} ANY} s)
     (let ([arg (s-exp->list
                 (first (s-exp->list 
                         (second (s-exp->list s)))))])
       (lamE (s-exp->symbol (first arg))
             (parse-type (third arg))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY -> ANY) s)
    (arrowT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [(s-exp-match? `(ANY * ANY) s)
    (starT (parse-type (first (s-exp->list s)))
           (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse `{snd {pair 7 7}})
        (sndE (pairE (numE 7) (numE 7))))
  (test (parse `{fst {pair 7 7}})
        (fstE (pairE (numE 7) (numE 7))))
  (test (parse `{pair 7 7})
        (pairE (numE 7) (numE 7)))
  (test (parse `{if true 7 7})
        (ifE (trueE (boolV #t)) (numE 7) (numE 7)))
  (test (parse `{= 7 7})
        (equalE (numE 7) (numE 7)))
  (test (parse `false)
        (falseE (boolV #f)))
  (test (parse `true)
        (trueE (boolV #t)))
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
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE 'x (numT) (idE 'y))
              (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE 'x (numT) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-type `(num * bool))
        (starT (numT) (boolT)))
  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (numT) (boolT)))
  (test/exn (parse-type `1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(trueE t) t]
    [(falseE f) f]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(equalE l r) (if (equal? (interp l env) (interp r env))
                      (boolV #t)
                      (boolV #f))]
    [(ifE tst te fe) (if (boolV-b (interp tst env))
                         (interp te env)
                         (interp fe env))]
    [(pairE f s) (pairV (interp f env) (interp s env))]
    [(fstE fe) (pairV-f (interp fe env))]
    [(sndE se) (pairV-s (interp se env))]
    [(lamE n t body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]))

(module+ test
  (test (interp (parse `{pair 10 8})
                mt-env)
        (pairV (numV 10) (numV 8)))
  
  (test (interp (parse `{fst {pair 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{snd {pair 10 8}})
                mt-env)
        (numV 8))
  
  (test (interp (parse `{let {[p : (num * num) {pair 10 8}]}
                          {fst p}})
                mt-env)
        (numV 10))
  (test (interp (parse `{pair 7 {+ 7 7}}) mt-env)
        (pairV (numV 7) (numV 14)))
  (test (interp (parse `{pair true false}) mt-env)
        (pairV (boolV #t) (boolV #f)))
  (test (interp (parse `{if true 4 5})
                mt-env)
         (numV 4))
  
  (test (interp (parse `{if false 4 5})
                mt-env)
         (numV 5))
  
  (test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
         (numV 5))
  (test (interp (parse `{if false 1 0}) mt-env)
        (numV 0))
  (test (interp (parse `{if true 1 0}) mt-env)
        (numV 1))
  (test (interp (parse `{= 7 71}) mt-env)
        (boolV #f))
  (test (interp (parse `{= 7 7}) mt-env)
        (boolV #t))
  (test (interp (parse `false) mt-env)
        (boolV #f))
  (test (interp (parse `true) mt-env)
        (boolV #t))
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
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

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
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))

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

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
  (type-case Exp a
    [(numE n) (numT)]
    [(trueE t) (boolT)]
    [(falseE f) (boolT)]
    [(plusE l r) (typecheck-nums l r tenv)]
    [(multE l r) (typecheck-nums l r tenv)]
    [(equalE l r)
     (type-case Type (typecheck l tenv)
       [(numT)
        (type-case Type (typecheck r tenv)
          [(numT) (boolT)]
          [else (type-error r "num")])]
       [else (type-error l "num")])]
    [(ifE tst te fe)
     (type-case Type (typecheck tst tenv)
       [(boolT) (if (equal? (typecheck te tenv) (typecheck fe tenv))
                    (typecheck te tenv)
                    (type-error te "no type"))]
       [else (type-error tst "bool")])]
    [(pairE f s) (starT (typecheck f tenv) (typecheck s tenv))]
    [(fstE fe)
     (type-case Type (typecheck fe tenv)
       [(starT f s) f]
       [else (type-error fe "pair")])]
    [(sndE se)
     (type-case Type (typecheck se tenv)
       [(starT f s) s]
       [else (type-error se "pair")])]
    [(idE n) (type-lookup n tenv)]
    [(lamE n arg-type body)
     (arrowT arg-type
             (typecheck body 
                        (extend-env (tbind n arg-type)
                                    tenv)))]
    [(appE fun arg)
     (type-case Type (typecheck fun tenv)
       [(arrowT arg-type result-type)
        (if (equal? arg-type
                    (typecheck arg tenv))
            result-type
            (type-error arg
                        (to-string arg-type)))]
       [else (type-error fun "function")])]))

(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (numT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))


(module+ test
  (test (typecheck (parse `{pair 10 8})
                   mt-env)
        (starT (numT) (numT)))
  
  (test (typecheck (parse `{fst {pair 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{+ 1 {snd {pair 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{lambda {[x : (num * bool)]}
                             {fst x}})
                   mt-env)
        (arrowT (starT (numT) (boolT)) (numT)))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {fst x}}
                            {pair 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {snd x}}
                            {pair 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse `{fst 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{lambda {[x : (num * bool)]}
                                 {if {fst x}
                                     1
                                     2}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{snd 7}) mt-env)
        "pair")
  (test/exn (typecheck (parse `{fst 7}) mt-env)
        "pair")
  (test (typecheck (parse `{snd {pair 7 7}}) mt-env)
        (numT))
  (test (typecheck (parse `{fst {pair true false}}) mt-env)
        (boolT))
  (test (typecheck (parse `{pair true {pair 7 7}}) mt-env)
        (starT (boolT) (starT (numT) (numT))))
  (test (typecheck (parse `{pair true 7}) mt-env)
        (starT (boolT) (numT)))
  (test (typecheck (parse `{pair 7 7}) mt-env)
        (starT (numT) (numT)))
  (test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
         (boolT))
  
  (test (typecheck (parse `{if {= 1 {+ -1 2}}
                               {lambda {[x : num]} {+ x 1}}
                               {lambda {[y : num]} y}})
                   mt-env)
        ;; This result may need to be adjusted after part 3:
        (arrowT (numT) (numT)))
  
  (test/exn (typecheck (parse `{+ 1 {if true true false}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{if true false 7}) mt-env)
            "no type")
  (test/exn (typecheck (parse `{if 1 7 7}) mt-env)
            "bool")
  (test (typecheck (parse `{if false 7 1}) mt-env)
        (numT))
  (test (typecheck (parse `{if true 7 1}) mt-env)
        (numT))
  (test/exn (typecheck (parse `{= 1 false}) mt-env)
            "num")
  (test/exn (typecheck (parse `{= true 1}) mt-env)
            "num")
  (test (typecheck (parse `{= {+ 1 1} 7}) mt-env)
        (boolT))
  (test (typecheck (parse `{= 7 7}) mt-env)
        (boolT))
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (numT) (numT)))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (numT) (arrowT (boolT)  (numT))))

  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))
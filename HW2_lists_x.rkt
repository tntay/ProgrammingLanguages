#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Trenton Taylor
; u0872466
; HW2
; September 5, 2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [arg : (Listof Exp)])
  (maxE [l : Exp]
        [r : Exp]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : (Listof Symbol)] 
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]  ;;(parse-list (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s)))))  ;;(parse-fundef-args-list (rest (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [(duplicates? (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))) empty) (error 'parse-fundef "bad syntax")]
    [else (error 'parse-fundef "invalid input")]))

(define (duplicates? [l : (Listof Symbol)] [s : (Listof Symbol)]) : Boolean
  (type-case (Listof Symbol) l
    [empty #f]
    [(cons f r) (if (sym-in-list? f s)
                    #t
                    (duplicates? r (cons f s)))]))

(define (sym-in-list? [s : Symbol] [l : (Listof Symbol)]) : Boolean
  (type-case (Listof Symbol) l
  [empty #f]
  [(cons f r) (if (equal? s (first l))
                  #t
                  (sym-in-list? s (rest l)))]))

;; Replaced calls to these functions with map ;;
;;(define (parse-fundef-args-list [l : (Listof S-Exp)]) : (Listof Symbol)
  ;;(type-case (Listof S-Exp) l
    ;;[empty empty]
    ;;[(cons f r) (cons (s-exp->symbol f) (parse-fundef-args-list r))]))
    
;;(define (parse-list [l : (Listof S-Exp)]) : (Listof Exp)
  ;;(type-case (Listof S-Exp) l
    ;;[empty empty]
    ;;[(cons f r) (cons (parse f) (parse-list r))]))

(module+ test
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
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  
  ;; 0 or more args tests
  (test (parse `{f})
        (appE 'f empty))
  (test (parse `{f 1})
        (appE 'f (list (numE 1))))
  (test (parse `{f 1 2 3})
        (appE 'f (list (numE 1) (numE 2) (numE 3))))

  ;; max tests
  (test (parse `{max 1 2})
        (maxE (numE 1) (numE 2)))
  (test (parse `{max {+ 4 5} {+ 2 3}})
        (maxE (plusE (numE 4) (numE 5)) (plusE (numE 2) (numE 3))))
  ;; parse-fundef 0 or more parameters tests
  (test (parse-fundef `{define {f} 10})
        (fd 'f empty (numE 10)))
  (test (parse-fundef `{define {f} {+ x 1}})
        (fd 'f empty (plusE (idE 'x) (numE 1))))
  (test (parse-fundef `{define {f x} 1})
        (fd 'f (list 'x) (numE 1)))
  (test (parse-fundef `{define {f x} {+ 1 1}})
        (fd 'f (list 'x) (plusE (numE 1) (numE 1))))
  (test (parse-fundef `{define {double x y} {+ x y}})
        (fd 'double (list 'x 'y) (plusE (idE 'x) (idE 'y))))
  (test (parse-fundef `{define {f x y} x})
        (fd 'f (list 'x 'y) (idE 'x)))
  (test (parse-fundef `{define {f x y z} x})
        (fd 'f (list 'x 'y 'z) (idE 'x)))
  (test/exn (parse-fundef `{define {f x x} x})
            "bad syntax")
  (test/exn (parse-fundef `{define {f x y z x} z})
            "bad syntax")
  
  ;;****

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))


;; interp ----------------------------------
;(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
;  (type-case Exp a
;    [(numE n) n]
;    [(idE s) (error 'interp "free variable")]
;    [(plusE l r) (+ (interp l defs) (interp r defs))]
;    [(multE l r) (* (interp l defs) (interp r defs))]
;    [(maxE e e1) (local [(define n (interp e defs))]
;                   (local [(define n1 (interp e1 defs))]
;                     (if (> n n1)
;                         n
;                         n1)))]
;    [(appE s arg) (local [(define fd (get-fundef s defs))]
;                    (interp (subst-list (map (lambda (n)
;                                          (numE n))
;                                        (interp-list arg defs))
;                                   (fd-arg fd)
;                                   (fd-body fd))
;                            defs))]))

;(define (interp-list [el : (Listof Exp)] [defs : (Listof Func-Defn)]) : Exp
;  (type-case (Listof Exp) el
;    [empty defs->body]
;    [(cons f r) (subst f defs) ( r defs))]))


;;(module+ test
;  (test (interp-list empty
;                     empty)
;        empty)
;  (test (interp-list (list (numE 1) (plusE (numE 2) (numE 3)))
;                     empty)
;        (list 1 5))
;  )
    
    
;;(module+ test
;  (test (interp (parse `2) empty)
;        2)
;  (test/exn (interp (parse `x) empty)
;            "free variable")
;  (test (interp (parse `{+ 2 1}) empty)
;        3)
;  (test (interp (parse `{* 2 1}) empty)
;        2)
;  (test (interp (parse `{+ {* 2 3}
;                           {+ 5 8}})
;                empty)
;        19)
;  (test (interp (parse `{double 8})
;                (list double-def))
;        16)
;  (test (interp (parse `{quadruple 8})
;                (list double-def quadruple-def))
;        32)
  ;; max tests
;  (test (interp (parse `{max 1 2})
;                (list))
;        2)
;  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
;                (list))
;        9)
;  (test (interp (parse `{max -1 0})
;                (list))
;        0)

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
;(define (subst [what : Exp] [for : Symbol] [in : Exp])
;  (type-case Exp in
;    [(numE n) in]
;    [(idE s) (if (eq? for s)
;                 what
;                 in)]
;    [(plusE l r) (plusE (subst what for l)
;                        (subst what for r))]
;    [(multE l r) (multE (subst what for l)
;                        (subst what for r))]
;    [(maxE e e1) (maxE (subst what for e)
;                       (subst what for e1))]
;    [(appE s arg) (appE s (list (subst what for (first arg))))]))

;(define (subst-list [whats : (Listof Exp)] [fors : (Listof Symbol)] [in : Exp]) : (Listof Exp)
;  (type-case (Listof Exp) whats
;   [empty empty]
;   [(cons f r) (cons (subst f (first fors) in) (subst-list r (rest fors) in))]))


;;(module+ test
  ;; {define {iendtity x} x}   {identity 8}
;  (test (subst (parse `8) 'x (parse `9))
;        (numE 9))
;  (test (subst (parse `8) 'x (parse `x))
;        (numE 8))
;  (test (subst (parse `8) 'x (parse `y))
;        (idE 'y))
  ;; {define {add x} {+ x y}}   {add 8}
;  (test (subst (parse `8) 'x (parse `{+ x y}))
;        (parse `{+ 8 y}))
;  (test (subst (parse `8) 'x (parse `{* y x}))
;        (parse `{* y 8}))
;  (test (subst (parse `8) 'x (parse `{double x}))
;        (parse `{double 8}))
  ;;****
;  (test (subst (parse `1) 'x (parse `{max x x}))
;        (maxE (numE 1) (numE 1)))
;  (test (subst (parse `1) 'x (parse `{max x y}))
;        (maxE (numE 1) (idE 'y)))
;  (test (subst (parse `2) 'x (parse `{max {+ 1 x} {+ 1 y}}))
;        (maxE (plusE (numE 1) (numE 2)) (plusE (numE 1) (idE 'y)))))

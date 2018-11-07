#|
CS 3520 Homework 4
Due: Wednesday, September 19th, 2018 11:59pm
Trenton Taylor
u0872466

Part 1 — Improving Assignment
Start with store-with.rkt. In the starting program, the representation of the
store grows every time that a box’s content is modified with set-box!. Change
the implementation of set-box! so that the old value of the box is dropped (i.e.,
replaced with the new value) instead of merely hidden by the outside-in search
order of fetch.

Example:

  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                           {set-box! b 2}
                           {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))

Part 2 — Sequences
Generalize begin to allow one or more sub-expressions, instead of exactly two
sub-expressions.

  <Exp> = ...
        | {begin <Exp>* <Exp>}
Example:

  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b {+ 2 {unbox b}}}
                            {set-box! b {+ 3 {unbox b}}}
                            {set-box! b {+ 4 {unbox b}}}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))

Part 3 — Records
Extend the interpreter to support the construction of records with named fields,
to support field selection from a record (as in record.rkt):

  <Exp> = ...
        | {record {<Sym> <Exp>}*}
        | {get <Exp> <Sym>}

Adding records means that the language now has four kinds of values: numbers,
functions, boxes, and records. At run-time, an error may occur because a record
is misused as a number or function, a number or function is supplied to get, or
a record supplied to get does not have the named field, and so on. Your error
message for the last case should include the words “no such field”, otherwise
you can make up your own error messages.

Expressions within a record form should be evaluated when the record form itself
is evaluated, and in the order that the expressions appear in the record form.
For example,

  {let {[b {box 0}]}
    {let {[r {record {a {unbox b}}}]}
      {begin
        {set-box! b 1}
        {get r a}}}}

should produce 0, not 1, because {unbox b} is evaluated when the record expression
is evaluated, not when the get expression is evaluated.

Note that you will not be able to use map to interp field values, since a store must
be carried from one field’s evaluation to the next. Instead, interping the field value
will be more like interping a sequence of expressions for begin.

For homework purposes, we don’t want to nail down the representation of a record value,
because there are many choices. The examples below therefore use interp-expr, which you
should define as a wrapper on interp that takes just an Exp and produces just an
S-expression: an S-expression number if interp produces any number, the S-expression
`function if interp produces a closure, the S-expression `box if interp produces a box,
or the S-expression `record if interp produces a record value.

Examples:

  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0)

Part 4 — Mutating Records
This exercise is optional for CS 3520 students, but it’s required for CS 6520 students.
It counts as extra points either way, but that’s not really “extra” for CS 6520.

Add a set form that modifies the value of a record field imperatively (as opposed to
functional update):

  <Exp> = ...
        | {set! <Exp> <Sym> <Exp>}

Evaluation of a record expression allocates a location for each of its fields. A get
expression accesses from the record produced by the sub-expression the value in the
location of the field named by the identifier. A set! form changes the value in the
location for a field; the value of the second sub-expression in set! determines the
field’s new value, and that value is also the result of the set! expression.

Note that making record fields mutable has the same effect as forcing every field of
a record to be a Curly box, where the box contain the proper value of the field.
Internal to the interpreter implementation, you could use Curly boxes in your
implementation of mutable records, or you could use addresses more directly. You
should not use Plait boxes at all.

Examples:

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {get r x}}))
        `1)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {begin
                                 {set! r x 5}
                                 {get r x}}}))
        `5)

  (test (interp-expr (parse `{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {d} r}]}
                                 {begin
                                   {set! {get-r 0} x 6}
                                   {get {get-r 0} x}}}}))
        `6)

  (test (interp-expr (parse `{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set! r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        `5)
|#

#lang plait



(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [ns : (Listof Symbol)]
        [vs : (Listof Value)]))

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
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (beginE [es : (Listof Exp)])
  (recordE [ns : (Listof Symbol)]
           [args : (Listof Exp)])
  (getE [rec : Exp]
        [s : Symbol]))
  ;(setE [rec : Exp]
  ;      [n : Symbol]
  ;      [val : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

(module+ test
  (print-only-errors #t))

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
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY ...} s)
     (beginE (cons (parse (second (s-exp->list s)))
                   (map parse (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (getE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    ;[(s-exp-match? `{set! ANY SYMBOL ANY} s)
    ; (setE (parse (second (s-exp->list s)))
    ;       (s-exp->symbol (third (s-exp->list s)))
    ;       (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  ;(test (parse `{set! {+ 1 2} a 7})
  ;      (setE (plusE (numE 1) (numE 2)) 'a (numE 7)))
  (test (parse `{record {x 2} {y 3}})
        (recordE (list 'x 'y)
                 (list (numE 2) (numE 3))))
  (test (parse `{get {+ 1 2} a})
        (getE (plusE (numE 1) (numE 2)) 'a))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
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
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1 2})
        (beginE (list (numE 1) (numE 2))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test (parse `{begin 1})
        (beginE (list (numE 1))))
  (test/exn (parse `{begin})
            "invalid input")
  (test (parse `{begin 1 2 3 4})
        (beginE (list (numE 1) (numE 2) (numE 3) (numE 4))))
  (test (parse `{begin {+ 1 1}
                       {+ 2 2}
                       {+ 3 3}
                       {+ 4 4}})
        (beginE (list (plusE (numE 1) (numE 1))
                      (plusE (numE 2) (numE 2))
                      (plusE (numE 3) (numE 3))
                      (plusE (numE 4) (numE 4))))))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [(v*s v-id sto-id) body]))

;; interp-exper ----------------------------------------

;;wapper on interp that returns an S-Exp of the kind of value returned by interp
(define (interp-expr [e : Exp]) : S-Exp
  (with [(ret-val ret-store) (interp e mt-env mt-store)]
        (cond
          [(numV? ret-val) (number->s-exp (numV-n ret-val))]
          [(closV? ret-val) `function]
          [(boxV? ret-val) `box]
          [(recV? ret-val) `record])))

(module+ test
  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env
                (bind n v-rhs)
                env)
               sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (interp body
                    (extend-env
                     (bind n v-a)
                     c-env)
                    sto-a)]
           [else (error 'interp "not a function")])))]
    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
       (let ([l (new-loc sto-v)])
         (v*s (boxV l) 
              (override-store (cell l v) 
                              sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
       (type-case Value v
         [(boxV l) (v*s (fetch l sto-v) 
                        sto-v)]
         [else (error 'interp "not a box")]))]
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
       (with [(v-v sto-v) (interp val env sto-b)]
         (type-case Value v-b
           [(boxV l)
            (v*s v-v
                 (update-store l v-v sto-v))]
           [else (error 'interp "not a box")])))]
    [(beginE l) (sequence (numV 0) l env sto)] ; just send in some value to sequence to start
    [(recordE ns as) (rec-eval ns empty as env sto)] ; need a list to store the values so send in empty list to rec-eval
    [(getE r s)
     (with [(val-ret sto-ret) (interp r env sto)]
           (type-case Value val-ret
             [(recV ls lv) (v*s (find s ls lv) sto-ret)]
             [else (error 'interp "not a record")]))]))
    ;[(setE r s v) ....]))

(module+ test
  ;(test/exn (interp (parse `{record {x x}}) mt-env mt-store) ; gets a free variable error before "not a record"
  ;          "not a record")
  (test/exn (interp (parse `{get 1 d}) mt-env mt-store)
        "not a record")
  (test/exn (interp (parse `{get {record {a 1} {b 2} {c 3}}
                             d})
                mt-env
                mt-store)
        "no such field")
  (test (interp (parse `{get {record {a 1} {b 2} {c 3}}
                             c})
                mt-env
                mt-store)
        (v*s (numV 3) mt-store))
  (test (interp (parse `{record}) mt-env mt-store)
        (v*s (recV empty
                   empty)
             mt-store))
  (test (interp (parse `{record {a 1}}) mt-env mt-store)
        (v*s (recV (list 'a)
                   (list (numV 1)))
             mt-store))
  (test (interp (parse `{record {a 1} {b 2} {c 3}}) mt-env mt-store)
        (v*s (recV (list 'a 'b 'c)
                   (list (numV 1) (numV 2) (numV 3)))
             mt-store))
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b {+ 2 {unbox b}}}
                            {set-box! b {+ 3 {unbox b}}}
                            {set-box! b {+ 4 {unbox b}}}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  ; set-box! no longer keeps the old box in the store
  ;(test (interp (parse `{set-box! {box 5} 6})
  ;              mt-env
  ;              mt-store)
  ;      (v*s (numV 6)
  ;           (override-store (cell 1 (numV 6))
  ;                           (override-store (cell 1 (numV 5))
  ;                                           mt-store))))
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  ; set-box! no longer keeps the old box in the store
  ;(test (interp (parse `{let {[b (box 5)]}
  ;                        {begin
  ;                          {set-box! b 6}
  ;                          {unbox b}}})
  ;              mt-env
  ;              mt-store)
  ;      (v*s (numV 6)
  ;           (override-store (cell 1 (numV 6))
  ;                           (override-store (cell 1 (numV 5))
  ;                                           mt-store))))
  (test (interp (parse `{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             mt-store)))
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                           {set-box! b 2}
                           {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))

  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
        "not a box")
  (test/exn (interp (parse `{set-box! 1 2}) mt-env mt-store)
        "not a box"))

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

;; sequence -----------------------------------------------
;; val will just hold the result of each interp so that we can return the result
;; on the last call to interp. All other vals will just be discarded.
(define (sequence [val : Value] [lst : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (type-case (Listof Exp) lst
    [empty (v*s val sto)]
    [(cons e rst) (with [(val-ret sto-ret) (interp e env sto)]
                        (sequence val-ret rst env sto-ret))]))

(module+ test
  (test (sequence (numV 0) (list (numE 1)) mt-env mt-store)
        (v*s (numV 1) mt-store))
  (test (sequence (numV 0) (list (numE 1) (numE 2) (numE 3) (numE 4))
                  mt-env
                  mt-store)
        (v*s (numV 4) mt-store)))

;; rec-eval -----------------------------------------------

;; takes two parrell lists (symbols and expressions) interps all the expressions and
;; cons them onto a third list of values. the final list of values is flipped so be
;; sure to flip the list in the result returned. 
(define (rec-eval [ls : (Listof Symbol)]
                  [lv : (Listof Value)]
                  [le : (Listof Exp)]
                  [env : Env]
                  [sto : Store]) : Result
  (type-case (Listof Exp) le
    [empty (v*s (recV ls (reverse lv)) sto)]
    [(cons e rst) (with [(val-ret sto-ret) (interp e env sto)]
                        (rec-eval ls (cons val-ret lv) rst env sto-ret))]))
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
   [empty 0]
   [(cons c rst-sto) (max (cell-location c)
                          (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
   [empty (error 'interp "unallocated location")]
   [(cons c rst-sto) (if (equal? l (cell-location c))
                         (cell-val c)
                         (fetch l rst-sto))]))

(define (update-store [l : Location] [v : Value] [sto : Store]) : Store
  (type-case (Listof Storage) sto
    [empty (error 'interp "unallocated location")]
    [(cons c rst-sto) (if (equal? l (cell-location c))
                          (cons (cell l v) (rest sto))
                          (cons c (update-store l v rst-sto)))]))

(module+ test
  (test (update-store 2 (numV 0) (override-store (cell 1 (numV 1))
                                                 (override-store (cell 2 (numV 2))
                                                                 mt-store)))
        (list (cell 1 (numV 1)) (cell 2 (numV 0))))
  (test (update-store 1 (numV 0) (override-store (cell 1 (numV 1))
                                                 mt-store))
        (list (cell 1 (numV 0))))
  (test/exn (update-store 1 (numV 0) mt-store)
        "unallocated location")
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))

;; find & update ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Value)])
  : Value
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (first vs)
             (find n (rest ns) (rest vs)))]))

;; Takes a name n, value v, and two parallel lists, returning a list
;; like the second of the given lists, but with v in place
;; where n matches the item from the first list.
(define (update [n : Symbol]
                [v : Value]
                [ns : (Listof Symbol)]
                [vs : (Listof Value)]) : (Listof Value)
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (cons v (rest vs))
              (cons (first vs) 
                    (update n v (rest ns) (rest vs))))]))

(module+ test
  (test (find 'a (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 1))
  (test (find 'b (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 2))
  (test/exn (find 'a empty empty)
            "no such field")

  (test (update 'a (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 0) (numV 2)))
  (test (update 'b (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 1) (numV 0)))
  (test/exn (update 'a (numV 0) empty empty)
            "no such field"))


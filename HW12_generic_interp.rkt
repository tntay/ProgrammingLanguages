#|
CS 3520 Homework 12
Due: Wednesday, December 5th, 2018 11:59pm
Trenton Taylor
u0872466

A Generic Interpreter
The interpereters number-lambda.rkt and string-lambda.rkt are extremely similar.
The former is our usual interpreter with first-class functions. The latter has
string expressions and values instead of number expressions and values, and it was
created by copying all of the code in number-lambda.rkt and modifying it to work
with strings.

Instead of copying the parse, interp, and lookup functions, we’d like to have a
single implementation that works with both. We don’t want numbers to work in
string programs or strings to work in number programs, though; we want to keep
the languages separate. When we start with an S-expression, we will decide whether
to parse and interpret it as a number program or a string program.

The interpereters number-lit-lambda.rkt and string-lit-lambda.rkt are even more
similar than number-lambda.rkt and string-lambda.rkt, because the literal-expression
constructor is renamed from numE or strE to litE in both interpreters. Similarly,
numV and strV are renamed to litV. New entry points parse/num and interp/num or
 parse/str and interp/str will let us pick which kind of program we want to parse
and interpret.

Your task is to merge number-lit-lambda.rkt and string-lit-lambda.rkt into a single
module with a single implementation of parse, interp, and lookup. All of the tests
in number-lit-lambda.rkt and string-lit-lambda.rkt should work unmodified with the
merged implementation.

To merge the implementation, you will need to change Exp to be parameterized over
a type for literals, so that parse/num returns a (Exp Number), and parse/str returns
a (Exp String). You’ll have to parameterize other types, and you’ll have to
parameterize functions over other functions.
|#


#lang plait

(define-type (Valueof 'a)
  (litV [n : 'a])
  (closV [arg : Symbol]
         [body : (Expof 'a)]
         [env : (Listof (Bindingof 'a))]))

(define-type (Expof 'a)
  (litE [n : 'a])
  (idE [s : Symbol])
  (plusE [l : (Expof 'a)] 
         [r : (Expof 'a)])
  (multE [l : (Expof 'a)]
         [r : (Expof 'a)])
  (lamE [n : Symbol]
        [body : (Expof 'a)])
  (appE [fun : (Expof 'a)]
        [arg : (Expof 'a)]))

(define-type (Bindingof 'a)
  (bind [name : Symbol]
        [val : (Valueof 'a)]))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]
               [pat : S-Exp]
               [s-exp-> : (S-Exp -> 'a)])
  (cond
    [(s-exp-match? pat s) (litE (s-exp-> s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)) pat s-exp->)
            (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)) pat s-exp->)
            (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s)) pat s-exp->))
             (parse (second bs) pat s-exp->)))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)) pat s-exp->)
           (parse (second (s-exp->list s)) pat s-exp->))]
    [else (error 'parse "invalid input")]))

(define (parse/num [s : S-Exp]) : (Expof 'a)
  (parse s `NUMBER s-exp->number))

(define (parse/str [s : S-Exp]) : (Expof 'a)
  (parse s `STRING s-exp->string))

(module+ test
  ;; number
  (test (parse/num `2)
        (litE 2))
  (test (parse/num `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/num `{+ 2 1})
        (plusE (litE 2) (litE 1)))
  (test (parse/num `{* 3 4})
        (multE (litE 3) (litE 4)))
  (test (parse/num `{+ {* 3 4} 8})
        (plusE (multE (litE 3) (litE 4))
               (litE 8)))
  (test (parse/num `{let {[x {+ 1 2}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE 1) (litE 2))))
  (test (parse/num `{lambda {x} 9})
        (lamE 'x (litE 9)))
  (test (parse/num `{double 9})
        (appE (idE 'double) (litE 9)))
  (test/exn (parse/num `{{+ 1 2}})
            "invalid input")
  (test/exn (parse/num `"a")
            "invalid input")
  ;; string
  (test (parse/str `"a")
        (litE "a"))
  (test (parse/str `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/str `{+ "b" "a"})
        (plusE (litE "b") (litE "a")))
  (test (parse/str `{* "c" "d"})
        (multE (litE "c") (litE "d")))
  (test (parse/str `{+ {* "c" "d"} "e"})
        (plusE (multE (litE "c") (litE "d"))
               (litE "e")))
  (test (parse/str `{let {[x {+ "a" "b"}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE "a") (litE "b"))))
  (test (parse/str `{lambda {x} "g"})
        (lamE 'x (litE "g")))
  (test (parse/str `{double "g"})
        (appE (idE 'double) (litE "g")))
  (test/exn (parse/str `{{+ "a" "b"}})
            "invalid input")
  (test/exn (parse/str `1)
            "invalid input"))

;; interp ----------------------------------------
(define interp : ((Expof 'a)
                  [(Valueof 'a) (Valueof 'a) -> (Valueof 'a)]
                  [(Valueof 'a) (Valueof 'a) -> (Valueof 'a)]
                  (Listof (Bindingof 'a)) -> (Valueof 'a))
  (lambda (a func+ func* env)
    (type-case (Expof 'a) a
      [(litE n) (litV n)]
      [(idE s) (lookup s env)]
      [(plusE l r) (func+ (interp l func+ func* env) (interp r func+ func* env))]
      [(multE l r) (func* (interp l func+ func* env) (interp r func+ func* env))]
      [(lamE n body)
       (closV n body env)]
      [(appE fun arg) (type-case (Valueof 'a) (interp fun func+ func* env)
                        [(closV n body c-env)
                         (interp body
                                 func+
                                 func*
                                 (extend-env
                                  (bind n
                                        (interp arg func+ func* env))
                                  c-env))]
                        [else (error 'interp "not a function")])])))

(define (interp/num [a : (Expof 'a)] [env : (Listof (Bindingof 'a))]) : (Valueof 'a)
  (interp a num+ num* env))

(define (interp/str [a : (Expof 'a)] [env : (Listof (Bindingof 'a))]) : (Valueof 'a)
  (interp a str+ str* env))

(module+ test
  ;; number
  (test (interp/num (parse/num `2) mt-env)
        (litV 2))
  (test/exn (interp/num (parse/num `x) mt-env)
            "free variable")
  (test (interp/num (parse/num `x) 
                    (extend-env (bind 'x (litV 9)) mt-env))
        (litV 9))
  (test (interp/num (parse/num `{+ 2 1}) mt-env)
        (litV 3))
  (test (interp/num (parse/num `{* 2 1}) mt-env)
        (litV 2))
  (test (interp/num (parse/num `{+ {* 2 3} {+ 5 8}})
                    mt-env)
        (litV 19))
  (test (interp/num (parse/num `{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {+ x x}})
                    mt-env)
        (litV 10))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[x {+ 1 x}]}
                                    {+ x x}}})
                    mt-env)
        (litV 12))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[y 6]}
                                    x}})
                    mt-env)
        (litV 5))
  (test (interp/num (parse/num `{{lambda {x} {+ x x}} 8})
                    mt-env)
        (litV 16))

  (test/exn (interp/num (parse/num `{1 2}) mt-env)
            "not a function")
  (test/exn (interp/num (parse/num `{+ 1 {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/num (parse/num `{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y 5]}
                                        {bad 2}}})
                        mt-env)
            "free variable")
  ;; string
  ;(test (interp/str (parse/str `"a") mt-env)
  ;      (litV "a"))
  (test (interp/str (parse/str `"b") mt-env)
        (litV "b"))
  (test/exn (interp/str (parse/str `x) mt-env)
            "free variable")
  (test (interp/str (parse/str `x) 
                    (extend-env (bind 'x (litV "g")) mt-env))
        (litV "g"))
  (test (interp/str (parse/str `{+ "b" "a"}) mt-env)
        (litV "ba"))
  (test (interp/str (parse/str `{* "b" "a"}) mt-env)
        (litV "a"))
  (test (interp/str (parse/str `{+ {* "a" "b"} {+ "c" "d"}})
                    mt-env)
        (litV "bcd"))
  (test (interp/str (parse/str `{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {+ x x}})
                    mt-env)
        (litV "ee"))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {let {[x {+ "a" x}]}
                                    {+ x x}}})
                    mt-env)
        (litV "aeae"))
  (test (interp/str (parse/str `{let {[x "e"]}
                                  {let {[y "f"]}
                                    x}})
                    mt-env)
        (litV "e"))
  (test (interp/str (parse/str `{{lambda {x} {+ x x}} "f"})
                    mt-env)
        (litV "ff"))

  (test/exn (interp/str (parse/str `{"a" "b"}) mt-env)
            "not a function")
  (test/exn (interp/str (parse/str `{+ "a" {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/str (parse/str `{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y "e"]}
                                        {bad "b"}}})
                        mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define num-op : ((Number Number -> Number)
                  (Valueof 'a)
                  (Valueof 'a)
                  -> (Valueof 'a))
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (num+ [l : (Valueof 'a)] [r : (Valueof 'a)]) : (Valueof 'a)
  (num-op + l r))
(define (num* [l : (Valueof 'a)] [r : (Valueof 'a)]) : (Valueof 'a)
  (num-op * l r))

(module+ test
  (test (num+ (litV 1) (litV 2))
        (litV 3))
  (test (num* (litV 2) (litV 3))
        (litV 6)))

;; str+ and str* ----------------------------------------
(define str-op : ((String String -> String)
                  (Valueof 'a)
                  (Valueof 'a)
                  -> (Valueof 'a))
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (str+ [l : (Valueof 'a)] [r : (Valueof 'a)]) : (Valueof 'a)
  (str-op string-append l r))
(define (str* [l : (Valueof 'a)] [r : (Valueof 'a)]) : (Valueof 'a)
  (str-op string-mult l r))

(define (string-mult [a : String] [b : String])
  (foldl (lambda (c r) (string-append b r))
         ""
         (string->list a)))

(module+ test
  (test (str+ (litV "abc") (litV "de"))
        (litV "abcde"))
  (test (str* (litV "abc") (litV "de"))
        (litV "dedede"))
  (test/exn (str+ (closV 'sym (litE "a") mt-env) (litV "a"))
            "not a literal"))

;; lookup ----------------------------------------
(define lookup : (Symbol (Listof (Bindingof 'a)) -> (Valueof 'a))
  (lambda (n env)
    (cond
      [(empty? env) (error 'lookup "free variable")]
      [else (cond
              [(symbol=? n (bind-name (first env)))
               (bind-val (first env))]
              [else (lookup n (rest env))])])))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV 8)) mt-env))
        (litV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'x (litV 8)) mt-env)))
        (litV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'y (litV 8)) mt-env)))
        (litV 8)))
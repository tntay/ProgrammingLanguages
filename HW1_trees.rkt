#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Trenton Taylor
; u0872466
; HW1
; August 29, 2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ------- data analysis -------

;; Tree
(define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))


; -------- examples -----------


(module+ test
  (print-only-errors #t)

  ;; sum tests
  (test (sum (node 5 (leaf 6) (leaf 7)))
        18)
  (test (sum (leaf 1))
        1)
  (test (sum (leaf 0))
        0)
  (test (sum (node 1 (leaf 1) (leaf 1)))
        3)
  (test (sum (node 1 (node 1 (leaf 1) (leaf 1)) (leaf 1)))
        5)
  (test (sum (node -1 (node -1 (leaf -1) (leaf -1)) (leaf -1)))
        -5)
  (test (sum (node 0 (node -1 (leaf -2) (leaf -1)) (leaf 4)))
        0)

  ;; negate tests
  (test (negate (node 5 (leaf 6) (leaf 7)))
        (node -5 (leaf -6) (leaf -7)))
  (test (negate (leaf 1))
        (leaf -1))
  (test (negate (leaf 0))
        (leaf 0))
  (test (negate (node -7 (leaf -6) (leaf -5)))
        (node 7 (leaf 6) (leaf 5)))
  (test (negate (node 0 (leaf 0) (leaf 0)))
        (node 0 (leaf 0) (leaf 0)))
  (test (negate (node 1 (node 2 (leaf 3) (leaf 4)) (leaf 5)))
        (node -1 (node -2 (leaf -3) (leaf -4)) (leaf -5)))

  ;; contains? tests
  (test (contains? (node 5 (leaf 6) (leaf 7)) 6)
        #t)
  (test (contains? (leaf 7) 7)
        #t)
  (test (contains? (leaf 7) 8)
        #f)
  (test (contains? (leaf 1+2i) 1+2i)
        #t)
  (test (contains? (node 5 (leaf 4) (leaf 2)) 5)
        #t)
  (test (contains? (node 2 (leaf 3) (leaf 4)) 4)
        #t)
  (test (contains? (node 1 (node 2 (leaf 3) (leaf 4)) (leaf 5)) 3)
        #t)
  (test (contains? (node 1 (node 2 (leaf 3) (leaf 4)) (leaf 5)) 7)
        #f)

  ;; big-leaves? tests
  (test (big-leaves? (node 5 (leaf 6) (leaf 7)))
        #t)
  (test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))
        #f)
  (test (big-leaves? (leaf 0))
        #f)
  (test (big-leaves? (leaf 1))
        #t)
  (test (big-leaves? (leaf -1))
        #f)
  (test (big-leaves? (node -1 (leaf 1) (leaf 2)))
        #t)
  (test (big-leaves? (node 0 (leaf -1) (leaf -2)))
        #f)
  (test (big-leaves? (node 2 (node 5 (leaf 8) (leaf 9)) (node 7 (leaf 10) (leaf 11))))
      #t)
  (test (big-leaves? (node 2 (node 5 (leaf 8) (leaf 9)) (node 7 (leaf 10) (leaf 3))))
      #f)
  (test (big-leaves? (node 0 (leaf 0) (leaf 0)))
        #f)
  (test (big-leaves? (node 0 (leaf 0) (leaf 1)))
        #f)
  (test (big-leaves? (node 0 (leaf 1) (leaf 1)))
        #t)
  (test (big-leaves? (node -2 (leaf 0) (leaf -1)))
        #t)

  ;; sorted tests
  (test (sorted? (leaf 0))
        #t)
  (test (sorted? (leaf 7))
        #t)
  (test (sorted? (node 2 (leaf 1) (leaf 3)))
        #t)
  (test (sorted? (node 3 (leaf 4) (leaf 5)))
        #f)
  (test (sorted? (node 2 (leaf 1) (node 4 (leaf 3) (leaf 5))))
        #t)
  (test (sorted? (node 2 (leaf 1) (node 3 (leaf 4) (leaf 5))))
        #f)
  (test (sorted? (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 5)))
        #t)
  (test (sorted? (node 4 (node 3 (leaf 1) (leaf 2)) (leaf 5)))
        #f)
  (test (sorted? (node 0 (node 1 (leaf -1) (leaf 5)) (leaf 3)))
        #f)
  (test (sorted? (node 5 (leaf 4) (node 6 (leaf 7) (leaf 8))))
        #f)
  ) ;(module+ test


; -------- template/body --------

;; sum
;; @brief sums the numbers in the tree
(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf i) i]
    [(node ii l r) (+ ii
                      (+ (sum l)
                         (sum r)))]))


;; negate
;; @brief negates all numbers in the tree without changing the
;;  shape of the tree
(define (negate [t : Tree]) : Tree
  (type-case Tree t
  [(leaf i) (leaf (* i -1))]
  [(node ii l r) (node (* ii -1) (negate l) (negate r))]))


;; contains?
;; @brief takes a tree and a number and returns #t if number is
;;  in the tree and #f otherwise
(define (contains? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf i) (if (equal? i n)
                  #t
                  #f)]
    [(node ii l r) (cond
                     [(equal? ii n) #t]
                     [(contains? l n) #t]
                     [(contains? r n) #t]
                     [#t #f])]))


;; big-leaves?
;; @brief takes a tree and returns #t if every leaf is bigger
;;  than the sum of numbers in the path of nodes from the root
;;  that reaches the leaf.
(define (big-leaves? [t : Tree]) : Boolean
  (type-case Tree t
    [(leaf i) (bigger-leaves? t 0)]
    [(node ii l r) (and (bigger-leaves? l ii)
                        (bigger-leaves? r ii))]))

;; bigger-leaves?
;; @brief helper function with an accumulater to keep track of sum so far from
;;  root node.
(define (bigger-leaves? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf i) (if (> i n)
                  #t
                  #f)]
    [(node ii l r)  (and (bigger-leaves? l (+ n ii))
                         (bigger-leaves? r (+ n ii)))]))


;; sorted?
;; @brief determines if a tree is sorted in terms of an in order traversal
(define (sorted? [t : Tree]) : Boolean
  (type-case Tree t
    [(leaf i) #t]
    [(node ii l r) (and (lsub-tree-sorted? l ii)
                        (rsub-tree-sorted? r ii))]))

;; lsub-tree-sorted?
;; @brief helper function to check if left sub tree is sorted
(define (lsub-tree-sorted? [t : Tree] [prev : Number]) : Boolean
  (type-case Tree t
    [(leaf i) (if (> prev i)
                  #t
                  #f)]
    [(node ii l r) (if (and (> prev ii)
                            (lsub-tree-sorted? l ii)
                            (rsub-tree-sorted? r ii))
                       #t
                       #f)]))

;; rsub-tree-sorted?
;; @brief helper function to check if right sub tree is sorted
(define (rsub-tree-sorted? [t : Tree] [prev : Number]) : Boolean
  (type-case Tree t
    [(leaf i) (if (> i prev)
                  #t
                  #f)]
    [(node ii l r) (if (and (> ii prev)
                            (lsub-tree-sorted? l ii)
                            (rsub-tree-sorted? r ii))
                       #t
                       #f)]))



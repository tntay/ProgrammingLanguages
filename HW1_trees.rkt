#|
CS 3520 Homework 1
Due: Wednesday, August 29th, 2018 11:59pm
Trenton Taylor
u0872466

The following Tree datatype implements a binary tree with a number in each
node and leaf:

  (define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))

Part 1 — Sum
Implement a sum function that takes a tree and returns the sum of the numbers
in the tree.

Example: (sum (node 5 (leaf 6) (leaf 7))) should produce 18.

Part 2 — Negate
Implement the function negate, which takes a tree and returns a tree that has
the same shape, but with all the numbers negated.

Example: (negate (node 5 (leaf 6) (leaf 7))) should produce (node -5 (leaf -6)
(leaf -7)).

Part 3 — Contains?
Implement the function contains?, which takes a tree and a number and returns
#t if the number is in the tree, #f otherwise.

Example: (contains? (node 5 (leaf 6) (leaf 7)) 6) should produce #t.

The second argument to the contains? function is “along for the ride.”

Part 4 — Big Leaves?
Implement the function big-leaves?, which takes a tree and returns #t if every
leaf is bigger than the sum of numbers in the path of nodes from the root that
reaches the leaf.

Examples: (big-leaves? (node 5 (leaf 6) (leaf 7))) should produce #t, while
(big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) should produce #f
(since 6 is smaller than 5 plus 2).

The big-leaves? function should be a thin wrapper on another function, perhaps
bigger-leaves?, that accumulates a sum of node values.

Part 5 — Optional challenge: Sorted?
Implement the function sorted?, which takes a tree and determines whether it is
sorted in the sense that the numbers increase (or stay the same) in a inorder
travsersal of the tree.

Your function should run in time proportional to the size of the tree, which rules
out making a list of the tree numbers using append on recursive calls. One possible
solution accumulates a compound value, and another possible solution accumulates
information on the way down the tree and returns more than just success or failure.
|#

#lang plait


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



#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Trenton Taylor
; u0872466
; HW0
; August 24, 2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ------- type definitions ------------

; Light
(define-type Light
    (bulb [watts : Number]
          [technology : Symbol])
    (candle [inches : Number]))


; ------- function definitions ---------

;; 3rd-power
(define (3rd-power [n : Number]) : Number
  (* n (* n n)))

;; 42nd-power
(define (42nd-power [n : Number]) : Number
  (* (3rd-power n)
     (* (3rd-power n)
        (* (3rd-power n)
           (* (3rd-power n)
              (* (3rd-power n)
                 (* (3rd-power n)
                    (* (3rd-power n)
                       (* (3rd-power n)
                          (* (3rd-power n)
                             (* (3rd-power n)
                                (* (3rd-power n)
                                   (* (3rd-power n)
                                      (* (3rd-power n)
                                         (3rd-power n)))))))))))))))

;; plural
(define (plural [str : String]) : String
  (if (equal? (string-length str) 0) ; treat empty string as any string not ending in y.
      (string-append str "s")
      (if (equal? (string-ref str (- (string-length str) 1)) #\y)
          (string-append (substring str 0 (- (string-length str) 1)) "ies")
          (string-append str "s"))))

;; energy-usage
(define (energy-usage [kwh : Light]) : Number
  (type-case Light kwh
    [(bulb w t) (* 24
                   (/ w 1000.0))]
    [(candle i) 0]))

;; use-for-one-hour
(define (use-for-one-hour [l : Light]) : Light
  (type-case Light l
    [(bulb w t) l]
    [(candle i) (if (equal? i 0.0)
                    (candle 0.0)
                    (candle (- i 1.0)))]))
  

; ------ examples / tests -----------

; 3rd-power
(test (3rd-power 17)
      4913)
(test (3rd-power 2)
      8)
(test (3rd-power 1)
      1)
(test (3rd-power 0)
      0)

; 42nd-power
(test (42nd-power 17)
      4773695331839566234818968439734627784374274207965089)
(test (42nd-power 2)
      4398046511104)
(test (42nd-power 1)
      1)
(test (42nd-power 0)
      0)

; plural
(test (plural "baby")
      "babies")
(test (plural "fish")
      "fishs")
(test (plural "hahaha")
      "hahahas")
(test (plural "May")
      "Maies")
(test (plural "")
      "s")
(test (plural "p")
      "ps")
(test (plural "y")
      "ies")
(test (plural "yy")
      "yies")

; energy-usage
(test (energy-usage (bulb 100.0 'halogen))
      2.4)
(test (energy-usage (bulb 240.0 'big_bulb))
      5.76)
(test (energy-usage (bulb 0.0 'dead_bulb))
      0.0)
(test (energy-usage (candle 10.0))
      0.0)

; user-for-one-hour
(test (use-for-one-hour (bulb 100.0 'halogen))
      (bulb 100.0 'halogen))
(test (use-for-one-hour (bulb 240.0 'big_bulb))
      (bulb 240.0 'big_bulb))
(test (use-for-one-hour (bulb 0.0 'dead_bulb))
      (bulb 0.0 'dead_bulb))
(test (use-for-one-hour (candle 10.0))
      (candle 9.0))
(test (use-for-one-hour (candle 1.0))
      (candle 0.0))
(test (use-for-one-hour (candle 0.0))
      (candle 0.0))



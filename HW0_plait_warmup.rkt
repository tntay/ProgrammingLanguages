#|
CS 3520 Homework 0
Due: Friday, August 24th, 2018 11:59pm
Trenton Taylor
u0872466

Part 0 — Create Handin Account
After installing the uu-cs3520 package as described in the course web page,
select the Manage CS 3520 Handin... menu item from DrRacket’s File menu.
Change to the New User panel, and pick a username and password. (Use your
real name and real Utah student ID, so that we can connect your homework
submissions with you.)

You will have to install uu-cs3520 for DrRacket on each different filesystem
that you use to get a Handin button. However, after creating a handin account
once from any machine, you can use DrRacket’s Handin button on any other machine.

Use #lang plait at the start of your program, so that it’s implemented in the
Plait language.

Part 1 — Implement 3rd-power
Define the function 3rd-power, which takes a number and raises it to the 3rd power.

Your program should include test forms to check 3rd-power on a few inputs.

Example use: (3rd-power 17) should produce 4913.

Part 2 — Implement 42nd-power
Define the function 42nd-power (in the same program), which takes a number and
raises it to the 42nd power.

It’s probably a good idea to use functions like 3rd-power to build up to 42nd-power.

Your program should include test forms to check 42nd-power on a few inputs.

Example use: (42nd-power 17) should produce
4773695331839566234818968439734627784374274207965089.

Part 3 — Implement plural
Define plural, which takes a string and returns a string. If the given string
ends in “y”, the the result should be the same as the input but with the “y”
replaced by “ies”. Otherwise, the result should be the same as the given string
with “s” added to the end.

Your program should include test forms to check plural on a few inputs.

Example uses: (plural "baby") should produce "babies", while (plural "fish") should
produce "fishs".

Part 4 — Implement energy-usage
Use the following type definition (add it to your program):

  (define-type Light
    (bulb [watts : number]
          [technology : symbol])
    (candle [inches : number]))

Implement the function enery-usage, which takes a Light and produces the number
of kilowatthours of electricity that the light uses in 24 hours. Your function
will need to use (type-case Light ....).

As always, include relevant test forms.

Example uses: (energy-usage (bulb 100.0 'halogen)) should produce 2.4, while
(energy-usage (candle 10.0)) should produce 0.0 or 0.

Part 5 — Implement use-for-one-hour
Implement the function use-for-one-hour, which takes a Light and produces another
Light that represents the given light source after it is used for another hour.
Assume that a candle burns one inch per hour (unless it is already gone), and assume
that a lightbulb is the same after one hour of use.

Example uses: (use-for-one-hour (bulb 100.0 'halogen)) should produce
(bulb 100.0 'halogen), while (use-for-one-hour (candle 10.0)) should produce
(candle 9.0).
|#

#lang plait



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



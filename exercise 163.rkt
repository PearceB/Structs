;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |exercise 163|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Exercise 163

(define-struct mail (from date message))
; A Mail Message is a structure: 
; – (make-mail String Number String)
; interp. (make-mail f d m) represents text m sent by
; f, d seconds after the beginning of time

; LOM (List-of-mail) is one of:
; - empty
; - (cons Mail LOM)
; interp. a List-of-Mail is a list of the Mail

; Mail LOM -> LOM
; produces a sorted version of LOM
(check-expect (insert empty) empty)
(check-expect (insert (make-mail "Bob" 2 "Hello, my name is Bob!")
                      (list (make-mail "Robert" 1 "Kon nishiwa")
                            (make-mail "Bobby" 5 "Hola, Anonymous!")))
              (list (make-mail "Robert" 1 "Kon nishiwa")
                     (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")))

(define (insert Mail LOM)
  LOM)

; LOM -> LOM
; Consume a LOM and return a LOM that has been sorted by date in ascending order
(check-expect (sort-mail empty) empty)
(check-expect (sort-mail
               (list (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")
                     (make-mail "Robert" 1 "Kon nishiwa")))
               (list (make-mail "Robert" 1 "Kon nishiwa")
                     (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")))

(define (sort-mail LOM)
  (cond
    [(empty? LOM) empty]
    [else
     (insert (first LOM) (sort-mail (rest LOM)))]))
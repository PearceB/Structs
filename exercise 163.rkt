;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |exercise 163|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
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
; insert a Mail into the sorted LOM by date
(check-expect (insert-date (make-mail "Bob" 2 "Hello, my name is Bob!") empty)
              (list (make-mail "Bob" 2 "Hello, my name is Bob!")))
(check-expect (insert-date (make-mail "Bob" 2 "Hello, my name is Bob!")
                      (list (make-mail "Robert" 1 "Kon nishiwa")
                            (make-mail "Bobby" 5 "Hola, Anonymous!")))
              (list (make-mail "Robert" 1 "Kon nishiwa")
                     (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")))

(define (insert-date Mail LOM)
  (cond
    [(empty? LOM) (list Mail)]
    [else (if (<= (mail-date Mail) (mail-date (first LOM)))
              (cons Mail LOM)
              (cons (first LOM) (insert-date Mail (rest LOM))))]))  
     
; LOM -> LOM
; produces a sorted version of LOM by date in ascending order
(check-expect (sort->mail_date empty) empty)
(check-expect (sort->mail_date
               (list (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")
                     (make-mail "Robert" 1 "Kon nishiwa")))
               (list (make-mail "Robert" 1 "Kon nishiwa")
                     (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")))

(define (sort->mail_date LOM)
  (cond
    [(empty? LOM) empty]
    [else
     (insert-date (first LOM) (sort->mail_date (rest LOM)))]))

; Mail LOM -> LOM
; insert a Mail into the sorted LOM by name
(check-expect (insert-name (make-mail "Bob" 2 "Hello, my name is Bob!") empty)
              (list (make-mail "Bob" 2 "Hello, my name is Bob!")))
(check-expect (insert-name (make-mail "Bob" 2 "Hello, my name is Bob!")
                      (list (make-mail "Bobby" 5 "Hola, Anonymous!")
                            (make-mail "Robert" 1 "Kon nishiwa")))
              (list (make-mail "Bob" 2 "Hello, my name is Bob!")
                    (make-mail "Bobby" 5 "Hola, Anonymous!")
                    (make-mail "Robert" 1 "Kon nishiwa")))

(define (insert-name Mail LOM)
  (cond
    [(empty? LOM) (list Mail)]
    [else (if (string<? (mail-from Mail) (mail-from (first LOM)))
              (cons Mail LOM)
              (cons (first LOM) (insert-name Mail (rest LOM))))]))

; LOM -> LOM
; produces a sorted version of LOM by name in alphabetical order
(check-expect (sort->mail_name empty) empty)
(check-expect (sort->mail_name
               (list (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")
                     (make-mail "Robert" 1 "Kon nishiwa")))
               (list (make-mail "Bob" 2 "Hello, my name is Bob!")
                     (make-mail "Bobby" 5 "Hola, Anonymous!")
                     (make-mail "Robert" 1 "Kon nishiwa")))

(define (sort->mail_name LOM)
  (cond
    [(empty? LOM) empty]
    [else
     (insert-name (first LOM) (sort->mail_name (rest LOM)))]))
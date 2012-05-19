(module engine (encript-password make-salt)
  (import scheme chicken)
  (use message-digest sha2 md5)

(define (sha2-encript str)
  (message-digest-string (sha256-primitive) str))

(define (md5-encript str)
  (message-digest-string (md5-primitive) str))

;;Make it random without using random generator function
;;If the attacker know the username and time he cannot guess the salt.
;;If the attacker know the password and time he cannot guess the salt.
;;If the attacker know the time in seconds, he will have a hard time guessing the time in milliseconds.
(define (make-salt username password)
  (substring 
    (md5-encript
      (string-append (number->string (current-seconds)) 
                     (number->string (current-milliseconds))
                     username 
                     password))
  0
  16))

;;Store password in hash value.
(define (encript-password password salt)
  (sha2-encript (string-append salt password)))

)

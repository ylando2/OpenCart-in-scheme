(require-extension irregex)
(require-extension srfi-69)

;Temporary before we find or create a faster way. 
(define (render str hash)
  (irregex-replace/all 
    '(: "<%" (* space) ($ (*? any)) (* space) "%>") 
    str 
    (lambda (x) 
      (let ((str2 (irregex-match-substring x 1)))
        (hash-table-ref/default hash str2 str2)))))

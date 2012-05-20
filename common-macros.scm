(module common-macros (ds-set! ds-let ds-define
                       if*
                       /= div rem inc inc! dec dec!
                       vref vset! hset! href 
                       enum-case
                       $i $f
                       when-let if-let swap! push! pop! 
                       point define+
                       while do-while until do-until 
                       each up down repeat
                       dbg
                       get* down* add-hash-table! compile-each)
  (import scheme chicken)
  
  (define-syntax ds-define-false
    (syntax-rules (o)
      ((_ ()) (void))
      ((_ ((o arg val) . rest))
       (begin
         (define arg #f)
         (ds-define-false rest)))
      ((_ (arg . rest))
       (begin
         (ds-define-false arg)
         (ds-define-false rest)))
      ((_ arg)
       (define arg #f))))

  (define-for-syntax (ds-let-helper vars rest body compare?)
     (if (null? vars) 
       `(begin ,@body)
       (if (atom? vars)
         `(let ((,vars ,rest)) ,@body)
         (let ((var (car vars)))
           (if (null? (cdr vars))
             (if (pair? var)
               (if (compare? (car var) 'o)
                 ;;(ds-let ((o x val)) lst body ...)
                 `(let ((,(cadr var) 
                          (if (null? ,rest) ,(caddr var) (car ,rest))))
                    ,@body)
                 ;;(ds-let ((x y ...)) lst body ...)
                 `(let ((,rest (car ,rest)))
                    ,(ds-let-helper var rest body compare?)))
               ;;(ds-let (x) lst body ...)
               `(let ((,var (car ,rest))) ,@body))
             (if (pair? var)
               (if (compare? (car var) 'o)
                 (if (atom? (cdr vars))
                   ;;(ds-let ((o x val) . rest) lst body ...)
                   `(let ((,(cadr var) 
                            (if (null? ,rest) ,(caddr var) (car ,rest))) 
                          (,(cdr vars) 
                            (if (null? ,rest) '() (cdr ,rest)))) ,@body)
                   ;;(ds-let ((o x val) y z ...) lst body ...)
                   `(let ((,(cadr var) 
                            (if (null? ,rest) ,(caddr var) (car ,rest))) 
                          (,rest (if (null? ,rest) '() (cdr ,rest))))
                      ,(ds-let-helper (cdr vars) rest body compare?)))
                 (let ((head (gensym)))
                   (if (atom? (cdr vars))
                     ;;(ds-let ((a b c ...) . rest) lst body ...)
                     `(let ((,head (car ,rest))) 
                        ,(ds-let-helper var head
                                        (list `(let ((,(cdr vars) (cdr ,rest))) ,@body))
                                        compare?))
                     ;;(ds-let ((a b c ...) x y ...) lst body ...)
                     `(let ((,head (car ,rest)) (,rest (cdr ,rest)))
                        ,(ds-let-helper var head 
                                        (list (ds-let-helper (cdr vars) rest body compare?))
                                        compare?)))))
               (if (atom? (cdr vars))
                 ;;(ds-let (x . y) lst body ...)
                 `(let ((,var (car ,rest)) (,(cdr vars) (cdr ,rest))) ,@body)
                 ;;(ds-let (x y z ...) lst body ...)  
                 `(let ((,var (car ,rest)) (,rest (cdr ,rest)))
                    ,(ds-let-helper (cdr vars) rest body compare?)))))))))
  
  (define-syntax ds-let
    (ir-macro-transformer
      (lambda (expr inject compare?)
        (let ((vars (cadr expr))
              (vals (caddr expr))
              (body (cdddr expr))
              (rest 'rest))
          `(let ((rest ,vals))
              ,(ds-let-helper vars rest body compare?))))))

  
  (define-for-syntax (ds-set!-helper vars rest compare?)
     (if (null? vars) 
       `(void)
       (if (atom? vars)
         `(set! ,vars ,rest)
         (let ((var (car vars)))
           (if (null? (cdr vars))
             (if (pair? var)
               (if (compare? (car var) 'o)
                 ;;(ds-set! ((o x val)) lst)
                 `(set! ,(cadr var) 
                    (if (null? ,rest) ,(caddr var) (car ,rest)))
                 ;;(ds-set! ((x y ...)) lst)
                 `(let ((,rest (car ,rest)))
                    ,(ds-set!-helper var rest compare?)))
               ;;(ds-set! (x) lst)
               `(set! ,var (car ,rest)))
             (if (pair? var)
               (if (compare? (car var) 'o)
                 (if (atom? (cdr vars))
                   ;;(ds-set! ((o x val) . rest) lst)
                   `(begin 
                      (set! ,(cadr var) (if (null? ,rest) ,(caddr var) (car ,rest))) 
                      (set! ,(cdr vars) (if (null? ,rest) '() (cdr ,rest))))
                   ;;(ds-set! ((o x val) y z ...) lst)
                   `(begin
                      (set! ,(cadr var) 
                        (if (null? ,rest) ,(caddr var) (car ,rest))) 
                      (let ((,rest (if (null? ,rest) '() (cdr ,rest))))
                        ,(ds-set!-helper (cdr vars) rest compare?))))
                 (let ((head (gensym)))
                   (if (atom? (cdr vars))
                     ;;(ds-set! ((a b c ...) . rest) lst)
                     `(let ((,head (car ,rest)))
                        ,(ds-set!-helper var head compare?)
                        (set! ,(cdr vars) (cdr ,rest)))
                     ;;(ds-set! ((a b c ...) x y ...) lst)
                     `(let ((,head (car ,rest)) (,rest (cdr ,rest)))
                        ,(ds-set!-helper var head compare?)
                        ,(ds-set!-helper (cdr vars) rest compare?)))))
             (if (atom? (cdr vars))
               ;;(ds-set! (x . y) lst)
               `(begin 
                  (set! ,var (car ,rest)) 
                  (set! ,(cdr vars) (cdr ,rest)))
               ;;(ds-set! (x y z ...) lst)  
               `(begin 
                  (set! ,var (car ,rest)) 
                  (let ((,rest (cdr ,rest)))
                    ,(ds-set!-helper (cdr vars) rest compare?))))))))))
  

  (define-syntax ds-set!
    (ir-macro-transformer
      (lambda (expr inject compare?)
        (let ((vars (cadr expr))
              (vals (caddr expr))
              (body (cdddr expr))
              (rest 'rest))
          `(let ((rest ,vals))
              ,(ds-set!-helper vars rest compare?))))))
  
  (define-syntax ds-define
  (syntax-rules ()
    ((_ arg lst) 
     (begin 
       (ds-define-false arg)
       (ds-set! arg lst)))))
 


(define-for-syntax (split sym lst compare?)
  (let loop ([l lst] [cur '()] [result '()])
    (if (null? l)
        (reverse (cons (reverse cur) result))
        (if (compare? (car l) sym)
            (loop (cdr l) '() (cons (reverse cur) result))
            (loop (cdr l) (cons (car l) cur) result)))))

(define-syntax if*
  (ir-macro-transformer
     (lambda (expr inject compare?)
       (let* ((body (cdr expr))
              (cond-then-lst (split 'then body compare?))
              (co (caar cond-then-lst))
              (then-else (split 'else (cadr cond-then-lst) compare?))
              (then (car then-else)))
              (if (null? (cdr then-else))
                `(when ,co ,@then)
                `(if ,co (begin ,@then) (begin ,@(cadr then-else))))))))

  
;;Add math function from common lisp

(define-syntax /=
  (syntax-rules ()
    [(_ args ...) (not (= args ...))]))

;;quotient and remainder are too long as operators name.
(define-syntax div
  (syntax-rules ()
    [(_ args ...) (quotient args ...)]))

(define-syntax rem
  (syntax-rules ()
    [(_ args ...) (remainder args ...)]))

(define-syntax inc 
  (syntax-rules ()
    [(_ var) (+ var 1)]
    [(_ var val) (+ var val)]))

(define-syntax inc!
  (syntax-rules ()
    [(_ var) (set! var (+ var 1))]
    [(_ var val) (set! var (+ var val))]))

(define-syntax dec 
  (syntax-rules ()
    [(_ var) (- var 1)]
    [(_ var val) (- var val)]))

(define-syntax dec!
  (syntax-rules ()
    [(_ var) (set! var (- var 1))]
    [(_ var val) (set! var (- var val))]))

;;Alias for vector-set! vector-ref hash-set! hash-ref those names are too long.

;;reference to single or multi-dimensional array
(define-syntax vref
  (syntax-rules ()
    [(_ v i) (vector-ref v i)]
    [(_ v i j ...) (vref (vector-ref v i) j ...)]))

;;set a single or multi-dimensional array
(define-syntax vset!
  (syntax-rules ()
    [(_ vec i val) (vector-set! vec i val)]
    [(_ vec i j ... val) (vset! (vector-ref vec i) j ... val)]))

(define-syntax hset!
  (syntax-rules ()
    [(_ args ...) (hash-table-set! args ...)]))

(define-syntax href
  (syntax-rules ()
    [(_ hash key) (hash-table-ref hash key)]
    [(_ hash key default) (hash-table-ref/default hash key default)]))

(define-syntax get*
  (syntax-rules ()
    [(_ hash key value) (if (hash-table-exists? hash key) 
                            (hash-table-ref hash key)
                            (let ([val value])
                              (hash-table-set! hash key val)
                              val))]))


;use with define-constant as c enum
(define-syntax enum-case
  (ir-macro-transformer
     (lambda (expr inject compare?)
       `(case ,(cadr expr)
          ,@(map
             (lambda (case-exp)
               (cons
                 (map
                   (lambda (const-exp)
                     (eval const-exp))
                   (car case-exp))
                 (cdr case-exp)))
             (cddr expr)))))) 


;;Check if op is an unary operator
(define-for-syntax (unary-op? op compare?)
   (compare? op '-))

;;Parse unary operators. 
;;Return a pair of the part that was parsed and the rest of the expression.
(define-for-syntax (uni-parse exp compare?)
  ;;Add the unary operators ops to the expression exp
  (define (add-ops ops exp)
    (if (null? ops)
        exp
        (foldl (lambda (x res) (cons x (list res))) exp ops)))
  ;;Parse the unary operators
  (define (parse-ops exp ops)
    (cond
      [(null? exp) (error (format #f "~a is not a unary operator"))]
      [(unary-op? (car exp) compare?) 
       (parse-ops (cdr exp) (cons (car exp) ops))]
      [else (cons ops exp)]))
  ;;Parse function call,bracket,quotation,or constant values
  (define (parse-function-or-bracket exp)
    (cond 
      ;Quotation look like this: ('quote exp)
      [(or (eq? (car exp) 'quote) (eq? (car exp) 'quasiquote))
           (cons (list (car exp) (cadr exp)) '())]
      ;Bracket look like this: (( exp ) the-rest-of-the-expression ...)
      [(list? (car exp)) (cons (parse (car exp)) (cdr exp))]
      ;Function call look like this: (fn (args ...) the-rest-of-the-expression ...)
      [(and (not (null? (cdr exp))) (list? (cadr exp))) 
       (cons (cons (car exp) (map (lambda (x) (parse (list x))) (cadr exp))) (cddr exp))]
      ;just a constant or variable
      [else (cons (car exp) (cdr exp))]))
  
  (let* ([ops+rest (parse-ops exp '())]
         [ops (car ops+rest)]
         [rest (cdr ops+rest)]
         [exp+rest (parse-function-or-bracket rest)]
         [exp (car exp+rest)]
         [rest2 (cdr exp+rest)])
    (cons (add-ops ops exp) rest2))) ;;End of uni-parse


;;Return the priority of op operator
(define-for-syntax (op-priority op compare?)
  (cond 
    [(compare? op 'or) 0]
    [(compare? op 'and) 1]
    [(compare? op '=) 2]
    [(compare? op '/=) 3]
    [(compare? op '>) 4]
    [(compare? op '<) 5]
    [(compare? op '>=) 6]
    [(compare? op '<=) 7]
    [(compare? op '+) 8]
    [(compare? op '-) 9]
    [(compare? op '*) 10]
    [(compare? op '/) 11]
    [(compare? op 'div) 12]
    [(compare? op 'rem) 13]
    [else (error (format #f "~a is not a binary operator" op))]))

;;Parse the binary operator
;;Return a pair of the part that was parsed and the rest of the expression.
(define-for-syntax (bi-parse left rest compare?)
  (if (null? rest)
      ;;Noting to parse
      (cons left rest)
      (let*
          ([op (car rest)] ;;parse the first operator
           [p (op-priority op compare?)]
           [temp+rest (uni-parse (cdr rest) compare?)]
           [temp (car temp+rest)]
           [rest (cdr temp+rest)])
        (let loop ([temp temp] [rest rest])
          (if (null? rest)
              ;;We finish parsing
              (cons (list op left temp) '())
              ;;Put in the right position operation with higher or equal priority
              ;;If the next operatoion has a smaller priority return the part that was parsed and the rest
              (if (< (op-priority (car rest) compare?) p) 
                  (cons (list op left temp) rest)
                  (let ([temp+rest (bi-parse temp rest compare?)])
                    (loop (car temp+rest) (cdr temp+rest)))))))))

;;Try to parse the expression until there is noting left to parse
(define-for-syntax (parse exp compare?)
  (let loop ([temp+rest (uni-parse exp compare?)])
    (let ([temp (car temp+rest)] [rest (cdr temp+rest)])
      (if (null? rest)
          temp
          (loop (bi-parse temp rest compare?))))))

;;Infix macro
(define-syntax $i
  (ir-macro-transformer
     (lambda (expr inject compare?)
       (parse (cdr expr) compare?))))

(define-syntax dbg
  (syntax-rules ()
    [(_ exp) (let ([val exp]) (format #t "Debug: ~a = ~a~%" 'exp val) val)]))

;;A helper macro similar to -> in clojure
;;use by ($f (fn args ...) (fn2 ... it ...) ...)
;;it assign "it" with the result of the last expression
(define-syntax $f
  (ir-macro-transformer
   (lambda (expr inject compare?)
     (let loop ((lst (cdr expr)))
        (if (null? (cdr lst))
          (car lst)
          `(let ((,(inject 'it) ,(car lst))) ,(loop (cdr lst))))))))

(define-syntax when-let
  (syntax-rules ()
    [(_ (var cond) body ...) (let ([var cond]) (when var body ...))]))

(define-syntax if-let
  (syntax-rules ()
    [(_ (var cond) then-body else-body) (let ([var cond]) (if cond then-body else-body))]))

(define-syntax swap!
  (syntax-rules ()
    [(_ var1 var2) (let ([temp var1]) (set! var1 var2) (set! var2 temp))]))

(define-syntax push!
  (syntax-rules ()
    [(_ val lst) (set! lst (cons val lst))]))

(define-syntax pop!
  (syntax-rules ()
    [(_ lst) (let ([temp (car lst)]) (set! lst (cdr lst)) temp)]))

;;Define a breaking point
(define-syntax point 
  (syntax-rules ()
    [(_ name body ...) (call/cc (lambda (name) body ...))]))

;;Define function that we can break out with return
(define-syntax define+
  (ir-macro-transformer
   (lambda (expr inject compare?)
     `(define ,(cadr expr) 
        (call/cc (lambda (,(inject 'return)) ,@(cddr expr)))))))

;;Loops
(define-syntax while
  (syntax-rules ()
    [(_ cond body ...) (let loop () (when cond body ... (loop)))]))

;;run at least once
(define-syntax do-while
  (syntax-rules ()
    [(_ cond body ...) (let loop () body ... (when cond (loop)))]))

(define-syntax until 
  (syntax-rules ()
    [(_ cond body ...) (let loop () (unless cond body ... (loop)))]))

;;run at least once
(define-syntax do-until
  (syntax-rules ()
    [(_ cond body ...) (let loop () body ... (unless cond (loop)))]))

(define-syntax each
  (syntax-rules ()
    [(_ (var lst) body ...) (for-each (lambda (var) body ...) lst)]))

(define-syntax up
  (syntax-rules ()
    [(_ (var end) body ...) (let loop ([var 0]) (when (< var end) body ... (loop (+ var 1))))]
    [(_ (var start end) body ...) (let loop ([var start]) (when (< var end) body ... (loop (+ var 1))))]
    [(_ (var start end by) body ...) (let loop ([var start]) (when (< var end) body ... (loop (+ var by))))]))

(define-syntax down
  (syntax-rules ()
    [(_ (var start) body ...) (let loop ([var start]) (when (>= var 0) body ... (loop (- var 1))))]
    [(_ (var start end) body ...) (let loop ([var start]) (when (>= var end) body ... (loop (- var 1))))]
    [(_ (var start end by) body ...) (let loop ([var start]) (when (>= var end) body ... (loop (- var by))))]))

(define-syntax down*
  (syntax-rules ()
    [(_ (var start) body ...) (let loop ([var (- start 1)]) (when (>= var 0) body ... (loop (- var 1))))]
    [(_ (var start end) body ...) (let loop ([var (- start 1)]) (when (>= var end) body ... (loop (- var 1))))]
    [(_ (var start end by) body ...) (let loop ([var (- start 1)]) (when (>= var end) body ... (loop (- var by))))]))

(define-syntax repeat
  (syntax-rules ()
    [(_ times body ...) (let loop ([i times]) (when (> i 0) body ... (loop (- i 1))))]))

(define-syntax add-hash-table!
  (syntax-rules (=>)
    [(_ hash key => val) (hash-table-set! hash key val)]
    [(_ hash key => val rest ...) 
     (begin
       (hash-table-set! hash key val)
       (add-hash-table! hash rest ...))]))

(define-syntax compile-each
  (ir-macro-transformer
   (lambda (expr inject compare?)
     (let* ((var/vals (cadr expr))
            (var (car var/vals))
            (vals (cadr var/vals)) 
            (body (cddr expr))) 
     `(begin
        ,@(map
           (lambda (val)
             `(let ((,var ,val)) ,@body))
           vals))))))


);end of module





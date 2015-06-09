
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Converts a pair to a string.
(define (pair->string pair)
  (list->string (list (car pair) (cdr pair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Comparator for the cadr of a list.
(define (cadr>? a b)(> (cadr a) (cadr b)))

(define (simple-eval expr)
  (eval expr (nearest-repl/environment)))

(define (repeat sym n)
  (make-list n sym))

(define (time func #!rest args)
  (with-timings
   (lambda () (apply func args))
   (lambda (run-time gc-time real-time)
     (write (internal-time/ticks->seconds run-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds gc-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds real-time)) (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pp+ #!rest args)
  (newline)
  (for-each pp args))

(define (pp-mat mat #!optional ff)
  (newline)
  (if (integer? ff)
      (set! mat (->ff mat ff)))
  (vector-for-each
   print-expression
   (vector-map vector->list (matrix->array mat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Repeatedly invokes display on a char.
  (define (repeat-display char times)
    (for-each display (make-list times char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAPPINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Maps combinations of elements to proc.
;; Reference positions are traversed by base-traverse.
;; Parameters: proc is a lambda expression.
;; Returns: A list of results from proc.
(define (map-combo proc #!rest lists)
  (bases-traverse
   (lambda (#!rest combo)
     (apply proc (map ref lists combo)))
   (map length lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Traverses a list of elements sequentially.
;; For each list, every element is traversed over every other list's elements.
;; Parameters: proc is a lambda expression
;;             bases is a list of numbers
;;             x, lim, result are not to be used
;; Returns: A list of results from proc.
(define (bases-traverse proc bases #!optional x lim result)
  (if (default-object? x) (set! x 0))
  (if (default-object? lim) (set! lim (apply * bases)))
  (if (default-object? result) (set! result '()))
  (define y x)
  (set! result (append result (list
  (apply proc
	 (map (lambda (ith)
		(define q (integer-divide y ith))
		(set! y (car q))
		(cdr q))
	      bases))
  )))
  (set! x (+ 1 x))
  (cond
   ((>= x lim) result)
   (else
    (bases-traverse proc bases x lim result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: traverses n^k s.t. proc is passed k
;; elements of digit i between -1 < i < n.
(define (traverse proc n k #!optional x lim)
  (if (default-object? x) (set! x 0))
  (if (default-object? lim) (set! lim (expt n k)))
  (define y x)
  (apply proc
	 (reverse
	  (map (lambda (ith)
		 (define q (integer-divide y n))
		 (set! y (car q))
		 (cdr q))
	       (iota k))))
  (set! x (+ 1 x))
  (cond
   ((>= x lim) #t)
   (else
    (traverse proc n k x lim))))

(define (ub-map func #!rest listt) ; unbalanced map (lazy imp)
  (define max-len
    (apply max
	   (map (lambda (item)
		  (if (list? item) (len item) 0))
		listt)))
  (apply map func
	 (map (lambda (item)
		(if (list? item)
		    (if (< (len item) max-len)
			(append item (make-list (- max-len (len item)) (last item)))
			item)
		    (make-list max-len item)))
	      listt)))
;(ub-map list '(1 2 3) '(1 1 3) '(2 2 2))
;(map list '(1 2 3) '(1 1 3) '(2 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flatten-once listt)
  (receive (lists rest)
	   (partition list? listt)
	   (append rest (fold-left append '() lists))))

(define (flatten listt)
  (define (itr result lists?)
    (set! result (flatten-once result))
    (set! lists? (fold-left (lambda (a b)(or a b)) #f (map list? result)))
    (if lists? (itr result lists?) result))
  (if (list? listt)
      (itr listt #t)
      listt))

(define (simple-eval expr)
  (eval expr (nearest-repl/environment)))

(define ref list-ref)

(define (deep-ref listt #!rest ith)
  (fold-left ref listt ith))

(define (repeat sym n)
  (make-list n sym))

(define (ub-map func #!rest listt) ; unbalanced map (lazy imp)
  (define max-len
    (apply max
	   (map (lambda (item)
		  (if (list? item) (length item) 0))
		listt)))
  (apply map func
	 (map (lambda (item)
		(if (list? item)
		    (if (< (length item) max-len)
			(append item (make-list (- max-len (length item)) (last item)))
			item)
		    (make-list max-len item)))
	      listt)))

;(ub-map list '(1 2 3) '(1 1 3) '(2 2 2))
;(map list '(1 2 3) '(1 1 3) '(2 2 2))

(define (flatten-once listt)
  (receive (lists rest)
	   (partition list? listt)
	   (append rest (fold-left append '() lists))))

(define (flatten listt)
  (define (itr result lists?)
    (set! result (flatten-once result))
    (set! lists? (fold-left (lambda (a b)(or a b)) #f (map list? result)))
    (if lists? (itr result lists?) result))
  (if (list? listt)
      (itr listt #t)
      listt))

(define ref list-ref)

(define (deep-ref listt #!rest ith)
  (fold-left ref listt ith))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH: Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:row m n)
  (m:nth-row m n))

(define (m:column m n)
  (m:nth-col m n))

(define (m:rows m)
  (vector-length (matrix->array m)))

(define (m:columns m)
  (m:cols (matrix->array m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH: Finite Fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m:rational->ff m f)
  (if (matrix? m)
      (m:generate (m:num-rows m)
		  (m:num-cols m)
		  (lambda (i j)
		    (rational->ff (m:ref m i j) f)))
      'no))

(define (rational->ff n f)
  (if (rational? n)
      (ff:/ (inexact->exact (numerator n))
	    (inexact->exact (denominator n)) f)
      'no))

(define (->ff x f)
  (cond ((matrix? x) (m:rational->ff x f))
	((rational? x) (rational->ff x f))
	(else 'no)))

; test
#|
(begin
  (define areal (/ 1 3))
  (pp+ (inexact->exact (numerator areal))
       (inexact->exact (denominator areal))
       (rational->ff areal 5))
  )
|#

(define (ff:num n f)
  (modulo n f))

(define (ff:+ a b f)
  (ff:num (+ a b) f))

(define (ff:- a b f)
  (ff:num (- a b) f))

(define (ff:* a b f)
  (ff:num (* a b) f))

(define (ff:/ a b f)
  (define r 0)
  (set! a (ff:num a f))
  (set! b (ff:num b f))
  (define (d n)
    (set! r (ff:* b n f))
    (if (eq? r a) n
	(if (< 1e+2 n) '?
	    (d (+ 1 n)))))
  (if (eq? b 0) 'NaN
      (d 0)))

(define (test-ff:/ idvr iff lim)
  (define low (- lim))
  (define hi lim)
  (define range (- hi low))
  (define ff (iota range iff 0))
  (define a (iota range idvr 0))
  (define b (map ff:num (iota range low 1) ff))
  ; x = (ff:/ b a)
  (define x (map ff:/ b a ff))
  (map pp (list a x b))
  ; a * x = b
  (pp (map ff:* a x ff))
  )
;(test-ff:/ -3 5 9)
;(map test-ff:/ (iota 9 1) (iota 9 1) (iota 9 20 0))

(define (ff:dot a b)
  (apply ff:+ (vector-map ff:* a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH: Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-seq seq)
  (lambda (n)
    (map seq (iota n))))

(define (pp-seq seq n)
  (for-each pp (map seq (iota n))))

(define (mat n #!rest elements)
  (apply matrix-by-rows 
	 (map (lambda(i)
		(sublist elements i (+ i n)))
	      (iota (car (integer-divide (length elements) n)) 0 n))))

(define (mat-sqr #!rest elements)
  (define n (sqrt (length elements)))
  (apply matrix-by-rows
	 (map (lambda(i)
		(sublist elements i (+ i n)))
	      (iota (car (integer-divide (length elements) n)) 0 n))))

(define (get-seq term first-term)
  (let next ((n first-term))
    (cons n (delay (next (term n))))))

(define (get-term seq i)
  (head
   (let jth-term ((j i))
     (if (< 0 j)
	 (tail (jth-term (- j 1)))
	 seq))))

(define seq make-seq)

#|
(define (get-seq func first len)
  (let next ((n 0) (term first) (buffer (make-vector len 1)))
    (vector-set! buffer (modulo n blen)
		 (next (+ n 1) (func n buffer) buffer))
    (cons n (delay (next (+ n))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH: Calculus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; difference equation

; unfinished
(define (diff f n p)
  (cond ((> p 0)
	 (- (f n) (f (- n 1) (diff f n (- p 1))))))
  (else 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (partition-xml xml-doc depth)
  (visit (list (list xml-doc) '()) depth))

(define (visit nodes depth)
  (define result (get-entries (car nodes)))
  (set! result (partition-entries result))
  (if (<= depth 1)
      result
      (visit result (- depth 1))))

(define (partition-entries nodes)
  (receive
   (records rest)
   (partition record? (flatten nodes))
   (list records rest)))

(define (get-entries nodes)
  (map (lambda (node)
	 (get-record-entries node))
       nodes))

(define (get-partitioned-entries node)
  (partition-entries (get-record-entries node)))

(define (get-record-entries record)
  (define type (record-type-descriptor record))
  (define fields (record-type-field-names type))
  (map (lambda (field)((record-accessor type field) record)) fields))

(define (get-entries-by-accessor accessors record)
  (map (lambda (accessor)(accessor record)) accessors))

(define (get-accessors record)
  (define type (record-type-descriptor record))
  (define fields (record-type-field-names type))
  (map (lambda (field)(record-accessor type field)) fields))
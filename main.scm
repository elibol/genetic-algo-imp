;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A genetic algorithm for generating
;; and sorting a search space of knapsack solutions.
;;
;; Sources:
;; http://www.ai-junkie.com/ga/intro/gat1.html
;; An Introduction to Genetic Algorithms, Melanie Mitchell
;;
;; Note:
;; A full description of the implementation can be found
;; in algorithm.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: My "sub-scheme."
(load "utils")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Makes a chromosome from id.
;; id represents the solution to be encoded
;; in this chromosome.
(define (encode-chromo len id)
  (unsigned-integer->bit-string len id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: generates a random chromosome 
;; of length len.
(define (random-chromo len)
  (encode-chromo len (random (expt 2 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Decodes an encoded chromosome into its constituents
(define (decode-chromo chromo len objects)
  ;(pp+ "decode-chromo" chromo len objects)
  (define (next pos result)
    (set! pos (bit-substring-find-next-set-bit chromo pos len))
    (if pos
	(next (+ pos 1) (cons (m:row objects pos) result))
	result))
  (next 0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Decodes an encoded chromosome into a solution.
(define (fitness chromo len objects max-weight)
  (define solution (list->vector (decode-chromo chromo len objects)))
  (define weight (apply + (vector->list (nth-col solution 0))))
  (define value (apply + (vector->list (nth-col solution 1))))
  ;(pp+ "fitness" chromo solution weight value max-weight)
  (list chromo (if (> weight max-weight) 0 value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Builds initial randomized population.
(define (generate-population size len objects max-weight)
  (define (next pos result)
    (if (< pos size)
	(next (+ pos 1)
	      (cons 
	       (fitness (random-chromo len) len objects max-weight)
	       result))
	result))
  (define pop (next 0 '()))
  (fitness-sort pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Sorts chromosomes by fitness.
(define (fitness-sort chromos)
  (sort chromos
	(lambda (a b)(> (second a) (second b)))))

(define (crossover x-genes y-genes a-genes b-genes len pivot)
  ; xxxx -> xyyy
  (bit-substring-move-right! y-genes 0 pivot a-genes 0)
  (bit-substring-move-right! x-genes pivot len a-genes pivot)
  ; yyyy -> yxxx
  (bit-substring-move-right! x-genes 0 pivot b-genes 0)
  (bit-substring-move-right! y-genes pivot len b-genes pivot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Mutates genes if prob holds.
(define (mutate genes locus prob)
  (if (> (* prob (random 1.0)) 0.5)
      (if (bit-string-ref genes locus)
	  (bit-string-clear! genes locus)
	  (bit-string-set! genes locus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Weak chromosomes are removed.
(define (die pop a-chromo b-chromo pop-size)
  (define augmented-pop (append (list a-chromo b-chromo) pop))
  (set! augmented-pop (fitness-sort augmented-pop))
  (sublist augmented-pop 0 pop-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Main iteration loop.
(define (iterate objects pop pop-size len max-weight mutation-prob)
  (define x-chromo (ref pop (random pop-size)))
  (define y-chromo (ref pop (random pop-size)))
  (define x-genes (ref x-chromo 0))
  (define y-genes (ref y-chromo 0))
  (define pivot (random len))
  
  (define a-genes (bit-string-allocate len))
  (define b-genes (bit-string-allocate len))
  
  ;(pp+ (list "encode" len) x-chromo y-chromo)  
  (crossover x-genes y-genes
	     a-genes b-genes
	     len pivot)
  
  ;(pp+ (list "crossover" pivot) a-genes b-genes)
  (mutate a-genes (random len) mutation-prob)
  (mutate b-genes (random len) mutation-prob)
  
  ;(pp+ "mutate" a-genes b-genes)
  
  (define a-chromo (fitness a-genes len objects max-weight))
  (define b-chromo (fitness b-genes len objects max-weight))
  
  ;(pp+ "die" x-chromo y-chromo ga gb)
  
  (die pop a-chromo b-chromo pop-size))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: To match specified interface.
(define (knapsack iterations objects max-weight pop-size mutation-prob)
  (define len (m:rows objects))
  (define pop (generate-population pop-size len objects max-weight))
  (define (iterator count evolved-pop)
    ;(pp+ "best solution" (car selection))
    (if (< count iterations)
	(iterator (+ count 1)
		  (iterate objects evolved-pop pop-size len max-weight mutation-prob))
	evolved-pop))
  (apply pp+ (cons "population" pop))
  (define evolved-pop (iterator 0 pop))
  (apply pp+ (cons "evolved-pop" evolved-pop))
  (car evolved-pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns best solution to the problem
;; after a given number of iterations.
(define (count-knapsack iterations pop-size mutation-prob objects max-weight)
  (define len (m:rows objects))
  (define pop (generate-population pop-size len objects max-weight))
  (define (iterator count evolved-pop)
    (if (< count iterations)
	(iterator (+ count 1)
		  (iterate objects evolved-pop pop-size len max-weight mutation-prob))
	evolved-pop))
  (define evolved-pop (iterator 0 pop))
  (car evolved-pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Runs best solution after a given duration.
(define (time-knapsack duration pop-size mutation-prob objects max-weight)
  (define len (m:rows objects))
  (define pop (generate-population pop-size len objects max-weight))
  (define start-time (get-universal-time))
  (define (timer time evolved-pop)
    (if (< time duration)
	(timer (- (get-universal-time) start-time)
	       (iterate objects evolved-pop pop-size len max-weight mutation-prob))
	evolved-pop))
  (define evolved-pop (timer 0 pop))
  (car evolved-pop))

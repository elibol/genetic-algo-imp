;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Melih Elibol
;;
;; Description: Test file!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-count-ks iterations pop-size mutation-prob)
  (load "main")
  (load "test-data")
  (define counted.solution
    (count-knapsack iterations pop-size mutation-prob test.objects test.max-weight))
  (pp+ "counted solution" counted.solution)
  )

(define (run-time-ks duration pop-size mutation-prob)
  (load "main")
  (load "test-data")
  (define timed.solution
    (time-knapsack duration pop-size mutation-prob test.objects test.max-weight))
  (pp+ "timed solution" timed.solution)
  )

(define (full-run)
  (load "main")
  (load "test-data")
  (define test.iterations 128)
  (define test.pop-size 16)
  (define test.mutation-prob .8)
  (define test.solution
    (knapsack test.iterations test.objects test.max-weight test.pop-size test.mutation-prob))
  (pp+ "solution" test.solution)
  )

(define (basic-test)
  (load "main")
  (define test.objects (mat 2
			    45 3
			    40 5
			    50 8
			    90 10))
  (define test.max-weight 100)
  (define test.iterations 100)
  (define test.pop-size 5)
  (define test.mutation-prob .8)
  (define test.solution
    (knapsack test.iterations test.objects test.max-weight test.pop-size test.mutation-prob))
  (pp+ "solution" test.solution)
  )

;(run-time-ks 5 16 .8)

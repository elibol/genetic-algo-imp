;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KNAPSACK: Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Encoding ;;;
;;; Solution -> Chromosome
;;; ObjectList -> Genes
;;; Object -> Gene

;;; Fitness ;;;
;;; Encoded chromosomes are accompanied
;;; by values that correspond to their
;;; likelihood for survival, called fitness.
;;; This is simply the sum of gene values,
;;; or zero if the weight of genes exceeds
;;; the maximum.

;;; Selection ;;;
;;; Two chromosomes are randomly chosen
;;; from the population, with a bias for
;;; more fit chromosomes.

;;; Crossover ;;;
;;; The selected chromosomes are mixed by
;;; swapping genes at a randomly chosen position.
;;; For position (locus) 3 in chromosomes with 4 genes,
;;; crossover between xxxx and yyyy yields:
;;; xxxx -> xyyy
;;; yyyy -> yxxx

;;; Mutation ;;;
;;; A probability test is evaluated
;;; for new chromosomes. If the
;;; test passes, then a randomly selected
;;; gene is flipped.
;;;
;;; Stick to low values for mutation probability?
;;; .0001 with binary numbers?

;;; Death ;;;
;;; The weakest chromosomes are removed from
;;; the population.
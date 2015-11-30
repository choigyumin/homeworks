#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)

(define (bstree-make)
  (mcons 'Root (mcons 'L 'R)))

(define (bstree-add-elmt t k v)
  (define (leaf key value)
    (mcons (mcons key value) (mcons 'L 'R)))
  (if (equal? t (mcons 'Root (mcons 'L 'R))) (begin (set-mcar! t (mcar (leaf k v))) #f) ; null tree case
      (cond [(equal? k (mcar (mcar t))) (begin (set-mcar! t (mcar(leaf k v))) #t)] ; overwrite
            [(< k (mcar (mcar t))) (if (equal? 'L (mcar (mcdr t))) (begin (set-mcar! (mcdr t) (leaf k v)) #f)
                                             (bstree-add-elmt (mcar (mcdr t)) k v))]
            [(> k (mcar (mcar t))) (if (equal? 'R (mcdr (mcdr t))) (begin (set-mcdr! (mcdr t) (leaf k v)) #f)
                                             (bstree-add-elmt (mcdr (mcdr t)) k v))]
            )))

(define (min_elmt t) ; find minimum element for deleting.
  (if (equal? (mcar (mcdr t)) 'L) (mcar t)
      (min_elmt (mcar (mcdr t)))))

(define (bstree-del-elmt t k)
  (if (symbol? t) #f
      (cond [(< k (mcar (mcar t))) (if (equal? 'L (mcar (mcdr t))) #f
                                       (bstree-del-elmt (mcar (mcdr t)) k))]
        [(> k (mcar (mcar t))) (if (equal? 'R (mcdr (mcdr t))) #f
                                   (bstree-del-elmt (mcdr (mcdr t)) k))]
        [else (cond [(and (not (equal? 'L (mcar (mcdr t))))
                          (not (equal? 'R (mcdr (mcdr t)))))
                     (begin (set-mcdr! (mcdr t) (min_elmt (mcdr (mcdr t))))
                            (bstree-del-elmt (mcdr (mcdr t)) (mcdr (mcdr t))))]
                    [(equal? 'L (mcar (mcdr t))) (begin (set-mcar! t (mcar (mcdr (mcdr t)))) (set-mcdr! t (mcdr (mcdr (mcdr t)))) #t)]
                    [(equal? 'R (mcdr (mcdr t))) (begin (set-mcar! t (mcar (mcar (mcdr t)))) (set-mcdr! t (mcdr (mcar (mcdr t)))) #t)])])))

(define (bstree-find-elmt t k)
  (if (symbol? t) (inr 'NO)
      (cond [(equal? k (mcar (mcar t))) (inl (mcdr (mcar t)))] ; find the element
            [(< k (mcar (mcar t))) (if (equal? 'L (mcar (mcdr t))) (inr 'NO)
                                         (bstree-find-elmt (mcar (mcdr t)) k))]
            [(> k (mcar (mcar t))) (if (equal? 'R (mcdr (mcdr t))) (inr 'NO)
                                         (bstree-find-elmt (mcdr (mcdr t)) k))])))

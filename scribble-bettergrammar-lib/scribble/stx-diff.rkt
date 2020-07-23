#lang racket/base

;;; sexp-diff.rkt -- diffs s-expressions based on Levenshtein-like edit distance.
;;; Ported more or less directly from Michael Weber's Common Lisp implementation.

;; This code is in the Public Domain.

;;; Description:

;; sexp-diff computes a diff between two s-expressions which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals #:new, #:old.

;;; Examples:

(module+ test
  (require rackunit)

  (check-equal?
   (stx-diff
    #'(define (f x) (+ (* x 2) 1))
    #'(define (f x) (- (* x 2) 3 1)))
   #'((define (f x) (#:new - #:old + (* x 2) #:new 3 1))))

  (check-equal?
   (stx-diff
    #'(define (f x) (+ (* x 2) 4 1))
    #'(define (f x) (- (* x 2) 5 3 1)))
   #'((define (f x) (#:new - #:old + (* x 2) #:new 5 #:new 3 #:old 4 1))))

  (check-equal?
   (stx-diff
    #'(define (f x) (+ (* x 2) 4 4 1))
    #'(define (f x) (- (* x 2) 5 5 3 1)))
   #'((define (f x)
       (#:new - #:old + (* x 2) #:new 5 #:new 5 #:new 3 #:old 4 #:old 4 1))))

  (check-equal?
   (stx-diff
    #:old-marker '#:expected #:new-marker '#:actual
    #'(1 2 3 4)
    #'(1 2 2 4))
   #'((1 #:actual 2 2 #:expected 3 4)))
  )

;;; Todo:

;; * Support for moved subtrees
;; * The algorithm treats vectors, arrays, etc. as opaque objects
;; * This article might describe a better method (unchecked):
;;   Hélène Touzet: "A linear tree edit distance algorithm for similar ordered trees"
;;   LIFL - UMR CNRS 8022 - Université Lille 1
;;   59 655 Villeneuve d'Ascq cedex, France
;;   Helene.Touzet@lifl.fr
;; * Format for reporting differences in improper lists is clunky


;;; Code:

(require syntax/stx racket/list syntax/srcloc)

(provide stx-diff maybe/free-identifier=?)

;; Computes the number of atoms contained in TREE.
(define (stx-size tree)
  (if (stx-pair? tree)
      (apply + 1 (stx-map stx-size tree))
      1))

(struct edit-record (edit-distance))

(struct unchanged-record edit-record (change))
(define (make-unchanged-record change)
  (unchanged-record (stx-size change) change))

(struct deletion-record edit-record (change))
(define (make-deletion-record change)
  (deletion-record (add1 (stx-size change)) change))

(struct insertion-record edit-record (change))
(define (make-insertion-record change)
  (insertion-record (add1 (stx-size change)) change))

(struct update-record edit-record (old new))
(define (make-update-record old new)
  (update-record (+ 1 (stx-size old)
                    1 (stx-size new))
                 old new))

(struct compound-record edit-record (changes))
(define (make-compound-record changes)
  (compound-record (apply + (stx-map edit-record-edit-distance changes)) changes))
(define (make-empty-compound-record)
  (make-compound-record '()))
(define (make-extend-compound-record r0 record)
  (make-compound-record (cons record (get-change r0))))


(define (get-change record)
  (cond [(unchanged-record? record) (unchanged-record-change record)]
        [(deletion-record?  record) (deletion-record-change  record)]
        [(insertion-record? record) (insertion-record-change record)]
        [(compound-record?  record) (compound-record-changes record)]))

(define (render-difference record old-marker new-marker)
  (cond [(insertion-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
           (#,(new-marker (insertion-record-change record))))]
        [(deletion-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
           (#,(old-marker (deletion-record-change record))))]
        [(update-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
             (#,(old-marker (update-record-old record))
              #,(new-marker (update-record-new record))))]
        [(unchanged-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
           (#,(unchanged-record-change record)))]
        [(compound-record? record)
         (quasisyntax/loc (build-source-location-syntax #f)
          (#,(for/fold ((res '()))
                       ((r (reverse (compound-record-changes record))))
               (let ([c (render-difference r old-marker new-marker)])
                 (quasisyntax/loc (build-source-location-syntax #f)
                   (#,@res #,@c))))))]))

;; Returns record with minimum edit distance.
(define (min/edit record . records)
  (foldr (lambda (a b) (if (<= (edit-record-edit-distance a)
                               (edit-record-edit-distance b))
                           a b))
         record records))


;; Prepares initial data vectors for Levenshtein algorithm from LST.
(define (initial-distance function stx)
  (let ([lst (syntax->list stx)])
    (let ((seq (make-vector (add1 (length lst)) (make-empty-compound-record))))
      (for ((i   (in-naturals))
            (elt (in-list lst)))
        (vector-set! seq (add1 i)
                     (make-extend-compound-record (vector-ref seq i)
                                                  (function elt))))
      seq)))

;; Calculates the minimal edits needed to transform OLD-TREE into NEW-TREE.
;; It minimizes the number of atoms in the result tree, also counting
;; edit conditionals.
(define (maybe/free-identifier=? id1 id2)
  (and (identifier? id1) (identifier? id2) (free-identifier=? id1 id2)))

(define (levenshtein-stx-edit old-stx new-stx)
  (cond
    ((maybe/free-identifier=? old-stx new-stx)
     (make-unchanged-record old-stx))
    ((and
      (or (not (identifier? old-stx))
          (not (identifier? new-stx)))
      (equal? (syntax->datum old-stx)
              (syntax->datum new-stx)))
     (make-unchanged-record old-stx))
    ((not (and (stx-pair? old-stx) (stx-pair? new-stx)))
     (make-update-record old-stx new-stx))
    (else
     (min/edit
      (make-update-record old-stx new-stx)
      (let* ((best-edit #f)
             (row (initial-distance make-deletion-record old-stx))
             (col (initial-distance make-insertion-record new-stx)))
        (for ((new-part (in-list (syntax->list new-stx)))
              (current (in-list (drop (vector->list col) 1))))
          (for ((old-part (in-list (syntax->list old-stx)))
                (row-idx  (in-naturals)))
            ;; TODO: This the syntax of new-stx or old-stx needs to be preserved onto the compound record and used when reconstructing.
            (set! best-edit (min/edit (make-extend-compound-record (vector-ref row (add1 row-idx))
                                                                   (make-insertion-record new-part))
                                      (make-extend-compound-record current
                                                                   (make-deletion-record old-part))
                                      (make-extend-compound-record (vector-ref row row-idx)
                                                                   (levenshtein-stx-edit old-part new-part))))
            (vector-set! row row-idx current)
            (set! current best-edit))
          (vector-set! row (sub1 (vector-length row)) best-edit))
        best-edit)))))

;; Computes a diff between OLD-TREE and NEW-TREE which minimizes the
;; number of atoms in the result tree, also counting inserted edit conditionals
;; #:new, #:old.
(define (stx-diff old-stx new-stx
                  #:old-marker [old-marker (lambda (x)
                                             (quasisyntax/loc x
                                               (#:old #,x)))]
                  #:new-marker [new-marker (lambda (x)
                                             (quasisyntax/loc x
                                               (#:new #,x)))])
  (render-difference (levenshtein-stx-edit old-stx new-stx)
                     old-marker new-marker))

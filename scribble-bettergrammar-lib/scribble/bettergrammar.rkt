#lang racket/base

(require
 racket/runtime-path
 racket/list
 scribble/base
 scribble/struct
 scribble/racket
 (only-in scribble/core make-style)
 scribble/html-properties
 (except-in scribble/manual racketgrammar*)
 (except-in scribble/private/manual-vars with-racket-variables)
 scribble/private/manual-utils
 racket/format
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax)
 racket/stxparam)

(provide
 bettergrammar*
 (rename-out [bettergrammar* typeset-grammar])
 bnf:add
 bnf:sub
 bnf-add
 bnf-sub
 define-grammar
 bettergrammar*-diff
 (rename-out [bettergrammar*-diff typeset-grammar-diff]))

(define-runtime-path css-path "bettergrammar.css")

(begin-for-syntax
  (struct grammar (literals datum-literals clauses typesetter)
    #:property prop:procedure (struct-field-index typesetter)))

(define-syntax (define-grammar stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:literals (id ...)))
        (~optional (~seq #:datum-literals (did ...)))
        rest ...)
     #:with typeset-name (format-id #'name "typeset-~a" #'name)
     #:with (dlit ...) #'(~? (did ...) ())
     #:with (lit ...) #'(~? (id ...) ())
     (quasisyntax/loc stx
       (begin
         (define-syntax name
           (grammar #'(lit ...) #'(dlit ...) (quote-syntax #,(attribute rest))
                    (syntax-rules ()
                      [(_ e)
                       (syntax-parameterize
                           ([current-datum-literals #'(dlit ...)]
                            [current-literals #'(lit ...)])
                         (interpose-on-racketform e))])))))]))

;; TODO: Need syntax parameter because with-racket-variables.
(define-syntax-parameter current-literals #'())
(define-syntax-parameter current-datum-literals #'())

(define datum-literal-style symbol-color)
(define-for-syntax datum-literal-transformer
  (make-element-id-transformer
   (lambda (x)
     (quasisyntax/loc x
       (elem #:style datum-literal-style (to-element '#,x))))))

(define-syntax (bettergrammar* stx)
  (syntax-parse stx
    [(_ #:literals (lit ...)
        #:datum-literals (dlit ...)
        ([addid addclause ...] ...)
        ([subid subclause ...] ...)
        ([id clause ...] ...))
     (syntax/loc stx
       (syntax-parameterize ([current-literals #'(lit ...)]
                             [current-datum-literals #'(dlit ...)])
         (letrec-syntaxes ([(dlit) datum-literal-transformer] ...)
           (with-racket-variables
             (lit ... dlit ...)
             ([non-term ((id clause ...) ...
                                         (subid subclause ...) ...
                                         (addid addclause ...) ...)])
             (*racketgrammar
              (lambda ()
                (list (list (racket addid)
                            (racketblock0/form addclause) ...)
                      ...))
              (lambda ()
                (list (list (racket subid)
                            (racketblock0/form subclause) ...)
                      ...))
              (lambda ()
                (list (list (racket id)
                            (racketblock0/form clause) ...)
                      ...)))))))]
    [(_ #:literals (lit ...)
        ([addid addclause ...] ...)
        ([subid subclause ...] ...)
        ([id clause ...] ...))
     (syntax/loc stx
       (bettergrammar* #:literals (lit ...)
                       #:datum-literals ()
                       ([addid addclause ...] ...)
                       ([subid subclause ...] ...)
                       ([id clause ...] ...)))]
    [(_ ([addid addclause ...] ...)
        ([subid subclause ...] ...)
        ([id clause ...] ...))
     (syntax/loc stx
         (bettergrammar* #:literals ()
                         #:datum-literals ()
                         ([addid addclause ...] ...)
                         ([subid subclause ...] ...)
                         ([id clause ...] ...)))]
    [(_ #:literals (lit ...)
        [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals (lit ...)
                       #:datum-literals ()
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ #:datum-literals (dlit ...)
        [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals ()
                       #:datum-literals (dlit ...)
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ #:literals (lit ...)
        #:datum-literals (dlit ...)
        [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals (lit ...)
                       #:datum-literals (dlit ...)
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals ()
                       #:datum-literals ()
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ id)
     (let ([v (syntax-local-value #'id)])
       (with-syntax ([(did ...) (grammar-datum-literals v)]
                     [(id ...) (grammar-literals v)])
         (quasisyntax/loc stx
           (bettergrammar* #:literals (id ...)
                           #:datum-literals (did ...)
                           ()
                           ()
                           #,(grammar-clauses v)))))]))

(define (*racketgrammar addclauseses-thunk subclauseses-thunk clauseses-thunk)
  (define (nontermify x)
    (make-element #f (list (hspace 2) (car x))))
  (define clauseify cdr)
  (let ([addl (addclauseses-thunk)]
        [subl (subclauseses-thunk)]
        [l (clauseses-thunk)])
    (*racketrawgrammars #f
                        (map nontermify addl)
                        (map nontermify subl)
                        (map nontermify l)
                        (map clauseify addl)
                        (map clauseify subl)
                        (map clauseify l))))

(define (*racketrawgrammars style addnonterms subnonterms nonterms
                            addclauseses subclauseses clauseses)
  (define (maybecdr x)
    (if (empty? x)
        '()
        (cdr x)))
  (define ((typeset-bnf bnfdef) nonterm clauses)
    (list*
     (list flow-empty-line flow-empty-line flow-empty-line
           flow-empty-line flow-empty-line)
     (list (to-flow nonterm) flow-empty-line (to-flow (tt bnfdef)) flow-empty-line
           (make-flow (list (car clauses))))
     (map (lambda (clause)
            (list flow-empty-line flow-empty-line
                  (to-flow "|") flow-empty-line
                  (make-flow (list clause))))
          (cdr clauses))))
  (make-table
   `((valignment baseline baseline baseline baseline baseline)
     (alignment right left center left left)
     (style ,style))
   (cdr
    (append
     (append-map (typeset-bnf "+::=") addnonterms addclauseses)
     (append-map (typeset-bnf "-::=") subnonterms subclauseses)
     (append-map (typeset-bnf "::=") nonterms clauseses)))))

(define better-bnf-props (list (make-css-addition css-path)))
(define bnf-add-style (make-style "bnf-add" better-bnf-props))
(define bnf-sub-style (make-style "bnf-sub" better-bnf-props))

;; Backwards compatibility
(define (bnf:add v)
  (make-element bnf-add-style (racketvarfont v)))

(define (bnf:sub v)
  (make-element bnf-sub-style (racketvarfont v)))

(define (later style f)
  (if style (elem #:style style (f)) (f)))

(require (for-syntax syntax/stx))
;; TODO: Document and maybe deprecate above interface.
(define-syntax (interpose-on-racketform stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx (interpose-on-racketform #f expr))]
    [(_ style expr)
     (with-syntax ([(dlit ...) (syntax-parameter-value #'current-datum-literals)]
                   [(lit ...) (syntax-parameter-value #'current-literals)])
       (quasisyntax/loc stx
         (letrec-syntaxes ([(dlit) datum-literal-transformer] ...)
           (with-racket-variables (lit ... dlit ...)
             ([non-term expr])
             ;; NOTE: Capturing dlit in the scope of expr
             ;; Ought I be able to do this with syntax-local-something bla?!
             (racket/form #,(datum->syntax #'(dlit ...) (syntax->datum #'expr)))))))]))

(define-syntax-rule (bnf-add expr)
  (interpose-on-racketform bnf-add-style expr))

(define-syntax-rule (bnf-sub expr)
  (interpose-on-racketform bnf-sub-style expr))

;; Need to extend this with code:hilite
(require
 (for-syntax
  syntax/boundmap))
(define-syntax (with-racket-variables stx)
  (syntax-case stx ()
    [(_ lits ([kind s-exp] ...) body)
     (let ([ht (make-bound-identifier-mapping)]
           [lits (syntax->datum #'lits)])
       (for-each (lambda (kind s-exp)
                   (case (syntax-e kind)
                     [(proc)
                      (letrec ([do-proc
                                (lambda (s-exp)
                                  (let ([s-exp (syntax->list s-exp)])
                                    (for-each
                                     (lambda (arg)
                                       (if (identifier? arg)
                                           (unless (or (eq? (syntax-e arg) '...)
                                                       (eq? (syntax-e arg) '...+)
                                                       (eq? (syntax-e arg) '_...superclass-args...)
                                                       (memq (syntax-e arg) lits))
                                             (bound-identifier-mapping-put! ht arg #t))
                                           (syntax-case arg ()
                                             [(kw arg . rest)
                                              (and (keyword? (syntax-e #'kw))
                                                   (identifier? #'arg))
                                              (bound-identifier-mapping-put! ht #'arg #t)]
                                             [(arg . rest)
                                              (identifier? #'arg)
                                              (bound-identifier-mapping-put! ht #'arg #t)]
                                             [else (void)])))
                                     (cdr s-exp))
                                    (unless (identifier? (car s-exp))
                                      ;; Curried:
                                      (do-proc (car s-exp)))))])
                        (do-proc s-exp))]
                     [(form form/none form/maybe non-term)
                      (define skip-id (case (syntax-e kind)
                                         [(form)
                                          (syntax-case s-exp ()
                                            [(defined-id actual-s-exp) (let ([id #'defined-id])
                                                                         (and (identifier? id)
                                                                              id))]
                                            [_ #f])]
                                         [else #f]))
                      (let loop ([form (case (syntax-e kind)
                                         [(form)
                                          (syntax-case s-exp ()
                                            [(defined-id actual-s-exp) #'actual-s-exp])]
                                         [(form/none) s-exp]
                                         [(form/maybe)
                                          (syntax-case s-exp ()
                                            [(#f form) #'form]
                                            [(#t (id . form)) #'form])]
                                         [(non-term) s-exp])])
                        (if (identifier? form)
                            (unless (or (and skip-id
                                             (free-identifier=? skip-id form))
                                        (eq? (syntax-e form) '...)
                                        (eq? (syntax-e form) '...+)
                                        (eq? (syntax-e form) 'code:line)
                                        (eq? (syntax-e form) 'code:blank)
                                        (eq? (syntax-e form) 'code:comment)
                                        (eq? (syntax-e form) '?)
                                        (eq? (syntax-e form) 'code:hilite)
                                        (memq (syntax-e form) lits))
                              (bound-identifier-mapping-put! ht form #t))
                            (syntax-case form (unsyntax)
                              [(unsyntax _) (void)]
                              [(a . b) (loop #'a) (loop #'b)]
                              [#(a ...) (loop #'(a ...))]
                              [_ (void)])))]
                     [else
                      (raise-syntax-error
                       #f
                       "unknown variable mode"
                       stx
                       kind)]))
                 (syntax->list #'(kind ...))
                 (syntax->list #'(s-exp ...)))
       (with-syntax ([(id ...) (bound-identifier-mapping-map ht (lambda (k v) k))])
         #'(letrec-syntaxes ([(id) (make-variable-id 'id)] ...)
             body)))]))

(define-syntax (bettergrammar*-diff stx)
   (syntax-parse stx
     [(_ (~optional (~seq (~datum #:include)
                          (include-nt:id ...)))
         (~optional (~seq (~datum #:exclude)
                          (exclude-nt:id ...)))
         (~optional (~seq (~datum #:literals)
                          (lid:id ...)))
         (~optional (~seq (~datum #:datum-literals)
                          (did:id ...)))
         old-name:id expr)
      (let ([v (syntax-local-value #'old-name)])
        (quasisyntax/loc stx
          (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                               (~? (~@ #:exclude (exclude-nt ...)))
                               #:literals ((~? (~@ lid ...)) (~@ . #,(grammar-literals v)))
                               #:datum-literals ((~? (~@ did ...)) (~@ . #,(grammar-datum-literals v)))
                               #,(grammar-clauses v)
                               expr)))]
     [(_ (~optional (~seq (~datum #:include)
                          (include-nt:id ...)))
         (~optional (~seq (~datum #:exclude)
                          (exclude-nt:id ...)))
         (~optional (~seq (~datum #:literals)
                          (lid:id ...)))
         (~optional (~seq (~datum #:datum-literals)
                          (did:id ...)))
         clauses1 new-name:id)
      (let ([v (syntax-local-value #'new-name)])
        (quasisyntax/loc stx
          (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                               (~? (~@ #:exclude (exclude-nt ...)))
                               #:literals ((~? (~@ lid ...)) (~@ . #,(grammar-literals v)))
                               #:datum-literals ((~? (~@ did ...)) (~@ . #,(grammar-datum-literals v)))
                               clauses1
                               #,(grammar-clauses v))))]
     [(_ (~optional (~seq (~datum #:include)
                          (include-nt:id ...)))
         (~optional (~seq (~datum #:exclude)
                          (exclude-nt:id ...)))
         (~optional (~seq (~datum #:literals)
                          (lid:id ...)))
         (~optional (~seq (~datum #:datum-literals)
                          (did:id ...)))
         clauses1 clauses2)
      (let-values ([(annotated-grammar)
                    (grammar-diff stx
                                  #'clauses1
                                  #'clauses2
                                  (attribute include-nt)
                                  (attribute exclude-nt))])
        #`(bettergrammar* (~? (~@ #:literals (lid ...)))
                          (~? (~@ #:datum-literals (did ...)))
                          #,@annotated-grammar))]))
(begin-for-syntax
  (require sexp-diff/stx-diff racket/function syntax/stx)

  (define (maybe/free-identifier=? id1 id2)
    (and (identifier? id1) (identifier? id2) (free-identifier=? id1 id2)))

  (define (maybe/map f e)
    (if (pair? e)
        (cons (f (car e)) (maybe/map f (cdr e)))
        (f e)))

  (define (erase-srcloc old)
    (if (syntax? old)
        (datum->syntax (syntax-disarm old #f)
                   (maybe/map erase-srcloc (syntax-e (syntax-disarm old #f)))
                   #f
                   old)
        old))

  (define (grammar-diff stx old-g-stxs new-g-stxs include-nts exclude-nts)
    (let ([diffed-grammar (stx-car (stx-diff old-g-stxs new-g-stxs
                                             #:old-marker (lambda (x)
                                                            (quasisyntax/loc x
                                                              ((#,#'unsyntax (bnf-sub #,x)))))
                                             #:new-marker (lambda (x)
                                                            (quasisyntax/loc x
                                                              ((#,#'unsyntax (bnf-add #,x)))))))]
          [fixup-nts (box '())])
      (map (compose
            ;; Regularize the srclocs of each production. This disrupts the
            ;; original typesetting based on source locations, but that
            ;; information is disrupted anyway when computing the diff.
            (lambda (x)
              (syntax-parse x
                [(nt prods ...)
                 (quasisyntax/loc this-syntax
                   (#,(erase-srcloc (attribute nt)) #,@(map erase-srcloc (attribute prods))))]))
            ;; Fix up when nt is deleted. The diff algorithm puts the annotation
            ;; in the wrong spot for typesetting.
            (lambda (x)
              (syntax-parse x
                [((~and us (~literal unsyntax)) (annotator (nt prods ...)))
                 (quasisyntax/loc x
                   ((us (annotator nt))
                    prods ...))]
                [_ x])))
           (filter (let ([include-nts
                          (when include-nts include-nts)]
                         [exclude-nts
                          (when exclude-nts exclude-nts)])
                     (lambda (x)
                       (and
                        (or (void? include-nts)
                            (findf (curry maybe/free-identifier=? (stx-car x)) include-nts))
                        (or (void? exclude-nts)
                            (not (findf (curry maybe/free-identifier=? (stx-car x)) exclude-nts))))))
                   (syntax->list diffed-grammar))))))

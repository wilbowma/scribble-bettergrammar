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
  syntax/parse))

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

(define-syntax (define-grammar stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:literals (id ...))) rest ...)
     (quasisyntax/loc stx
       (define-syntax name (cons #'(~? (id ...) ()) (quote-syntax #,(attribute rest)))))]))

(define-syntax (bettergrammar* stx)
  (syntax-parse stx
    [(_ #:literals (lit ...)
        ([addid addclause ...] ...)
        ([subid subclause ...] ...)
        ([id clause ...] ...))
     (syntax/loc stx
       (with-racket-variables
         (lit ...)
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
                  ...)))))]
    [(_ ([addid addclause ...] ...)
        ([subid subclause ...] ...)
        ([id clause ...] ...))
     (syntax/loc stx
         (bettergrammar* #:literals ()
                         ([addid addclause ...] ...)
                         ([subid subclause ...] ...)
                         ([id clause ...] ...)))]
    [(_ #:literals (lit ...)
        [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals (lit ...)
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ [id clause ...] ...)
     (syntax/loc stx
       (bettergrammar* #:literals ()
                       ()
                       ()
                       ([id clause ...] ...)))]
    [(_ id)
     (let ([v (syntax-local-value #'id)])
       (quasisyntax/loc stx
         (bettergrammar* #:literals #,(car v) () () #,(cdr v))))]))

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

;; TODO: Document and maybe deprecate above interface.
(define-syntax-rule (bnf-add x)
  (make-element bnf-add-style (racket x)))

(define-syntax-rule (bnf-sub x)
  (make-element bnf-sub-style (racket x)))

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
         old-name:id expr)
      (quasisyntax/loc stx
        (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                             (~? (~@ #:exclude (exclude-nt ...)))
                             #,(cdr (syntax-local-value #'old-name))
                             expr))]
     [(_ (~optional (~seq (~datum #:include)
                          (include-nt:id ...)))
         (~optional (~seq (~datum #:exclude)
                          (exclude-nt:id ...)))
         clauses1 new-name:id)
      (quasisyntax/loc stx
        (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                             (~? (~@ #:exclude (exclude-nt ...)))
                             clauses1
                             #,(cdr (syntax-local-value #'new-name))))]
     [(_ (~optional (~seq (~datum #:include)
                          (include-nt:id ...)))
         (~optional (~seq (~datum #:exclude)
                          (exclude-nt:id ...)))
         clauses1 clauses2)
      (let-values ([(annotated-grammar fixup-nts)
                    (grammar-diff stx
                                  #'clauses1
                                  #'clauses2
                                  (attribute include-nt)
                                  (attribute exclude-nt))])
        (with-syntax ([(nts ...) (datum->syntax this-syntax fixup-nts)])
          #`(letrec-syntaxes ([(nts) (make-variable-id 'nts)] ...)
              (bettergrammar* #,@annotated-grammar))))]))
(begin-for-syntax
  (require sexp-diff)

  (define (grammar-diff stx old-g-stxs new-g-stxs include-nts exclude-nts)
    (let ([diffed-grammar (car (sexp-diff (syntax->datum old-g-stxs) (syntax->datum new-g-stxs)))]
          [fixup-nts (box '())])
      (values
       (datum->syntax
        stx
        (filter (let ([include-nts
                       (when include-nts (map syntax->datum include-nts))]
                      [exclude-nts
                       (when exclude-nts (map syntax->datum exclude-nts))])
                  (lambda (x)
                    (and
                     (or (void? include-nts)
                         (memq (car x) include-nts))
                     (or (void? exclude-nts)
                         (not (memq (car x) exclude-nts))))))
                (let loop ([pos 0])
                  (if (eq? pos (length diffed-grammar))
                      '()
                      (let ([nt-def (list-ref diffed-grammar pos)])
                        ;; Either '#:old, #:new, or a non-terminal definition
                        (cond
                          [(eq? nt-def '#:old)
                           (let ([real-nt-def (list-ref diffed-grammar (add1 pos))])
                             (set-box! fixup-nts (cons (car real-nt-def) (unbox fixup-nts)))
                             (cons
                              `((,#'unsyntax (bnf-sub ,(car real-nt-def)))
                                ,@(cdr real-nt-def))
                              (loop (+ 2 pos))))]
                          [(eq? nt-def '#:new)
                           (let ([real-nt-def (list-ref diffed-grammar (add1 pos))])
                             (set-box! fixup-nts (cons (car real-nt-def) (unbox fixup-nts)))
                             (cons
                              `((,#'unsyntax (bnf-add ,(car real-nt-def)))
                                ,@(cdr real-nt-def))
                              (loop (+ 2 pos))))]
                          [else
                           (cons
                            (cons (car nt-def) (render-s-expr-diff (cdr nt-def)))
                            (loop (add1 pos)))]))))))
       (unbox fixup-nts))))

  (define (render-s-expr-diff prods)
    (if (list? prods)
        (let loop ([pos 0])
          (if (eq? pos (length prods))
              '()
              (let ([prod (list-ref prods pos)])
                (cond
                  ;; Either '#:old, #:new, and atom, or a (non-atom) s-expr
                  [(eq? prod '#:old)
                   (cons
                    `(,#'unsyntax (bnf-sub ,(list-ref prods (add1 pos))))
                    (loop (+ 2 pos)))]
                  [(eq? prod '#:new)
                   (cons
                    `(,#'unsyntax (bnf-add ,(list-ref prods (add1 pos))))
                    (loop (+ 2 pos)))]
                  [(not (list? prod))
                   (cons prod (loop (add1 pos)))]
                  [else
                   (cons (render-s-expr-diff prod) (loop (add1 pos)))]))))
        prods)))

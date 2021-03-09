#lang racket/base

(require
 racket/stream
 racket/runtime-path
 racket/list
 scribble/base
 (only-in scribble/struct make-flow make-table)
 scribble/racket
 (except-in scribble/core make-table)
 scribble/html-properties
 (except-in scribble/manual racketgrammar*)
 (except-in scribble/private/manual-vars with-racket-variables)
 scribble/private/manual-utils
 racket/format
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax))

(provide
 bettergrammar*
 (rename-out [bettergrammar* typeset-grammar])
 bnf:add
 bnf:sub
 bnf-add
 bnf-sub
 define-grammar
 bettergrammar*-diff
 (rename-out [bettergrammar*-diff typeset-grammar-diff])
 bettergrammar*-ndiff)

(define-runtime-path css-path "bettergrammar.css")
(define-runtime-path js-path "bettergrammar-fixup.js")

(begin-for-syntax
  (provide (struct-out grammar))
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
                       (interpose-on-racketform #f #,(attribute lit) #,(attribute dlit) e)])))))]))

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
                    ...))))))]
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

(define (wrap style expr)
  (if style (elem #:style style expr) expr))

(define-syntax let*-syntax
  (syntax-rules ()
    [(_ () body)
     body]
    [(_ ([id expr]) body)
     (let-syntax ([id expr]) body)]
    [(_ ([id expr] rest ...) body)
     (let-syntax ([id expr]) (let*-syntax (rest ...) body))]))

(require (for-syntax racket/syntax racket/function))
;; TODO: Document and maybe deprecate above interface.
(define-syntax (interpose-on-racketform stx)
  (syntax-case stx ()
    [(_ style (lit ...) (dlit ...) expr)
     (with-syntax ([(did ...) (map (curry format-id #'expr "~a") (syntax->list #'(dlit ...)))])
       (quasisyntax/loc stx
         (with-racket-variables (lit ...)
           ([non-term expr])
           (let*-syntax ([did datum-literal-transformer] ...)
             (wrap style (racket/form expr))))))]))

(define-syntax-rule (bnf-add lit dlit expr)
  (interpose-on-racketform bnf-add-style lit dlit expr))

(define-syntax-rule (bnf-sub lit dlit expr)
  (interpose-on-racketform bnf-sub-style lit dlit expr))

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
         (~optional (~and #:reverse (~bind (rflag #t))))
         old-name:id expr)
      (let ([v (syntax-local-value #'old-name)])
        (quasisyntax/loc stx
          (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                               (~? (~@ #:exclude (exclude-nt ...)))
                               #:literals ((~? (~@ lid ...)) (~@ . #,(grammar-literals v)))
                               #:datum-literals ((~? (~@ did ...)) (~@ . #,(grammar-datum-literals v)))
                               #,@(if (attribute rflag) (list #'#:reverse) '())
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
         (~optional (~and #:reverse (~bind (rflag #t))))
         clauses1 new-name:id)
      (let ([v (syntax-local-value #'new-name)])
        (quasisyntax/loc stx
          (bettergrammar*-diff (~? (~@ #:include (include-nt ...)))
                               (~? (~@ #:exclude (exclude-nt ...)))
                               #:literals ((~? (~@ lid ...)) (~@ . #,(grammar-literals v)))
                               #:datum-literals ((~? (~@ did ...)) (~@ . #,(grammar-datum-literals v)))
                               #,@(if (attribute rflag) (list #'#:reverse) '())
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
         (~optional (~and #:reverse (~bind (rflag #t))))
         clauses1 clauses2)
      #:with (lit ...) #'(~? (lid ...) ())
      #:with (dlit ...) #'(~? (did ...) ())
      (let-values ([(annotated-grammar)
                    (grammar-diff stx
                                  (if (attribute rflag) #'clauses2 #'clauses1)
                                  (if (attribute rflag) #'clauses1 #'clauses2)
                                  (attribute include-nt)
                                  (attribute exclude-nt)
                                  (attribute lit)
                                  (attribute dlit))])
        #`(bettergrammar* #:literals (lit ...)
                          #:datum-literals (dlit ...)
                          #,@annotated-grammar))]))

(define-syntax (bettergrammar*-ndiff stx)
  (syntax-parse stx
    [(_
      (~optional (~seq #:labels (strs ...)))
      (spec ... lang)
      (specs ... langs) ...)
     #`(tabbed-view
        (~? (list strs ...) '())
        (bettergrammar*-diff spec ... lang lang)
        (bettergrammar*-diff specs ... lang langs)
        ...)]))

(define tab-frame-style
  (make-style "tab-frame" (list 'never-indents (alt-tag "div"))))

(define (tab-style props)
  (make-style "tab" (append (list 'never-indents (alt-tag "div"))
                            props)))

(define fixup-style (make-style "delete-me" (list 'never-indents 'div (js-addition js-path))))

(require (only-in xml cdata))
(define do-fixup (xexpr-property (cdata #f #f "") (cdata #f #f "<script type=text/javascript>fixup_bettergrammar();</script>")))

(define fresh-tab-id
  (let ([x (box 0)])
    (lambda ()
      (set-box! x (add1 (unbox x)))
      (unbox x))))

(define (tabbed-view maybe-strs . grammars)
  (compound-paragraph
   (make-style #f '())
   (list
    (nested-flow
     tab-frame-style
     (cons
      (paragraph
       fixup-style
       (apply
        append
        (for/list ([_ grammars]
                   ;; maybe-strs followed by an infinite stream of #f
                   [str (letrec ([x (stream-append maybe-strs (stream-cons #f
                                                                           x))])
                          x)]
                   [n (in-naturals 1)])
          (let ([is (fresh-tab-id)])
            (list
             (elem #:style (make-style #f (list
                                           (alt-tag "input")
                                           (make-attributes
                                            `((type . "radio")
                                              (name . "tab")
                                              (id . ,(format "tab~a" id))
                                              ,@(if (= n 1)
                                                    `((checked . "checked"))
                                                    '()))))))
             (elem (or str (format "View ~a" n))
                   #:style (make-style #f (list
                                           (alt-tag "label")
                                           (make-attributes `((for . ,(format "tab~a"
                                                                              id))))))))))))
      (for/list ([g grammars])
        ;; Grammars are tables with a #f style name
        #;(tab-style (style-properties (table-style g)))
        #;(with-output-to-file "log" (thunk (displayln (style-properties (table-style g)))))
        (table (tab-style (style-properties (table-style g)))
               (table-blockss g)))))
    (paragraph
     (make-style #f '())
     (element (style #f (list do-fixup)) "")))))

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

  (define (grammar-diff stx old-g-stxs new-g-stxs include-nts exclude-nts lit dlit)
    (let ([diffed-grammar (stx-car (stx-diff old-g-stxs new-g-stxs
                                             #:old-marker (lambda (x)
                                                            (quasisyntax/loc x
                                                              ((#,#'unsyntax (bnf-sub #,lit #,dlit #,x)))))
                                             #:new-marker (lambda (x)
                                                            (quasisyntax/loc x
                                                              ((#,#'unsyntax (bnf-add #,lit #,dlit #,x)))))))]
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
                [((~and us (~literal unsyntax)) (annotator lit dlit (nt prods ...)))
                 (quasisyntax/loc x
                   ((us (annotator lit dlit nt))
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

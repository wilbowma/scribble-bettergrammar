#lang scribble/manual

@(require
  (for-label scribble/bettergrammar)
  (for-label scribble/manual)
  (for-label (except-in racket link))
  scribble/bettergrammar
  scribble/eval)

@title{Better Grammar}
@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]
@defmodule[scribble/bettergrammar]

It's like @racket[racketgrammar*], but better.

Specifically, @racketmodname[scribble/bettergrammar] is library for typesetting
`racketgrammar*`s with difference annotations, for when you are typesetting lots
of grammars with small changes between them.
It supports defining and reusing grammars, annotations for differences and
changes between grammars, automatically computing the difference between two
grammars (using `sexp-diff`), and rendering grammar differences with both
highlighting and annotations to emphasize what has changed between grammars.

@(require racket/format)
@(define-syntax-rule (scribble-example rest ...)
  (list
    @para{Example:}
    (codeblock rest ...)))

@(define (scribble-renders . rest)
  (list
    @para{Renders like:}
    @(apply nested #:style 'inset rest)))

@(define-grammar stlc-grammar (e (λ (x) e) (e e) x))

@(define-grammar stlc-grammar-v
(e (λ (x) e) (e e) x v)
(v natural ()))

@(define-grammar let-grammar
#:literals (integer?)
#:datum-literals (let)
(e integer? (let ([x e]) e)))

@defform[(define-grammar maybe-literals maybe-datum-literals id [id clause-datum ...+] ...)
         #:grammar [(maybe-literals (code:line) (#:literals (id ...)))
                    (maybe-datum-literals (code:line) (#:datum-literals (id ...)))]
]{
Defines @racket[id], @racket[id-block], and @racket[id-block0] as rendering
forms analogou to @racket[racket], @racket[racketblock], and
@racket[racketblock0], and defines the grammar @racket[id] to be typeset by
@racket[bettergrammar*], @racket[bettergrammar*-diff], and
@racket[bettergrammar*-ndiff].

The @racket[[id clause-datum] ...], @racket[maybe-literals], and
@racket[maybe-datum-literals] are the same as specified by second form or
@racket[bettergrammar*].

Must be used in definition context.

@examples[
(require scribble/bettergrammar)
(define-grammar stlc-grammar (e (λ (x) e) (e e) x))
(define-grammar let-grammar
 #:literals (integer?)
 #:datum-literals (let)
 (e (integer? (let ([x e]) e))))
]

@scribble-example|{
@bettergrammar*[stlc-grammar]
@bettergrammar*[let-grammar]
}|

@scribble-renders[
@bettergrammar*[stlc-grammar]
@bettergrammar*[let-grammar]
]

We can now also use @racket[let-grammar] to typeset inline examples which are
typeset according to the grammar specification, with any datum literals or
literals typeset as in the rendering above, and other symbols typeset as
variables using @racket[racketvarfont].
For example:

@scribble-example|{
@let-grammar[(let ([x 5]) x)]
}|

@scribble-renders[@let-grammar[(let ([x 5]) x)]]

@scribble-example|{
@let-grammar-block[
(let ([x 5])
  x)
]
}|

@scribble-renders[
@let-grammar-block[
(let ([x 5])
  x)
]]

@scribble-example|{
@let-grammar-block0[
(let ([x integer?])
  x)
]
}|

@scribble-renders[
@let-grammar-block0[
(let ([x integer?])
  x)
]]

@history[
#:changed "1.4" @elem{Added @racket[#:literals], @racket[#:datum-literals] support.}
#:changed "1.6" @elem{Added rendering forms that obey @racket[#:literals] and
                      @racket[#:datum-literals] used in definition.}
]
}


@defform*[((bettergrammar* maybe-literals
                           maybe-datum-literals
                           ([addid clause-datum ...+] ...)
                           ([subid clause-datum ...+] ...)
                           ([id clause-datum ...+] ...))
           (bettergrammar* maybe-literals
                           maybe-datum-literals
                           [id clause-datum ...+] ...)
           (bettergrammar* maybe-literals maybe-datum-literals id))
           #:grammar [(maybe-datum-literals (code:line) (#:datum-literals (id ...)))]]{
Like @racket[racketgrammar*], but supports typesetting a grammar with difference
annotations for non-terminals, and typesetting grammars pre-defined using
@racket[define-grammar].
@racket[maybe-literals] have the same interpretation as in
@racket[racketgrammar*].

Identifiers included in @racket[maybe-datum-literals] are not typeset as
variables, nor as @racket[racket] literals.

The @racket[addid] clauses are typeset as additions to the grammar, using
@racket[+::=] instead of @racket[::=] to indicate the addition to an existing
nonterminal.

The @racket[subid] clauses are typeset as removed from the grammar,
using @racket[-::=] instead of @racket[::=] to indicate the addition to an
existing nonterminal.

The @racket[id] clauses are typeset as in @racket[racketgrammar*], but using
@racket[::=] instead of @racket[=] to define the nonterminal.

The form supports a second syntax that is identical to @racket[racketgrammar*],
except the nonterminal is still separated from its productions using
@racket[::=].o

The third syntax enables typesetting a grammar previously defined by
@racket[define-grammar].
If the grammar was defined with literals or datum-literals, these are merged
into any specified directly via @racket[maybe-literals] or
@racket[maybe-datum-literals].

All forms also support escapes in Scribble using @racket[unsyntax], and various
Scribble element transformers, such as @racket[code:hilite].

@scribble-example|{
@bettergrammar*[(e (λ (x) e) x (e e) 1)]
}|

@scribble-renders[
@bettergrammar*[(e (λ (x) e) x (e e) 1)]
]

@scribble-example|{
@bettergrammar*[
((e v))
()
((v natural ()))
]
}|

@scribble-renders[
@bettergrammar*[
((e v))
()
((v natural ()))
]
]

@scribble-example|{
@bettergrammar*[
(e (unsyntax @bnf:add{v}) (λ (x) e) x (e e) (unsyntax @bnf:sub{1}))
((unsyntax @bnf:add{v}) natural ())
]
}|

@scribble-renders[
@bettergrammar*[
(e (unsyntax @bnf:add{v}) (λ (x) e) x (e e) (unsyntax @bnf:sub{1}))
((unsyntax @bnf:add{v}) natural ())
]
]

@scribble-example|{
@bettergrammar*[
(e (code:hilite v) (λ (x) e) x (e e))
(v natural ())
]
}|

@scribble-renders[
@bettergrammar*[
(e (code:hilite v) (λ (x) e) x (e e))
(v natural ())
]

@examples[
(require scribble/bettergrammar)
(define-grammar stlc-grammar (e (λ (x) e) (e e) x))
]

@scribble-example|{
@bettergrammar*[stlc-grammar]
}|

@scribble-renders[
@bettergrammar*[stlc-grammar]
]
]

@history[#:changed "1.2" @elem{Merged functionality from
         @racket[typeset-grammar-diff] into this form.}

         #:changed "1.4" @elem{Added @racket[#:literals],
         @racket[#:datum-literals] support}
]
}

@defproc[(bnf:add [datum string?]) string?]{
Typesets @racket[datum] as an addition to a @racket[bettergrammar*].
In the default style, this means hilighting with a blue background and adding a
@racket[+] superscript.
You can redefine this by redefining the @tt{bnf-add} class in your style file.

As it works over decoded strings, it can also be used outside of
@racket[bettergrammar*], although this interface may change.

@scribble-example|{
@bettergrammar*[
(e (unsyntax (bnf:add "v")) (λ (x) e) x (e e))
((unsyntax (bnf:add "v")) natural ())
]
}|

@scribble-renders[
@bettergrammar*[
(e (unsyntax (bnf:add "v")) (λ (x) e) x (e e))
((unsyntax (bnf:add "v")) natural ())
]
]

@scribble-example|{
@bnf:add{natural}
}|

@scribble-renders[
@bnf:add{natural}
]
}

@defproc[(bnf:sub [datum string?]) string?]{
Typesets @racket[datum] as a removal from a @racket[bettergrammar*].
In the default style, this means hilighting with a red background and adding a
@racket[-] superscript.
You can redefine this by redefining the @tt{bnf-sub} class in your style file.

As it works over decoded strings, it can also be used outside of
@racket[bettergrammar*], although this interface may change.

@scribble-example|{
@bettergrammar*[
(e (unsyntax (bnf:sub "v")) (λ (x) e) x (e e))
((unsyntax (bnf:sub "v")) natural ())
]
}|

@scribble-renders[
@bettergrammar*[
(e (unsyntax (bnf:sub "v")) (λ (x) e) x (e e))
((unsyntax (bnf:sub "v")) natural ())
]
]

@scribble-example|{
@bnf:sub{natural}
}|

@scribble-renders[
@bnf:sub{natural}
]
}

@defform[(typeset-grammar id)]{
Typeset the grammar defined as @racket[id] using @racket[bettergramar*].
@racket[id] must have been previously defined by @racket[define-grammar].

@scribble-example|{
(define-grammar stlc-grammar (e (λ (x) e) (e e) x))
}|

@scribble-example|{
@typeset-grammar[stlc-grammar]
}|

@scribble-renders[
@typeset-grammar[stlc-grammar]
]

@deprecated[#:what "form" @racket[bettergrammar*] @elem{Functionality merged into @racket[bettergrammar*]; exists for backwards compatibility and will eventually be removed.}]
@history[#:changed "1.2" "Deprecated"]
}

@(define-grammar stlc-grammar-v1
  (e (λ (x) e) (e e) x)
  (v (λ (x) e))
  (x name))
@(define-grammar stlc-grammar-v2
  (e (e e) x v)
  (v (λ (x) e) natural ())
  (natural 0 (add1 natural)))

@defform[(bettergrammar*-diff maybe-literals
                              maybe-datum-literals
                              maybe-include
                              maybe-exclude clause-spec clause-spec)
           #:grammar [(maybe-include (code:line) (#:include (id ...)))
                      (maybe-exclude (code:line) (#:include (id ...)))
                      (maybe-literals (code:line) (#:literals (id ...)))
                      (maybe-datum-literals (code:line) (#:datum-literals (id ...)))
                      (clause-spec identifier? (clauses ...))]]{
Compute and typeset the differnce between two grammars.
Either @racket[clause-spec] can be a valid @racket[racketgrammar*] spec, or an identifier previously defined by @racket[define-grammar]

If the optional @racket[maybe-include] is given, then only the nonterminals
specified in @racket[(id ...)] are included in the rendered grammar.

If the optional @racket[maybe-exclude] is given, then any nonterminals
specified in @racket[(id ...)] are excluded from the rendered grammar.

If the optional @racket[maybe-literals] is given, the literals are passed to
@racket[bettergrammar*].
If an identifier from @racket[define-grammar] is used, the literal used in that
definition will be merged with the @racket[maybe-literals].

If the optional @racket[maybe-datum-literals] is given, the datum literals are
passed to @racket[bettergrammar*].
If an identifier from @racket[define-grammar] is used, the datum literal used in
that definition will be merged with the @racket[maybe-datum-literals].

Renders a new grammar where non-terminals and productions the old grammar
that have been removed in new grammar are typeset like with @racket[bnf:sub], and
non-terminals and productions that have been added in the new grammar are
typeset like with @racket[bnf:add].

The diff algorithm may not preserve source location information used to typeset,
so large productions may be collapsed to a single line.
The diff should preserve paren-shape.
Literals should be linked properly with @racket[racket].

@scribble-example|{
(define-grammar stlc-grammar-v1
  (e (λ (x) e) (e e) x)
  (v (λ (x) e))
  (x name))
(define-grammar stlc-grammar-v2
  (e (e e) x v)
  (v (λ (x) e) natural ())
  (natural 0 (add1 natural)))
}|

@scribble-example|{
@bettergramar*-diff[stlc-grammar-v1 stlc-grammar-v2]
}|

@scribble-renders[
@bettergrammar*-diff[stlc-grammar-v1 stlc-grammar-v2]
]

@scribble-example|{
@bettergramar*-diff[#:include (e) stlc-grammar-v1 stlc-grammar-v2]
}|

@scribble-renders[
@bettergrammar*-diff[#:include (e) stlc-grammar-v1 stlc-grammar-v2]
]

@scribble-example|{
@bettergramar*-diff[#:exclude (e) stlc-grammar-v1 stlc-grammar-v2]
}|

@scribble-renders[
@bettergrammar*-diff[#:exclude (e) stlc-grammar-v1 stlc-grammar-v2]
]

@scribble-example|{
@bettergrammar*[(e (1 ...)

(3 ...))]

@bettergrammar*[(e (2 ...))]

@bettergrammar*-diff[
((e (1 ...)

(3 ...)))

((e (2 ...)))
]
}|

@scribble-renders[
@bettergrammar*[(e (1 ...)

(3 ...))]

@bettergrammar*[(e (2 ...))]

@bettergrammar*-diff[
((e (1 ...)

(3 ...)))

((e (2 ...)))
]
]

@scribble-example|{
@bettergrammar*-diff[
#:literals (integer?)
#:datum-literals (let)
((e integer? (let ([x e]) e)))
((e (let ([x v]) v)
 (v integer?)))
]
}|

@scribble-renders[
@bettergrammar*-diff[
#:literals (integer?)
#:datum-literals (let)
((e integer? (let ([x e]) e)))
((e (let ([x v]) v)
 (v integer?)))
]
]

@history[#:changed "1.1" "Support for second, anonymous form."
         #:changed "1.2" @elem{Renamed from @racket[typeset-grammar-diff]}
         #:changed "1.3" @elem{Added @racket[#:include] and @racket[#:exclude] support.}
         #:changed "1.4" @elem{Added @racket[#:literals], @racket[#:datum-literals] support; enabled mixed-diff between clauses and defined grammars.}
         ]
}

@defform[(bettergrammar*-ndiff
          maybe-labels
          (maybe-include maybe-exclude maybe-clause-spec clause-spec) ...)
          #:grammar [(maybe-labels (code:line) (#:labels (string-literal ...)))
                     (maybe-include (code:line) (#:include (id ...)))
                     (maybe-exclude (code:line) (#:include (id ...)))
                     (maybe-literals (code:line) (#:literals (id ...)))
                     (maybe-datum-literals (code:line) (#:datum-literals (id ...)))
                     (clause-spec identifier? (clauses ...))
                     (maybe-clause-spec (code:line) clause-spec)]]{

Compute and typeset a sequence of differences.
The intention is for this to only be supported on the HTML backend, to support
an interactive view comparing different grammars.

The main input is a sequence of clause specifications that are valid for
@racket[bettergrammar*-diff].
Each clause specification takes the same optional include and exclude arguments,
can be a clause specification directly or a defined grammar.
The first clause spec is optional; if excluded, then no difference is computed,
and the required clause spec is typeset normally.

If labels are provided, then there must be one label for each pair of difference
specification.
The labels are used in the tabbed interface.

@scribble-example|{
@bettergrammar*-ndiff[
(stlc-grammar-v1)
]
}|

@scribble-renders[
@bettergrammar*-ndiff[
(stlc-grammar-v1)
]
]

@scribble-example|{
@bettergrammar*-ndiff[
(stlc-grammar-v1 stlc-grammar-v2)
]
}|

@scribble-renders[
@bettergrammar*-ndiff[
(stlc-grammar-v1 stlc-grammar-v2)
]
]

@scribble-example|{
@bettergrammar*-ndiff[
#:labels ("Diff" "stlc-grammar-v2")
(stlc-grammar-v1 stlc-grammar-v2)
(stlc-grammar-v2)
]
}|

@scribble-renders[
@bettergrammar*-ndiff[
#:labels ("Diff" "stlc-grammar-v2")
(stlc-grammar-v1 stlc-grammar-v2)
(stlc-grammar-v2)
]
]

@scribble-example|{
@bettergrammar*-ndiff[
#:labels ("Diff" "stlc-grammar-v2")
(#:include (e v) stlc-grammar-v1 stlc-grammar-v2)
(stlc-grammar-v2)
]
}|


@scribble-renders[
@bettergrammar*-ndiff[
#:labels ("Diff" "stlc-grammar-v2")
(#:include (e v) stlc-grammar-v1 stlc-grammar-v2)
(stlc-grammar-v2)
]
]

@history[#:added "1.5"]
}


@defidform[typeset-grammar-diff]{
An alias for @racket[bettergrammar*-diff] for backwards compatibility.

@deprecated[#:what "form" @racket[bettergrammar*-diff] @elem{Renamed to @racket[bettergrammar*-diff] for nicer interface; exists for backwards compatibility and will eventually be removed.}]
@history[#:changed "1.2" "Made an alias; deprecated"]
}

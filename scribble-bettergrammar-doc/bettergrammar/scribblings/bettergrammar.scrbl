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

@defform*[((bettergrammar* maybe-literals
                           ([addid clause-datum ...+] ...)
                           ([subid clause-datum ...+] ...)
                           ([id clause-datum ...+] ...))
           (bettergrammar* maybe-literals
                           [id clause-datum ...+] ...))]{
Like @racket[racketgrammar*], but supports typesetting a grammar with difference
annotations for non-terminals. @racket[maybe-literals] have the same
interpretation as in @racket[racketgrammar*].

The @racket[addid] clauses are typeset as additions to the grammar, using
@racket[+::=] instead of @racket[::=] to indicate the addition to an existing
nonterminal.

The @racket[subid] clauses are typeset as removed from the grammar,
using @racket[-::=] instead of @racket[::=] to indicate the addition to an
existing nonterminal.

The @racket[id] clauses are typeset as in @racket[racketgrammar*], but using
@racket[::=] instead of @racket[=] to define the nonterminal.

The form supports second syntax that is identical to @racket[racketgrammar*],
except the nonterminal is still separated from its productions using
@racket[::=].

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

@(define-grammar stlc-grammar (e (λ (x) e) (e e) x))

@(define-grammar stlc-grammar-v
  (e (λ (x) e) (e e) x v)
  (v natural ()))


@defform[(define-grammar id clauses ...)]{
Defines @racket[id] as a grammar to be typeset by @racket[typeset-grammar] and
@racket[typeset-grammar-diff].
The @racket[clauses ...] are the same as specified by @racket[bettergrammar*].

Does not render anything by on its own.

Must be used in definition context.

@examples[
(require scribble/bettergrammar)
(define-grammar stlc-grammar (e (λ (x) e) (e e) x))
]

@scribble-example|{
@typeset-grammar[stlc-grammar]
}|

@scribble-renders[
@typeset-grammar[stlc-grammar]
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
}

@(define-grammar stlc-grammar-v1
  (e (λ (x) e) (e e) x)
  (v (λ (x) e))
  (x name))
@(define-grammar stlc-grammar-v2
  (e (e e) x v)
  (v (λ (x) e) natural ())
  (natural 0 (add1 natural)))


@define-grammar[meow (e (1 ...)

(3 ...))]
@define-grammar[meow2 (e (2 ...))]

@defform[(typeset-grammar-diff old-id new-id)]{
Compute and typeset the differnce between the grammars defined as
@racket[old-id] and @racket[new-id].
The two grammars must have been previously defined using @racket[define-grammar].
Renders a new grammar were non-terminals and productions from @racket[old-id] that have been removed in @racket[new-id] are typeset with @racket[bnf:sub], and non-terminals and productions that have been added in @racket[new-id] are typeset with @racket[bnf:add].


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
@typeset-grammar-diff[stlc-grammar-v1 stlc-grammar-v2]
}|

@scribble-renders[
@typeset-grammar-diff[stlc-grammar-v1 stlc-grammar-v2]
]

@scribble-example|{
@define-grammar[meow (e (1 ...)

(3 ...))]
@define-grammar[meow2 (e (2 ...))]

@typeset-grammar[meow]
@typeset-grammar-diff[meow meow2]
}|

@scribble-renders[
@typeset-grammar[meow]
@typeset-grammar-diff[meow meow2]
]
}

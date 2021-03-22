#lang scribble/manual

@(require
  scribble/example
  (for-label
    sexp-diff
    scribble/manual
    scribble/bettergrammar
    racket/base))

@(define (exercise . rest) (nested #:style "boxed" @bold{Exercise: } rest))

@title{RacketFest Demo}

I've been teaching introduction to compilers using the nanopass approach.
We have many many passes, each with a source and target language.
Each week, we extend the previous iteration of the compiler with a new feature.

This means I have a lot of grammars to typeset and share with students.

@section{Regular Grammar}

I started by writing grammars with @racket[racketgrammar*].
This worked okay.

Here a λ-calculus:
@racketgrammar*[
(e x (lambda (x) e) (e e))
(x (unsyntax @racket[symbol?]))
]

Here a target λ-calculus:
@racketgrammar*[
(e (let ([x v]) e) n)
(n v (v v))
(v x (lambda (x) e))
(x (unsyntax @racket[symbol?]))
]

I typeset a reference to a Racket predicate to define something that isn't
easily syntactically specified.

Let's extend those languages with some new feature.

Here an extension of λ-calculus:
@racketgrammar*[
(e x (lambda (x) e) (e e) integer (+ e e))
(integer (unsyntax @racket[integer?]))
(x (unsyntax @racket[symbol?]))
]

Here an extension of the target λ-calculus:
@racketgrammar*[
(e (let ([x v]) e) n)
(n v (v v) (+ v v))
(v x (lambda (x) e) integer)
(integer (unsyntax @racket[integer?]))
(x (unsyntax @racket[symbol?]))
]

Do you see what differences?

Sure, after a minute.

We can do better, with @racketmodname[scribble/bettergrammar]

@section{Better Grammar}
@(require scribble/bettergrammar)

@racket[bettergrammar*] provides an enriched specification language for
typesetting grammars, capturing a few of my convnetions and patterns into a
language.

@bettergrammar*[
#:literals (symbol?)
#:datum-literals (lambda)

(e x (lambda (x) e) (e e))
(x symbol?)
]

@racket[bettergrammar*] takes optional specifications to differentiate keywords
and Racket-escapes.

But @racket[bettergrammar*] is better. Grammars are first class-constructs.
We can define them:

@define-grammar[source-v1
#:literals (symbol?)
#:datum-literals (lambda)

(e x (lambda (x) e) (e e))
(x symbol?)
]

@define-grammar[target-v1
#:literals (symbol?)
#:datum-literals (lambda let)

(e (let ([x n]) e) n)
(n v (v v))
(v x (lambda (x) e))
(x symbol?)
]

And then typeset them:

@bettergrammar*[source-v1]

That's nice.

We can also use the language name as a formatting macro, to typeset examples:
@source-v1[(lambda (y) y)] and @source-v1[symbol?].
It's not perfect.

@racketmodname[scribble/bettergrammar] can compute and typeset differences
between grammars.
First, let's define some extended grammars.

@define-grammar[source-v2
#:literals (symbol? integer?)
#:datum-literals (lambda +)

(e x (lambda (x) e) (e e) integer (+ e e))
(integer integer?)
(x symbol?)
]

@define-grammar[target-v2
#:literals (symbol? integer?)
#:datum-literals (lambda let +)

(e (let ([x n]) e) n)
(n v (v v) (+ v v))
(v x (lambda (x) e) integer)
(integer integer?)
(x symbol?)
]

Next we use @racket[bettergrammar*-diff] to typeset the differences:

Differences between source v1 and v2:
@bettergrammar*-diff[
source-v1
source-v2
]


Differences between target v1 and v2:
@bettergrammar*-diff[
target-v1
target-v2
]

The difference algorithm is a bit naive.
It uses @racket[stx-diff] over the grammar represented as an
s-expression.
A better domain-specific diff probably exists.

Typesetting differences between single pairs of languages works okay, but
sometimes this is a lot of information to display.
For example, suppose we want to tell students to extend their ANF translation:

@defproc[(anf-translate [n source-v2]) target-v2]{
Translates source-v2 into target-v2.
}

@exercise{Extend your @racket[anf-translate] function to support the source-v2
language.}

In this case, it is useful to view 3 pairs differences:
@itemlist[
@item{The difference between the source and target, to focus on the changes
implemented by the compiler.}
@item{The difference between the old version and new version of the source
language, to visualize what new cases need to be added.}
@item{The difference between the old version and new version of the target
language, to see what features might be used in the output.}
]

@racketmodname[scribble/bettergrammar] provides @racket[bettergrammar*-ndiff],
which types sets @racket[n] diffs with a tabbed interface.
@margin-note{Currently undocumented; only supported by the HTML backend.}

@bettergrammar*-ndiff[
#:labels ("Source/Target" "Source v1/v2" "Target v1/v2")
(#:exclude (integer x) source-v2 target-v2)
(source-v1 source-v2)
(target-v1 target-v2)
]

@section{Building on Better Grammar}

The @racket[grammar] structure is provided @racket[for-syntax], so clients can
transform the @racket[grammar] pretty easily.
For example, the typesetting specification language is a subset of
@racketmodname[redex]'s pattern language, so we can easily transform the typeset
grammar into a Redex language.
I've used this to generate contracts for each compiler pass from the grammar
spec.

@examples[
(require
  racket/list
  scribble/core
  scribble/bettergrammar
  cpsc411/utils/redex-gen
  redex/reduction-semantics)

@define-grammar/pred[target-v2^
#:literals (symbol? integer?)
#:datum-literals (lambda let +)
(e (let ([x n]) e) n)
(n v (v v) (+ v v))
(v x (lambda (x) e) int)
(int integer?)
(x symbol?)
]

(first (first (table-blockss (bettergrammar* target-v2^))))
(target-v2^? '(let ([x 5]) x))
(redex-match? target-v2^L int 5)
(redex-match? target-v2^L e '(let ([x 5]) x))
]

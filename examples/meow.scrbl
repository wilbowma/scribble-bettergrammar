#lang scribble/manual

@; scribble --html ++xref-in setup/xref load-collections-xref meow.scrbl

@(require
  scribble/bettergrammar
  (for-label racket/base))

@define-grammar[int+bool-lang
#:literals (integer? symbol? boolean?)
#:datum-literals (let * +)
[tail  value (let ([x value] ...) tail)]
[value triv
       (binop triv triv)
       (let ([x value] ...) value)]
[triv integer boolean? x]
[x symbol?]
[binop * +]
[integer integer?]
]

@define-grammar[int-lang
#:literals (integer? symbol?)
#:datum-literals (let * +)
[tail  value (let ([x value] ...) tail)]
[value triv
       (binop triv triv)
       (let ([x value] ...) value)]
[triv integer x]
[x symbol?]
[binop * +]
[integer integer?]
]

@define-grammar[anf-lang
#:literals (integer? symbol?)
#:datum-literals (let * +)
[tail  value (let ([x value] ...) tail)]
[value triv
       (binop triv triv)]
[triv integer x]
[x symbol?]
[binop * +]
[integer integer?]
]

@; Above I've defined some grammars that I want to view the diffs between.
@; Maybe I want to include and exclude certain non-terminals
@bettergrammar*-ndiff[
#:labels ("Int+Bool-Lang" "vs Int-Lang" "vs ANF-Lang" "(r) vs ANF-lang")
(#:exclude (binop integer x) int+bool-lang)
(int-lang)
(#:include (value triv) anf-lang)
(#:include (value triv) #:reverse anf-lang)
]

#lang scribble/manual

@(require scriblib/figure)
@(require (for-label racket))

@title[#:tag "top" #:tag-prefix "extensible-embedded-compiler"]{Extensible embedded compiler}

@section{Intent}

Extensible embedded compilers are a good way to implement full-featured domain-specific languages that:

@itemlist[
  @item{can employ multiple passes of analysis and compilation in their implementation.}
  @item{integrate fluidly with Racket and other DSLs while protecting DSL abstractions.}
  @item{are macro-extensible.}
]

@section{Motivation}

While one use of macros is to implement new language forms that extend the base Racket language, another is to implement conceptually distinct domain-specific langauges (DSLs).

A DSL has its own grammar, static semantics, and evaluation model. Implementing these features may require the flexibility of a traditional multi-pass compiler. At the same time, we often want DSLs to integrate fluidly with Racket and other DSLs. And just as programers can create abstractions atop Racket using macros, they should be able to create abstractions atop DSLs as well.

The extensible embedded compiler pattern is a way of structuring a DSL implementation to support all of these properties. It replicates the structure of the implementation of Racket, using a macro expander tailored to the DSL as front-end and a traditional multi-pass compiler as back-end. The DSL's macro expander shares the Racket expander's hygiene mechanism and expander environment in order to integrate the DSL syntax with Racket.

@section{Applicability}

Use an extensible embedded compiler when:

@itemlist[
  @item{the implementation of the DSL requires non-local analysis or transformation passes on the DSL syntax such as typechecking, flow analysis, or optimizing compilation.}
  @item{and either:
    @itemlist[
      @item{the DSL should integrate with Racket or other DSLs via shared variable bindings.}
      @item{DSL definitions should be managed by Racket modules.}
      @item{users of the DSL should be able to abstract over DSL syntax using macros.}
    ]
  }
]

@section{Solution and example}

The high level components of an extensible embedded compiler are a vocabulary of core syntactic forms, a macro expander, a back-end compiler, and macros to embed the DSL in Racket. The following sections address each component in turn.

We use the DSL of parsing expression grammars (PEGs) @cite["peg"] as an example to illustrate the pattern. Basic parsing expressions match empty input, characters, sequences, and alternatives. A local binding form enables named recursive grammars.

@verbatim|{
peg := -eps
     | (-char <character>)
     | (-seq <peg> <peg>)
     | (-or <peg> <peg>)
     | (-local ([<identifier> <peg>]) <peg>)
}|

We embed the DSL in Racket with a syntactic form called @racket[parse]:

@verbatim|{
racket-exp := (parse <peg> <racket-exp>)
}|

Its Racket sub-expression should evaluate to a string, which will be parsed according to the grammar of the PEG subexpression. The form returns the number of characters of the string that match the grammar.

@subsection{The vocabulary of syntactic forms}

Racket associates syntactic forms with an identifier binding in a module or local scope, just like runtime bindings. This allows the visibility and name of syntactic forms to be controlled by modules and lexical scope, and is essential to Racket's conception of "languages as libraries". Consequently, the first task in defining a DSL is to create bindings for the core syntactic forms of the DSL.

When we create identifier bindings, we also have to define their meaning when they appear as normal Racket expressions. DSL forms only make sense in the context of the DSL, so we declare that expansion should raise an error if they appear in a Racket context.

The @seclink["top" #:tag-prefixes '("literal")] pattern describes how to establish the bindings. Using @racket[define-literals] from @hyperlink["https://github.com/michaelballantyne/syntax-generic2"]{syntax-generic2} to abstract over that pattern, the literal definitions for the PEG language may be written as follows:

@racketblock[
(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (-eps
   -char
   -seq
   -or
   -local))
]

@subsection{The expander}

@;The expander has three jobs: parsing the syntax of the DSL's core syntactic forms, implementing scope and binding structure, and expanding macros. The work of each of these three tasks must be interleaved. The expander is also responsible for defining interfaces through which other languages can interact with the variables and macros of the DSL.

The expander checks that a program conforms to the syntax of the DSL, expands macros, and reconstructs fully-expanded syntax. It also needs to construct representations of the program's scopes and bindings in order to implement hygienic name resolution, and because macro names are scoped and may be shadowed.

As a first step we need to define data structures for the representations of DSL variable and macro bindings in the expander environment. Both kinds of binding need to be distinguished from those belonging to other languages, so we create new structure types to represent them. There isn't any static information associated with PEG variables, so the corresponding structure has no fields. For macros we need to remember the transformer procedure:

@racketblock[
(begin-for-syntax
  (struct peg-variable [])
  (struct peg-macro [transformer]))
]

In DSLs with richer static semantics additional information such as types would be associated with variables. More sophisticated extensible embedded compilers usually employ the @secref["top" #:tag-prefixes '("binding-interface")] pattern in their expander environment representations to allow other languages to create variable bindings that interoperate with the DSL.

The main part of the expander for PEGs is defined in @figure-ref["peg-expander"].

@(require scribble/html-properties scribble/core)
@(define figure-fix
   (css-addition
     (string->bytes/utf-8
       ".Figure { border: 1px solid #929292 }")))
@figure["peg-expander" "PEG DSL Expander" #:style (style "foo" (list figure-fix))]{
@codeblock0[#:keep-lang-line? #f #:line-numbers 1]|{
#lang racket
(begin-for-syntax
  (define/hygienic (expand-peg stx)
    (syntax-parse stx
      #:literal-sets (peg-literals)
      ; Core forms
      [-eps this-syntax]
      [(-char c:char) this-syntax]
      [(-seq e1 e2)
       (def/stx e1^ (expand-peg #'e1))
       (def/stx e2^ (expand-peg #'e2))
       (qstx/rc (-seq e1^ e2^))]
      [(-or e1 e2)
       (def/stx e1^ (expand-peg #'e1))
       (def/stx e2^ (expand-peg #'e2))
       (qstx/rc (-or e1^ e2^))]
      [(-local [g e]
               b)
       (define sc (make-scope))
       (def/stx g^ (bind! (add-scope #'g sc)
                          #'(peg-variable)))
       (def/stx e^ (expand-peg (add-scope #'e sc)))
       (def/stx b^ (expand-peg (add-scope #'b sc)))
       (qstx/rc
        (-local [g^ e^]
                b^))]
      [name:id
       (when (not (peg-variable? (lookup #'name)))
         (raise-syntax-error #f "not bound as a peg" #'name))
       this-syntax]

      ; Macros
      [(head . rest)
       #:when (peg-macro? (lookup #'head))
       (expand-peg
        ((peg-macro-transformer (lookup #'head)) stx))])))
}|}



@subsection{The compiler}

Phase 1 function over syntax

Can use apply-as-transformer hygiene to ensure generated code is fresh

[[Persistent symbol tables]] as needed

@subsection{Embedding macros}

Macro in the language that the DSL is to be embedded in; usually Racket.

Invoke the expander and the compiler.

Perform and wrapping or conversion of values between the internal representation of the DSL and the external rep shared with Racket.

@section{Consequences}

@section{Implementation details}

@section{Known uses}

@section{Related patterns}

@bibliography[
  @bib-entry[#:key "peg" #:title "Parsing expression grammars: a recognition-based syntactic foundation"
             #:author "Bryan Ford"
             #:date "2004"
             #:location "POPL"
             #:url "https://doi.org/10.1145/964001.964011"]
]

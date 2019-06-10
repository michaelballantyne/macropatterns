#lang scribble/manual

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

@section{Solution}

The high level components of an extensible embedded compiler are a vocabulary of core syntactic forms, a macro expander, a back-end compiler, and macros to embed the DSL in Racket. The following sections address each component in turn.

In order to illustrate the pattern, we use as an example the DSL of parsing expression grammars (PEGs). Basic parsing expressions match empty input, characters, sequences, and alternatives. A local binding form enables named recursive grammars.

@verbatim|{
peg := (~eps)
     | (~char <character>)
     | (~seq <peg> <peg>)
     | (~or <peg> <peg>)
     | (~local ([<identifier> <peg>]) <peg>)
}|

A Racket macro defines the syntax @racket[parse] which accepts a PEG expression and a string to match and returns a number indicating how many characters were consumed by the match.

@subsection{The vocabulary of syntactic forms}

Syntactic forms in Racket are associated with an identifier binding in a module or local scope, just like runtime bindings. This allows the visibility and name of syntactic forms to be controlled by modules and lexical scope, and is essential to Racket's conception of "languages as libraries". Consequently, the first task in defining a DSL is to create bindings for the core syntactic forms of the DSL.

When we create identifier bindings, we also have to define their meaning if they appear as a normal Racket expression. DSL forms should appear in the context of the DSL, not in Racket, so we define their meaning as normal Racket expressions as an error.

The @seclink["top" #:tag-prefixes '("literal")] pattern describes how to establish the bindings. Using @racket[define-literals] from syntax-generic2 to abstract over that pattern, the literal definitions for the PEG language may be written as follows:

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

The expander has three jobs: parsing the syntax of the DSL's core syntactic forms, implementing scope and binding structure, and expanding macros. The work of each of these three tasks must be interleaved. Furthermore, the expander is responsible for defining interfaces...

@subsection{The compiler}

Phase 1 function over syntax

Can use apply-as-transformer hygiene to ensure generated code is fresh

[[Persistent symbol tables]] as needed

@subsection{Embedding macros}

Macro in the language that the DSL is to be embedded in; usually Racket.

Invoke the expander and the compiler.

Perform and wrapping or conversion of values between the internal representation of the DSL and the external rep shared with Racket.



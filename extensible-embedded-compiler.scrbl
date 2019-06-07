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

The high level components of an extensible embedded compiler include the vocabulary of core syntactic forms, interfaces describing the information associated with DSL variables and macros in the expander environment, the DSL's macro expander, the back-end compiler passes, and macros that embed the DSL within other languages and drive DSL compilation. The following sections address each component in turn.

@subsection{The vocabulary of syntactic forms}

Create the binding identities. Doesn't define the syntax of each form; just its identity which is needed to recognize it. Just like a Racket variable binding, these bindings can be renamed, hidden, etc on export and import.

Defines a literal set for use with syntax parse in the expander and compiler in order to recognize the forms.

@subsection{Interfaces for expander environment bindings}

@subsection{The expander}

The expander has three jobs: parsing the syntax of the DSL's core syntactic forms, implementing scope and binding structure, and expanding macros.

@subsection{The compiler}

Phase 1 function over syntax

Can use apply-as-transformer hygiene to ensure generated code is fresh

[[Persistent symbol tables]] as needed

@subsection{Embedding macros}

Macro in the language that the DSL is to be embedded in; usually Racket.

Invoke the expander and the compiler.

Perform and wrapping or conversion of values between the internal representation of the DSL and the external rep shared with Racket.



#lang scribble/manual

@(require (for-label racket syntax/id-table))

@title[#:style 'toc]{Racket Macro Patterns}

@section{Patterns}

@subsection[#:tag "top" #:tag-prefix "extensible-embedded-compiler"]{Extensible embedded compiler}

@subsubsection{Intent}

Implement full-featured domain-specific languages that:

@itemlist[
  @item{can employ multiple passes of analysis and compilation in their implementation.}
  @item{integrate fluidly with Racket and other DSLs while protecting DSL abstractions.}
  @item{are macro-extensible.}
]

@subsubsection{Motivation}

While one use of macros is to implement new language forms that extend the base Racket language, another is to implement conceptually distinct domain-specific langauges (DSLs).

A DSL has its own grammar, static semantics, and evaluation model. Implementing these features may require the flexibility of a traditional multi-pass compiler. At the same time, we often want DSLs to integrate fluidly with Racket and other DSLs. And just as programers can create abstractions atop Racket using macros, they should be able to create abstractions atop DSLs as well.

The extensible embedded compiler pattern is a way of structuring a DSL implementation to support all of these properties. It replicates the structure of the implementation of Racket, using a macro expander tailored to the DSL as front-end and a traditional multi-pass compiler as back-end. The DSL's macro expander shares the Racket expander's hygiene mechanism and expander environment in order to integrate the DSL syntax with Racket.

@subsubsection{Applicability}

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

@subsubsection{Solution}

The implementation of an extensible embedded compiler consists of the following high-level components:

@itemlist[
  @item{a set of @seclink["top" #:tag-prefixes '("literal")]{literals} for the core forms of the language}
  @item{@seclink["top" #:tag-prefixes '("syntax-interface")]{syntax interfaces} for DSL macros}
  @item{@seclink["top" #:tag-prefixes '("syntax-interface")]{syntax interfaces} for access to static information associated with DSL variable bindings}
  @item{phase 1 functions implementing the expand loops for each non-terminal of the DSL grammar and expansion rules for each core syntactic form}
  @item{phase 1 functions implementing the back-end compiler passes, and any needed @seclink["top" #:tag-prefixes '("persistent-symbol-table")]{persistent symbol tables}}
  @item{boundary macros that connect the DSL to Racket or other DSLs}
]


@subsection[#:tag "top" #:tag-prefix "literal"]{Literal}

@subsection[#:tag "top" #:tag-prefix "syntax-interface"]{Syntax interface}



@subsection[#:tag "top" #:tag-prefix "persistent-symbol-table"]{Persistent symbol table}

@subsubsection{Intent}

A persistent symbol table stores mappings from source name bindings to information needed for processing references to the names, including those found in other compilation units.

@subsubsection{Motivation}

Languages in Racket can be implemented as embedded multi-pass compilers with a macro expansion pass followed by traditional typechecking, optimization and compilation passes (see the @secref["top" #:tag-prefixes '("extensible-embedded-compiler")] pattern). Compilers often need to keep track of information associated with names. Information known during the expansion pass can be naturally associated with the name's binding in the @seclink["expander-environment"]{expander environment}. However, information computed in later compiler passes must be stored elsewhere.

For example, consider a parser generator that compiles each grammar non-terminal to a Racket function that matches it. The compiler needs a symbol table mapping non-terminal names to Racket function names. Because the name of the Racket function is generated during compilation, not grammar expansion, it cannot be stored in the expander environment binding for the non-terminal name.

The symbol table also needs to be available when dependent modules are compiled, because grammar productions in one file may reference non-terminals in another.

A persistent symbol table implements a mapping that is keyed by name bindings and persists across separate compilation just like the expander environment, but for use by other compiler passes.

@subsubsection{Applicability}

Use a persistent symbol table when a compiler pass needs to associate information with source-program names, but it does not make sense to include the information in the expander environment binding for the name. This could be because:

@itemlist[
  @item{the information is computed in a later compilation pass}
  @item{the mapping from source names to information depends on additional context, like the compilation target for a language that can be compiled to multiple backends}
  @item{the symbol table is not always needed and should be lazily loaded}
]

@subsubsection{Solution}

Mappings from variable bindings to compile-time information are stored in a @hyperlink["https://docs.racket-lang.org/syntax/syntax-helpers.html?q=free-id#%28mod-path._syntax%2Fid-table%29"]{free-identifier table}. The table is stored in a phase 1 variable in the module defining the compiler pass:

@racketblock[
(require (for-syntax syntax/id-table))
(begin-for-syntax
  (define table (make-free-id-table)))
]

Information about local variables is only needed during the extent of processing a single compilation unit. As the compilation pass processes the binding occurances of local names, it can simply mutate the table:

@racketblock[
(free-id-table-set! table #'id <data>)
]

However, information about module-level bindings may be needed when processing dependent modules.
Racket's system of @seclink["mod-parse" #:doc '(lib "scribblings/reference/reference.scrbl")]{phases and visits} provides a mechanism to run compile-phase code anytime a module is loaded to support the expansion of a dependent module.

Compilation of a module-level form that needs a symbol table entry should insert a @racket[begin-for-syntax] block in the fully-expanded module with code to update the symbol table. This code will be executed when the module is loaded for expansion of a dependent module, reconstructing the table entries for use during that expansion. For example, a definition form that acts as the boundary between Racket and a DSL might expand to a begin block containing a runtime definition and a @racket[begin-for-syntax] block with a symbol table assignment:

@RACKETBLOCK[
(define-syntax (my-definition stx)
  (syntax-parse stx
    [(_ name rhs)
     (define data _) (code:comment "compute information to store in symbol table")
     #`(begin
         (define tmp rhs)
         (begin-for-syntax
           (free-id-table-set! table #'name #'#,data)))]))
]

Depending on the structure of the compilation pass, the @racket[begin-for-syntax] block may be inserted differently. A @racket[#%module-begin] macro processing a whole module body may collect a set of such blocks and emit them as part of its final expansion.

@subsubsection{Consequences}

Associations can be made after the original expander environment bindings for the source names were made. Persistent symbol tables are thus well-suited for storing information computed in compiler passes that run after the original name binding was made for expansion.

Arbitrary code can be used in the construction and access of the symbol table, so the identity of the table can be computed. A single source name could be compiled multiple times for different backends, for example, with different symbol tables for each compilation.

@subsubsection[#:tag "example"]{Example}

The following example demonstrates how part of a typechecking pass that infers types for Racket core forms might use a persistent symbol table. Expressions are first expanded using the standard Racket expander, and then typechecked in a separate pass. The typechecking pass uses the symbol table to associate bound names with inferred types, for use in typechecking references.

@;In addition to expanding macros, the expansion pass establishes the name bindings that allow the free identifier table to function in the typechecking pass.

@codeblock|{
#lang racket

(require
  (for-syntax
   syntax/parse
   syntax/id-table))

(begin-for-syntax
  (define table (make-free-id-table))

  (define (well-formed? type)
    (syntax-parse type
      #:datum-literals (-> Bool)
      [Bool #t]
      [Number #t]
      [_ #f]))
  (define (check-well-formed! type)
    (when (not (well-formed? type))
      (raise-syntax-error #f "not a valid type" type)))

  (define (infer-type e)
    (syntax-parse e
      #:literals (quote let-values)
      [(quote v:boolean)
       #'Bool]
      [(quote v:number)
       #'Number]
      [v:identifier
       (syntax-local-introduce (free-id-table-ref table #'v))]
      [(let-values ([(v) e]) b)
       (define t (infer-type #'e))
       (free-id-table-set! table #'v (syntax-local-introduce t))
       (infer-type #'b)]
      [_ (raise-syntax-error #f "unsupported syntax" e)])))

(define-syntax define/typed
  (syntax-parser
    #:datum-literals (:)
    [(_ name : type e)
     (check-well-formed! #'type)
     #'(begin
         (define name (typechecked-expression e type))
         (begin-for-syntax
             (free-id-table-set! table #'name #'type)))]))

(define-syntax typechecked-expression
  (syntax-parser
    [(_ e type)
     (check-well-formed! #'type)
     (define e^ (local-expand #'e 'expression null))
     (when (not (equal? (syntax-e (infer-type e^))
                        (syntax-e #'type)))
       (raise-syntax-error #f "type mismatch" #'type))
     #'e]))

(module* a #f
  (define/typed x : Number (let ([x 5]) x))
  (provide x))

(module* b #f
  (require (submod ".." a))
  ; well-typed with cross-module reference
  (define/typed y : Bool (let ([y #t]) y))
  ; type error!
  (define/typed z : Bool (let ([x x]) x)))
}|

@subsubsection{Implementation details}

Free-identifier tables are keyed based on the bindings of identifiers in the @seclink["binding-store"]{binding store}. A variable must be bound before adding it to the free identifier table, because lookup will misbehave if the binding of a key changes after it is added to the table. Usually a persistent symbol table for a compiler pass will re-use bindings for source names that were created during an earlier macro expansion pass. In the typechecking @seclink["example"]{example} this appears in two places. Expressions are expanded using @racket[local-expand] before they are typechecked, which creates bindings for variables in @racket[let-values] forms. The @racket[define/typed] typed definition form expands to an untyped runtime definition plus code to update the symbol table. It is important that the runtime definition is placed first, so that expansion of the @racket[define] form creates a binding for the name before the table update is executed.

This pattern involves communicating data between macro invocations. If that data contains syntax, the implementation of this pattern needs to be careful to @seclink["syntax-local-introduce"]{adjust scopes with @racket[syntax-local-introduce]}. In the typechecking example only the symbols within the type syntax are used, so the scopes attached don't matter. In a more complex example with locally bound polymorphic type variables, this detail of scope would be important.

@subsubsection{Known uses}

@itemlist[
  @item{The Typed Racket global type environment. See @hyperlink["https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/env/global-env.rkt"]{typed-racket/env/global-env}, as well as the @seclink["references"]{references} below.}
  @item{My embedded compilers for PEGs and miniKanren. TODO: link}
]

@subsubsection{Related patterns}

@itemlist[
  @item{Attaching extra information to bindings via binding to struct. TODO}
  @item{Separately-compiled DSL definition form. TODO}
]

@subsubsection[#:tag "references"]{References}

@itemlist[
  @item{@hyperlink["https://www.cs.utah.edu/plt/publications/macromod.pdf"]{Composable and Compilable Macros}}
  @item{@hyperlink["https://www.cs.utah.edu/plt/publications/pldi11-tscff.pdf"]{Languages as Libraries}}
  @item{@hyperlink["https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf"]{Advanced Macrology and the Implementation of Typed Scheme}}
]


@section{Explainers}

@;@subsection{Separate compilation}

@subsection[#:tag "expander-environment"]{The expander environment}

@subsection[#:tag "binding-store"]{The binding store}

@subsection[#:tag "syntax-local-introduce"]{@racket[syntax-local-introduce]}

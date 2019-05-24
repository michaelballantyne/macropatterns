#lang scribble/manual

@(require (for-label racket))

@title[#:style 'toc]{Racket Macro Patterns}

@section{Patterns}

@subsection{Persistent mapping table}

@subsubsection{Intent}

A persistent mapping table stores mappings from source name bindings to information needed for processing references to the names, including those found in other compilation units.

@subsubsection{Motivation}

Compilers often need to keep track of information associated with names. In a DSL compiler constructed with macros, information known during expansion can be naturally associated with the name's binding in the expander environment. However, other information is computed in a later language-processing pass and must be stored elsewhere.

For example, consider a parser generator that compiles each grammar non-terminal to a Racket function that matches it. The compiler needs a mapping from non-terminal names to Racket function names. Because the name of the Racket function determined during compilation, not grammar expansion, it cannot be stored in the expander environment binding for the non-terminal name.

The mapping also needs to be available when dependent modules are compiled, because grammar productions in one file may reference non-terminals in another.

A persistent mapping table implements a mapping that is keyed by name bindings and persists across separate compilation just like the expander environment, but for use by other compiler passes.

@subsubsection{Applicability}

Use a persistent mapping table when a language-processing pass needs to associate information with source-program names, but it does not make sense to include the information in the expander environment binding for the name. This could be because:

@itemlist[
  @item{the information is computed in a later compilation pass}
  @item{the mapping from source names to information depends on additional context, like the compilation target for a language that can be compiled to multiple backends}
  @item{the mapping is not always needed and should be lazily loaded}
]

@subsubsection{Solution}

Mappings from variable bindings to compile-time information are stored in a @hyperlink["https://docs.racket-lang.org/syntax/syntax-helpers.html?q=free-id#%28mod-path._syntax%2Fid-table%29"]{free-identifier table}. The table is stored in a phase 1 variable in the module defining the language-processing pass:


@racketblock[
(require (for-syntax syntax/id-table))
(begin-for-syntax
  (define table (make-free-id-table)))
]

Information about local variables is only needed during the extent of processing a single compilation unit. As the compilation pass processes the binding occurances of local names, it can simply mutate the table:

@racketblock[
(set! table #'id <data>)
]

However, information about module-level bindings may be needed when processing dependent modules.
Racket's system of [ext[phases and visits|https://docs.racket-lang.org/reference/syntax-model.html#%28part._mod-parse%29]] provides a mechanism to run compile-phase code anytime a module is loaded to support the expansion of a dependent module. The paper [ext[Composable and Compilable Macros|https://www.cs.utah.edu/plt/publications/macromod.pdf]] provides a deeper description.

Compilation of a module-level form that needs a mapping entry should insert a @racket[begin-for-syntax] block in the fully-expanded module with code to update the mapping table. This code will be executed when the module is loaded for expansion of a dependent module, reconstructing the table entries for use during that expansion.

@racketblock[
(begin-for-syntax
  (set! table #'id <data>))
]

Depending on the structure of the compilation pass, this block may be inserted differently. A @racket[#%module-begin] macro processing a whole module body may collect a set of such blocks and emit them as part of its final expansion. Alternatively, a definition form that acts as the boundary between Racket and a DSL may expand to a begin block containing runtime definitions, syntax definitions, and @racket[begin-for-syntax] blocks with mapping table assignments.

@subsubsection{Consequences}

Associations can be made after the original expander environment bindings for the source names were made. Mapping tables are thus well-suited for use in language processing passes that happen after expansion, and where the information to be stored is not known during expansion.

Arbitrary code can be used in the construction and access of the mapping table, so the identity of the table can be computed. A single source name could be compiled multiple times for different backends, for example, with different mapping tables for each compilation.

@subsubsection{Example}

The following example demonstrates how part of a typechecking pass that infers types for Racket core forms might use a persistent mapping table. Expressions are first expanded using the standard Racket expander, and then typechecked in a separate pass. The typechecking pass uses the mapping table to associate bound names with inferred types, for use in typechecking references. In addition to expanding macros, the expansion pass establishes the name bindings that allow the free identifier table to function in the typechecking pass.

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

A variable must be bound before adding it to the free identifier table, because lookup could misbehave if the binding of a key changes after it is added to the table. For example, in the typechecking example above it is important that the expansion of @racket[define/typed] place the @racket[define] before the @racket[begin-for-syntax] block.

This pattern involves communicating data between macro invocations. If that data contains syntax, the implementation of this pattern needs to be careful to adjust scopes with @racket[syntax-local-introduce] as discussed in TODO Moving syntax between macros with @racket[syntax-local-introduce]. In the typechecking example only the symbols within the type syntax are used, so the scopes attached don't matter. In a more complex example with locally bound polymorphic type variables, this detail of scope would be important.

@subsubsection{Known uses}

@itemlist[
  @item{The Typed Racket global type environment. See @hyperlink["https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed-racket/env/global-env.rkt"]{typed-racket/env/global-env}}
  @item{My embedded compilers for PEGs and miniKanren. TODO: link}
]

@subsubsection{Related patterns}

@itemlist[
  @item{Moving syntax between macros with @racket[syntax-local-introduce]. TODO}
  @item{Attaching extra information to bindings via binding to struct. TODO}
  @item{Separately-compiled DSL definition form. TODO}
]

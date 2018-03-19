#lang scribble/manual

@title[#:style 'toc]{Racket Macro Patterns}



@local-table-of-contents[]

@include-section{staticinfo.scrbl}

@include-section{hygiene.scrbl}

@include-section{phasing.scrbl}



@section{Rewriting patterns}

@subsection{Rewriting variable references}

@subsection{Push-down definition context rewritings}

Like splicing stuff, wrapping module begin.

@subsection{Wrapping forms to stop expansion}

Like erased in turnstile.


@section{Extensibility}

@subsection{Macro-extensible DSLs requiring multiple-form compilation}

Like class

@subsection{Interposition Points}

@subsection{Simulating Transformer Invocation}


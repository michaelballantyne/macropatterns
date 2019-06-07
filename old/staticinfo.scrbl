#lang scribble/manual

@(require (for-label racket))

@title{Conveying information between macros}

@section{Binding compile-time information}

@section{Bindings with both runtime and static meaning}

Sometimes a binding needs to convey both a runtime value and static information for use by other macros. Consider structure types defined by Racket's `struct` macro:

@racketblock[
(struct point [x y] #:transparent)
]

The name `point` is bound to the structure type's constructor procedure, and acts as a runtime value (not as syntax). The constructor can be passed to higher-order functions, for example:

@racketblock[
 (define l (map point '(1 2 3) '(1 2 3)))
]

The same name also has meaning in match patterns:

@racketblock[
(match (first l)
  [(point x y)
   x])
]

The constructor procedure can't be responsible for this behavior, however! It only allows construction of points, not deconstruction. Further, match patterns are checked statically, and we'll get an error if we provide the wrong number of arguments in the pattern:

@racketblock[
(match (first l)
  [(point x)
   x])
]
@codeblock|{
=>
 match: insufficient number of fields for structure
   point: expected 2 but got 1; missing fields (point-y)
}|

To support these uses, the binding for `point` needs to provide two things: a runtime value, and static information for other macros like `match` that want to treat uses of the constructor specially.

@section{Syntax parameters}

@section{Registries of compile-time information}

@section{Attaching static information to variable references}

(As in turnstile)

@section{Exporting extra static information with submodules}
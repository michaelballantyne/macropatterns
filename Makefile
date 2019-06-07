all: docs/index.html

open: all
	xdg-open docs/index.html

docs/index.html: index.scrbl
	scribble +m --htmls --dest-name docs --redirect-main https://docs.racket-lang.org/ index.scrbl

all: docs/index.html

open: all
	xdg-open docs/index.html

docs/index.html: index.scrbl
	cd docs; scribble +m --redirect-main https://docs.racket-lang.org/ ../index.scrbl

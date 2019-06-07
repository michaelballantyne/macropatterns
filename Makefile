all:
	scribble +m --htmls --dest-name docs --redirect-main https://docs.racket-lang.org/ index.scrbl

open: all
	xdg-open docs/index.html

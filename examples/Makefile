%.html: %.scrbl
	scribble --html ++xref-in setup/xref load-collections-xref $<

%.pdf: %.scrbl
	scribble --pdf ++xref-in setup/xref load-collections-xref $<

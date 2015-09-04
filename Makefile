examples=examples/sqrt-newton.scm examples/definitions-lists.scm \
	examples/map-01.scm examples/map-02.scm examples/sum.scm \
	examples/foldl.scm examples/quicksort.scm lib/lists.scm \
	examples/strict-map.scm examples/map-03.scm

pdfs=README.pdf

htmls=README.html

all: ${examples} ${pdfs} ${htmls}

${examples}: README.org
	emacs --batch -l make-documents.el README.org -f make-examples

${pdfs}: README.org
	emacs --batch -l make-documents.el README.org -f make-pdf

${htmls}: README.org
	emacs --batch -l make-documents.el README.org -f make-html

clean:
	rm ${examples} ${pdfs} ${htmls}

.PHONY: all clean

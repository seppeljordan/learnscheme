examples=examples/sqrt-newton.scm examples/definitions-lists.scm \
	examples/map-01.scm examples/map-02.scm examples/sum.scm \
	examples/foldl.scm examples/quicksort.scm lib/lists.scm \
	examples/strict-map.scm

pdfs=README.pdf

all: ${examples} ${pdfs}

${examples}: README.org
	emacs --batch -l make-documents.el README.org -f make-examples

${pdfs}: README.org
	emacs --batch -l make-documents.el README.org -f make-pdf

clean:
	rm ${examples} ${pdfs}

.PHONY: all clean

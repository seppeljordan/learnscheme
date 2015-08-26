examples=sqrt-newton.scm definitions-lists.scm map-example-01.scm map-example-02.scm sum-example.scm foldexamples.scm iter-list.scm quicksort-example.scm lib/lists.scm strict-map.scm
pdfs=README.pdf

all: ${examples} ${pdfs}

${examples}: README.org
	emacs --batch -l make-documents.el README.org -f make-examples

${pdfs}: README.org
	emacs --batch -l make-documents.el README.org -f make-pdf

clean:
	rm ${examples} ${pdfs}

.PHONY: all clean

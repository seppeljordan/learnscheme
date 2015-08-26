examples=sqrt-newton.scm definitions-lists.scm map-example-01.scm map-example-02.scm sum-example.scm foldexamples.scm iter-list.scm quicksort-example.scm lib/lists.scm strict-map.scm
pdfs=lessons.pdf

all: ${examples} ${pdfs}

${examples}: lessons.org
	emacs --batch -l make-documents.el lessons.org -f make-examples

${pdfs}: lessons.org
	emacs --batch -l make-documents.el lessons.org -f make-pdf

clean:
	rm ${examples} ${pdfs}

.PHONY: all clean



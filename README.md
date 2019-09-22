# fortsearch - Search engine in FORTRAN

## Why?

Written initially as a joke for a class assignment. The professor made a comment that in general submission quality is inversely proportional to the length of the language name. I chose one of the "long name" languages which I knew I could make run fast

## Building

Use make. A fairly recent versions of GFortran is also required which supports 2003 language features.

## Usage

The file to be indexed must contain documents in XML or HTML concated together, each prefixed with a `<DOCNO>identifier</DOCNO>` tag.

There are two programs. Index and search. Index builds an index over the document. Search takes terms from stdin and outputs two columns, the document id and the relevancy score. To index run `index document.xml`

## Done

* Multi term searches

## Todo

* Better parser
* BM25 ranking
* Speed improvements

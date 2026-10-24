#lang scribble/manual
@(require (only-in datasets/r-data/r-objects r-objects) (only-in racket/match match))

@title[#:tag "datasets-from-r"]{Datasets from R}

@(define catalog '(lib "datasets/core-doc/datasets-core.scrbl"))

@(define (r-object-cell x)
   (match x
     [(? string?) x]
     [(list 'tt s) (tt s)]
     [(list 'dataset id) (tt (symbol->string id))]))

Every object of the @tt{datasets} package of R 4.3.3 is imported by
@tt{datasets-r}, linked to equivalent data in @tt{datasets-core}, or skipped, as
the table below records. R's @tt{iris} links to the @tt{datasets-core}
@tt{iris}: the same measurements, with UCI species labels. Each dataset's entry
is in the @secref["datasets-catalog" #:doc catalog]. Imports are mechanical:

@itemlist[
 @item{Names: a hyphen goes between a lowercase letter or digit and a capital,
   and before the last capital of a run followed by a lowercase letter; then
   lowercase, with any other characters as hyphens (@tt{state.x77} gives
   @tt{state-x77}, @tt{UKDriverDeaths} gives @tt{uk-driver-deaths}). Column
   names follow the same rule. Exceptions: @tt{BJsales} @tt{bj-sales}, @tt{CO2}
   @tt{co2-uptake} (@tt{co2} is the Mauna Loa series), @tt{DNase} @tt{dnase},
   @tt{UKgas} @tt{uk-gas}, @tt{UScitiesD} @tt{us-cities-d}, @tt{WWWusage}
   @tt{www-usage}.}
 @item{Data frames: columns as in R. Row names that are not numbers become a
   first identifier column; nlme groupedData attributes are dropped.}
 @item{Time series: index columns @tt{year} and, by frequency, @tt{quarter},
   @tt{month} or @tt{business-day} (@tt{time} for series that start at 1 with
   frequency 1), with @tt{time-series} metadata. @tt{eu-stock-markets} is on
   R's nominal grid of 260 business days a year, not calendar dates. A
   univariate series is a @tt{value} column; a multivariate one has a column
   per series. Series that share an index are joined: @tt{bj-sales} and
   @tt{uk-lung-deaths}.}
 @item{Contingency tables: long form, one categorical column per dimension and
   a @tt{count} column, one row per cell in R array order, zero cells kept.}
 @item{Matrices: row names as a first column, an identifier unless they are
   numbers; @tt{volcano} in long form (@tt{row}, @tt{column}, @tt{value}).
   Covariance lists: the @tt{cov} matrix with a @tt{variable} column.}
 @item{Distance matrices: one row per pair of the lower triangle, (@tt{from},
   @tt{to}, @tt{distance}).}
 @item{Vectors: a @tt{value} column, after a @tt{name} column for a named
   vector, an identifier when the names are unique; the @tt{state.*} objects
   are joined into @tt{state}.}]

@tabular[#:sep (hspace 2)
         (cons (list "R object" "Class" "Dataset")
               (for/list ([row (in-list r-objects)])
                 (map r-object-cell row)))]

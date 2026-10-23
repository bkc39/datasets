#lang scribble/manual
@(require datasets/core racket/file racket/list racket/match racket/string
          (for-label racket/base datasets/core))
@title[#:tag "datasets-catalog"]{Dataset Catalog and Provenance}

The following catalog is generated from the same registry used by
@racket[dataset-info]. Column order matches the full table returned by
@racket[load-dataset-table]. It lists every registry installed when this manual
was built, one section per package.

Redistribution follows upstream distribution terms. Each package section lists
its data licenses and each entry names its own: in @tt{datasets-core}, R data
under GPL-2 or GPL-3, @tt{lars} diabetes under GPL-2, and UCI data under CC BY
4.0. This records the redistribution basis, without asserting independent
original-owner permission. Code is Apache-2.0 OR MIT. Full license texts and
upstream variable descriptions and citations are bundled in
@tt{datasets/licenses} and @tt{datasets/provenance} for @tt{datasets-core}, and
in @tt{licenses} and @tt{provenance} beside the registry of any other package.
The maintainer importer pins source archives by SHA-256. Normalized checksums cover
the exact UTF-8 bytes of each dataset's uncompressed @tt{.rktd} content. Historical observations are not
silently corrected except for choosing the documented corrected UCI Iris variant.

@(define (fetch-line info)
   (match (hash-ref info "fetch")
     [#f '()]
     [fetch (para "Fetch tier: download SHA-256 " (tt (hash-ref fetch "sha256"))
                  ", fetched on first use; see " (secref "datasets-core-fetch-guide") ".")]))

@(define (dataset-entry name)
   (define info (dataset-info name))
   (list (subsection #:tag (format "dataset-~a" name) (symbol->string name))
         (para (format "~a rows × ~a columns~a. ~a" (hash-ref info "rows")
                       (hash-ref info "column-count")
                       (if (equal? (hash-ref info "tier") "fetch") ", fetch tier" "")
                       (hash-ref info "notes")))
         (para (hash-ref info "citation"))
         (para (hyperlink (hash-ref info "source-url") (hash-ref info "source-version"))
               "; " (hash-ref info "license"))
         (fetch-line info)
         (tabular #:sep (hspace 2)
                  (cons (list "Column" "Original" "Type" "Description / unit / levels")
                        (for/list ([c (in-list (hash-ref info "columns"))])
                          (list (hash-ref c "name") (hash-ref c "original-name")
                                (hash-ref c "type")
                                (format "~a; ~a; ~a" (hash-ref c "description")
                                        (hash-ref c "unit") (hash-ref c "levels"))))))))

@(define (r-object-row object)
   (match object
     [(list name class "dataset" id) (list (tt name) class (secref (format "dataset-~a" id)))]
     [(list name class "core" id) (list (tt name) class (secref (format "dataset-~a" id)))]
     [(list name class "skipped" reason) (list (tt name) class (string-append "Skipped. " reason))]))

@(define (package-preface package)
   (match package
     ["datasets-r"
      (list
       (para "Every object of the " (tt "datasets") " package of R 4.3.3 is imported here, kept in "
             (tt "datasets-core") ", or skipped, as the table below records. Imports are mechanical:")
       (itemlist
        (item "Names: a hyphen goes between a lowercase letter or digit and a capital, and before "
              "the last capital of a run followed by a lowercase letter; then lowercase, with any "
              "other characters as hyphens (" (tt "state.x77") " gives " (tt "state-x77") ", "
              (tt "UKDriverDeaths") " gives " (tt "uk-driver-deaths") "). Column names follow the "
              "same rule. Exceptions: " (tt "BJsales") " " (tt "bj-sales") ", " (tt "CO2") " "
              (tt "co2-uptake") " (" (tt "co2") " is the Mauna Loa series), " (tt "DNase") " "
              (tt "dnase") ", " (tt "UKgas") " " (tt "uk-gas") ", " (tt "UScitiesD") " "
              (tt "us-cities-d") ", " (tt "WWWusage") " " (tt "www-usage") ".")
        (item "Data frames: columns as in R. Row names that are not numbers become a first "
              "identifier column; nlme groupedData attributes are dropped.")
        (item "Time series: index columns " (tt "year") " and, by frequency, " (tt "quarter") ", "
              (tt "month") " or " (tt "business-day") " (" (tt "time") " for series that start at 1 "
              "with frequency 1), with " (tt "time-series") " metadata. A univariate series is a "
              (tt "value") " column; a multivariate one has a column per series. Series that share "
              "an index are joined: " (tt "bj-sales") " and " (tt "uk-lung-deaths") ".")
        (item "Contingency tables: long form, one categorical column per dimension and a "
              (tt "count") " column, one row per cell in R array order, zero cells kept.")
        (item "Matrices: row names as a first column, an identifier unless they are numbers; "
              (tt "volcano") " in long form (" (tt "row") ", " (tt "column") ", " (tt "value") "). "
              "Covariance lists: the " (tt "cov") " matrix with a " (tt "variable") " column.")
        (item "Distance matrices: one row per pair of the lower triangle, (" (tt "from") ", "
              (tt "to") ", " (tt "distance") ").")
        (item "Vectors: " (tt "name") " and " (tt "value") " columns; the " (tt "state.*") " "
              "objects are joined into " (tt "state") "."))
       (tabular #:sep (hspace 2)
                (cons (list "R object" "Class" "Dataset")
                      (map r-object-row
                           (file->value (collection-file-path "r-objects.rktd" "datasets"
                                                              "r-data"))))))]
     [_ '()]))

@(for/list ([package (in-list (dataset-packages))])
   (define names (dataset-names #:package package))
   (define licenses
     (sort (remove-duplicates
            (map (lambda (name) (hash-ref (dataset-info name) "license")) names))
           string<?))
   (list (section #:tag (format "datasets-package-~a" package) package)
         (para (format "~a datasets. Licenses: ~a." (length names)
                       (string-join licenses "; ")))
         (package-preface package)
         (map dataset-entry names)))

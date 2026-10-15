#lang scribble/manual
@(require datasets/core racket/list racket/string
          (for-label racket/base datasets/core))
@title[#:tag "datasets-catalog"]{Dataset Catalog and Provenance}

The following catalog is generated from the same registry used by
@racket[dataset-info]. Column order matches the full table returned by
@racket[load-dataset-table]. It lists every registry installed when this manual
was built, one section per package.

Redistribution follows upstream distribution terms: R data under GPL-2 or GPL-3,
@tt{lars} diabetes under GPL-2, and UCI data under CC BY 4.0. This records the
redistribution basis, without asserting independent original-owner permission.
Code is Apache-2.0 OR MIT. Full license texts and upstream variable descriptions
and citations are bundled in @tt{datasets/licenses} and @tt{datasets/provenance}.
The maintainer importer pins source archives by SHA-256. Normalized checksums cover
the exact UTF-8 bytes of each dataset's uncompressed @tt{.rktd} content. Historical observations are not
silently corrected except for choosing the documented corrected UCI Iris variant.

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
         (tabular #:sep (hspace 2)
                  (cons (list "Column" "Original" "Type" "Description / unit / levels")
                        (for/list ([c (in-list (hash-ref info "columns"))])
                          (list (hash-ref c "name") (hash-ref c "original-name")
                                (hash-ref c "type")
                                (format "~a; ~a; ~a" (hash-ref c "description")
                                        (hash-ref c "unit") (hash-ref c "levels"))))))))

@(for/list ([package (in-list (dataset-packages))])
   (define names (dataset-names #:package package))
   (define licenses
     (sort (remove-duplicates
            (map (lambda (name) (hash-ref (dataset-info name) "license")) names))
           string<?))
   (list (section #:tag (format "datasets-package-~a" package) package)
         (para (format "~a datasets. Licenses: ~a." (length names)
                       (string-join licenses "; ")))
         (map dataset-entry names)))

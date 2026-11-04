#lang racket/base
(require (only-in datasets/core dataset-info dataset-names)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/list add-between remove-duplicates)
         (only-in racket/match match)
         (only-in racket/string string-join)
         (only-in scribble/base hspace hyperlink para secref section subsection tabular tt)
         (only-in "catalog-rows.rkt" catalog-column-rows))
(provide (contract-out [package-catalog (-> string? list?)]))

(define (fetch-line info)
  (define fetch-guide
    (secref "datasets-core-fetch-guide" #:doc '(lib "datasets/core-doc/datasets-core.scrbl")))
  (match (hash-ref info "fetch")
    [#f '()]
    [(hash-table ("format" "convert") ("files" files)
                 ("converter" (hash-table ("module" module) ("binding" binding))))
     (define n (length files))
     (para (format "Fetch tier: converted on first use from ~a upstream ~a by " n
                   (if (= n 1) "file" "files"))
           (tt binding) " in " (tt module) ": "
           (add-between (for/list ([f (in-list files)])
                          (list (tt (hash-ref f "name")) " (SHA-256 " (tt (hash-ref f "sha256")) ")"))
                        ", ")
           "; see " fetch-guide ".")]
    [fetch (para "Fetch tier: download SHA-256 " (tt (hash-ref fetch "sha256"))
                 ", fetched on first use; see " fetch-guide ".")]))

(define (dataset-entry name)
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
                       (catalog-column-rows (hash-ref info "columns"))))))

(define (package-catalog package)
  (define names (dataset-names #:package package))
  (define licenses
    (sort (remove-duplicates (map (lambda (name) (hash-ref (dataset-info name) "license")) names))
          string<?))
  (list (section #:tag (format "datasets-package-~a" package) package)
        (para (format "~a datasets. Licenses: ~a." (length names) (string-join licenses "; ")))
        (map dataset-entry names)))

#lang racket/base
(require (for-syntax racket/base)
         (only-in compiler/distribute assemble-distribution)
         (only-in compiler/embed create-embedding-executable)
         (only-in racket/file copy-directory/files delete-directory/files find-files make-directory*)
         (only-in racket/port open-output-nowhere port->string)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "fixture.rkt" call-with-temporary-fixture))
(define-runtime-path collection "..")
(module+ test
  (require (only-in rackunit check-equal? check-pred test-case))
  (define (run-isolated exe home)
    (define env (environment-variables-copy (current-environment-variables)))
    (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
    (define-values (process out in _err)
      (parameterize ([current-environment-variables env])
        (subprocess #f #f 'stdout exe)))
    (close-output-port in)
    (define status
      (cond [(sync/timeout 120 process) (subprocess-status process)]
            [else (subprocess-kill process #t) (subprocess-wait process) 'timeout]))
    (define output (port->string out))
    (close-input-port out)
    (list status output))
  (test-case "raco exe + raco distribute: the executable loads iris without installed packages"
    (call-with-temporary-fixture
     (lambda (dir)
       (define main (build-path dir "main.rkt"))
       (define exe (build-path dir "build" "main"))
       (define dist (build-path dir "dist"))
       (define home (build-path dir "home"))
       (define collects (build-path dir "collects"))
       (make-directory* (build-path dir "build"))
       (make-directory* home)
       (make-directory* collects)
       (copy-directory/files collection (build-path collects "datasets"))
       (for ([compiled (in-list (find-files (lambda (p) (regexp-match? #rx"(^|/)compiled$" (path->string p)))
                                            collects))])
         (delete-directory/files compiled #:must-exist? #f))
       (call-with-output-file main
         (lambda (out)
           (write '(module main racket/base
                     (require datasets/core)
                     (write (dataset-table-row-count (load-dataset-table 'iris))))
                  out)))
       (parameterize ([current-namespace (make-base-namespace)]
                      [current-library-collection-paths (cons collects (current-library-collection-paths))]
                      [current-library-collection-links (list #f)])
         (check-equal? (collection-file-path "core.rkt" "datasets") (build-path collects "datasets" "core.rkt"))
         (create-embedding-executable exe
                                      #:modules (list (list '#%mzc: main))
                                      #:configure-via-first-module? #t
                                      #:literal-expression (compile '(namespace-require ''#%mzc:main))
                                      #:cmdline '("-U" "--")))
       (parameterize ([current-output-port (open-output-nowhere)])
         (assemble-distribution dist (list exe)))
       (define registries
         (find-files (lambda (p) (regexp-match? #rx"(^|/)registry\\.rktd$" (path->string p))) dist))
       (delete-directory/files collects)
       (check-equal? (length registries) 1)
       (for ([registry (in-list registries)])
         (define-values (registry-dir _name _dir?) (split-path registry))
         (check-pred file-exists? (build-path registry-dir "data" "iris.rktd.gz")))
       (check-equal? (run-isolated (build-path dist "bin" "main") home) '(0 "150"))))))

#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core dataset-info dataset-names dataset-table-columns dataset-table-names
                  load-dataset-table)
         (only-in racket/match match-define match-lambda)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "fixture.rkt" call-with-registry-fixtures call-with-temporary-fixture gen-entry
                  gunzip-file gzip-bytes hex registry-fixture rows->rktd-bytes table-digest
                  write-registry-fixture!))
(define-runtime-path core-data "../core-data")
(define-runtime-path pre-change-registry "pre-change-registry.rktd")
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false test-case))
  (define pinned-sha256
    '((air-passengers "7f5e28170e7d79384a758fca5ca1f4e0dc018dc4717bc4864eb197e545e6eeac")
      (airquality "8274e5211f8a68c76e80f5ea8ec31d65be653a097a911adb22d086a2309d4110")
      (anscombe "cab6cb9691c9652b0dcf0d034828fecd324f9bd548359fe5e0bda92aa849cf61")
      (breast-cancer "3fbf52dbd7b836f3e8934d7f42ad0501c997f4813283f455827651f3576ef76f")
      (diabetes "a138c43502ef40072a26a9b85065b5fc0fcdd77ebc0e61ebd1fb8cd4c6daae41")
      (faithful "1f988d2a8be2a26dd1784eb6d1e6c77a9529f70e67212fdcf3d79b4fe483ec8f")
      (iris "72b803250a0224cb6c60f40703e85157aeace4ef681e41ac4f1f5a39cb546b14")
      (mtcars "ab924161d4ef2ef15e8652faaabd9c78fa134656240895ceba388ea85451fe01")
      (plant-growth "88b664ea2e16d0c318414cb0fee34ed68b94da3979da97e9e743655767a4b0d6")
      (titanic "fbc92b7271730bf959f64102c7c9090390ea7726889f94b9d3109264317b63fe")
      (tooth-growth "6abe5699789cad8cd0fd254d7832fb4a6c4c87ce86ab05de7a7b8a7f275cfbd1")
      (us-arrests "ba0af6b2a54267cdcad914a0afe135ae2f9f8dba2af23619c5629ad0006e1be3")
      (wine "9a652e243d1640fa22e50429e5fcd4daf95db1fcb6b939d72dc37091430930c2")))
  (define pre-change-table-digests
    '((air-passengers "0527ca4cd4a20796928d06e3e1fb3cb7602a52220066c386d8d9b9cb5075b5a2")
      (airquality "61504b24d19f59b2b345ef33857a4a8a3a9120d254ceab4b5cb1245cd5627b70")
      (anscombe "ecb927f57ee64bb1c7d12030eceb984fd107c1b253c738f0d051d1af9bf728ca")
      (breast-cancer "6a62ac048fd564e3f1aa7ea4bc3583de8c623522af221ec989d34ae526d49c6d")
      (diabetes "9cc8c228e95f648469cdfd65b65a2486251181ce91c5498c90f4d343be96bbe5")
      (faithful "aa2cf056aa714505b9aed691273bc0c933cb80101d6c7f12298da84fb6a163c3")
      (iris "d2c6f203895185b81319539f85d0854154dc62f0424a44b482e7f0512977da65")
      (mtcars "87babe7b1afda110a27ceeeb317e76ee6b45081ed1f6e948095768f40992d23b")
      (plant-growth "72d681ad71202a71e6b14c21349b19158250884568e3fc2b912db8fb5ab83c74")
      (titanic "33a24074374d6c3969e6c6034cea198c45ead027ca67eddbc0700776279aa45f")
      (tooth-growth "c8728cd8d33e3ae1ea1524655d8b97469828edaf52b4cfd7d12c1b95c01faf0c")
      (us-arrests "977321e29af666bad3dbb477a996cd34e5226ed4c53c147e659e762eb252128f")
      (wine "c1cc1fb06b1ae8df2d0406c02148112c0371516b7086ab6145cbd6e9ab90e32f")))
  (define (data-file id) (build-path core-data "data" (format "~a.rktd.gz" id)))
  (define old-registry (call-with-input-file pre-change-registry read))

  (test-case "S1 decompressed bytes byte-identical to 007c85a"
    (for ([p (in-list pinned-sha256)])
      (match-define (list id sha) p)
      (check-equal? (hex (sha256-bytes (gunzip-file (data-file id)))) sha)
      (check-equal? (hash-ref (dataset-info id) "sha256") sha)))

  (test-case "S3 loaded values identical to pre-change loads"
    (for ([p (in-list pre-change-table-digests)])
      (match-define (list id digest) p)
      (check-equal? (table-digest (load-dataset-table id)) digest)))

  (define (check-core-storage)
    (test-case "S2 metadata unchanged apart from 3 new keys"
      (for ([id (in-list (dataset-names #:package "datasets-core"))])
        (define info (dataset-info id))
        (check-equal? (for/fold ([h info]) ([k (in-list '("tier" "package" "fetch"))]) (hash-remove h k))
                      (hash-ref old-registry (symbol->string id)))
        (check-equal? (hash-ref info "tier") "bundled")
        (check-equal? (hash-ref info "package") "datasets-core")
        (check-false (hash-ref info "fetch"))))

    (test-case "S3b column selection is a permutation of the full load"
      (for ([id (in-list (dataset-names #:package "datasets-core"))])
        (define full (load-dataset-table id))
        (define picked (load-dataset-table id #:columns (reverse (dataset-table-names full))))
        (check-equal? (vector->list (dataset-table-columns picked))
                      (reverse (vector->list (dataset-table-columns full))))))

    (test-case "S4 reproducible gzip header"
      (for ([id (in-list (dataset-names #:package "datasets-core"))])
        (define header (call-with-input-file (data-file id) (lambda (in) (read-bytes 10 in))))
        (check-equal? (subbytes header 0 3) (bytes #x1f #x8b 8))
        (check-equal? (bitwise-and (bytes-ref header 3) 8) 0)
        (check-equal? (subbytes header 4 8) (bytes 0 0 0 0)))))
  (check-core-storage)
  (test-case "S2 S3b S4 hold with a sibling registry installed"
    (call-with-registry-fixtures (list (registry-fixture "sibling")) check-core-storage))

  (test-case "S6 no uncompressed or stray files"
    (check-equal? (sort (map path->string (directory-list (build-path core-data "data"))) string<?)
                  (map (match-lambda [(list id _) (format "~a.rktd.gz" id)]) pinned-sha256))
    (check-false (directory-exists? (build-path core-data 'up "private"))))

  (test-case "S7 corrupt or mismatched data raises a non-contract exn:fail naming the dataset, no remedy"
    (define good (rows->rktd-bytes '((1))))
    (define bad (rows->rktd-bytes '((2))))
    (define (check-load-fails bytes rxs #:sha256 [sha256 (hex (sha256-bytes good))])
      (call-with-temporary-fixture
       (lambda (dir)
         (write-registry-fixture! dir #:info '("registry.rktd")
                                  #:registries (hash "registry.rktd" (hash "gen" (gen-entry "gen" #:sha256 sha256)))
                                  #:data (hash "gen" bytes))
         (define file-rx (regexp (regexp-quote (path->string (build-path dir "data" "gen.rktd.gz")))))
         (call-with-registry-fixtures (list dir)
           (lambda ()
             (check-exn (lambda (e) (and (exn:fail? e) (not (exn:fail:contract? e))
                                         (not (regexp-match? #rx"remedy:" (exn-message e)))
                                         (for/and ([rx (in-list (list* #rx"gen" file-rx rxs))])
                                           (regexp-match? rx (exn-message e)))))
                        (lambda () (load-dataset-table 'gen))))))))
    (check-load-fails (subbytes (gzip-bytes good) 0 12) '())
    (check-load-fails good (list #rx"cannot read dataset data"))
    (check-load-fails (gzip-bytes bad)
                      (list #rx"sha256 mismatch"
                            (regexp (string-append "expected: " (hex (sha256-bytes good))))
                            (regexp (string-append "actual: " (hex (sha256-bytes bad))))))
    (check-load-fails (gzip-bytes #"(1 2")
                      (list #rx"cannot parse dataset data" #rx"package: datasets-gen")
                      #:sha256 (hex (sha256-bytes #"(1 2")))))

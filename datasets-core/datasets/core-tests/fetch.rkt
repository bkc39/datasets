#lang racket/base
(require (for-syntax racket/base)
         (only-in datasets/core clear-dataset-cache dataset-cache-directory dataset-cached?
                  dataset-info dataset-names dataset-table-column dataset-table-names fetch-dataset
                  load-dataset-table)
         (only-in racket/file file->bytes make-directory*)
         (only-in racket/list last remove-duplicates)
         (only-in racket/match match match-define)
         (only-in racket/path path-only)
         (only-in racket/place dynamic-place place-channel-put)
         (only-in racket/runtime-path define-runtime-path)
         (only-in "fixture.rkt" cache-files call-with-artifact-url call-with-environment
                  call-with-fetch-fixture call-with-temporary-fixture fetch-entry gunzip-bytes
                  gzip-bytes hex rows->rktd-bytes table-digest toy-artifact toy-artifact-sha256
                  toy-table-digest wait-until)
         (only-in "http-fixture.rkt" call-with-http-fixture http-fixture-closed-evt
                  http-fixture-requests http-fixture-stalled-evt http-fixture-url))
(define-runtime-path fetch-place "fetch-place.rkt")
(module+ test
  (require (only-in rackunit check-equal? check-exn check-false check-not-false check-pred
                    check-regexp-match check-true test-case))
  (define final-rel (format "toy-remote/~a.rktd.gz" toy-artifact-sha256))
  (define part-rx #px"^toy-remote/[0-9a-f]{64}\\.rktd\\.gz\\.[A-Za-z0-9]+\\.part$")
  (define bad (gzip-bytes (rows->rktd-bytes '((9 "a" 0.5)))))
  (define ((fetch-error? . rxs) e)
    (and (exn:fail? e) (not (exn:fail:contract? e))
         (for/and ([rx (in-list rxs)])
           (regexp-match? (if (string? rx) (regexp (regexp-quote rx)) rx) (exn-message e)))))
  (define (with-server routes entries-of proc)
    (call-with-http-fixture routes
      (lambda (srv)
        (call-with-fetch-fixture (entries-of (lambda (p) (http-fixture-url srv p)))
          (lambda (root registry) (proc srv root registry))))))
  (define (toy-at path)
    (lambda (u) (list (fetch-entry (list (u path))))))
  (define (call-within seconds thunk)
    (define result (make-channel))
    (define worker
      (thread (lambda ()
                (channel-put result (with-handlers ([(lambda (_) #t)
                                                     (lambda (e) (lambda () (raise e)))])
                                      (define v (thunk))
                                      (lambda () v))))))
    (match (sync/timeout seconds result)
      [#f (kill-thread worker) #f]
      [outcome outcome]))

  (test-case "C0 loopback server reachable"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (check-equal? (fetch-dataset 'toy-remote) (build-path root final-rel)))))

  (test-case "C1 miss: fetch writes exactly the pinned file"
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (root registry)
            (check-false (dataset-cached? 'toy-remote))
            (check-equal? (cache-files root) '())
            (define p (fetch-dataset 'toy-remote))
            (check-equal? p (build-path root final-rel))
            (check-equal? (cache-files root) (list final-rel))
            (check-equal? (file->bytes p) toy-artifact)
            (check-true (dataset-cached? 'toy-remote))
            (check-equal? (fetch-dataset 'toy-remote) p))))))

  (test-case "C2 C3 implicit download, then hits without network"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest)
        (check-equal? (http-fixture-requests srv) '("/toy"))
        (check-equal? (cache-files root) (list final-rel))
        (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest)
        (call-with-environment (hash "DATASETS_OFFLINE" "1")
          (lambda ()
            (check-equal? (table-digest (load-dataset-table 'toy-remote #:download? #f))
                          toy-table-digest)
            (check-equal? (dataset-table-names
                           (load-dataset-table 'toy-remote #:columns '("score" "id")))
                          '("score" "id"))
            (check-equal? (fetch-dataset 'toy-remote) (build-path root final-rel))))
        (check-equal? (http-fixture-requests srv) '("/toy")))))

  (test-case "C4 mismatch names dataset, url, expected, actual; nothing cached"
    (define zeros (make-string 64 #\0))
    (for ([c (in-list
              (list (list bad (lambda (u) (fetch-entry (list u) #:size (bytes-length bad)))
                          (list "sha256 mismatch" toy-artifact-sha256 (hex (sha256-bytes bad))))
                    (list toy-artifact (lambda (u) (fetch-entry (list u) #:size 61))
                          (list "size mismatch" "61" "60"))
                    (list toy-artifact (lambda (u) (fetch-entry (list u) #:content-sha256 zeros))
                          (list "content sha256 mismatch" zeros
                                (string-append "80b5018dc4754eafe4a8105356287019"
                                               "f87c4b5c8a081bb590ef131d0e815a5c")))))])
      (match-define (list served entry-of rxs) c)
      (call-with-artifact-url served
        (lambda (url)
          (call-with-fetch-fixture (list (entry-of url))
            (lambda (root registry)
              (check-exn (apply fetch-error? #rx"^fetch-dataset: cannot download" "toy-remote" url
                                rxs)
                         (lambda () (fetch-dataset 'toy-remote)))
              (check-exn (apply fetch-error? #rx"^load-dataset-table: cannot download" "toy-remote"
                                url rxs)
                         (lambda () (load-dataset-table 'toy-remote)))
              (check-equal? (cache-files root) '())
              (check-false (dataset-cached? 'toy-remote))))))))

  (test-case "C5 C13 C23 mirrors in order, redirect followed, all-fail lists each url"
    (define routes (hash "/missing" '(status 404) "/bad" `(ok ,bad) "/good" `(ok ,toy-artifact)
                         "/r" '(redirect "/good")))
    (with-server routes
                 (lambda (u)
                   (list (fetch-entry
                          (list "ftp://127.0.0.1/x" (u "/missing") (u "/bad") (u "/r")))))
      (lambda (srv root registry)
        (check-equal? (fetch-dataset 'toy-remote) (build-path root final-rel))
        (check-equal? (http-fixture-requests srv) '("/missing" "/bad" "/r" "/good"))))
    (with-server routes
                 (lambda (u)
                   (list (fetch-entry (list "ftp://127.0.0.1/x" "127.0.0.1/toy.rktd.gz"
                                            (u "/missing") (u "/bad")))))
      (lambda (srv root registry)
        (check-exn (fetch-error? "ftp://127.0.0.1/x" "unsupported URL scheme ftp"
                                 "127.0.0.1/toy.rktd.gz" "URL has no scheme"
                                 (http-fixture-url srv "/missing") "HTTP status 404"
                                 (http-fixture-url srv "/bad") #rx"size mismatch|sha256 mismatch")
                   (lambda () (fetch-dataset 'toy-remote)))
        (check-equal? (cache-files root) '()))))

  (test-case "C6 C7 offline refusal: no request, nothing written, env beats keyword"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (check-exn (fetch-error? "toy-remote" "not cached" "#:download? #f" "fetch-dataset")
                   (lambda () (load-dataset-table 'toy-remote #:download? #f)))
        (check-exn exn:fail:contract?
                   (lambda () (load-dataset-table 'toy-remote #:columns '("absent"))))
        (for ([v (in-list '("1" "true" "TRUE" "yes" "on" "anything"))])
          (call-with-environment (hash "DATASETS_OFFLINE" v)
            (lambda ()
              (check-exn (fetch-error? "toy-remote" "not cached" "DATASETS_OFFLINE" "fetch-dataset")
                         (lambda () (load-dataset-table 'toy-remote #:download? #t)))
              (check-exn (fetch-error? "toy-remote" "DATASETS_OFFLINE")
                         (lambda () (fetch-dataset 'toy-remote)))
              (check-equal? (hash-ref (dataset-info 'toy-remote) "tier") "fetch"))))
        (check-equal? (http-fixture-requests srv) '())
        (check-equal? (cache-files root) '())
        (for ([v (in-list '("" "0" "false" "no" "off" "OFF"))] [downloads (in-naturals 1)])
          (clear-dataset-cache)
          (call-with-environment (hash "DATASETS_OFFLINE" v)
            (lambda ()
              (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest)))
          (check-equal? (http-fixture-requests srv) (build-list downloads (lambda (_) "/toy")))))))

  (test-case "C8 C11 break aborts the fetch and removes the part; idle timeout falls through"
    (with-server (hash "/stall" `(stall ,toy-artifact 30) "/good" `(ok ,toy-artifact))
                 (lambda (u) (list (fetch-entry (list (u "/stall") (u "/good")))))
      (lambda (srv root registry)
        (define result (make-channel))
        (define t (thread (lambda () (channel-put result (with-handlers ([exn:break? values])
                                                           (fetch-dataset 'toy-remote))))))
        (check-not-false (sync/timeout 10 (http-fixture-stalled-evt srv)))
        (check-true (wait-until 10 (lambda () (pair? (cache-files root)))))
        (check-regexp-match part-rx (car (cache-files root)))
        (break-thread t)
        (check-pred exn:break? (sync/timeout 10 result))
        (check-not-false (sync/timeout 10 (http-fixture-closed-evt srv)))
        (check-equal? (cache-files root) '())
        (check-equal? (http-fixture-requests srv) '("/stall"))
        (define t0 (current-inexact-milliseconds))
        (define fetched (call-within 10 (lambda () (fetch-dataset 'toy-remote #:idle-timeout 0.5))))
        (check-not-false fetched)
        (check-equal? (fetched) (build-path root final-rel))
        (check-true (< (- (current-inexact-milliseconds) t0) 10000))
        (check-not-false (sync/timeout 10 (http-fixture-closed-evt srv)))
        (check-equal? (http-fixture-requests srv) '("/stall" "/stall" "/good"))))
    (with-server (hash "/stall" `(stall ,toy-artifact 30)) (toy-at "/stall")
      (lambda (srv root registry)
        (define failed (call-within 10 (lambda () (fetch-dataset 'toy-remote #:idle-timeout 0.5))))
        (check-not-false failed)
        (check-exn (fetch-error? "no data for 0.5 seconds") failed)
        (check-equal? (cache-files root) '()))))

  (test-case "C9 C12 truncated and oversized bodies fail the url, nothing cached"
    (with-server (hash "/x" `(truncate ,toy-artifact 30)) (toy-at "/x")
      (lambda (srv root registry)
        (check-exn (fetch-error? (http-fixture-url srv "/x") #rx"size mismatch|connection")
                   (lambda () (fetch-dataset 'toy-remote)))
        (check-equal? (cache-files root) '())))
    (with-server (hash "/x" `(stall ,(bytes-append toy-artifact (make-bytes 100 0)) 160))
                 (toy-at "/x")
      (lambda (srv root registry)
        (define failed (call-within 10 (lambda () (fetch-dataset 'toy-remote))))
        (check-not-false failed)
        (check-exn (fetch-error? (http-fixture-url srv "/x")
                                 "size mismatch: expected 60 bytes, received more than 60")
                   failed)
        (check-equal? (cache-files root) '()))))

  (test-case
   "C10 stale part from a killed process: not cached, not touched by fetch, removed by clear"
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (root registry)
            (define stale (format "~a.4242.part" final-rel))
            (make-directory* (build-path root "toy-remote"))
            (call-with-output-file (build-path root stale)
              (lambda (o) (write-bytes (subbytes toy-artifact 0 30) o)))
            (check-false (dataset-cached? 'toy-remote))
            (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest)
            (check-equal? (cache-files root) (sort (list final-rel stale) string<?))
            (clear-dataset-cache)
            (check-equal? (cache-files root) '()))))))

  (test-case "C14 C15 concurrent fetchers in places and threads; a reader never sees a partial file"
    (with-server (hash "/toy" `(barrier ,toy-artifact 4)) (toy-at "/toy")
      (lambda (srv root registry)
        (define job (list (path->string registry) (path->string root)))
        (define fetchers (for/list ([_ (in-range 4)]) (dynamic-place fetch-place 'main)))
        (define reader (dynamic-place fetch-place 'main))
        (place-channel-put reader (cons 'read job))
        (for ([p (in-list fetchers)])
          (place-channel-put p (cons 'fetch job)))
        (define paths (for/list ([p (in-list fetchers)]) (sync/timeout 60 p)))
        (check-equal? (remove-duplicates paths) (list (path->string (build-path root final-rel))))
        (define outcomes (sync/timeout 60 reader))
        (check-equal? (last outcomes) toy-table-digest)
        (check-equal? (remove* (list "not-cached" toy-table-digest) outcomes) '())
        (check-equal? (cache-files root) (list final-rel))
        (check-equal? (length (http-fixture-requests srv)) 4)))
    (with-server (hash "/toy" `(barrier ,toy-artifact 8)) (toy-at "/toy")
      (lambda (srv root registry)
        (define results (for/list ([_ (in-range 8)])
                          (define ch (make-channel))
                          (thread (lambda () (channel-put ch (fetch-dataset 'toy-remote))))
                          ch))
        (check-equal? (remove-duplicates (for/list ([ch (in-list results)]) (sync/timeout 60 ch)))
                      (list (build-path root final-rel)))
        (check-equal? (cache-files root) (list final-rel)))))

  (test-case "C16 DATASETS_HOME"
    (call-with-environment (hash "DATASETS_HOME" "/srv/racket-datasets")
      (lambda () (check-equal? (dataset-cache-directory) (string->path "/srv/racket-datasets"))))
    (for ([v (in-list (list #f ""))])
      (call-with-environment (hash "DATASETS_HOME" v)
        (lambda () (check-equal? (dataset-cache-directory)
                                 (build-path (find-system-path 'cache-dir) "racket-datasets")))))
    (call-with-environment (hash "DATASETS_HOME" "relative/dir")
      (lambda ()
        (check-exn (fetch-error? "DATASETS_HOME" "relative/dir") dataset-cache-directory)
        (check-exn (fetch-error? "DATASETS_HOME") (lambda () (dataset-cached? 'iris)))))
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (a registry)
            (fetch-dataset 'toy-remote)
            (call-with-temporary-fixture
             (lambda (b)
               (call-with-environment (hash "DATASETS_HOME" (path->string b))
                 (lambda ()
                   (check-false (dataset-cached? 'toy-remote))
                   (check-equal? (fetch-dataset 'toy-remote) (build-path b final-rel))))
               (check-equal? (cache-files a) (list final-rel))
               (check-equal? (cache-files b) (list final-rel))))
            (call-with-environment (hash "DATASETS_HOME" #f)
              (lambda ()
                (check-false
                 (directory-exists? (build-path (dataset-cache-directory) "toy-remote"))))))))))

  (test-case "C17 clear removes only cache-created files"
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (root registry)
            (fetch-dataset 'toy-remote)
            (define leftover (format "gone/~a.rktd.gz" (make-string 64 #\b)))
            (define foreign
              '("README.txt" "my-stuff/data.csv" "toy-remote/notes.txt" "Upper/x.rktd.gz"))
            (for ([f (in-list (cons leftover foreign))])
              (make-directory* (path-only (build-path root f)))
              (call-with-output-file (build-path root f) (lambda (o) (write-bytes #"x" o))))
            (clear-dataset-cache 'toy-remote)
            (check-equal? (cache-files root) (sort (cons leftover foreign) string<?))
            (clear-dataset-cache 'iris)
            (clear-dataset-cache)
            (check-equal? (cache-files root) (sort foreign string<?))
            (check-true (directory-exists? root))
            (check-false (directory-exists? (build-path root "gone")))
            (check-exn #rx"unknown dataset" (lambda () (clear-dataset-cache 'absent)))
            (call-with-environment (hash "DATASETS_HOME" (path->string (build-path root "never")))
              (lambda ()
                (clear-dataset-cache)
                (check-false (directory-exists? (build-path root "never"))))))))))

  (test-case "C17 clear does not follow a linked directory out of the root"
    (call-with-fetch-fixture '()
      (lambda (root registry)
        (call-with-temporary-fixture
         (lambda (outside)
           (define kept (build-path outside (format "~a.rktd.gz" (make-string 64 #\c))))
           (call-with-output-file kept (lambda (o) (write-bytes #"x" o)))
           (make-file-or-directory-link outside (build-path root "elsewhere"))
           (clear-dataset-cache)
           (check-true (file-exists? kept))
           (check-true (link-exists? (build-path root "elsewhere"))))))))

  (test-case "C18 contract blame names this module"
    (for ([thunk (in-list (list (lambda () (fetch-dataset "toy-remote"))
                                (lambda () (fetch-dataset 'iris #:idle-timeout 0))
                                (lambda () (dataset-cached? "iris"))
                                (lambda () (clear-dataset-cache "iris"))
                                (lambda () (load-dataset-table 'iris #:download? 'yes))
                                (lambda () (load-dataset-table 'iris #:columns '()))))])
      (check-exn #rx"blaming: [^\n]*fetch\\.rkt" thunk)))

  (test-case "C19 bundled datasets are unaffected"
    (call-with-fetch-fixture '()
      (lambda (root registry)
        (call-with-environment (hash "DATASETS_OFFLINE" "1")
          (lambda ()
            (check-true (dataset-cached? 'iris))
            (check-true (file-exists? (fetch-dataset 'iris)))
            (check-equal? (load-dataset-table 'iris #:download? #f) (load-dataset-table 'iris))))
        (check-equal? (cache-files root) '()))))

  (test-case "C20 corrupt cache file raises with remedy, never re-downloads"
    (with-server (hash "/toy" `(ok ,toy-artifact)) (toy-at "/toy")
      (lambda (srv root registry)
        (define p (fetch-dataset 'toy-remote))
        (call-with-output-file p #:exists 'truncate (lambda (o) (write-bytes bad o)))
        (check-exn (fetch-error? "data sha256 mismatch" (path->string p)
                                 "(clear-dataset-cache 'toy-remote)")
                   (lambda () (load-dataset-table 'toy-remote)))
        (check-true (dataset-cached? 'toy-remote))
        (check-equal? (http-fixture-requests srv) '("/toy"))
        (clear-dataset-cache 'toy-remote)
        (check-equal? (table-digest (load-dataset-table 'toy-remote)) toy-table-digest))))

  (test-case "C21 unsupported format refused before network"
    (with-server (hash "/toy" `(ok ,toy-artifact))
                 (lambda (u) (list (fetch-entry (list (u "/toy")) #:format "zip")))
      (lambda (srv root registry)
        (check-not-false (memq 'toy-remote (dataset-names)))
        (check-exn (fetch-error? "unsupported fetch format" "zip" "toy-remote" "rktd.gz")
                   (lambda () (fetch-dataset 'toy-remote)))
        (check-exn (fetch-error? "unsupported fetch format")
                   (lambda () (load-dataset-table 'toy-remote)))
        (check-equal? (http-fixture-requests srv) '()))))

  (test-case "C22 the cache is keyed by the pinned hash"
    (define other (gzip-bytes (rows->rktd-bytes '((7 "c" 2.5) (8 "a" 3.5) (9 missing missing)))))
    (define other-sha (hex (sha256-bytes other)))
    (call-with-fetch-fixture '()
      (lambda (root _)
        (define (with-home thunk)
          (call-with-environment (hash "DATASETS_HOME" (path->string root)) thunk))
        (call-with-artifact-url toy-artifact
          (lambda (url)
            (call-with-fetch-fixture (list (fetch-entry (list url)))
              (lambda (_r _g) (with-home (lambda () (fetch-dataset 'toy-remote)))))))
        (call-with-artifact-url other
          (lambda (url)
            (call-with-fetch-fixture
             (list (fetch-entry (list url) #:sha256 other-sha #:size (bytes-length other)
                                #:content-sha256 (hex (sha256-bytes (gunzip-bytes other)))))
             (lambda (_r _g)
               (with-home
                (lambda ()
                  (check-false (dataset-cached? 'toy-remote))
                  (check-equal? (vector->list
                                 (dataset-table-column (load-dataset-table 'toy-remote) "id"))
                                '(7 8 9))))))))
        (check-equal? (cache-files root)
                      (sort (list final-rel (format "toy-remote/~a.rktd.gz" other-sha))
                            string<?)))))

  (test-case "C25 downloads are logged on topic datasets"
    (define receiver (make-log-receiver (current-logger) 'info 'datasets))
    (call-with-artifact-url toy-artifact
      (lambda (url)
        (call-with-fetch-fixture (list (fetch-entry (list url)))
          (lambda (root registry)
            (fetch-dataset 'toy-remote)
            (define msgs (let loop () (match (sync/timeout 0 receiver)
                                        [#f '()] [(vector _ m _ _) (cons m (loop))])))
            (check-true (for/or ([m (in-list msgs)])
                          (and (regexp-match? #rx"toy-remote" m)
                               (regexp-match? (regexp-quote url) m)))))))))

  (test-case "C27 queries create nothing"
    (call-with-fetch-fixture (list (fetch-entry (list "file:///nonexistent/toy.rktd.gz")))
      (lambda (root registry)
        (define sub (build-path root "sub"))
        (call-with-environment (hash "DATASETS_HOME" (path->string sub))
          (lambda ()
            (dataset-cache-directory)
            (dataset-cached? 'toy-remote)
            (dataset-info 'toy-remote)
            (dataset-names)
            (check-false (directory-exists? sub))))))))

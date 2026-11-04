#lang racket/base
(require (only-in datasets/core-private/resource with-custodian)
         (only-in file/gunzip gunzip-through-ports)
         (only-in file/untar untar)
         (only-in file/unzip read-zip-directory unzip-entry zip-directory-entries)
         (only-in racket/contract/base -> contract-out listof)
         (only-in racket/generator in-generator yield)
         (only-in racket/list filter-not)
         (only-in racket/match match)
         (only-in racket/port call-with-output-bytes copy-port))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [gunzip-bytes (-> bytes? bytes?)]
          [tar-gz-member (-> path-string? string? bytes?)]
          [tar-gz-members (-> path-string? (listof string?) (listof bytes?))]
          [zip-members (-> path-string? (listof string?) (listof bytes?))]))

(define pipe-bytes (* 1024 1024))

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define (gunzip-bytes data)
  (call-with-output-bytes (lambda (out) (gunzip-through-ports (open-input-bytes data) out))))

(define (call-with-gunzip-port path proc)
  (with-custodian
    (define-values (in out) (make-pipe pipe-bytes))
    (define failure (box #f))
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (set-box! failure e))])
                (call-with-input-file path (lambda (file) (gunzip-through-ports file out))))
              (close-output-port out)))
    (define (gunzip-failure) (unbox failure))
    (with-handlers ([exn:fail? (lambda (e) (raise (or (gunzip-failure) e)))])
      (proc in gunzip-failure))))

(define (member-error who message archive member)
  (make-exn:fail (format "~a: ~a\n  archive: ~a\n  member: ~a" who message archive member)
                 (current-continuation-marks)))

(define (tar-gz-member archive member)
  (match (tar-gz-members archive (list member))
    [(list data) data]))

(define (read-member archive path in size gunzip-failure)
  (define data (read-bytes size in))
  (cond
    [(and (bytes? data) (= (bytes-length data) size)) data]
    [else (raise (or (gunzip-failure) (member-error 'tar-gz-member "truncated member" archive path)))]))

(define (tar-gz-members archive names)
  (define wanted (for/hash ([name (in-list names)]) (values name #t)))
  (call-with-gunzip-port
   archive
   (lambda (in gunzip-failure)
     (define members
       (in-generator
        #:arity 2
        (untar in
               #:filter (lambda (name _stripped type _size _link _time _mode)
                          (and (eq? type 'file) (hash-has-key? wanted (path->string name))))
               #:handle-entry (lambda (_kind path in size _attribs)
                                (yield (path->string path) (read-member archive path in size gunzip-failure))
                                '()))))
     (define found
       (for/fold ([found (hash)] [missing wanted] #:result found)
                 ([(name data) members]
                  #:when (hash-has-key? missing name)
                  #:final (= (hash-count missing) 1))
         (values (hash-set found name data) (hash-remove missing name))))
     (for/list ([name (in-list names)])
       (hash-ref found name
                 (lambda () (raise (or (gunzip-failure) (member-error 'tar-gz-member "no such member" archive name)))))))))

(define (zip-members archive names)
  (define directory (read-zip-directory archive))
  (define entries (zip-directory-entries directory))
  (match (filter-not (lambda (name) (member (string->bytes/utf-8 name) entries)) names)
    [(cons name _) (raise (member-error 'zip-members "no such member" archive name))]
    ['()
     (for/list ([name (in-list names)])
       (call-with-output-bytes
        (lambda (out)
          (unzip-entry archive directory (string->bytes/utf-8 name)
                       (lambda (_name _dir? in) (copy-port in out))))))]))

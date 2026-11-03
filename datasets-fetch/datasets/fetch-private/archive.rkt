#lang racket/base
(require (only-in file/gunzip gunzip-through-ports)
         (only-in file/untar untar)
         (only-in file/unzip read-zip-directory unzip-entry zip-directory-entries)
         (only-in racket/contract/base -> contract-out listof)
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
  (define-values (in out) (make-pipe pipe-bytes))
  (define failure (box #f))
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (set-box! failure e))])
                (call-with-input-file path (lambda (file) (gunzip-through-ports file out))))
              (close-output-port out))))
  (dynamic-wind
   void
   (lambda () (with-handlers ([exn:fail? (lambda (e) (raise (or (unbox failure) e)))]) (proc in)))
   (lambda () (custodian-shutdown-all custodian))))

(define (missing-member who archive member)
  (error who "no such member\n  archive: ~a\n  member: ~a" archive member))

(define (tar-gz-member archive member)
  (car (tar-gz-members archive (list member))))

(define (tar-gz-members archive names)
  (define wanted (for/hash ([name (in-list names)]) (values name #t)))
  (define found (make-hash))
  (let/ec done
    (call-with-gunzip-port
     archive
     (lambda (in)
       (untar in
              #:filter (lambda (name _stripped type _size _link _time _mode)
                         (and (eq? type 'file) (hash-has-key? wanted (path->string name))
                              (not (hash-has-key? found (path->string name)))))
              #:handle-entry (lambda (_kind path in size _attribs)
                               (hash-set! found (path->string path) (read-bytes size in))
                               (cond
                                 [(= (hash-count found) (hash-count wanted)) (done)]
                                 [else '()]))))))
  (for/list ([name (in-list names)])
    (hash-ref found name (lambda () (missing-member 'tar-gz-member archive name)))))

(define (zip-members archive names)
  (define directory (read-zip-directory archive))
  (define entries (zip-directory-entries directory))
  (define requested (map string->bytes/utf-8 names))
  (for ([name (in-list names)] [entry (in-list requested)] #:unless (member entry entries))
    (missing-member 'zip-members archive name))
  (for/list ([entry (in-list requested)])
    (call-with-output-bytes
     (lambda (out) (unzip-entry archive directory entry (lambda (_name _dir? in) (copy-port in out)))))))

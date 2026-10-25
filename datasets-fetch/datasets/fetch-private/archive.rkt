#lang racket/base
(require (only-in file/gunzip gunzip-through-ports)
         (only-in file/untar untar)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/port call-with-output-bytes)
         (only-in threading ~>))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [gzip-content-size (-> path-string? exact-nonnegative-integer?)]
          [tar-gz-member (-> path-string? string? bytes?)]))

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define gzip-size-field-bytes 4)

(define (gzip-content-size path)
  (call-with-input-file path
    (lambda (in)
      (file-position in (- (file-size path) gzip-size-field-bytes))
      (integer-bytes->integer (read-bytes gzip-size-field-bytes in) #f #f))))

(define (tar-gz-member archive member)
  (let/ec return
    (~> archive
        gunzip-file
        open-input-bytes
        (untar #:filter (lambda (name _stripped type _size _link _time _mode)
                          (and (eq? type 'file) (equal? (path->string name) member)))
               #:handle-entry (lambda (_kind _path in size _attribs) (return (read-bytes size in)))))
    (error 'tar-gz-member "no such member\n  archive: ~a\n  member: ~a" archive member)))

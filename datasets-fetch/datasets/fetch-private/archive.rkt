#lang racket/base
(require (only-in file/gunzip gunzip-through-ports)
         (only-in file/untar untar)
         (only-in racket/contract/base -> contract-out)
         (only-in racket/port call-with-output-bytes))
(provide (contract-out
          [gunzip-file (-> path-string? bytes?)]
          [tar-gz-member (-> path-string? string? bytes?)]))

(define (gunzip-file path)
  (call-with-input-file path
    (lambda (in) (call-with-output-bytes (lambda (out) (gunzip-through-ports in out))))))

(define (tar-gz-member archive member)
  (let/ec return
    (untar (open-input-bytes (gunzip-file archive))
           #:filter (lambda (name _stripped type _size _link _time _mode)
                      (and (eq? type 'file) (equal? (path->string name) member)))
           #:handle-entry (lambda (_kind _path in size _attribs) (return (read-bytes size in))))
    (raise (exn:fail (format "tar-gz-member: no such member\n  archive: ~a\n  member: ~a" archive member)
                     (current-continuation-marks)))))

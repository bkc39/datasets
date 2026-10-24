#lang racket/base
(require (only-in racket/contract/base -> contract-out list/c listof)
         (only-in racket/list append* last)
         (only-in racket/match match match*))
(provide (contract-out [catalog-column-rows
                        (-> (listof hash?) (listof (list/c string? string? string? string?)))]))

(define (indexed name)
  (match (regexp-match #px"^(.*?)([0-9]+)([^0-9]*)$" name)
    [(list _ prefix index suffix) (list prefix (string->number index) suffix)]
    [#f #f]))

(define (follows? previous name)
  (match* ((indexed previous) (indexed name))
    [((list prefix i suffix) (list prefix j suffix)) (= j (add1 i))]
    [(_ _) #f]))

(define (own-description column)
  (define description (hash-ref column "description"))
  (and (not (equal? description (hash-ref column "original-name"))) description))

(define (run-key column)
  (match (indexed (hash-ref column "name"))
    [(list prefix _ suffix)
     (list prefix suffix (hash-ref column "type") (hash-ref column "unit") (hash-ref column "levels")
           (own-description column))]
    [#f #f]))

(define (continues? run column)
  (define previous (car run))
  (and (run-key column)
       (equal? (run-key column) (run-key previous))
       (follows? (hash-ref previous "name") (hash-ref column "name"))
       (follows? (hash-ref previous "original-name") (hash-ref column "original-name"))))

(define (runs columns)
  (for/fold ([runs '()] #:result (reverse (map reverse runs))) ([column (in-list columns)])
    (match runs
      [(cons run more) #:when (continues? run column) (cons (cons column run) more)]
      [_ (cons (list column) runs)])))

(define (details column description)
  (format "~a; ~a; ~a" description (hash-ref column "unit") (hash-ref column "levels")))

(define (column-row column)
  (list (hash-ref column "name") (hash-ref column "original-name") (hash-ref column "type")
        (details column (hash-ref column "description"))))

(define (range-row run)
  (define (span key) (format "~a … ~a" (hash-ref (car run) key) (hash-ref (last run) key)))
  (list (span "name") (span "original-name") (hash-ref (car run) "type")
        (details (car run) (or (own-description (car run)) (span "original-name")))))

(define (catalog-column-rows columns)
  (append* (for/list ([run (in-list (runs columns))])
             (if (>= (length run) 3) (list (range-row run)) (map column-row run)))))

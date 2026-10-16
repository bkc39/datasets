#lang racket/base
(require (for-syntax racket/base (only-in syntax/parse ...+ expr id))
         (only-in syntax/parse/define define-syntax-parse-rule))
(provide with-custodian with-resource)

(define-syntax-parse-rule (with-resource [name:id acquire:expr release:expr] body:expr ...+)
  (let ([breaks? (break-enabled)])
    (parameterize-break #f
      (let ([name acquire])
        (dynamic-wind void
                      (lambda () (parameterize-break breaks? body ...))
                      (lambda () release))))))

(define-syntax-parse-rule (with-custodian body:expr ...+)
  (with-resource [custodian (make-custodian) (custodian-shutdown-all custodian)]
    (parameterize ([current-custodian custodian])
      body ...)))

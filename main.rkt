#!/usr/bin/env racket
#lang racket

(require racket/string
         json
         net/http-client
         threading)

(define (get-request-body host uri)
  (let-values ([(status headers in) (http-sendrecv host uri)])
    (read-json in)))

(define (fetch-jokes host uri)
  (let ([body (get-request-body host uri)])
    (map (λ (item) (hash-ref item 'joke))
         (hash-ref body 'value))))

(define (word-counts lines)
  (let ([words (make-hash)])
    (for* ([joke lines]
           [word (string-split (string-downcase (string-replace joke #px"\\P{^P}" "")))]
           #:when (> (string-length word) 3))
      (let ([key (string->keyword word)])
        (if (hash-has-key? words key)
            (hash-set! words key (+ 1 (hash-ref words key)))
            (hash-set! words key 1))))
    (sort (hash->list words)
          > #:key cdr)))

(define (display-word-counts wc)
  (for ([item wc]
        [index (in-range (length wc))])
    (printf "~a: ~a~n"
            (+ 1 index)
            (keyword->string (car item)))))

;; Find out 10 most frequently used words found in Chuck Norris jokes
(~> (fetch-jokes "api.icndb.com" "/jokes")
    (word-counts)
    (take 10)
    (display-word-counts))

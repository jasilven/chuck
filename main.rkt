#!/usr/bin/env racket
#lang racket

(require racket/string
         json
         net/http-client
         threading
         plot
         net/url)

(define (get-request-body host path)
  (let-values ([(status headers in) (http-sendrecv host path)])
    (read-json in)))

(define (fetch-jokes from)
  (let* ([url (string->url from)]
         [body (get-request-body (url-host url) (path->string (url->path url)))])
    (map (λ (item) (hash-ref item 'joke))
         (hash-ref body 'value))))

(define (word-counts jokes)
  (let ([words (make-hash)])
    (for* ([joke jokes]
           [word (string-split (string-downcase (string-replace joke #px"\\P{^P}" "")))]
           #:when (> (string-length word) 3))
      (let ([key (string->keyword word)])
        (if (hash-has-key? words key)
            (hash-set! words key (+ 1 (hash-ref words key)))
            (hash-set! words key 1))))
    (sort (words-hash->list words)
          > #:key cdr)))

(define (words-hash->list words)
  (map (λ (item)
         (cons (keyword->string (car item)) (cdr item)))
       (hash->list words)))

(define (display-word-counts wc)
  (for ([item wc]
        [index (in-range (length wc))])
    (printf "~a: ~a~n" (+ 1 index) (car item)))
  wc)

(define (pair->vector p)
  (vector (car p) (cdr p)))

(define (display-word-counts-histogram wc)
  (let ([vals (map (λ (item) (pair->vector item)) wc)])
    (parameterize ([plot-width 900]
                   [plot-x-label "words"]
                   [plot-y-label "word count"])
      (plot (list (discrete-histogram vals))))))

;; Show 10 most frequently used words found in Chuck Norris jokes
(~> (fetch-jokes "http://api.icndb.com/jokes")
    (word-counts)
    (take 10)
    (display-word-counts)
    (display-word-counts-histogram))

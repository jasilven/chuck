#!/usr/bin/env racket
#lang racket

(require racket/string
         json
         net/http-client
         threading
         plot
         net/url)

(define (get-json-data host path)
  (let-values ([(status headers in) (http-sendrecv host path)])
    (read-json in)))

(define (fetch-jokes from)
  (let* ([url (string->url from)]
         [path (path->string (url->path url))]
         [json (get-json-data (url-host url) path)])
    (map (λ (j) (hash-ref j 'joke))
         (hash-ref json 'value))))

(define (strip-punctuation astr)
  (string-replace astr #px"\\P{^P}" ""))

(define (word-frequencies jokes #:sort do-sort)
  (define words (make-hash))
  (define (words->list words)
    (map (λ (w) (cons (keyword->string (car w)) (cdr w)))
         (hash->list words)))
  (for* ([joke (map (λ (j) (string-downcase j)) jokes)]
         [word (string-split (strip-punctuation joke))]
         #:when (> (string-length word) 3))
    (hash-update! words (string->keyword word) add1 0))
  (if do-sort
      (sort (words->list words) > #:key cdr)
      (words->list words)))

(define (word-frequencies-histogram freq #:width w)
  (define (pair->vector p) (vector (car p) (cdr p)))
  (define vals (map (λ (item) (pair->vector item)) freq))
  (parameterize ([plot-width w]
                 [plot-new-window? #t]
                 [plot-x-label "words"]
                 [plot-y-label "word count"])
    (plot (list (discrete-histogram vals)))))

;; Show 10 most frequently used words found in Chuck Norris jokes
(define (main)
  (~> (fetch-jokes "http://api.icndb.com/jokes")
      (word-frequencies #:sort #t)
      (take 10)
      (word-frequencies-histogram #:width 900)))

(main)

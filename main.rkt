#!/usr/bin/env racket
#lang racket

(require json
         net/url
         plot)

(define (fetch-jokes url)
  (define jokes (call/input-url (string->url url)
                                get-pure-port
                                read-json))
  (for/list ([item (in-list (hash-ref jokes 'value))])
    (hash-ref item 'joke)))

(define (strip-puncts str)
  (string-replace str #px"\\P{^P}" ""))

(define (word-frequencies jokes #:sort sort?)
  (define words (make-hash))
  (for* ([joke (in-list jokes)]
         [word (string-split (strip-puncts joke))]
         #:when (> (string-length word) 3))
    (hash-update! words (string-downcase word) add1 0))
  (if sort?
      (sort (hash->list words) > #:key cdr)
      (hash->list words)))

(define (word-frequencies-histogram freq #:width w)
  (define vals (for/list ([pair (in-list freq)])
                 (vector (car pair) (cdr pair))))
  (parameterize ([plot-width w]
                 [plot-new-window? #t]
                 [plot-x-label "words"]
                 [plot-y-label "word count"])
    (plot (list (discrete-histogram vals)))))

;; Show 10 most frequently used words found in Chuck Norris jokes
(define (main)
  (define jokes (fetch-jokes "http://api.icndb.com/jokes"))
  (define freq (take (word-frequencies jokes #:sort #t)
                     10))
  (word-frequencies-histogram freq #:width 900))

(main)

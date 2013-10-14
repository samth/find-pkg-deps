#lang racket

(require pkg/lib setup/getinfo)

(define (get-deps p #:local [local #t])
  (cond [local
         (extract-pkg-dependencies (get-info/full (pkg-directory p)))]
        [else
         (define-values (a b deps)
           (get-pkg-content (pkg-desc p #f #f #f #f)))
         deps]))

(define (go local ps)
  (define table (make-hash))
  (define (find-all ps)
    (remove-duplicates (apply append (map find ps))))
  (define (find s)
    (define r (hash-ref table s #f))
    (cond [(list? r) r]
          [r null] ;; cycle handling
          [else 
           (define deps (get-deps s #:local local))
           (hash-set! table s #t)
           (define l
             (for/list ([i (filter (or/c symbol? string?) deps)])
                       (cons i (find i))))
           (hash-set! table s (remove-duplicates (apply append l)))
           (hash-ref table s)]))
  (find-all ps))

(module+ main
         (define local #t)
         (command-line
          #:once-any
          ["--local" "local only" (set! local #t)]
          ["--net" "use remote pkg server" (set! local #f)]
          #:args ps (pretty-print (sort (go local ps)  string<?))))
  

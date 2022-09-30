#lang at-exp racket/base

(provide pict->png!
         pict->svg!
         pict->pdf!)

(require pict
         file/convertible)

(define (pict->png! pict path)
  (void
   (call-with-output-file path
     #:exists 'replace
     (λ (out) (write-bytes (convert pict 'png-bytes) out)))))

(define (pict->svg! pict path)
  (void
   (call-with-output-file path
     #:exists 'replace
     (λ (out) (write-bytes (convert pict 'svg-bytes) out)))))

(define (pict->pdf! pict path)
  (void
   (call-with-output-file path
     #:exists 'replace
     (λ (out) (write-bytes (convert pict 'pdf-bytes) out)))))

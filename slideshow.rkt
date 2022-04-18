#lang at-exp slideshow

(provide slide*
         slide+
         app/lines
         title-slide)

(define (app/lines f . texts)
  (apply f (filter (λ (x) (not (equal? x "\n"))) texts)))
(define appl app/lines)

(define ((slide* [title #f] [text->pict t]) . texts)
  (define text-picts (map (λ (x) (if (string? x) (text->pict x) x)) texts))
  (if title
      (apply slide #:title title text-picts)
      (apply slide text-picts)))

(define (slide+ #:title [title #f]
                #:text->pict [text->pict item]
                . texts)
  (define text-picts
    (filter-map (match-lambda ["\n" #f]
                              [(? string? s) (text->pict s)]
                              [other other])
                texts))
  (if title
      (apply slide #:title title text-picts)
      (apply slide text-picts)))

(define (title-slide #:title title-str
                     #:subtitle [subtitle-str ""]
                     #:authors [authors '()]
                     #:icon-left [icon-left (blank 0)]
                     #:icon-right [icon-right (blank 0)]
                     #:icon-offset [icon-offset 500])
  (define title
    (match title-str
      [(? string?)
       (parameterize ([current-font-size (floor (* 2 (current-font-size)))])
         (t title-str))]
      [other other]))
  (define sub-title (parameterize ([current-font-size (floor (* 3/2 (current-font-size)))])
                      (t subtitle-str)))
  (define icons (hc-append icon-offset
                           icon-left
                           icon-right))
  (slide title
         sub-title
         (apply vc-append 5 (map t authors))
         (inset icons
                0 0 0 (- (pict-height icons)))))

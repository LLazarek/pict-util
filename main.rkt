#lang at-exp racket/base

(provide (all-defined-out))

(require pict
         pict/code
         racket/contract/base
         racket/contract/region
         racket/list
         racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define (line #:text->pict [string->pict text]
              . things)
  (define thing-picts (map (match-lambda [(? string? x) (string->pict x)]
                                         [other other])
                           things))
  (apply hc-append thing-picts))

(define-simple-macro (code+results stx results)
  (list (code stx)
        @text{⇓}
        results))

(define/match (code/images code-list #:spacing [spacing 30])
  [{(? pict? p) _} p]
  [{(list elts ...) _}
   (define elt-images (map code/images elts))
   (apply combine/scale-height
          #:spacing spacing
          #:before (inset/clip (code ()) 0 0 -20 0)
          #:after (inset/clip (code ()) -20 0 0 0)
          elt-images)]
  [{datum _} (typeset-code (datum->syntax #f datum #f))])

(define (blank-pad p
                   #:left [left 0]
                   #:right [right 0]
                   #:above [above 0]
                   #:below [below 0])
  (vl-append
   (blank 1 above)
   (hc-append (blank left 1) p (blank right 1))
   (blank 1 below)))

(begin-for-syntax
  (require syntax/parse
           racket/list)
  (define-splicing-syntax-class line-spec
    #:commit
    #:attributes [from find-from to find-to opts
                       (opt 1) (val 1)]
    [pattern {~seq from:expr find-from:expr to:expr find-to:expr
                   {~seq opt:keyword val:expr} ...}
             #:with opts #'({~? {~@ opt val}} ...)])
  (define (split-opts line-opts-stx)
    (syntax-parse line-opts-stx
      [({~alt {~seq {~and other-kw:keyword
                          {~not {~or #:head #:width}}}
                    other-val:expr}
              {~seq #:head head:expr}
              {~seq #:width width:expr}} ...)
       (define head-specs (attribute head))
       (define width-specs (attribute width))
       (values (if (empty? head-specs) #f (first head-specs))
               (if (empty? width-specs) #f (first width-specs))
               (flatten (map list
                             (attribute other-kw)
                             (attribute other-val))))])))
(define-syntax-parser pin-arrow-lines
  [(_ #:head head:expr
      #:width width:expr
      base)
   #'base]
  [(_ #:head head:expr
      #:width width:expr
      base line:line-spec more:line-spec ...)
   #:do [(define-values (maybe-head-spec
                         maybe-width-spec
                         other-opts)
           (split-opts #'line.opts))]
   #`(let ([head-val head]
           [width-val width])
       (pin-arrow-lines #:head head-val #:width width-val
                        (pin-arrow-line #,(if maybe-head-spec
                                              maybe-head-spec
                                              #'head-val)
                                        #:line-width
                                        #,@(if maybe-width-spec
                                               (list maybe-width-spec)
                                               #'(width-val))
                                        base
                                        #,@other-opts
                                        line.from line.find-from
                                        line.to line.find-to)
                        {~@ more.from
                            more.find-from
                            more.to
                            more.find-to
                            {~@ more.opt more.val} ...}
                        ...))])

(define-syntax-parser pin-lines
  [(_ #:width width:expr
      base)
   #'base]
  [(_ #:width width:expr
      base line:line-spec more:line-spec ...)
   #:do [(define-values (maybe-head-spec
                         maybe-width-spec
                         other-opts)
           (split-opts #'line.opts))]
   #`(let ([width-val width])
       (pin-lines #:width width-val
                  (pin-line #:line-width
                            #,@(if maybe-width-spec
                                   (list maybe-width-spec)
                                   #'(width-val))
                            base
                            #,@other-opts
                            line.from line.find-from
                            line.to line.find-to)
                  {~@ more.from
                      more.find-from
                      more.to
                      more.find-to
                      {~@ more.opt more.val} ...}
                  ...))])

(define finder? any/c) ;; see `pin-over`
(define/contract (pin-over* base . pin-list)
  (->* [pict?]
       #:rest (flat-rec-contract
               pin-triple/c
               (or/c empty?
                     (cons/c pict-path?
                             (cons/c finder?
                                     (cons/c pict?
                                             pin-triple/c)))
                     (cons/c real?
                             (cons/c real?
                                     (cons/c pict?
                                             pin-triple/c)))))
       pict?)
  (match pin-list
    ['() base]
    [(list-rest find-pict/dx find/dy pict pin-list-rest)
     (apply pin-over*
            (pin-over base find-pict/dx find/dy pict)
            pin-list-rest)]))

(define (underline p color
                   #:thickness [thickness (floor (* 1/10 (pict-height p)))]
                   #:offset [offset 0])
  (define w (pict-width p))
  (vc-append offset
             p
             (colorize (filled-rectangle w thickness) color)))

(define ((scale* target-height) p)
  (scale p (/ target-height (pict-height p))))

(define (combine/scale-height #:spacing [spacing 30]
                              #:before [before #f]
                              #:after [after #f]
                              #:combine [combine hc-append]
                              #:pick-height [pick-height max]
                              . picts)
  (define target-height (apply pick-height (map pict-height picts)))
  (define scale** (scale* target-height))
  (define picts/scaled (map scale** picts))
  (define appended (apply combine spacing picts/scaled))
  (define appended+before (if before
                              (combine (scale** before) appended)
                              appended))
  (define appended+before+after (if after
                                    (combine appended+before (scale** after))
                                    appended+before))
  appended+before+after)

(define (make-multi p offset #:count [count 3])
  (apply
   lt-superimpose
   (build-list count
               (λ (n)
                 (blank-pad p
                            #:left (* n offset)
                            #:above (* n offset))))))

(define (copy p)
  (scale p 1))

(define (offset-find find dx dy)
  (λ (base inner)
    (define-values {x y} (find base inner))
    (values (+ x dx) (+ y dy))))

(define (table/fill-missing list-of-picts
                            #:columns picts-per-row
                            #:column-spacing col-space
                            #:row-spacing row-space
                            #:filler [filler (blank 0)])
  (let append-next-row ([the-pict (blank 0)]
                        [remaining-picts list-of-picts]
                        [remaining-count (length list-of-picts)])
    (define-values {row-parts rest}
      (if (< remaining-count picts-per-row)
          (values (append remaining-picts
                          (make-list (- picts-per-row
                                        remaining-count)
                                     filler))
                  empty)
          (split-at remaining-picts picts-per-row)))
    (define row (apply hc-append col-space row-parts))
    (define the-pict+row (vl-append row-space the-pict row))
    (if (empty? rest)
        the-pict+row
        (append-next-row the-pict+row
                         rest
                         (- remaining-count picts-per-row)))))

(define (fill-background pict
                         #:color [color "white"]
                         #:rounded? [rounded? #f]
                         #:background
                         [bg-pict ((if rounded?
                                       filled-rounded-rectangle
                                       filled-rectangle)
                                   (pict-width pict)
                                   (pict-height pict)
                                   #:color color
                                   #:draw-border? #f)])
  (define combined
    (cc-superimpose bg-pict
                    pict))
  (clip (refocus combined pict)))

(define/contract (add-multiline-labels picts
                                       labels
                                       label->lines
                                       #:spacing [label-gap 10]
                                       #:style [style null]
                                       #:size [size 12])
  ({(listof pict?) (listof any/c) (any/c . -> . (listof string?))}
   {#:spacing real?
    #:style any/c
    #:size integer?}
   . ->* .
   (listof pict?))

  (define (text* str) (text str style size))
  (define label-picts
    (for/list ([label (in-list labels)])
      (apply vc-append
             (map text* (label->lines label)))))
  (define uniform-label-filler (ghost (foldl cc-superimpose (blank 0) label-picts)))
  (for/list ([pict (in-list picts)]
             [label-pict (in-list label-picts)])
    (vc-append label-gap
               (cb-superimpose label-pict uniform-label-filler)
               pict)))

#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define (main)
  (with-continuation-mark 'key 'mark-main
    (if #t
      (with-continuation-mark 'key 'mark-if
        (extract-current-continuation-marks 'key))
        #f)))

(displayln (main))

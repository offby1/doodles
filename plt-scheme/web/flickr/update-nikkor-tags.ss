#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui update-nikkor-tags-tests 'verbose))"
|#
(module update-nikkor-tags mzscheme
(require (only (lib "1.ss" "srfi") iota)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         "flickr.ss")

;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(define *my-NSID*
  "20825469@N00"
;;; (car ((sxpath '(user @ nsid *text*))
;;;                         (flickr.people.findByUsername
;;;                          'username "offby1")))
  )

(define (all-pix)
  (let loop ((pix-remaining-to-snarf #f)
             (pages-requested 0)
             (rv '()))
    (cond
     ((not pix-remaining-to-snarf)
      (let* ((one-batch (search-stub #:page (add1 pages-requested)))
             (total (string->number (car ((sxpath '(photos @ total *text*)) one-batch)))))
        (loop (- total (string->number (car ((sxpath '(photos @ perpage *text*)) one-batch))))
              (add1 pages-requested)
              (cons one-batch rv))))
     ((zero? pix-remaining-to-snarf)
      rv)
     ((positive? pix-remaining-to-snarf)
      (loop pix-remaining-to-snarf
            (add1 pages-requested)
            (cons (search-stub #:page (add1 pages-requested)) rv)))
     )))

;; a quick stub that acts vaguely like flickr.photos.search
(define/kw (search-stub #:key
                        [page 1]
                        [per_page 3])
  (let* (
         ;; completely arbitrary
         (total 10)
         (num-on-last-page (remainder total per_page))
         (pages (+ (quotient total per_page)
                   (if (zero? num-on-last-page)
                       0
                       1))))

    ;; Yeah, this is ugly.  Do better.
    (when (zero? num-on-last-page)
      (set! num-on-last-page per_page))

    (cons '*TOP*
          (cons 'photos
                (cons
                 `(@ (total ,total)
                     (perpage ,per_page)
                     (pages ,pages)
                     (page ,page))
                 (map
                  (lambda (n)
                    `(photo
                      (@ (title Yours Truly)
                         (server 2305)
                         (secret c8c4e9bf53)
                         (owner 20825469@N00)
                         (ispublic 1)
                         (isfriend 0)
                         (isfamily 0)
                         (id ,n)
                         (farm 3))))
                  (iota per_page (* (sub1 page) per_page))))))))

(let ((all-my-photos
       (
        search-stub
        #:page      1
        )))
  (printf "Golly, here's all my photos:~%")
  (pretty-display (all-pix)))


(define update-nikkor-tags-tests

  (test-suite
   "update-nikkor-tags"
))

(provide (all-defined))
)

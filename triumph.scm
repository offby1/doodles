;; This code solves a problem I had long ago, when I worked for a
;; small software company.

;; The problem was putting our product onto floppy disks for
;; distribution to customers.  The product consisted of about 3000
;; files of various sizes, adding up to about ten megabytes.  We
;; wanted to distribute the product on the fewest floppies, to save
;; money.  It was easy to figure out how many floppies we'd need for
;; one copy of the product: just take the total size of all the files,
;; divide by the capacity of one floppy, and round up to the next
;; largest integer.

;; The real problem was that there'd be some unused space on each
;; floppy, and I wanted that unused space distributed as evenly as
;; possible over all the floppies (so that if we needed to add another
;; file at the last minute, we'd be able to put it on any floppy we
;; liked, since each would have some free space).  I thought for a
;; while, and decided that to "load" the floppies evenly, I'd sort a
;; list of all the files, in decreasing order by their size, and then
;; I'd take the top item on the list and add it to the emptiest
;; floppy.  So I wanted a computer program that would read the list of
;; items, and return a list of the floppies, each containing a list of
;; the items that were to be stored on it.

;; I wrote some Emacs Lisp code to do this.  As I recall, it took me a
;; few hours to write it.  And now I've written it again, this time in
;; Scheme; it took me a few hours to write this, too.

;; Instead of dealing with "floppies" and "files", this code deals
;; with "containers" and "items".  Same difference, though.

;; An "item" is either a number, or a two-element list, the first of
;; which is a symbol (which could be the name of the file), and the
;; second of which is a number (which could be the size of the file).
;; Perhaps I should use dotted pairs instead of lists.

;; Given a bunch of items, and a bunch of containers, put the items
;; into the containers in such a way as to  fill the containers
;; evenly.

;; Here's what a bunch of items might look like:

;;	'((bob 200) (fred 75) (sally 12) (znorf 66) 44)

;; That is, a bunch of items is a list; each element of the list is
;; either a number, or a two-element list, whose car is a symbol and
;; whos cadr is a number.  The number represents the "size" of the
;; item; the symbol represents its name.  A number by itself is an
;; item without a name.

;; A bunch of containers is just a list of symbols.  This code will
;; return a list of lists; one list for each symbol.  Each such list
;; will have as its car the symbol, and as its remaining elements,
;; some items.  For example, given the items above, and the list

;;	'(container-1 container-2)

;; we'd get back

;;	'((container-1 (bob 200)) (container-2 (fred 75) (znorf 66) 44 (sally 12)))

;; Each item will appear on exactly one of the returned lists.

(require 'sort)
(require 'random)

;; normally-distributed random number, or at least something that
;; vaguely looks like one.

(define normal
  (let ((pi  (* 4 (atan 1))))
    (lambda ()
      (tan (* pi (- (random 1.0) 1/2))))))

(define (prompted-read prompt)
  (display prompt)
  (read))

(define (interactive)
  (distribute-evenly
   (prompted-read "Enter an unquoted list of items: ")
   (prompted-read "Enter an unquoted list of container names: ")))

;; This simply builds a list of containers from the list of symbols,
;; then modifies that list of containers by calling
;; evenly-distribute!, then returns the modified list.
(define (distribute-evenly items containers)
  (evenly-distribute!
   (sort items
	 (lambda (item1 item2)
	   (> (item:weight item1)
	      (item:weight item2))))
   (map list containers)))


;; This does the real work.  It modifies each list in `containers' by
;; adding items to it.

(define (evenly-distribute! item-list containers)
  (cond
   ((null? item-list)
    containers)
   (else
    (container:add-item! (container:emptiest containers)
			 (car item-list))
    (evenly-distribute! (cdr item-list)
			containers))))

(define (container:emptiest containers)
  (if (= (length containers) 1)
      (car containers)
    (let ((cdr-emptiest (container:emptiest (cdr containers))))
      (cond
       ((<
	 (container:weight (car containers))
	 (container:weight cdr-emptiest))
	(car containers))
       (else
	cdr-emptiest)))))

(define (container:weight container)
  (apply + (map item:weight (cdr container))))

(define (container:add-item! container item)
  (set-cdr! container (cons item (cdr container)))
  container)

(define (item:weight item)
  (cond
   ((number? item)
    item)
   (else
    (cadr item))))

(define (container:simpler-equivalent cont)
  (if (null? cont)
      '()
    (list (car cont)
	  (container:weight cont))))

;; Use this to examine the output from distribute-evenly, to see just
;; how evenly things got distributed.

(define (container-list:weights lis)
  (if (null? lis)
      '()
    (cons (container:simpler-equivalent (car lis))
	  (container-list:weights (cdr lis)))))

(require 'pretty-print)

(pretty-print (distribute-evenly
 ;; This generates some input data.
 (let loop ((count 100))
   (if (= count 0)
       '()
     (cons (inexact->exact (round (expt (* 10 (normal)) 2)))
	   (loop (- count 1)))))
 '(one two three)))

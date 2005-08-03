#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module network mzscheme
  (print-struct #t)
  (require
   (lib "setf.ss" "swindle")
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (planet "util.ss"    ("schematics" "schemeunit.plt" 1)))

  (provide
   put-node!
   get-node-by-name
   link-nodes!
   new-network
   new-node
   get-neighbor-names
   network->list
   node-name
   node->string
   )

  (define-struct node (name data neighbors) #f)
  (define (get-neighbor-names n)
    (hash-table-map (node-neighbors n) (lambda (k v) (node-name k))))
  (define (node->string n)
    (let ((v (struct->vector n)))
      (format "Name:~s Data:~s Neighbors:~s"
              (vector-ref v 1)
              (vector-ref v 2)
              (get-neighbor-names n))))

  (define (new-node name data)
    (make-node name data (make-hash-table)))
  (define-struct network (ht) #f)
  
  ;; overwrites any node that might already exist under the given name
  (define (put-node! net node)
    (hash-table-put! (network-ht net) (node-name node) node))

  (define (get-node-by-name net name)
    (hash-table-get (network-ht net) name (lambda () #f)))

  (define (link-nodes! n1 n2)
    
    (hash-table-put! (node-neighbors n1) n2 #t)
    (hash-table-put! (node-neighbors n2) n1 #t)
    )

  (define (new-network) (make-network (make-hash-table 'equal)))

  (define (network->list nw)
    (hash-table-map (network-ht nw) cons))

  (test/text-ui
   (let ((nw (new-network))
         (nd (new-node "fred" 0)))
      
     (make-test-suite
      "Network tests."
    
      (make-test-case
       "Creation"
     
       (assert-false (get-node-by-name nw "fred"))
       (put-node! nw nd)
       
       (assert-true  (node?  (get-node-by-name nw "fred")))
       (assert-true (eq? nd  (get-node-by-name nw "fred"))))

      (make-test-case
       "Linking"
       (let ((n2 (new-node "sam" 12)))
         (assert-true (node? nd))
         (assert-true (node? n2))
         (link-nodes! nd n2)
         (assert-not-false (hash-table-get (node-neighbors nd) n2 (lambda () #f)))
         (assert-not-false (hash-table-get (node-neighbors n2) nd (lambda () #f)))))

      (make-test-case
       "No duplicates"
       (let ((sam (new-node "sam" 12))
             (fred (new-node "fred" 77)))
         (link-nodes! sam fred)
         (link-nodes! sam fred)
         (display (node->string sam)) (newline)
         (assert = 1 (hash-table-count (node-neighbors fred)))
         (assert = 1 (hash-table-count (node-neighbors sam)))))
      ))))
#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module tree-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
           (planet "util.ss" ("schematics" "schemeunit.plt" 1))

           "auction.ss"
           "tree.ss")

  (when (not
         (test/text-ui
          (make-test-suite
           "Tree"
           (make-test-case
            "Exercise best-auction-from-prefix"
            (let ((seconds-to-wait 10))
              (printf "Waiting ~a seconds for auction generator to come up with some auctions ... " seconds-to-wait)
              (flush-output)
              (let ((bsa (best-auction-from-prefix  (make-auction 'south) seconds-to-wait)))
                (when bsa
                  (let ((s (auction->string bsa)))
                    (assert-regexp-match "^S +W +N +E\n-+\np- +p- +p- +" s "Auction string don't look right!")
                    (printf "One auction:~n~a~n ~n"
                            s)
                    )
                  )))))))
    (exit 1)))

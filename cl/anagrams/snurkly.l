(defpackage :anagrams
  (:use :common-lisp :bag)
  (:export :anagrams))
(in-package :anagrams)

(defvar *dict*
  (with-open-file
   (dict "/usr/share/dict/words")
   (let ((d '()))
     (loop
      (let ((datum (read-line dict nil nil)))
        (if (null datum)
            (return d))
        (setq d (cons (bag:string->bag datum) d)))))))

(defun anagrams (string dict)
  (list (list "a" "be")))

;; apply #'append
;;   mapcar lambda word
;;     if this word can be made from our letters
;;       this word "distributed over" anagrams made from letters less this word
;;     else nil
;;   for each word in the dictionary

(defun distribute (str lists)
  (mapcar #'(lambda (l)
              (cons str l))
          lists))

(defmacro expect (expr expected)
  (let ((actual (gensym))
        (e (gensym)))
    `(let ((,actual ,expr)
           (,e ,expected))
       (assert (equal ,actual ,e)
               nil
               "From ~S, expected ~S; got ~S" ',expr ,e ,actual))))

(expect (distribute "bob" nil) nil)
(expect (distribute "sam" (list (list "fred")
                                nil
                                (list "my" "dog" "has"
                                      "fleas"))) 
         (list (list "sam" "fred")
               (list "sam")
               (list "sam" "my" "dog" "has" "fleas")))

(defun my-sort  (list-o-lists) 
  (sort 
   (mapcar #'(lambda (l)
               (sort l #'string-lessp))
           list-o-lists
           )
   #'(lambda (l1 l2)
       (string-lessp (apply #'concatenate 'string l1)
                     (apply #'concatenate 'string l2)))))


(expect (my-sort (list (list "eggbeater")
                       (list "my" "dog" "has" "fleas")
                       (list "i'm gonna be first!!" "aaaxelrod")
                       (list "frobotz" "a" "b" "c")))
        '(("aaaxelrod" "i'm gonna be first!!") ("a" "b" "c" "frobotz")
          ("dog" "fleas" "has" "my") ("eggbeater")))

(let ((dict  '("a" "be" "cat" "do" "em" "go" "me" "now")))
  (defun test (string expected)
    (let ((actual (anagrams string dict)))
      (expect  (my-sort expected)
               (my-sort actual))))

  (test "" nil)
  (test "abe" '(("a" "be")))
  (test "cat me" '(("cat" "me") ("cat" "em")))
  (test "be cat cat" '(("be" "cat" "cat")))
  (test "xyzzy" nil)
  (test "cat" '(("cat")))
  (test "catx" nil)
  (test "xcat" nil))
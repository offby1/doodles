
;"rrrs2txi.scm", A program to convert Scheme Reports to TeXInfo format.
; Copyright (c) 1998 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; This program translates the LaTeX source ".tex" files of the

;;   "Revised(3) Report on the Algorithmic Language Scheme",
;;   "Revised(4) Report on the Algorithmic Language Scheme", or
;;   "Revised(5) Report on the Algorithmic Language Scheme"

;; to TeXInfo format.  If "rrrs2txi.scm" is LOADed into Scheme in a
;; directory containing "r3rs.tex", "r4rs.tex", or "r5rs.tex", then
;; calling the Scheme procedure GO with no arguments (go) will
;; translate the report contained in that directory.  The resulting
;; ".txi" file is written to the current directory.

;; Otherwise, calling the GO procedure will translate all reports
;; contained in "r3rs", "r4rs", or "r5rs" subdirectories of the
;; current directory to texinfo format.  The resulting ".txi" files
;; are written to the current directory.

;; If a Scheme report is successfully converted, GO also tries to
;; compile the new ".txi" file to INFO format.  The resulting info
;; files are placed in the directory named by the variable
;; *INFO-VICINITY*, defined here.
(define *info-vicinity* (make-vicinity "/usr/local/info/"))
;; Change *INFO-VICINITY*'s value to suit your platform.

(require 'common-list-functions)
(require 'string-search)
(require 'string-port)
(require 'fluid-let)
(require 'line-i/o)
(require 'printf)

(define *input-vicinity* (user-vicinity))
(define *output-vicinity* (user-vicinity))
(define *input-basename* #f)
(define *tex-input-port* #f)
(define *txi-output-port* #f)
(define *begin-stack* '())
(define *entry-type* #f)
(define *brace-depth* 0)
(define *paren-depth* 0)
(define *previous-char* #f)
(define *closer* #f)
(define *html* #f)

(define (tex:errwarn class . args)
  (define cep (current-error-port))
  (force-output (current-output-port))
  (force-output *txi-output-port*)
  (if *input-basename*
      (fprintf cep "%s: \"%s.tex\": %s: " class *input-basename* *name*)
      (fprintf cep "%s: %s: " class *name*))
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep))
(define (tex:warn . args)
  (apply tex:errwarn "WARN" args))
(define (tex:error . args)
  (apply tex:errwarn "ERROR" args)
  '(print '*closer* *closer* '*brace-depth* *brace-depth*
	  '*previous-char* *previous-char*
	  'peek-char (peek-char *tex-input-port*))
  (require 'pretty-print)
  (pretty-print *begin-stack*)
  (abort))

(define *name* "?")
(define *previous-labels* '())
(define *new-labels* '(("define-syntax" . "Syntax definitions")))
(define (define-label label)
  ;;(print 'define-label label)
  (let ((pair (assoc label *new-labels*))
	(old-pair (assoc label *previous-labels*)))
    (cond ((not pair)
	   (set! *new-labels* (cons (cons label *name*) *new-labels*)))
	  ((equal? (cdr pair) *name*))
	  ((and old-pair (not (equal? (cdr old-pair) *name*)))
	   (set-cdr! pair *name*)
	   (tex:warn label 'changed-from (cdr old-pair) 'to *name*))
	  (else (tex:warn label 'changed-from (cdr pair) 'to *name*)
		(set-cdr! pair *name*)))))
;;; \pproto has some weird stuff.  Try to extract a label; the only
;;; one seems to be `do'
(define (extract-do-label proc)
  (if (equal? "(do " (substring proc 0 4))
      (define-label "do")))
(define (label->name label)
  (let ((pair (or (assoc label *new-labels*)
		  (assoc label *previous-labels*))))
    (cond (pair (cdr pair))
	  (else (tex:warn 'label-not-found label) label))))

;;;; space and special character utilities

(define (string-whitespace? str)
  (do ((idx (+ -1 (string-length str)) (+ -1 idx)))
      ((or (negative? idx) (not (char-whitespace? (string-ref str idx))))
       (negative? idx))))
(define (output-text-lines . lines)
  (for-each (lambda (str)
	      (display str *txi-output-port*)
	      (newline *txi-output-port*))
	    lines))
(define (escape-special chr)
  (case chr
    ((#\@ #\{ #\}) (string #\@ chr))
    ((#\#) "@pounds{}")
    (else chr)))

;;;; Make index entries

(define *index-entries* '())
(define (make-index-entry name before after)
  (let ((new-index (sprintf #f "@%s%s%s" before name after)))
    (if (not (member new-index *index-entries*))
	(set! *index-entries*
	      (cons new-index *index-entries*)))))
(define (rule:index cmd me before after)
  (fluid-let ((*tex-rules* (append (rule #\newline " ") *tex-rules*)))
    (make-index-entry (capture-argument) before after))
  #t)
(define (tex:newline chr me)
  (newline *txi-output-port*)
  (for-each (lambda (str)
	      (display str *txi-output-port*)
	      (newline *txi-output-port*))
	    *index-entries*)
  (set! *index-entries* '())
  #t)

;;;; Process files.

(define (check-file-end iport)
  (do ((chr (read-char iport) (read-char iport)))
      ((or (eof-object? chr) (not (char-whitespace? chr)))
       (if (not (eof-object? chr))
	   (tex:warn 'process-rrrs-file (string-append *input-basename* ".tex")
		     'ended-prematurely chr (read-line iport))))))
(define (process-rrrs-file base-name)
  (fluid-let ((*input-basename* base-name))
    (fprintf (current-error-port) "Translating \"%s.tex\"\\n" base-name)
    (call-with-input-file
	(in-vicinity *input-vicinity* (string-append base-name ".tex"))
      (lambda (iport)
	(fluid-let ((*tex-input-port* iport))
	  (process-tex-input #t)
	  (check-file-end iport)
	  #t)))))
(define (tex->txi . optargs)
  (cond ((null? optargs)
	 (set! *txi-output-port* (current-output-port))
	 (set! *tex-input-port* (current-input-port))
	 (process-tex-input #t))
	(else
	 (newline)
	 (cond ((not (null? (cdr optargs)))
		(set! *input-vicinity* (car optargs))
		(set! optargs (cdr optargs))))
	 (cond ((not (null? (cdr optargs)))
		(set! *output-vicinity* (cadr optargs))))
	 (let* ((basename (car optargs))
		(labels (in-vicinity *output-vicinity*
				     (string-append basename "-nod.scm"))))
	   (if (file-exists? labels)
	       (call-with-input-file labels
		 (lambda (lport)
		   (set! *previous-labels* (read lport))
		   (set! *previous-nodes*  (read lport))
		   (if (eof-object? *previous-nodes*)
		       (set! *previous-nodes* '())))))
	   (call-with-output-file
	       ;;"/dev/tty"
	       (in-vicinity *output-vicinity* (string-append basename ".txi"))
	     (lambda (oport)
	       (set! *txi-output-port* oport)
	       (process-rrrs-file basename)))
	   (if (and (not (null? *new-labels*)) (not (null? *new-nodes*)))
	       (call-with-output-file labels
		 (lambda (oport)
		   (fprintf oport "(\\n")
		   (for-each (lambda (pair) (fprintf oport " %#a\\n" pair))
			     *new-labels*)
		   (fprintf oport ")\\n")
		   (fprintf oport "(\\n")
		   (for-each (lambda (lst) (fprintf oport " %#a\\n" lst))
			     *new-nodes*)
		   (fprintf oport ")\\n"))))
	   (and (equal? *previous-labels* *new-labels*)
		(equal? *previous-nodes* *new-nodes*))))))

;;;; Use `system' to convert to info format
(define (txi->info vic base . info-vic)
  (newline)
  (set! info-vic (if (null? info-vic) *info-vicinity* (car info-vic)))
  (and (provided? 'system)
       (zero? (system (sprintf #f "makeinfo %s%s.txi -o %s%s.info"
			       vic base info-vic base)))
       (zero? (system (sprintf #f "install-info %s%s.info %sdir"
			       info-vic base info-vic)))))

;;;; Rules

(define rules append)
(define (rule toks . args)
  (map (lambda (tok) (cons tok args))
       (if (pair? toks) toks (list toks))))

(define (process-rule rul)
  (cond ((procedure? (cadr rul))
	 (let ((ans (apply (cadr rul) rul)))
	   (set! *previous-char* (car rul))
	   ans))
	((char? (cadr rul))
	 (set! *previous-char* (car rul))
	 (display (cadr rul) *txi-output-port*)
	 #t)
	((symbol? (cadr rul))
	 (cadr rul))
	((string? (cadr rul))
	 (set! *previous-char* (car rul))
	 (apply fprintf *txi-output-port* (cdr rul))
	 #t)
	(else
	 (set! *previous-char* (car rul))
	 (tex:error (car rul) '=> 'malformed-rule rul))))
(define (process-one left)
  (let* ((tok (if (eqv? #t left) (read-char *tex-input-port*) left))
	 (rul (or (assv tok *tex-rules*))))
    ;;(print *brace-depth* (make-string (max 0 *brace-depth*) #\ ) tok '*closer* *closer*)
    (cond ((eof-object? tok)
	   (cond ((not (zero? *brace-depth*))
		  (tex:error '*brace-depth* 'not-zero *brace-depth*)))
	   tok)
	  ((not rul)
	   (set! *previous-char* tok)
	   (display tok *txi-output-port*)
	   (if (not (char? tok)) (tex:warn tok 'unknown))
	   #t)
	  (else (process-rule rul)))))
(define (process-tex-input left)
  (do ((left (process-one left) (process-one left)))
      ((or (eqv? #f left) (eof-object? left))
       left)))

;;;; Arguments to backslash commands

(define (process-full-bracketed capture?)
  (if capture?
      (case (read-char *tex-input-port*)
	((#\[)
	 (call-with-output-string
	  (lambda (oport)
	    (fluid-let
		((*closer* #\])
		 (*txi-output-port* oport)
		 (*tex-rules*
		  (append (rule #\] tex:close)
			  (rule 'tt (lambda (cmd me)
				      (fprintf *txi-output-port* "@t{")
				      (process-tex-input #t)
				      (fprintf *txi-output-port* "}")
				      #f))
			  *tex-rules*)))
	      (process-tex-input #t)))))
	(else (tex:error 'missing #\[)))
      (case (read-char *tex-input-port*)
	((#\[)
	 (fluid-let ((*closer* #\])
		     (*tex-rules*
		      (append (rule #\] tex:close)
			      (rule 'tt
				    (lambda (cmd me)
				      (fprintf *txi-output-port* "@t{")
				      (process-tex-input #t)
				      (fprintf *txi-output-port* "}")
				      #f))
			      *tex-rules*)))
	   (process-tex-input #t)))
	(else (tex:error 'missing #\[)))))
(define (process-opt-arg cmd)
  (case cmd
    ((linebreak) (process-full-bracketed #t) #t)
    ((documentstyle documentclass)
     (let* ((optarg (process-full-bracketed #t))
	    (arg (capture-braced-expression)))
       (output-text-lines "\\input texinfo @c -*-texinfo-*-"
			  "@c %**start of header")
       (fprintf *txi-output-port* "@setfilename %s\\n"
		(string-append *input-basename* ".info"))
       (fprintf *txi-output-port* "@settitle Revised(%c) Scheme\\n"
		(string-ref *input-basename* 1))))
    ((topnewpage)
     (cond
      (*html*
       (fluid-let
	   ((*tex-rules*
	     (append
	      (rule #\newline "")
	      (rule '(begin-center end-center) "")
	      (rule '(#\& bs-bs) "\\n@author ")
	      (rule 'begin-tabular
		    (lambda (cmd me)
		      (capture-braced-expression)
		      (fluid-let
			  ((*closer* 'end-tabular)
			   (*tex-rules*
			    (append (rule 'end-tabular tex:close "" "")
				    (rule '(#\& bs-bs) "\\n@author ")
				    compress-spaces
				    *tex-rules*)))
			(process-tex-input #t)
			#t)))
	      (rule 'multicolumn
		    (lambda (cmd me . ruls)
		      (fluid-let ((*tex-rules* (append (apply rules ruls)
						       *tex-rules*)))
			(let* ((arg1 (capture-argument))
			       (arg2 (capture-braced-expression)))
			  (capture-braced-expression)
			  #t)))
		    (rule #\newline ""))
	      *tex-rules*)))
	 (process-full-bracketed #f))
       (fluid-let ((*tex-rules* (append (rule 'eject tex:close) *tex-rules*))
		   (*closer* 'eject))
	 (process-tex-input #\newline)))
      (else
       (fluid-let ((*tex-rules* (append (rule #\newline "")
					(rule '(begin-center end-center) "")
					*tex-rules*)))
	 (process-full-bracketed #f))
       (fluid-let ((*tex-rules* (append (rule 'eject tex:close) *tex-rules*))
		   (*closer* 'eject))
	 (process-tex-input #\newline)))))
    ((item)
     (fprintf *txi-output-port* "@item ")
     (process-full-bracketed #f))
    (else (tex:error cmd 'does-not-take-optional-arguments)))
  #t)

(define (capture-braced-expression)
  (call-with-output-string
   (lambda (oport)
     (fluid-let ((*txi-output-port* oport))
       (process-braced-expression)))))
(define (process-braced-expression)
  (case (peek-char *tex-input-port*)
    ((#\{)
     (read-char *tex-input-port*)
     (fluid-let ((*tex-rules*
		  (append (rule #\} tex:close) *tex-rules*))
		 (*brace-depth* (+ 1 *brace-depth*))
		 (*closer* #\}))
       (process-tex-input #t)
       #t))
    (else (tex:error 'process-braced-expression 'missing #\{))))
(define (capture-argument)
  (call-with-output-string
   (lambda (oport)
     (fluid-let ((*txi-output-port* oport))
       (process-argument)))))
(define (check-brace-depth! bd tok)
  (cond ((eqv? bd *brace-depth*))
	(else (tex:warn (if (< *brace-depth* bd)
			    'brace-over-used-by
			    'brace-under-used-by)
			tok
			*brace-depth* 'not bd))))
;;; Stub for top-level definition.
(define (process-argument)
  (tex:error 'process-argument "called without setup-argument-processing"))

;;;; Backslash commands

;;; TeX is nasty in its treatment of curly braces:
;;; {\rm text}		=> (rm text)
;;; \rm{text}		=> (rm text)
;;; \rm{\var{proc}}	=> (rm (var proc))
;;; \rm{\var proc}	=> (rm (var proc))

;;;	 1 pair: \foo{...}
;;; early-brace: {\foo ...}
;;;	 2 pair: {\foo{...} ...} strip outer pair

(define (read-bs-token)
  (define (char-alphabetic*? chr)
    (or (char-alphabetic? chr)
	(eqv? #\* chr)))
  (do ((chr (peek-char *tex-input-port*) (peek-char *tex-input-port*))
       (lst '() (cons chr lst)))
      ((or (eof-object? chr)
	   (not (char-alphabetic*? chr)))
       (let ((str (list->string (reverse lst))))
	 (if (equal? "" str)
	     (tex:error 'null-bs-token)
	     (string->symbol str))))
    (read-char *tex-input-port*)))
(define (read-bs-command early-brace?)
  (define bschr (peek-char *tex-input-port*))
  (define processed-argument? #f)
  (cond
   ((char-alphabetic? bschr)
    (let* ((tok (read-bs-token))
	   (chr (peek-char *tex-input-port*)))
      (cond
       ((eqv? #\[ chr) (process-opt-arg tok))
       (else
	(fluid-let
	    ((process-argument
	      (lambda ()
		(set! processed-argument? #t)
		(let ((chr (peek-char *tex-input-port*))
		      (bd *brace-depth*))
		  (cond ((eqv? chr #\{)
			 (read-char *tex-input-port*)
			 (fluid-let
			     ((*brace-depth* (+ 1 bd)) ;(if early-brace? 2 1)
			      (*tex-rules*
			       (append (rule #\} tex:close) *tex-rules*))
			      (*closer* #\}))
			   (process-tex-input #t)
			   (check-brace-depth! bd tok)
			   #f))
			(else (if (and (char-whitespace? chr)
				       (not (eqv? #\newline chr)))
				  (read-char *tex-input-port*))
			      (if early-brace?
				  (process-tex-input #t)
				  (if (zero? bd)
				      (fluid-let
					  ((*tex-rules*
					    (append (rule #\newline tex:close)
						    *tex-rules*))
					   (*closer* #\newline))
					(process-tex-input #t)
					(check-brace-depth! bd tok)
					(set! processed-argument? #f)
					#\newline)
				      (process-tex-input #t)))))))))
	  (let ((ans
		 (cond ((and early-brace? (not (eqv? chr #\{)))
			(fluid-let ((*tex-rules*
				     (append (rule #\} tex:close) *tex-rules*))
				    (*brace-depth* (+ 1 *brace-depth*))
				    (*closer* #\}))
			  (process-tex-input tok)
			  #t))
		       ((and early-brace? (eqv? chr #\{))
			(fluid-let ((*tex-rules*
				     (append (rule #\} tex:close) *tex-rules*))
				    (*brace-depth* (+ 1 *brace-depth*))
				    (*closer* #\}))
			  (process-tex-input tok)
			  (process-tex-input #t)
			  #t))
		       (else (process-one tok)))))
	    ;;(print 'processed-argument? processed-argument? 'ans ans)
	    (if processed-argument?
		(if (or (eqv? chr #\{) early-brace?)
		    (if (eqv? ans #f) #t ans)
		    #f)
		ans)))))))
   ((char-numeric? bschr)
    (tex:error 'bs-digit? bschr))
   (else (case bschr
	   ((#\/) (read-char *tex-input-port*) 'italic-space)
	   ((#\\) (read-char *tex-input-port*)
	    (cond ((char-whitespace? (read-char *tex-input-port*))
		   'bs-bs)
		  (else (tex:error 'non-whitespace 'after-bs-bs))))
	   ((#\  #\newline)
	    (case *previous-char*
	      ((#\.) (display "@:" *txi-output-port*)))
	    (display (read-char *tex-input-port*) *txi-output-port*)
	    #t)
	   ((#\#) (read-char *tex-input-port*)
	    (display (if (member "scheme" *begin-stack*) #\# "@pounds{}")
		     *txi-output-port*)
	    #t)
	   ((#\{ #\} #\-)
	    (fprintf *txi-output-port* "@%c" (read-char *tex-input-port*))
	    #t)
	   ;;((#\$ #\& #\% #\_))
	   (else (display (read-char *tex-input-port*) *txi-output-port*)
		 #t)))))
(define (tex:input cmd me)
  (let ((name (capture-argument)))
    (case (string->symbol name)
      ((first)
       (output-text-lines "@c @include{first}" "@titlepage" "" "@ifclear html"
			  "@c TeX first page")
       (fluid-let
	   ((vet-node-name #f)
	    (*tex-rules*
	     (append (rule 'chapter* node 'majorheading)
		     (rule `huge
			   process-argument-with-rules "" "}"
			   compress-spaces
			   (rule #\newline "\\n@center @titlefont{")
			   (rule 'bs-bs "}\\n")
			   (rule '(#\^ bf) ""))
		     (rule (string->symbol "Huge") "")
		     *tex-rules*)))
	 (process-rrrs-file name))
       (output-text-lines "@end ifclear" "" "@ifset html"
			  "@c HTML first page" "@title Scheme")
       (fprintf *txi-output-port*
	"@subtitle Revised(%c) Report on the Algorithmic Language Scheme\\n"
	(string-ref *input-basename* 1))
       (fluid-let
	   ((vet-node-name #f)
	    (*html* #t)
	    (*tex-rules*
	     (append (rule 'chapter* node 'unnumbered)
		     (rule `huge
			   (lambda (cmd me) (capture-argument) #f))
		     (rule (string->symbol "Huge") "")
		     (rule 'vskip disappear)
		     (rule '$$ "")
		     *tex-rules*)))
	 (process-rrrs-file name))
       (output-text-lines "@end ifset"
			  "@end titlepage" ""
			  "@c INFO first page"
			  "@ifinfo" "")
       (fluid-let
	   ((*tex-rules*
	     (append
	      (rule `huge node 'top
		    compress-spaces
		    (rule '(bs-bs #\newline #\^ bf) "")
		    (rule 'vskip
			  (lambda (cmd me) (read-line *tex-input-port*) #t)))
	      (rule (string->symbol "Huge") "")
	      (rule 'chapter* node 'majorheading)
	      *tex-rules*)))
	 (process-rrrs-file name))
       (output-text-lines "@end ifinfo" ""))
      ((commands)
       (fprintf (current-error-port) "...Skipping \"%s.tex\"\\n" name))
      ((sem)
       (fprintf (current-error-port) "...Skipping \"%s.tex\"\\n" name)
       (emit-node! 'section "Formal semantics")
       (define-label "formalsemanticssection")
       (output-text-lines "" ""
"This section provides a formal denotational semantics for the primitive"
"expressions of Scheme and selected built-in procedures.  The concepts"
"and notation used here are described in @sc{[Stoy77]}."
""
"@quotation"
"@emph{Note:} The formal semantics section was written in La@TeX{} which"
"is incompatible with @TeX{}info.  See the Formal semantics section of"
"the original document from which this was derived."
"@end quotation"
			  )
       (newline *txi-output-port*))
      ((index) (output-text-lines "@unnumberedsec Concepts"
				  "@printindex cp" "@page"
				  "@unnumberedsec Procedures"
				  "@printindex fn"))
      (else
       (fprintf *txi-output-port* "@c @include{%s}\\n" name)
       (process-rrrs-file name))))
  #f)
;;;; Texinfo nodes

(define *new-nodes* '())
(define *previous-nodes* '())
(define *node-stack* '())
(define (node-rank type)
  (case (if (symbol? type) type (string->symbol type))
    ((top) 1)
    ((chapter unnumbered appendix) 3)
    ((majorheading chapheading) 4)
    ((section unnumberedsec appendixsec) 5)
    ((heading) 6)
    ((subsection unnumberedsubsec appendixsubsec) 7)
    ((subheading) 8)
    ((subsubsection unnumberedsubsubsec appendixsubsubsec) 9)
    ((subsubheading) 10)
    (else (tex:error 'unknown-node-type type))))
(define (find-previous-node name rank stack)
  (cond ((null? stack) "(dir)")
	((= (cadar stack) rank)
	 (if (caddar stack) (tex:error 'previous-already-set (car stack)))
	 (set-car! (cddar stack) name)
	 (caar stack))
	((< (cadar stack) rank)
	 (cond ((equal? "top" (caar stack)) (set-car! (cddar stack) name)))
	 (caar stack))
	(else (find-previous-node name rank (cdr stack)))))
(define (find-parent-node name rank stack)
  (cond ((null? stack) "(dir)")
	((< (cadar stack) rank)
	 (set-car! (last-pair (car stack))
		   (append (car (last-pair (car stack))) (list name)))
	 (caar stack))
	(else (find-parent-node name rank (cdr stack)))))
(define (update-stack rank nod stack)
  (cond ((null? stack)
	 (if (not (eqv? 1 rank))
	     (tex:error 'null-stack-with-non-zero-rank? rank nod))
	 (list nod))
	((< rank (cadar stack))
	 (update-stack rank nod (cdr stack)))
	(else (cons nod stack))))
(define (vet-node-name cmd name)
  (if (not (symbol? cmd)) (tex:error 'vet-node-name 'symbol? cmd))
  (cond ((substring? "Appendix: " name)
	 (set! name (substring name (+ (string-length "Appendix: ")
				       (substring? "Appendix: " name))
			       (string-length name))))
	((substring? "index " name) (set! name  "Index"))
	((eq? 'top cmd)
	 (newline *txi-output-port*)
	 (set! name "top")))
  (cond ((not (assoc name *new-nodes*))
	 (let ((rank (node-rank cmd)))
	   (if (odd? rank)
	       (let ((nod (list name
				rank
				#f
				(find-previous-node name rank *node-stack*)
				(find-parent-node name rank *node-stack*)
				'())))
		 (set! *new-nodes* (cons nod *new-nodes*))
		 (set! *node-stack* (update-stack rank nod *node-stack*)))))
	 name)
	((eq? 'top cmd)
	 (tex:error 'multiple-top-nodes? cmd name *name*))
	((eqv? #\s (string-ref name (+ -1 (string-length name))))
	 (vet-node-name cmd (substring name 0 (+ -1 (string-length name)))))
	(else
	 (vet-node-name
	  cmd
	  (string-append
	   name (if (eqv? #\  (string-ref name (+ -2 (string-length name))))
		    "I" " I"))))))
(define (emit-node! cmd name)
  (set! *name* (if vet-node-name (vet-node-name cmd name) name))
  (fprintf (current-error-port) " %s \"%s\"\\n" cmd *name*)
  (if vet-node-name
      (let ((nod (assoc *name* *previous-nodes*)))
	(cond ((not nod)
	       (fprintf *txi-output-port* "@%s %s\\n" cmd name))
	      (else
	       (fprintf *txi-output-port* "@node %s, %s, %s, %s\\n"
			*name*
			(or (caddr nod) " ") (or (cadddr nod) " ")
			(list-ref nod 4))
	       (fprintf *txi-output-port* "@%s %s\\n" cmd name)
	       (cond
		((not (null? (list-ref nod 5)))
		 (fprintf *txi-output-port* "\\n@menu\\n")
		 (for-each (lambda (menu-line)
			     (fprintf *txi-output-port* "* %-30s\\n"
				      (string-append menu-line "::  ")))
			   (list-ref nod 5))
		 (fprintf *txi-output-port* "@end menu\\n"))))))
      (fprintf *txi-output-port* "@%s %s\\n" cmd name)))
(define (node cmd me alias . ruls)
  (define ans #f)
  (let ((name (call-with-output-string
	       (lambda (oport)
		 (fluid-let ((*txi-output-port* oport)
			     (*tex-rules* (append (rule 'tt "")
						  (rule #\, ";")
						  (apply rules ruls)
						  *tex-rules*)))
		   (set! ans (process-argument)))))))
    (cond ((string-ci=? "Contents" name) ans)
	  (else (emit-node! (or alias cmd) name)
		ans))))

(define setboxes (make-vector 10 ""))
(define (setbox cmd me)
  (let* ((idx (string->number (string (read-char *tex-input-port*))))
	 (expn (call-with-output-string
		(lambda (oport)
		  (fluid-let ((*txi-output-port* oport))
		    (process-one #t))))))
    (vector-set! setboxes idx expn)
    #t))
(define (copy-box cmd me)
  (let* ((idx (string->number (string (read-char *tex-input-port*)))))
    (display (vector-ref setboxes idx) *txi-output-port*)
    #t))

;;;; Rule functions

(define (unmatched-close chr me) (tex:error 'unmatched chr))
(define (tex:close chr me . lines)
  (cond ((not (eqv? chr *closer*))
	 (tex:error 'close-mismatch chr 'should-be *closer*)))
  (case chr ((#\}) (set! *brace-depth* (+ -1 *brace-depth*))))
  (apply output-text-lines lines)
  #f)

(define (% tok me) #\%)
(define (commentize cmd me . ruls)
  (fprintf *txi-output-port* "@c \\\\%s %s" cmd (read-line *tex-input-port*))
  #\newline)
(define (disappear cmd me . ruls)
  (read-line *tex-input-port*)
  #\newline)

(define (postfix cmd me suffix)
  (let ((name (capture-argument)))
    (fprintf *txi-output-port* "%s%s" name suffix))
  #f)

(define (encapsulate from me to . ruls)
  (fprintf *txi-output-port* "@%s{" to)
  (if (null? ruls)
      (let ((ans (process-argument)))
	(fprintf *txi-output-port* "}")
	ans)
      (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	(let ((ans (process-argument)))
	  (fprintf *txi-output-port* "}")
	  ans))))

(define (process-argument-with-rules cmd me . ruls)
  (define post #f)
  (cond ((and (not (null? ruls)) (not (list? (car ruls))))
	 (fprintf *txi-output-port* (car ruls))
	 (set! ruls (cdr ruls))
	 (cond ((and (not (null? ruls)) (not (list? (car ruls))))
		(set! post (car ruls))
		(set! ruls (cdr ruls))))
	 (let ((ans (if (null? ruls)
			(process-argument)
			(fluid-let ((*tex-rules*
				     (append (apply rules ruls) *tex-rules*)))
			  (process-argument)))))
	   (if post (fprintf *txi-output-port* post))
	   ans))
	((null? ruls) (process-argument))
	(else (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
		(process-argument)))))

;;;; The Rules

(define compress-spaces
  (rule #\  (lambda (chr me)
	      (display #\  *txi-output-port*)
	      (do ((ch (peek-char *tex-input-port*)
		       (peek-char *tex-input-port*)))
		  ((not (eqv? ch #\ )) #t)
		(read-char *tex-input-port*)))))
(define *tex-rules*
  (rules
   (rule #\~ #\ )
   (rule #\@ "@@")
   (rule #\% (lambda (chr me)
	       (let ((line (read-line *tex-input-port*)))
		 (if (not (string-whitespace? line))
		     (fprintf *txi-output-port* "@c %s" line))
		 #\newline)))
   (rule #\{ (lambda (chr me . ruls)
	       (case (peek-char *tex-input-port*)
		 ((#\}) (read-char *tex-input-port*) #t)
		 ((#\\)
		  (read-char *tex-input-port*)
		  (read-bs-command #t))
		 (else
		  (fluid-let ((*tex-rules*
			       (append (apply rules ruls) *tex-rules*))
			      (*brace-depth* (+ 1 *brace-depth*))
			      (*closer* #\}))
		    (process-tex-input #t)
		    #t))))
	 (rule #\} tex:close))
   (rule #\} unmatched-close)
   (rule #\] unmatched-close)
   (rule #\\ (lambda (chr me) (read-bs-command #f)))
   (rule #\$ (lambda (chr me)
	       (case (peek-char *tex-input-port*)
		 ((#\$) (read-char *tex-input-port*) '$$)
		 (else '$))))
   (rule slib:tab "")
   (rule #\newline tex:newline)

   (rule 'bs-bs "@\\n")
   (rule '($ $$) (lambda (tok me . ruls)
		   (fluid-let ((*tex-rules* (append (rule tok tex:close)
						    (apply rules ruls)
						    *tex-rules*))
			       (*closer* tok))
		     (case tok
		       (($$)
			(fprintf *txi-output-port* "\\n@center ")
			(process-tex-input #\newline))
		       (($) (process-tex-input #t)))
		     #t))
	 (rule #\newline "")
	 (rule 'sqrt "sqrt")
	 (rule 'sin "sin")
	 (rule 'cos "cos")
	 (rule 'tan "tan")
	 (rule 'log "log")
	 (rule 'pi "pi")
	 (rule 'over "/")
	 (rule 'bf process-argument-with-rules "(" ")"))

   (rule 'char
	 (lambda (cmd me)
	   (let ((chr (read-char *tex-input-port*)))
	     (display
	      (escape-special
	       (integer->char
		(case chr
		  ((#\')
		   (let* ((c1 (read-char *tex-input-port*))
			  (c2 (read-char *tex-input-port*)))
		     (string->number (string c1 c2) 8)))
		  ((#\")
		   (let* ((c1 (read-char *tex-input-port*))
			  (c2 (read-char *tex-input-port*)))
		     (string->number (string c1 c2) 16)))
		  (else
		   (tex:error 'char-argument-not-understood chr)))))
	      *txi-output-port*)
	     #t)))

   (rule 'multicolumn
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (let* ((arg1 (capture-argument))
		    (arg2 (capture-braced-expression)))
	       (fprintf *txi-output-port* "\\n@center ")
	       (process-braced-expression))))
	 (rule #\newline ""))
   (rule 'begin-center
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*))
		       (*closer* 'end-center))
	     (fprintf *txi-output-port* "\\n@center ")
	     (process-tex-input #t)
	     #\newline))
	 compress-spaces
	 (rule 'end-center tex:close)
	 (rule #\newline "\\n@center "))
   (rule 'extracolsep (lambda (cmd me) (capture-argument) #f))
   (rule 'verb
	 (lambda (cmd me)
	   (let ((closer (read-char *tex-input-port*)))
	     (do ((chr (read-char *tex-input-port*)
		       (read-char *tex-input-port*)))
		 ((or (eqv? chr closer) (eof-object? chr)) #t)
	       (display (escape-special chr) *txi-output-port*)))))

   ;; These commands translate simply.
   (rule (string->symbol "TeX") "@TeX{}")
   (rule 'sharpfoo process-argument-with-rules "@r{#" "}")
   (rule 'cite process-argument-with-rules "[" "]"
	 (rule #\, "], ["))
   (rule 'schfalse "@r{#f}")
   (rule 'schtrue "@r{#t}")
   (rule 'unspecified "@emph{unspecified}")
   (rule 'scherror "@emph{error}")
   (rule 'semantics "@emph{semantics}")
   (rule 'syntax "@emph{syntax}")
   (rule 'exprtype "syntax")
   (rule 'singlequote "@r{'}")
   (rule 'backquote "@r{`}")
   (rule 'backwhack "\\\\")
   (rule 'atsign "@@")
   (rule 'sharpsign "#")
   (rule 'prime "^")
   (rule 'dots "@dots{}")
   (rule 'dotsfoo "@dots{},")
   (rule 'makeindex "")
   (rule 'iet "  @equiv{}")
   (rule 'ae "@ae{}")
   (rule 'le "<=")
   (rule 'leq "<=")
   (rule 'neq "~=")
   (rule 'langle "<")
   (rule 'rangle ">")
   (rule 'cdot ".")
   (rule 'ldots "@dots{}")
   (rule 'vdots "@dots{}")
   (rule 'ev "@result{}")
   (rule '(goesto evalsto) "@result{}")
   (rule 'lev "@expansion{}")
   (rule 'break "@*")
   (rule '(hfil hfill wd vfill par nobreak qquad
		unsection footnotesize tableofcontents) "")

   ;; These come with {} after them.
   (rule (string->symbol "Lambdaexp") "lambda expression")
   (rule 'lambdaexp "lambda expression")
   (rule 'nopagebreak "")
   (rule 'doublequote "\"")
   (rule 'coerce "->")

   ;; These begin lines
   (rule '(newpage eject) "\\n@page\\n")
   (rule 'medskip "@sp 3")
   (rule 'bigskip "@sp 6")
   (rule 'noindent "\\n@noindent\\n")
   (rule 'item "\\n\\n@item ")

   ;; These occur as {\tt ...}
   (rule 'tt encapsulate 't)
   (rule 'rm encapsulate 'r)
   (rule 'it encapsulate 'i)
   (rule 'itshape encapsulate 'i)
   (rule 'italic-space "")		;\/
   (rule 'bf encapsulate 'b)
   (rule 'mathbf 'bf)
   (rule 'sc encapsulate 'sc)
   (rule 'authorsc encapsulate 'sc)
   (rule 'em encapsulate 'emph)
   (rule 'cf (lambda (cmd me)
	       (let ((arg (capture-argument)))
		 (fprintf *txi-output-port* "@%s{%s}"
			  (if (substring? "http://" arg) 'url 'samp) arg)
		 #f)))
   (rule 'hbox encapsulate 'w)

   ;; These occur as \vr{...}
   (rule 'vr encapsulate 'var)
   (rule 'var encapsulate 'var)
   (rule 'type encapsulate 'i)
   (rule 'tupe encapsulate 'r)
   (rule 'meta encapsulate 'r)

   (rule 'displaystyle postfix "")

   (rule 'begin-scheme "@lisp")
   (rule 'end-scheme (lambda (cmd me) (set! *paren-depth* 0)
			     (output-text-lines "@end lisp")
			     #t))
   (rule '(begin-schemenoindent begin-grammar)
	 (lambda (cmd me)
	   (if (eqv? #\newline (peek-char *tex-input-port*))
	       (read-char *tex-input-port*))
	   (fprintf *txi-output-port* "@format\\n@t{")
	   #t))
   (rule '(end-schemenoindent end-grammar) "}\\n@end format\\n")
   (rule '(begin-note begin-rationale)
	 (lambda (cmd me)
	   (fprintf *txi-output-port* "@emph{%s:}\\n@quotation\\n"
		    (let ((str (symbol->string cmd)))
		      (set! str (substring str 6 (string-length str)))
		      (string-set! str 0 (char-upcase (string-ref str 0)))
		      str))
	   #\newline))
   (rule 'begin-tabbing "@quotation")
   (rule 'end-tabbing "@end quotation")
   (rule '(end-note end-rationale) "@end quotation\\n")
   (rule 'begin-entry "")
   (rule 'end-entry
	 (lambda (tok me)
	   (fprintf *txi-output-port* "@end %s" *entry-type*)
	   (set! *entry-type* #f)
	   #t))
   (rule 'unpenalty
	 (lambda (tok me)
	   (fprintf *txi-output-port* "\\n@end %s" *entry-type*)
	   (set! *entry-type* #f)
	   #t))
   (rule '(begin-itemize) "@itemize @bullet\\n")
   (rule '(begin-description) "@table @t\\n")
   (rule 'begin-tabular
	 (lambda (cmd me)
	   (define numcols
	     (string-length
	      (fluid-let ((*tex-rules* (append (rule #\@ "")
					       *tex-rules*)))
		(capture-braced-expression))))
	   (fprintf *txi-output-port* "@c %s\\n" cmd)
	   (fprintf *txi-output-port* "@quotation\\n")
	   (fprintf *txi-output-port* "@multitable @columnfractions")
	   (do ((i (+ -1 numcols) (+ -1 i)))
	       ((negative? i))
	     (fprintf *txi-output-port* " %.2f" (/ numcols)))
	   (fprintf *txi-output-port* "\\n@item ")
	   (fluid-let
	       ((*closer* 'end-tabular)
		(*tex-rules* (append (rule 'end-tabular tex:close
					   "" "@end multitable"
					   "@end quotation" "")
				     (rule #\& "@tab ")
				     (rule #\newline "")
				     (rule 'bs-bs "\\n@item ")
				     *tex-rules*)))
	     (process-tex-input #t)
	     #t)))
   (rule 'end-description "@end table\\n")
   (rule '(end-itemize end-thebibliography) "@end itemize\\n")
   (rule 'begin-thebibliography
	 (lambda (cmd me)
	   (emit-node! 'unnumbered "Bibliography")
	   (output-text-lines "" "" "@itemize @bullet")
	   (fprintf *txi-output-port* "@c ")
	   (fluid-let ((*tex-rules* (append (rule 'cite encapsulate 'cite)
					    *tex-rules*)))
	     (process-braced-expression)
	     #\newline)))
   (rule 'begin-theindex
	 (lambda (tok me)
	   (fprintf (current-error-port) "...Indexing\\n")
	   (emit-node! 'unnumbered
"Alphabetic index of definitions of concepts, keywords, and procedures")
	   #\newline))
   (rule 'end-theindex
	 "@ifinfo\\n@unnumberedsec References\\n@printindex pg\\n@end ifinfo\\n"
	 )
   (rule 'end-theindex
	 (lambda (tok me)
	   (output-text-lines "@ifinfo"
			      "@unnumberedsec References"
			      "@printindex pg"
			      "@end ifinfo" "")
	   #\newline))
   (rule 'begin-document
	 (lambda (tok me)
	   (output-text-lines "@c %**end of header"
			      "@c syncodeindex fn cp" ""
			      "@ifinfo"
			      "@dircategory The Algorithmic Language Scheme"
			      "@direntry")
	   (fprintf *txi-output-port*
		    "* %s: (%s).         The Revised(%c) Report on Scheme.\\n"
		    (string-upcase *input-basename*)
		    *input-basename*
		    (string-ref *input-basename* 1))
	   (output-text-lines "@end direntry" "@end ifinfo")
	   #t))
   (rule 'end-document
	 (lambda (tok me)
	   (output-text-lines "@contents" "@bye")
	   (cond ((not (null? *begin-stack*))
		  (tex:error '*begin-stack* 'not-empty *begin-stack*))
		 ((not (zero? *brace-depth*))
		  (tex:error '*brace-depth* 'not-zero *brace-depth*)))
	   #f))

   ;; \-commands which take arguments
   ;; but aren't simple substitutions.
   (rule 'vest (lambda (cmd me)
		 (case (peek-char *tex-input-port*)
		   ((#\ ) (read-char *tex-input-port*)))
		 #t))
   (rule 'begin
	 (lambda (cmd me)
	   (let* ((name (capture-argument))
		  (tok (string->symbol (string-append "begin-" name))))
	     (set! *begin-stack* (cons name *begin-stack*))
	     (cond ((assv tok *tex-rules*) tok)
		   (else (tex:warn tok 'not-recognized) #f)))))
   (rule 'end
	 (lambda (cmd me)
	   (let* ((name (capture-argument))
		  (tok (string->symbol (string-append "end-" name))))
	     (cond ((null? *begin-stack*)
		    (tex:error 'end name 'begin-stack-empty))
		   ((not (equal? name (car *begin-stack*)))
		    (tex:error 'end name 'doesn't-match
			       'begin (car *begin-stack*)))
		   (else (set! *begin-stack* (cdr *begin-stack*))))
	     (cond ((assv tok *tex-rules*) tok)
		   (else (tex:warn tok 'not-recognized) #f)))))
   (rule 'arbno postfix "@r{*}")
   (rule 'atleastone postfix "@r{+}")
   (rule 'hyper process-argument-with-rules "@r{<" ">}")
   (rule '(todo nodomain)
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (cond ((not (eqv? #\newline *previous-char*))
		    (newline *txi-output-port*)))
	     (fprintf *txi-output-port* "@ignore %s\\n" cmd)
	     (process-argument)
	     (fprintf *txi-output-port* "\\n@end ignore\\n")
	     #t))
	 (rule #\[ "[")
	 (rule #\] "]")
	 (rule 'begin "\\begin")
	 (rule 'end "\\end")
	 (rule '(subsection subsection*) ""))
   (rule 'domain (lambda (cmd me) (process-argument) #t))

   (rule 'vari process-argument-with-rules "@var{" "1}")
   (rule 'varii process-argument-with-rules "@var{" "2}")
   (rule 'variii process-argument-with-rules "@var{" "3}")
   ;;(rule 'variv process-argument-with-rules "@var{" "4}")

   (rule 'vri process-argument-with-rules "@var{" "1}")
   (rule 'vrii process-argument-with-rules "@var{" "2}")
   (rule 'vriii process-argument-with-rules "@var{" "3}")
   (rule 'vriv process-argument-with-rules "@var{" "4}")

   (rule 'hyperi process-argument-with-rules "@r{<" "1>}")
   (rule 'hyperii process-argument-with-rules "@r{<" "2>}")
   (rule 'hyperiii process-argument-with-rules "@r{<" "3>}")
   (rule 'hyperiv process-argument-with-rules "@r{<" "4>}")
   (rule 'hypern process-argument-with-rules "@r{<" "_n>}")

   (rule 'index rule:index "cindex @w{" "}")
   (rule 'mainindex rule:index "cindex @w{" "}")
   (rule 'mainschindex rule:index "cindex @w{" "}")
   (rule 'schindex rule:index "vindex " "")
   (rule 'sharpindex rule:index "vindex #" "")
   (rule 'ide
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@code{%s}" name)
	     (make-index-entry name "vindex @w{" "}"))
	   #f))
   (rule 'defining
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@dfn{%s}" name)
	     (make-index-entry name "cindex @w{" "}"))
	   #f))
   (rule 'bibitem
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf *txi-output-port* "@item [%s]" name)
	     (make-index-entry name "pindex " ""))
	   #f))
   (rule 'foo
	 (lambda (cmd me)
	   (let ((name (capture-argument)))
	     (fprintf
	      *txi-output-port*
	      "@var{%s}, @var{%s1}, @dots{} @var{%sj}, @dots{}"
	      name name name))
	   #f))
   (rule 'clearextrapart node 'unnumbered)
   (rule 'extrapart node 'unnumbered)
   (rule 'subsection* node 'unnumberedsec)
   (rule '(chapter section subsection) node #f)
   (rule 'label (lambda (cmd me) (define-label (capture-argument)) #f))
   (rule #\( (lambda (chr me) (set! *paren-depth* (+ 1 *paren-depth*))
		     (display chr *txi-output-port*) #t))
   (rule #\) (lambda (chr me) (set! *paren-depth* (+ -1 *paren-depth*))
		     (display chr *txi-output-port*) #t))
   (rule 'ref (lambda (cmd me)
		(let ((name (label->name (capture-argument))))
		  (cond ((positive? *paren-depth*)
			 (fprintf *txi-output-port* "@pxref{%s}" name))
			(else (fprintf *txi-output-port* "@ref{%s}" name)))
		  #f)))
   (rule '(proto rproto)
	 (lambda (cmd me . ruls)
	   (let* ((proc (capture-argument))
		  (args (fluid-let ((*tex-rules*
				     (append (apply rules ruls) *tex-rules*)))
			  (capture-braced-expression)))
		  (type (capture-braced-expression)))
	     (fprintf *txi-output-port* "@%s {%s} %s %s"
		      (if *entry-type* "deffnx" "deffn")
		      type proc args)
	     (case cmd ((proto) (define-label proc)))
	     (set! *entry-type* "deffn"))
	   #f)
	 (rule '(vari vri) postfix "1")
	 (rule '(varii vrii) postfix "2")
	 (rule '(variii vriii) postfix "3")
	 (rule 'vriv postfix "4")
	 (rule 'hyperi process-argument-with-rules "<" "1>")
	 (rule 'hyperii process-argument-with-rules "<" "2>")
	 (rule 'hyperiii process-argument-with-rules "<" "3>")
	 (rule 'hyperiv process-argument-with-rules "<" "4>")
	 (rule 'hypern process-argument-with-rules "<" "_n>"))
   (rule 'vproto
	 (lambda (cmd me)
	   (let* ((var (capture-argument))
		  (type (capture-braced-expression)))
	     (fprintf *txi-output-port* "@%s {%s} %s"
		      (if *entry-type* "defvrx" "defvr") type var)
	     (define-label var)
	     (set! *entry-type* "defvr"))
	   #f))
   (rule 'pproto			; like proto without args
	 (lambda (cmd me . ruls)
	   (fluid-let ((*tex-rules* (append (apply rules ruls) *tex-rules*)))
	     (let* ((proc (capture-argument))
		    (type (capture-braced-expression)))
	       (if *entry-type*
		   (fprintf *txi-output-port* "\\n@deffnx {%s} %s" type proc)
		   (fprintf *txi-output-port* "@deffn {%s} %s" type proc))
	       (extract-do-label proc)
	       (set! *entry-type* "deffn")
	       (process-argument))))
	 (rule 'tt ""))
   (rule 'obeyspaces process-argument-with-rules
	 (rule '(bs-bs #\newline #\%) "")
	 compress-spaces)
   (rule 'vspace
	 (lambda (cmd me)
	   (let ((arg (capture-argument)))
	     (set! arg (string->number (substring arg 0 (substring? "ex" arg))))
	     (fprintf *txi-output-port* "@sp %d" (abs arg))
	     #f)))
   (rule 'vskip
	 (lambda (cmd me)
	   (let* ((line (read-line *tex-input-port*))
		  (linl (string-length line)))
	     (do ((idx 0 (+ 1 idx)))
		 ((or (eqv? idx linl)
		      (not (char-whitespace? (string-ref line idx))))
		  (cond ((or (eqv? idx linl)
			     (not (or (substring? "ex" line)
				      (substring? "pt" line))))
			 (tex:error 'vskip-not-understood: line)))
		  (set! line (substring line idx
					(or (substring? "ex" line)
					    (substring? "pt" line))))))
	     (set! linl (string->number line))
	     (cond (linl (fprintf *txi-output-port* "@sp %d"
				  (inexact->exact linl)))
		   (else (tex:error 'vskip-number-not-understood: line))))
	   #\newline))
   (rule '(theevenhead headertitle)
	 (lambda (cmd me)
	   ;;(fprintf *txi-output-port* "@settitle %s\\n" (capture-argument))
	   (output-text-lines "@setchapternewpage on"
			      "@paragraphindent 0" "")
	   #t))
   (rule 'input tex:input)
   (rule 'setbox setbox)
   (rule 'copy copy-box)
   ;; R5RS additional symbols.
   (rule 'integerversion "5")
   (rule 'callcc "@t{call-with-current-continuation}")
   ;; These commands occur at beginning of lines; we treat as comments.
   (rule 'showboxdepth %)
   ;; \-commands{...} which turn into single-line comments.

   (rule '(newcommand pagestyle thispagestyle clearchapterstar topmargin
		      headsep textheight textwidth columnsep columnseprule
		      parskip parindent topsep oddsidemargin evensidemargin
		      addvspace renewcommand tocshrink def)
	 commentize
	 (rule 'type "\\\\type"))
   ))

(define t (lambda args (apply tex->txi args) (newline)))

'(begin
  (define r restart)
  (trace-all "rrrs2txi.scm")
  ;;(trace read-char char-whitespace?)
  (untrace rule rules read-bs-token string-whitespace?
	   ;;process-tex-input tex:close process-one
	   tex:warn tex:error tex:errwarn check-brace-depth!))
;;(trace encapsulate process-one process-tex-input tex:close)

(define go
  (let ((me (in-vicinity (program-vicinity) "rrrs2txi")))
    (lambda ()
      (do ((r 3 (+ 1 r))
	   (name "r3rs" (string-append "r" (number->string (+ 1 r)) "rs")))
	  ((or (> r 6) (file-exists? (string-append name ".tex")))
	   (cond ((> r 6)
		  (do ((found? #f)
		       (r 3 (+ 1 r))
		       (base "r3rs"
			     (string-append "r" (number->string (+ 1 r)) "rs")))
		      ((> r 6)
		       (if (not found?) (slib:error 'could-not-find "r?rs.tex"))
		       #t)
		    (let ((vic (sub-vicinity (user-vicinity) base)))
		      (cond ((file-exists?
			      (in-vicinity vic (string-append base ".tex")))
			     (set! found? #t)
			     (load me)
			     (cond ((tex->txi vic base)
				    (txi->info (user-vicinity) base))
				   ((begin (load me) (tex->txi vic base))
				    (txi->info (user-vicinity) base))))))))
		 ((begin (load me) (tex->txi name))
		  (txi->info (user-vicinity) base))
		 ((begin (load me) (tex->txi name))
		  (txi->info (user-vicinity) base))))))))

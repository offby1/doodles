;; Redefine all the functions that work with scheme ports (and by
;; implication, ports themselves) so that they read and write into a
;; (hidden) global variable, instead of an actual file.  This would be
;; useful if Scheme were running on a machine that had no file system.

;; I haven't yet redefined the `read' procedure, because to do that
;; (as far as I can tell), I'd have to write a whole parser for
;; Scheme, which would be a lot of work.

(require 'record)
(require 'filter)
(require 'dynamic-wind)

;; this function of no arguments displays a list of names of the
;; "files" that live in the global variable.  Naturally we'll define
;; the function later.
(define dir #f)

;; Save the filesystem to a real file, and restore it.
;; These functions both take no arguments.
(define save #f)
(define restore #f)

(let ()

  (define my-error
    (lambda args
      (let ((str    (car args))
            (rest   (cdr args)))
        (apply error
               (append (list (string-append "Eric sez: " str))
                       rest)))))

  (define (object->string obj machine-readable)

    ;; Gambit 2.7 lacks this R5RS procedure.
    (define (port? obj) (or (input-port? obj)
                            (output-port? obj)))

    (define (char->string c)
      (if (not machine-readable)
          (string c)
        (case c
          ((#\" #\\)
           (string-append "\\" (string c)))
          (else
           (string c)))))

    ;; Given '("hey" "you"), returns "hey you".
    (define (concat-with-spaces strs)
      (let loop ((strs strs)
                 (result ""))
        (cond  
         ((null? strs)
          result)
         ((null? (cdr strs))
          (string-append result (car strs)))
         (#t
          (loop (cdr strs)
                (string-append result (car strs) " "))))))

    (cond ((symbol? obj)
           (symbol->string obj))
          ((number? obj)
           (number->string obj))
          ((boolean? obj)
           (if obj "#T" "#F"))

          ((list? obj)
           (apply string-append (append (list "(")
                                        (list (concat-with-spaces (map (lambda (x) (object->string x machine-readable)) obj)))
                                        (list ")"))))
        
          ((vector? obj)
           (apply string-append (append (list "#(")
                                        (list (concat-with-spaces (map (lambda (x) (object->string x machine-readable)) (vector->list obj))))
                                        (list ")"))))

          ((pair? obj)
           (string-append "("
                          (object->string (car obj) machine-readable)
                          " . "
                          (object->string (cdr obj) machine-readable)
                          ")"))
          ((char? obj)
           (string-append
            (if machine-readable
                "#\\"
              "")
            (string obj)))
          ((string? obj)
           (apply string-append
                  ((lambda (arg)
                     (if machine-readable
                         (append
                          (list "\"")
                          arg
                          (list "\""))
                       arg))
                   (map char->string (string->list obj)))))

          ((port? obj)
           "#<some port>")
          ((procedure? obj)
           "#<some procedure>")
          (#t
           (my-error "What the heck is this? " obj))))

     ;; A stub, obviously ... if it were properly implemented, it would
     ;; prevent more than one thread from running the thunk at once.
  (define (call-with-monitor thunk) (thunk))
  
  (define eof-rtd    (make-record-type "end-of-file" '()))
  (define eof-object ((record-constructor eof-rtd)))

  (define input-rtd        (make-record-type "input-port" '(file-name contents chars-read)))
  (define input-ctor       (record-constructor input-rtd))
  (define input-mod        (record-modifier    input-rtd 'chars-read))
  (define input-content    (record-accessor    input-rtd 'contents))
  (define input-chars-read (record-accessor    input-rtd 'chars-read))

  (define (check-input-port thing) (if (not (input-port? thing)) (my-error "That ain't no input port:" thing)))

     ;; Always closed, since I can't think of what else to do.
  (define CIP (input-ctor #f #f 'closed))

     ;; return either CIP, or the open input port in the arg list.
  (define (input-port-from-arglist args)
    (if (> (length args) 1)
        (my-error "Wrong number of arguments")
        
      (let ((result 
             (if (null? args)
                 CIP
               (car args))))
        (check-input-port result)
        (if (ip-closed? result)
            (my-error "Open PORT expected"))
        result)))
     
  (define (at-end-of-file? ip) (= (input-chars-read ip) (string-length (input-content ip))))
  (define (ip-closed? ip) (eq? (input-chars-read ip) 'closed))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; An in-memory file system.

     ;; If named file already exists, overwrites it.
     ;; Returns an unspecified value.
  (define my-create-file #f)

     ;; if named file exists, returns its contents as a string; otherwise
     ;; returns #f.
  (define my-read-file #f)

  (define my-file-exists? #f)

     ;; Returns #f if no such file; #t otherwise
  (define my-delete-file #f)

  (let (
        ;; The "global" variable that holds all the files.  Each is a
        ;; pair of strings; the first string is the "name", and the
        ;; second is the "contents".
        (filesystem '())
        )

    (define (find-file name)  (assoc name filesystem))

    ;; To allow us to save and restore the "filesystem" to disk, we
    ;; need access to some of the standard R5Rs functions that we're
    ;; about to overwrite.  So we capture them here.
    (let ((save-fs-name  "c:/scheme/doodles/wish/fs")
          (call-with-output-file call-with-output-file)
          (call-with-input-file call-with-input-file)
          (write write))
      (set! save    (lambda () (call-with-output-file save-fs-name (lambda (p) (write filesystem p)))))
      (set! restore (lambda () (set! filesystem (call-with-input-file  save-fs-name read)))))

    (set! my-file-exists? (lambda (name) (and (find-file name) #t))) 

    (set! my-create-file (lambda (name contents)
                           (call-with-monitor
                            (lambda ()
                              (if (my-delete-file name)
                                  (display-many "Note: deleted file `"
                                                name
                                                "'" #\newline))
                              (set! filesystem (cons
                                                (cons name contents)
                                                filesystem))))))

    (set! my-delete-file
          (lambda (name)
            (call-with-monitor
             (lambda ()
               (if (my-file-exists? name)
                   (begin
                     (set! filesystem
                           (filter 
                            (lambda (some-file)
                              (not  
                                                                           
                               ;; Some files have names that aren't
                               ;; strings! in particular, the current
                               ;; output port goes into such a file. 

                               (and (string? (car some-file))
                                    (string=? (car some-file) name))))

                            filesystem))
                     #t)
                 #f))))) 

    (set! my-read-file (lambda (name)
                         (let ((f (find-file name)))
                           (and f (cdr f))))) 

    (set! dir (lambda () (map car filesystem))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (set! input-port?  (record-predicate input-rtd))

  (set! open-input-file (lambda (filename)
                          (let ((content (my-read-file filename)))
                            (if (not content)
                                (my-error "No such file:" filename))
                            (input-ctor filename content 0))))

  (set! close-input-port (lambda (ip) (input-mod ip 'closed)))
  
  (set! read-char (lambda args
                    (let ((ip (input-port-from-arglist args)))
                      
                      (if (at-end-of-file? ip)
                          eof-object
                        (let ((result (string-ref (input-content ip)
                                                  (input-chars-read ip))))
                          (input-mod ip (+ 1 (input-chars-read ip)))
                          result)))))

  (set! peek-char (lambda args
                    (let ((ip (input-port-from-arglist args)))
                      (check-input-port ip)
                      
                      (if (at-end-of-file? ip)
                          eof-object
                        (string-ref (input-content ip)
                                    (input-chars-read ip))))))

  (set! char-ready? (lambda (ip) (check-input-port ip) #t))

  (set! call-with-input-file (lambda (fn proc)
                               (let ((ip (open-input-file fn)))
                                 (if ip
                                     (let ((result (proc ip)))
                                       (close-input-port ip)
                                       result)
                                   (my-error "Can't open file" fn)))))
  (set! eof-object? (lambda (obj) ((record-predicate eof-rtd) obj)))
  (set! current-input-port (lambda () CIP))
  (set! with-input-from-file 
        ;; Note that we don't *need* to use `dynamic-wind' here,
        ;; because R5RS leaves unspecified our behavior if we escape
        ;; from this procedure.  But let's be nice, eh?
        (lambda (fn thunk)
          (let ((original-CIP CIP))
            (call-with-input-file fn
              (lambda (port)
                (dynamic-wind
                 (lambda () (set! CIP port))
                 (lambda () (thunk))
                 (lambda () (set! CIP original-CIP))))))))

  (let ()
    (define output-rtd        (make-record-type "output-port" '(file-name contents)))
    (define output-ctor       (record-constructor output-rtd))
    (define output-set-content! (record-modifier output-rtd 'contents))
    (define output-close     (lambda (op) (output-set-content! op 'this-port-is-closed!-dont-write-to-it)))
    (define output-fn        (record-accessor output-rtd 'file-name))
    (define output-content   (record-accessor output-rtd 'contents))

    (define (check-output-port thing) (if (not (output-port? thing)) (my-error "That ain't no output port:" thing)))

       ;; Writing to this port (as to any output port) has no effect
       ;; on the global "filesystem" until you close the port ... but
       ;; in this case, all that happens is we get a new file whose
       ;; name isn't a string, and is thus inaccessible.
    (define COP (output-ctor 'current-output-port ""))
       
       ;; return either COP, or the output port in the arg list.
       (define (output-port-from-arglist args)
         (let ((result (if (null? args)
                           COP
                         (car args))))
           (check-output-port result)
           result))
       
    (define append-to-open-output-port
      (lambda (str op)
        (output-set-content! op (string-append  (output-content op) str))))

    (set! output-port? (record-predicate output-rtd))

    (set! open-output-file  (lambda (filename) (output-ctor filename "")))
    (set! close-output-port (lambda (op)
                              (check-output-port op)
                              (my-create-file (output-fn op)
                                              (output-content op))
                              (output-close op)))

    (set! call-with-output-file (lambda (fn proc)
                                  (let ((op (open-output-file fn)))
                                    (if op
                                        (let ((result (proc op)))
                                          (close-output-port op)
                                          result)
                                      (my-error "Can't open file"
                                             fn)))))
    (set! current-output-port (lambda () COP))
    (set! with-output-to-file
          (lambda (fn thunk)
            (let ((original-COP COP))
              (call-with-output-file fn
                (lambda (port)
                  (dynamic-wind
                   (lambda () (set! COP port))
                   (lambda () (thunk))
                   (lambda () (set! COP original-COP))))))))

    (set! write-char (lambda (obj . args) (append-to-open-output-port (string obj)            (output-port-from-arglist args))))
    (set! write      (lambda (obj . args) (append-to-open-output-port (object->string obj #t) (output-port-from-arglist args))))
    (set! display    (lambda (obj . args) (append-to-open-output-port (object->string obj #f) (output-port-from-arglist args))))
    (set! newline    (lambda args         (append-to-open-output-port (string #\newline)      (output-port-from-arglist args))))))


(lambda ()
  ;; A cumbersome way to get a copy of the hidden variable.
  (define (filesystem)
    (define file->string
      (lambda (filename)
        (call-with-input-file
            filename 
          (lambda (port)
            (define loop
              (lambda (result)
                (if (eof-object? (peek-char port))
                    result
                  (loop (string-append result (string (read-char port)))))))
            (loop  "")))))
    (map (lambda (fn)
           (cons fn (file->string fn))) (dir))))

#!/usr/bin/guile -s
!#

(with-output-to-file "many-unicode-characters.html"
  (lambda ()
    (display 
     (string-append 
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
      "<html>\n"
      "  <head>\n"
      "    <meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\">\n"
      "    <title>Many Unicode Characters</title>\n"
      "  </head>\n"
      "  <body>\n"))

    (let loop ((x 0))
      (if (< x (expt 2 17))
          (begin
            (if (zero? (remainder x 128))
                (display (string-append
                          "<hr>\n<h1>" (number->string x) "</h1>")))
            
            (display (string-append "&#" (number->string x) "; "))
            (loop (+ x 1)))))
    (display "\n</html>\n")))
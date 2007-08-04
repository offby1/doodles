#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui tinyurl-tests)'
|#

(module tinyurl mzscheme
(require (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (lib "uri-codec.ss" "net")
         (only (lib "url.ss" "net")
               post-pure-port
               string->url)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt"))
               port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define (make-tiny-url url)
  (car
   ((sxpath '(html body (table 2) tr (td 2) p blockquote small a @ href *text*))
    (html->shtml
     (port->string
      (post-pure-port
       (string->url
        "http://tinyurl.com/create.php")
       (string->bytes/utf-8 (parameterize ((current-alist-separator-mode 'amp))
                              (alist->form-urlencoded `((url . ,url)))))

       ;; this works as is, but let us note for the record that the
       ;; "tinyurl creator" extension for Firefox
       ;; (https://addons.mozilla.org/en-US/firefox/addon/126) passes
       ;; a buttload more headers, namely

       ;; ("User-Agent", navigator.userAgent);
       ;; ("Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,video/x-mng,image/png,image/jpeg,image/gif;q=0.2,*/*;q=0.1");
       ;; ("Accept-Language", navigator.language);
       ;; ("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7");
       ;; ("Referer", "http://tinyurl.com/");

       (list "Content-Type: application/x-www-form-urlencoded")
                                                               ))))))

(define tinyurl-tests

  (test-suite
   "tinyurl"
   (test-case
    "photo.net"
    (check-regexp-match
    #rx"^http://tinyurl.com/.....$"
    (make-tiny-url "http://photo.net")))))

(provide (all-defined))
)
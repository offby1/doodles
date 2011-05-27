#lang racket

(require (lib "foreign.ss")
         (except-in (lib "contract.ss")
                    ->)
         (rename-in (lib "contract.ss")
                    [-> c->]))
(unsafe!)

(define openssl-crypto
  (case (system-type)
    [(windows)
     (ffi-lib "libeay32")]
    [else
     (ffi-lib "libcrypto")]))

(define EVP_SHA256
  (get-ffi-obj 'EVP_sha256 openssl-crypto
               (_fun -> _fpointer)))

(define HMAC-SHA256/raw
  (get-ffi-obj 'HMAC openssl-crypto
               (_fun [EVP_MD : _fpointer = (EVP_SHA256)]
                     [key : _bytes]
                     [key_len : _int = (bytes-length key)]
                     [data : _bytes]
                     [data_len : _int = (bytes-length data)]
                     [md : _int = 0]
                     [md_len : _int = 0]
                     ->
                     _pointer)))

(define (HMAC-SHA256 key data)
                                        ; It returns the same pointer always
  (bytes-copy
                                        ; A SHA256 is 32 bytes, including 0s
   (make-sized-byte-string (HMAC-SHA256/raw key data) 32)))

(provide/contract
 [HMAC-SHA256 (bytes? bytes? . c-> . bytes?)])
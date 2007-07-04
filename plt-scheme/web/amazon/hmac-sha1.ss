(module hmac-sha1 mzscheme
  (require (lib "foreign.ss")
           (all-except (lib "contract.ss")
                       ->)
           (rename (lib "contract.ss")
                   c-> ->))
  (unsafe!)

  (define openssl-crypto
    (case (system-type)
      [(windows)
       (ffi-lib "libeay32")]
      [else
       (ffi-lib "libcrypto")]))

  (define EVP_SHA1
    (get-ffi-obj 'EVP_sha1 openssl-crypto
                 (_fun -> _fpointer)))

  (define (HMAC-SHA1 key data)
    ;; we need to call bytes-copy because hmac-sha1-internal always
    ;; returns the same pointer (to static storage).
    (bytes-copy

     ;; we need to call make-sized-byte-string because otherwise our
     ;; return value would get truncated at the first 0 byte.  This is
     ;; an insidious problem, since most return values don't _have_ a
     ;; 0 byte, and thus omitting this call will _appear_ to work,
     ;; most of the time.
     (make-sized-byte-string
      (hmac-sha1-internal key data)
      20)))

  (define hmac-sha1-internal
    (get-ffi-obj 'HMAC openssl-crypto
                 (_fun [EVP_MD : _fpointer = (EVP_SHA1)]
                       [key : _bytes]
                       [key_len : _int = (bytes-length key)]
                       [data : _bytes]
                       [data_len : _int = (bytes-length data)]
                       [md : _int = 0]
                       [md_len : _int = 0]
                       ->
                       _bytes)))

  (provide/contract
   [HMAC-SHA1 (bytes? bytes? . c-> . bytes?)]))
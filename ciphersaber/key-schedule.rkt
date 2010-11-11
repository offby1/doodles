#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (only-in srfi/43 vector-swap!)
         "misc.rkt")

(provide permute-state-from-key)
(define (permute-state-from-key keybytes)
  (define state  (list->vector (build-list 256 values)))
  (let loop ([i 0]
             [j 0])
    (when (< i (vector-length state))
      (let* ([Si (vector-ref state i)]
             [j (m+ j Si (bytes-ref keybytes (modulo i (bytes-length keybytes))))])
        (vector-swap! state i j)
        (loop (add1 i) j))))
  state)


(require rackunit rackunit/text-ui)

(define-test-suite permute-tests
  (check-equal?
   (permute-state-from-key #"x")
   ;; Got this value by slightly hacking, then running,
   ;; http://code.activestate.com/recipes/576736-rc4-arc4-arcfour-algorithm/

   #(19 2 41 116 194 57 179 80 141 17 95 226 10 235 142 100 211 222
137 30 138 55 33 139 32 117 131 154 187 242 78 150 125 72 166 74 28
132 91 193 97 107 152 4 13 183 237 134 115 232 208 207 37 202 0 223
181 12 199 153 99 195 234 225 89 103 157 68 43 82 190 94 66 11 42 161
230 200 83 220 73 171 25 90 101 8 244 249 159 129 69 45 212 51 40 88
63 168 227 155 145 143 21 64 196 165 67 241 31 217 146 151 60 253 36
185 123 120 14 248 70 104 160 209 20 58 113 246 62 162 156 255 61 46
203 221 106 110 6 84 76 205 5 182 176 81 173 15 238 136 229 79 239 29
114 169 163 39 27 108 215 175 24 158 75 186 38 147 130 213 77 112 56
218 59 191 224 50 219 1 198 164 254 144 204 23 54 9 102 148 189 135 71
109 105 245 174 210 178 240 16 122 184 126 197 92 96 124 180 214 26
128 243 127 247 44 49 53 34 216 228 177 149 121 3 35 22 192 167 201 65
206 250 236 85 233 48 231 98 251 7 93 52 118 172 47 119 87 111 140 133
86 188 170 252 18))

  (check-equal?
   ;; http://ciphersaber.gurus.org/faq.html
   (permute-state-from-key
    (bytes-append
     #"asdfg"
     (apply bytes (hex->integers"6f6d0babf3aa67190315"))))

   #(176 32 49 160 15 112 58 8 186 19 50 161 60 17 82 153
         37 141 131 127 59 2 165 103 98 53 9 57 41 150 174 64
         36 62 191 154 44 136 149 158 226 113 230 227 247 155 221 34
         125 20 163 95 128 219 1 181 201 146 88 204 213 80 143 164
         145 234 134 248 100 77 188 235 76 217 194 35 75 99 126 92
         243 177 52 180 83 140 198 42 151 18 91 33 16 192 101 48
         97 220 114 110 124 72 139 218 142 118 81 84 31 29 195 68
         209 172 200 214 93 240 61 22 206 123 152 7 203 10 119 171
         79 250 109 137 199 167 11 104 211 129 208 216 178 207 242 162
         30 120 65 115 87 170 47 69 244 212 45 85 73 222 225 185
         63 0 179 210 108 245 202 46 96 148 51 173 24 182 89 116
         3 67 205 94 231 23 21 13 169 215 190 241 228 132 252 4
         233 56 105 26 12 135 223 166 238 229 246 138 239 54 5 130
         159 236 66 175 189 147 193 237 43 40 117 157 86 249 74 27
         156 14 133 251 196 187 197 102 106 39 232 255 121 122 253 111
         90 38 55 70 184 78 224 25 6 107 168 254 144 28 183 71)))

(define-test-suite all-tests
  permute-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))

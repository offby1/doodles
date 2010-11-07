#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (only-in srfi/43 vector-swap!)
         "cruft.rkt")

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
   #(19 2 41 116 194 57 179 80 141 17 95 226 10 235 142 100 211 222 137 30 138 55 33 139 32 117 131 154 187 242 78 150 125 72 166 74 28 132 91 193 97 107 152 4 13 183 237 134 115 232 208 207 37 202 0 223 181 12 199 153 99 195 234 225 89 103 157 68 43 82 190 94 66 11 42 161 230 200 83 220 73 171 25 90 101 8 244 249 159 129 69 45 212 51 40 88 63 168 227 155 145 143 21 64 196 165 67 241 31 217 146 151 60 253 36 185 123 120 14 248 70 104 160 209 20 58 113 246 62 162 156 255 61 46 203 221 106 110 6 84 76 205 5 182 176 81 173 15 238 136 229 79 239 29 114 169 163 39 27 108 215 175 24 158 75 186 38 147 130 213 77 112 56 218 59 191 224 50 219 1 198 164 254 144 204 23 54 9 102 148 189 135 71 109 105 245 174 210 178 240 16 122 184 126 197 92 96 124 180 214 26 128 243 127 247 44 49 53 34 216 228 177 149 121 3 35 22 192 167 201 65 206 250 236 85 233 48 231 98 251 7 93 52 118 172 47 119 87 111 140 133 86 188 170 252 18)
   ))

(define-test-suite all-tests
  permute-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))

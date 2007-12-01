;; From: "Matt Jadud" <jadudm@gmail.com>

(require (lib "class.ss")
         (lib "mred.ss" "mred"))

;; Updatable provides the mechanisms
;; for registering and notifying listeners.
(define updatable%
  (class object%
    (define listeners (make-hash-table))

    (define/public (add-listener o)
      (hash-table-put! listeners (gensym) o))

    (define/public (notify-listeners)
      (hash-table-for-each
       listeners (lambda (k v)
                   (send v update))))
    (super-new)))

;; The model% inherits from updatable%, and
;; represents the state of our GUI. I'm not sure if
;; all of this code belongs in the "model"...
(define model%
  (class updatable%
    (inherit add-listener notify-listeners)
    (define button-states (make-hash-table))

    (define/public (add-button b state)
      (hash-table-put! button-states (send b get-id) state)
      (add-listener b))

    (define/public (flip-button b)
      (hash-table-put!
       button-states
       (send b get-id)
       (not (hash-table-get button-states (send b get-id))))
      (notify-listeners))

    (define/public (get-button-state b)
      (if (is-a? b observer-button%)
          (hash-table-get button-states (send b get-id))
          (hash-table-get button-states b)))

    (super-new)))

;; An observer-button% needs to extend
;; button and be updatable.
(define observer-button%
  (class button%
    (init-field (model (void))
                (id (gensym)))
    (inherit enable)

    (define/public (update)
      (if (send model get-button-state id)
          (enable #t)
          (enable #f)))

    (define/public (get-id) id)

    (super-new)
    ))

(define m (new model%))

(define frame (new frame% (label "Constraint Testing")))

(define button
 (new observer-button%
      (parent frame)
      (model m)
      (label "Click me!")
      (callback (lambda (item event)
                  (message-box "OK" "Now what?")))))

(send m add-button button #t)

(define radio-box
 (new radio-box% (label "Pick one")
      (choices '("Disable that button there" "Let it be enabled"))
      (parent frame)
      (callback
       (lambda (w e)
         ;; Perhaps the actual choice could be involved here?
         (send m flip-button button)
      ))))

(send frame show #t)


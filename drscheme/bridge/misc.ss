(module misc mzscheme
  (provide
   index
   *compass-directions*)
  (define (index item seq )
    (- (length seq) (length (member item seq))))
  (define *compass-directions*
     `(north east south west)))

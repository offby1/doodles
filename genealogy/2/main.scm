(load "lines.scm")                      ; defines thunk `x' -- reads a
					; gedcom file and
                                        ; returns a big fat list

(load "gedcom-record.scm")


(load "kdp.scm")                        ; defines a record that
                                        ; represents one line in the
                                        ; gedcom file

(load "blood.scm")                      ; code to walk through a
                                        ; family tree and return some
                                        ; sort of collection of those
                                        ; people who are blood
                                        ; relatives of a particular
                                        ; person 

(require 'sort)
(require 'filter)
(require 'format)

(display
 (let ()
   ;; Like the common-lisp function `union', but uses `member' instead
   ;; of `memq'.
   (define (union seq1 seq2)
     (let loop ((seq1 seq1)
                (result seq2))
       (if (null? seq1)
           result
         (loop (cdr seq1)
               (if (not (member (car seq1) result))
                   (cons (car seq1) result)
                 result)))))

   (define (uniquify seq)
     (cond
      ((null? seq)
       seq)
      ((null? (cdr seq))
       seq)
      ((equal? (car seq)
               (cadr seq))
       (uniquify (cdr seq)))
      (#t
       (cons (car seq) (uniquify (cdr seq))))))
   (define (sort-strings seq) (sort seq string<?))
   (define Out-of-Sight
     (list
      "Steven Soderbergh"
      "George Clooney"
      "Jennifer Lopez"
      "Ving Rhames"
      "Don Cheadle"
      "Dennis Farina"
      "Albert Brooks"
      "Steve Zahn"
      "Luis Guzmán"
      "Catherine Keener"
      "Isaiah Washington"
      "Keith Loneker"
      "Nancy Allen"
      "Jimmie Belle"
      "Donna Frenzel"
      "Wendell B. Harris Jr."
      "Samuel L. Jackson"
      "Michael Keaton"
      "Elmore Leonard"
      "Scott Frank"
      "Danny DeVito"
      "John Hardy (III)"
      "Michael Shamberg"
      "Stacey Sher"
      "Barry Sonnenfeld"
      ))

   (define Jackie-Brown
     (list
      "Quentin Tarantino"
      "Pam Grier"
      "Samuel L. Jackson"
      "Robert Forster"
      "Bridget Fonda"
      "Michael Keaton"
      "Robert De Niro"
      "Michael Bowen"
      "Lisa Gay Hamilton"
      "Tom 'Tiny' Lister Jr."
      "Hattie Winston"
      "Denise Crosby"
      "Sid Haig"
      "Aimee Graham"
      "Chris Tucker (I)"
      "Ellis E. Williams"
      "Tangie Ambrose"
      "T'Keyah 'Crystal' Keymáh"
      "Venessia Valentino"
      "Diana Uribe"
      "Renee Kelly (II)"
      "Elizabeth McInerney"
      "Colleen Mayne"
      "Laura Lovelace"
      "Quentin Tarantino"
      "Elmore Leonard"
      "Quentin Tarantino"
      "Lawrence Bender"
      "Richard N. Gladstein"
      "Paul Hellerman"
      "Elmore Leonard"
      "Bob Weinstein"
      "Harvey Weinstein"
      ))
   (define Get-Shorty
     (list
      "Barry Sonnenfeld"
      "John Travolta"
      "Gene Hackman"
      "Rene Russo"
      "Danny DeVito"
      "Dennis Farina"
      "Delroy Lindo"
      "James Gandolfini"
      "Jon Gries"
      "Renee Props"
      "David Paymer"
      "Martin Ferrero"
      "Miguel Sandoval (I)"
      "Jacob Vargas"
      "Linda Hart"
      "Bobby Slayton"
      "Ron Karabatsos"
      "Alison Waddell"
      "Amber Waddell"
      "John Cothran Jr."
      "Jack Conley"
      "Bernard Hocke"
      "Big Daddy Wayne"
      "Xavier Montalvo"
      "Carlease Burke"
      "Vito Scotti"
      "Rino Piccolo"
      "Alfred Dennis"
      "Ralph Manza"
      "Zed Frizzelle"
      "Harry Victor"
      "Patrick Breen"
      "Barry Sonnenfeld"
      "Donna W. Scott"
      "Zack Phifer"
      "Greg Goossen"
      "Stephanie Kemp"
      "Rebeca Arthur"
      "Jeffrey J. Stephan"
      "Ernest 'Chili' Palmer"
      "David Groh"
      "Harvey Keitel"
      "Penny Marshall"
      "Bette Midler"
      "Alex Rocco (I)"
      "Elmore Leonard"
      "Scott Frank"
      "Danny DeVito"
      "Graham Place"
      "Susan Ringo"
      "Michael Shamberg"
      "Stacey Sher"
      "Barry Sonnenfeld"
      "Quentin Tarantino"
      ))
   (define Pulp-Fiction
     (list
      "Quentin Tarantino"
      "Tim Roth"
      "Amanda Plummer"
      "Laura Lovelace"
      "John Travolta"
      "Samuel L. Jackson"
      "Phil LaMarr"
      "Frank Whaley"
      "Burr Steers"
      "Bruce Willis"
      "Ving Rhames"
      "Paul Calderon"
      "Bronagh Gallagher"
      "Rosanna Arquette"
      "Eric Stoltz"
      "Uma Thurman"
      "Jerome Patrick Hoban"
      "Michael Gilden"
      "Gary Shorelle"
      "Susan Griffiths"
      "Eric Clark"
      "Joseph Pilato"
      "Brad Parker"
      "Steve Buscemi"
      "Lorelei Leslie"
      "Emil Sitka"
      "Brenda Hillhouse"
      "Christopher Walken"
      "Chandler Lindauer"
      "Sy Sher"
      "Robert Ruth"
      "Rich Turner"
      "Angela Jones (I)"
      "Don Blakely"
      "Carl Allen"
      "Maria de Medeiros"
      "Karen Maruyama"
      "Kathy Griffin"
      "Venessia Valentino"
      "Linda Kaye"
      "Duane Whitaker"
      "Peter Greene (I)"
      "Stephen Hibbert"
      "Alexis Arquette"
      "Quentin Tarantino"
      "Harvey Keitel"
      "Julia Sweeney"
      "Lawrence Bender"
      "Dick Miller (I)"
      "Roger Avary"
      "Quentin Tarantino"
      "Quentin Tarantino"
      "Lawrence Bender"
      "Danny DeVito"
      "Richard N. Gladstein"
      "Michael Shamberg"
      "Stacey Sher"
      "Bob Weinstein"
      "Harvey Weinstein"
      ))
   (let ()


     ;; Cast and crew of some movies that I like.  From us.imdb.com


     (define everyone (union (union (union (uniquify (sort-strings Out-of-Sight))
                                           (uniquify (sort-strings Get-Shorty)))
                                    (uniquify (sort-strings Pulp-Fiction)))
                             (uniquify (sort-strings Jackie-Brown))))

     (define (bool->integer b) (if b 1 0))

     (define number-of-movies (lambda (person)
                                (apply + (map (lambda (cast-list)
                                                (bool->integer (member person cast-list)))
                                              (list Out-of-Sight Get-Shorty Jackie-Brown Pulp-Fiction)))))

     ;; All the people who worked on more than two of the four movies
     (filter 
      (lambda (number-name-pair) (> (car number-name-pair) 2))
      (map (lambda (person)
             (cons (number-of-movies person)
                   person)) everyone)))))


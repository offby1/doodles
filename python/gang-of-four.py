import functools
import pprint


# Cast and crew of some movies that I like.  From us.imdb.com
Out_of_Sight = set(
    [
        "Nancy Allen",
        "Jimmie Belle",
        "Albert Brooks",
        "Don Cheadle",
        "George Clooney",
        "Danny DeVito",
        "Dennis Farina",
        "Scott Frank",
        "Donna Frenzel",
        "Luis Guzmán",
        "John Hardy (III)",
        "Wendell B. Harris Jr.",
        "Samuel L. Jackson",
        "Michael Keaton",
        "Catherine Keener",
        "Elmore Leonard",
        "Keith Loneker",
        "Jennifer Lopez",
        "Ving Rhames",
        "Michael Shamberg",
        "Stacey Sher",
        "Steven Soderbergh",
        "Barry Sonnenfeld",
        "Isaiah Washington",
        "Steve Zahn",
    ]
)

Jackie_Brown = set(
    [
        "Tangie Ambrose",
        "Lawrence Bender",
        "Michael Bowen",
        "Denise Crosby",
        "Robert De Niro",
        "Bridget Fonda",
        "Robert Forster",
        "Richard N. Gladstein",
        "Aimee Graham",
        "Pam Grier",
        "Sid Haig",
        "Lisa Gay Hamilton",
        "Paul Hellerman",
        "Samuel L. Jackson",
        "Michael Keaton",
        "Renee Kelly (II)",
        "T'Keyah 'Crystal' Keymáh",
        "Elmore Leonard",
        "Tom 'Tiny' Lister Jr.",
        "Laura Lovelace",
        "Colleen Mayne",
        "Elizabeth McInerney",
        "Quentin Tarantino",
        "Chris Tucker (I)",
        "Diana Uribe",
        "Venessia Valentino",
        "Bob Weinstein",
        "Harvey Weinstein",
        "Ellis E. Williams",
        "Hattie Winston",
    ]
)

Get_Shorty = set(
    [
        "Rebeca Arthur",
        "Patrick Breen",
        "Carlease Burke",
        "Jack Conley",
        "John Cothran Jr.",
        "Danny DeVito",
        "Alfred Dennis",
        "Dennis Farina",
        "Martin Ferrero",
        "Scott Frank",
        "Zed Frizzelle",
        "James Gandolfini",
        "Greg Goossen",
        "Jon Gries",
        "David Groh",
        "Gene Hackman",
        "Linda Hart",
        "Bernard Hocke",
        "Ron Karabatsos",
        "Harvey Keitel",
        "Stephanie Kemp",
        "Elmore Leonard",
        "Delroy Lindo",
        "Ralph Manza",
        "Penny Marshall",
        "Bette Midler",
        "Xavier Montalvo",
        "Ernest 'Chili' Palmer",
        "David Paymer",
        "Zack Phifer",
        "Rino Piccolo",
        "Graham Place",
        "Renee Props",
        "Susan Ringo",
        "Alex Rocco (I)",
        "Rene Russo",
        "Miguel Sandoval (I)",
        "Donna W. Scott",
        "Vito Scotti",
        "Michael Shamberg",
        "Stacey Sher",
        "Bobby Slayton",
        "Barry Sonnenfeld",
        "Jeffrey J. Stephan",
        "Quentin Tarantino",
        "John Travolta",
        "Jacob Vargas",
        "Harry Victor",
        "Alison Waddell",
        "Amber Waddell",
        "Big Daddy Wayne",
    ]
)

Pulp_Fiction = set(
    [
        "Carl Allen",
        "Alexis Arquette",
        "Rosanna Arquette",
        "Roger Avary",
        "Lawrence Bender",
        "Don Blakely",
        "Steve Buscemi",
        "Paul Calderon",
        "Eric Clark",
        "Maria de Medeiros",
        "Danny DeVito",
        "Bronagh Gallagher",
        "Michael Gilden",
        "Richard N. Gladstein",
        "Peter Greene (I)",
        "Kathy Griffin",
        "Susan Griffiths",
        "Stephen Hibbert",
        "Brenda Hillhouse",
        "Jerome Patrick Hoban",
        "Samuel L. Jackson",
        "Angela Jones (I)",
        "Linda Kaye",
        "Harvey Keitel",
        "Phil LaMarr",
        "Lorelei Leslie",
        "Chandler Lindauer",
        "Laura Lovelace",
        "Karen Maruyama",
        "Dick Miller (I)",
        "Brad Parker",
        "Joseph Pilato",
        "Amanda Plummer",
        "Ving Rhames",
        "Tim Roth",
        "Robert Ruth",
        "Michael Shamberg",
        "Stacey Sher",
        "Sy Sher",
        "Gary Shorelle",
        "Emil Sitka",
        "Burr Steers",
        "Eric Stoltz",
        "Julia Sweeney",
        "Quentin Tarantino",
        "Uma Thurman",
        "John Travolta",
        "Rich Turner",
        "Venessia Valentino",
        "Christopher Walken",
        "Bob Weinstein",
        "Harvey Weinstein",
        "Frank Whaley",
        "Duane Whitaker",
        "Bruce Willis",
    ]
)

all_the_movies = [Out_of_Sight, Jackie_Brown, Pulp_Fiction, Get_Shorty]
everyone = functools.reduce(lambda a, b: a.union(b), all_the_movies)


def number_of_movies(person):
    return sum([1 if person in movie else 0 for movie in all_the_movies])


def hard_workers():
    for p in everyone:
        n = number_of_movies(p)
        if n > 2:
            yield p, n


pprint.pprint(list(hard_workers()))

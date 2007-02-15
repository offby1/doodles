# -*-pir-*-
.include  "cclass.pasm"

.sub 'bag_init' :main
        .local pmc silly
        silly = make_bag ("dog!DIG")
        bag_dump (silly)

        silly = make_bag ("Kansas!")
        bag_dump (silly)
.end

.sub 'bag_dump'
        .param pmc b
        print b
        print "\n"
.end

.sub 'make_bag'
        .param string arg
        .local pmc rv
        .local int chars_examined
        .local int len
        .local string char
        .local int isalpha

        new rv, .BigInt
        rv = 1
        chars_examined = 0
        length len, arg

next:   
        if chars_examined == len goto done
        substr char, arg, chars_examined, 1
        downcase char
        is_cclass isalpha, .CCLASS_ALPHABETIC, char, 0
        unless isalpha goto skip

                                # BUGBUG -- obviously this isn't correct.
        rv = rv * 2

skip:   
        inc chars_examined
        goto next

done:   
        
        .return(rv)
                         
.end


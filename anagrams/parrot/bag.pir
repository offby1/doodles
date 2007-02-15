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
        .local pmc iterator
        .local string char
        .local int val
        iter iterator, b
        if iterator goto next_key
        print "No iterator?\n"
        goto cleanup

next_key:
        shift char, iterator
        print char
        print ": "

        val = b[char]
        print val
        print " "

        if iterator goto next_key
cleanup:
        print "\n"
.end

.sub 'make_bag'
        .param string arg
        .local pmc rv
        .local int chars_examined, len
        .local string char
        .local int isalpha

        new rv, .Hash
        chars_examined = 0
        length len, arg

next:   
        if chars_examined == len goto done
        substr char, arg, chars_examined, 1
        downcase char
        is_cclass isalpha, .CCLASS_ALPHABETIC, char, 0
        unless isalpha goto skip
        I0 = rv[char]
        inc I0
        rv[char] = I0

skip:   
        inc chars_examined
        goto next

done:   
        .return(rv)
                         
.end


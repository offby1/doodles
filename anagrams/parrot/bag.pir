# -*-pir-*-
.sub 'bag_init' :main
        .local pmc silly, other
        new silly, .Hash
        set silly["d"], 1
        set silly["o"], 1
        set silly["g"], 1

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
        end

next_key:
        shift char, iterator
        print char
        print ": "

        val = b[char]
        print val
        print "\n"

        if iterator goto next_key
.end

.sub 'make_bag'
        .param string arg
        .local pmc rv
        .local int chars_examined, len
        .local string char

        new rv, .Hash
        chars_examined = 0
        length len, arg

next:   
        if chars_examined == len goto done
        substr char, arg, chars_examined, 1
        I0 = rv[char]
        inc I0
        rv[char] = I0

        print chars_examined
        print ": "
        print char
        print "\n"
        inc chars_examined
        goto next

done:   
        .return(rv)
                         
.end

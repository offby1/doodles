# -*-pir-*-
.sub 'bag_init' :main
        new P0, .Hash
        set P0["d"], 1
        set P0["o"], 1
        set P0["g"], 1

        bag_dump (P0)
.end

.sub 'bag_dump'
        .param pmc b
        iter P1, b
        if P1 goto next_key
        print "No iterator?\n"
        end

next_key:
        shift P2, P1
        print P2
        print ": "

        I0 = b[P2]
        print I0
        print "\n"

        if P1 goto next_key
        end
.end

.sub 'make_bag'
        .param string arg
        new P0, .Hash
        .local int chars_examined, len
        .local string char
        chars_examined = 0
        length len, arg

next:   
        if chars_examined == len goto done
        substr char, arg, chars_examined, 1
        I0 = P0[char]
        inc I0
        P0[char] = I0

        print chars_examined
        print ": "
        print char
        print "\n"
        inc chars_examined
        goto next

done:   
        .return(P0)
                         
.end

# -*-pir-*-
.sub 'bag_init' :main
        new P0, .Hash
        set P0["d"], 1
        set P0["o"], 1
        set P0["g"], 1

        iter P1, P0
        if P1 goto fiddle
        print "No iterator?\n"
        end

fiddle: 
                                # Just for fun, increment a value.
        I0 = P0["o"]
        inc I0
        set  P0["o"], I0

next_key:
        shift P2, P1
        print P2
        print ": "

        I0 = P0[P2]
        print I0
        print "\n"

        if P1 goto next_key
        make_bag("zippy")
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
        print chars_examined
        print ": "
        print char
        print "\n"
        inc chars_examined
        goto next

done:   
        .return(0)
                         
.end

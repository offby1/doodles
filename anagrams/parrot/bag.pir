# -*-pir-*-
.sub 'bag' :main
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
        end
.end

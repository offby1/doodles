# -*-pir-*-
.sub 'bag' :main
        load_bytecode 'dumper.pir'
        load_bytecode 'PGE/Dumper.pir'

        new P0, .Hash
        set P0["d"], 1
        set P0["o"], 1
        set P0["g"], 1
        _dumper(P0)
        iter P1, P0
        _dumper(P1)

        if P1 goto next_key
        print "No iterator?\n"
        end
        
next_key:
        shift P2, P1
        _dumper (P2)

        if P1 goto next_key
        end
.end

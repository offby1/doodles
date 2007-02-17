.sub 'main' :main
        .param pmc args
        load_bytecode 'dict.pir'
        load_bytecode 'bag.pir'
        load_bytecode 'dumper.pir'
        .local string input
        .local BigInt ibag
        .local pmc dict
        _dumper (args)
        input = shift args
        _dumper (args)
        join input, " ", args
        print "input is `"
        print input
        print "'\n"
        bag_init()
        ibag = make_bag(input)
        _dumper(ibag)
        input = ibag
        say input
        #dict = snarf_dict ()
.end

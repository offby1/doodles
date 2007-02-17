.sub 'main' :main
        .param pmc args
        load_bytecode 'dict.pir'
        load_bytecode 'bag.pir'
        load_bytecode 'dumper.pir'
        .local string input
        .local BigInt ibag
        .local pmc dict

        input = shift args
        join input, " ", args



        bag_init()
        ibag = make_bag(input)


        input = ibag
        dict = snarf_dict ()
        anagrams (ibag, dict)
.end

.sub 'anagrams'
        .param BigInt input_bag
        .param pmc dict
        .local pmc iterator
        new iterator, .Iterator, dict
next_entry:     
        unless iterator goto done
        .local pmc one_entry
        .local BigInt entry_bag
        .local BigInt smaller_bag
        one_entry = shift iterator
        entry_bag = shift one_entry

        smaller_bag = subtract_bags (input_bag, entry_bag)
        unless smaller_bag goto next_entry

        print "Top, bottom, diff:\n"
        print input_bag
        print "; "
        print entry_bag
        print "; "
        print smaller_bag
        print "\n"

        goto next_entry
done:   
.end

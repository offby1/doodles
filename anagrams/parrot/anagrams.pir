## -*-pir-*-
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
        .local pmc result
        result = anagrams (ibag, dict)
        _dumper (result)
.end

.sub 'anagrams'
        .param BigInt input_bag
        .param pmc dict
        .local pmc iterator
        .local pmc rv
        new iterator, .Iterator, dict
        new rv, .ResizablePMCArray
next_entry:     
        unless iterator goto done
        .local pmc one_entry
        .local BigInt entry_bag
        .local BigInt smaller_bag
        one_entry = shift iterator
        entry_bag = shift one_entry

        smaller_bag = subtract_bags (input_bag, entry_bag)
        unless smaller_bag goto next_entry
        unless smaller_bag == 1 goto recur
        
        .local pmc words_it
        .local pmc list_of_one_word
        new words_it, .Iterator, one_entry
next_word:      
        unless words_it goto done
        new list_of_one_word, .ResizableStringArray
        .local string one_word
        shift one_word, words_it
        push list_of_one_word, one_word
        push rv, list_of_one_word
        goto next_word

        print "Top, bottom, diff:\n"
        print input_bag
        print "; "
        print entry_bag
        print "; "
        print smaller_bag
        print "\n"

recur:  
        goto next_entry
done:
        .return (rv)
.end

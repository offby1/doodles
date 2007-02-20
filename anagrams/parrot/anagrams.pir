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
        .local pmc iterator
        new iterator, .Iterator, dict
        result = anagrams (ibag, iterator, 0)
        print "OK, here's the final result:"
        _dumper (result)
.end

.sub 'anagrams'
        .param BigInt input_bag
        .param pmc dict_it
        .param int to_skip
        .local pmc rv
        new rv, .ResizablePMCArray
        new dict_it, .Iterator, dict_it
        print "input_bag is "
        print input_bag
        print "\n"
next_entry:     
        unless dict_it goto done
        .local pmc one_entry
        .local BigInt entry_bag
        .local BigInt smaller_bag
        shift one_entry, dict_it
        clone one_entry, one_entry
        _dumper (one_entry, "one_entry")
        entry_bag = shift one_entry
        smaller_bag = subtract_bags (input_bag, entry_bag)

        unless smaller_bag == 0 goto nonzero
        print "subtracting failed -- input_bag "
        print input_bag
        print " doesn't contain entry_bag "
        print entry_bag
        print "\n"
        goto next_entry
nonzero:        
        unless smaller_bag == 1 goto recur
        print "subtracting yielded 1 -- input_bag "
        print input_bag
        print " equals entry_bag "
        print entry_bag
        print "\n"

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
        print "subtracting yielded > 1 -- input_bag "
        print input_bag
        print " contains entry_bag "
        print entry_bag
        print " with leftover of "
        print smaller_bag
        print "\n"
        .local pmc from_smaller_bag
        from_smaller_bag = anagrams (smaller_bag, dict_it, to_skip)
        unless from_smaller_bag goto next_entry
        .local pmc combined
        combined = combine (one_entry, from_smaller_bag)
        push rv, combined
        goto next_entry
done:
        .return (rv)
.end

.sub 'combine'
        .param pmc words
        .param pmc in_anagrams
        .local pmc rv
        .local pmc anagrams
        print "combine: "
        _dumper (words, "words")
        _dumper (in_anagrams, "in_anagrams")
        new rv, .ResizablePMCArray
next_word:
        unless words goto cleanup
        clone anagrams, in_anagrams
        .local string one_word
        shift one_word, words
        .local pmc one_anagram
next_anagram:   
        unless anagrams goto next_word
        shift one_anagram, anagrams
        clone one_anagram, one_anagram
        unshift one_anagram, one_word
        push rv, one_anagram
        goto next_anagram
cleanup:
        .return (rv)
.end

.sub 'test_combine'
        load_bytecode 'dumper.pir'
        .local pmc anagrams
        new anagrams, .ResizablePMCArray
        .local pmc temp_list
        new temp_list, .ResizableStringArray
        push temp_list, "foo"
        push temp_list, "bar"
        push temp_list, "baz"
        push anagrams, temp_list
        new temp_list, .ResizableStringArray
        push temp_list, "zap"
        .local pmc result
        result = combine (temp_list, anagrams)
        _dumper (result)
        print "\n\n"
        new temp_list, .ResizableStringArray
        push temp_list, "zip"
        push temp_list, "zap"
        push temp_list, "zop"
        result = combine (temp_list, anagrams)
        _dumper (result)
        print "\n\n"
.end

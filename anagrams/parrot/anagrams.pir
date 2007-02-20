## -*-pir-*-
.sub 'main'
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
        result = anagrams (ibag, dict, 0, 0)
        _dumper (result)
.end

.sub 'anagrams'
        .param BigInt input_bag
        .param pmc dict
        .param int to_skip
        .param int trace_level
        .local pmc rv
        new rv, .ResizablePMCArray
next_entry:     
        if dict == to_skip goto done
        .local pmc one_entry
        .local BigInt entry_bag
        .local BigInt smaller_bag
        one_entry = dict[to_skip]
        clone one_entry, one_entry
        inc to_skip
        entry_bag = shift one_entry
        smaller_bag = subtract_bags (input_bag, entry_bag)

        if smaller_bag == 0 goto next_entry
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
        .local pmc from_smaller_bag
        from_smaller_bag = anagrams (smaller_bag, dict, to_skip, 5)
        unless from_smaller_bag goto next_entry
        print "Pretend I'm combining this: "
        print smaller_bag
        print " with this: "
        print from_smaller_bag
        print "\n"
        goto next_entry
done:
        .return (rv)
.end

.sub 'combine'
        .param pmc words
        .param pmc in_anagrams
        .local pmc rv
        .local pmc anagrams
        new rv, .ResizablePMCArray
        _dumper (words, "words")
        _dumper (in_anagrams, "in_anagrams")
next_word:
        unless words goto cleanup
        clone anagrams, in_anagrams
        .local string one_word
        shift one_word, words
        print "one_word: "
        print one_word
        print "\n"
        .local pmc one_anagram
next_anagram:   
        unless anagrams goto next_word
        shift one_anagram, anagrams
        clone one_anagram, one_anagram
        _dumper (one_anagram, "one_anagram")
        unshift one_anagram, one_word
        push rv, one_anagram
        _dumper (one_anagram, "one_anagram after unshifting one word")
        goto next_anagram
cleanup:
        .return (rv)
.end

.sub 'test_combine' :main
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

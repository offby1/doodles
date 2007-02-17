# -*-pir-*-
.sub 'snarf_dict' :main
        load_bytecode 'PGE.pbc'
        load_bytecode 'bag.pbc'
        load_bytecode 'dumper.pir'
        load_bytecode 'String/Utils.pbc'
        .local pmc chomp
        .local string one_line
        .local pmc infile_handle
        .local string cache_file
        .local int stat_info
        cache_file = 'dict.cache'
        .local pmc dict_hash    # used to build up the final entries
        new dict_hash, .Hash

        chomp = get_global ['String';'Utils'], 'chomp'
        print chomp

        bag_init()
        test_adjoin ()
        stat stat_info, cache_file, 0
        if stat_info goto call_snarf_cache
        say "No dict cache; reading the actual dictionary"
#        infile_handle = open "words-100", "<"
        infile_handle = open "/usr/share/dict/words", "<"

        .local pmc p5regex_compile
        p5regex_compile = compreg 'PGE::P5Regex'         # get the compiler
        .local string has_a_vowel, long_enough, non_alpha
        .local pmc has_a_vowel_rulesub, long_enough_rulesub, non_alpha_rulesub, match
        .local pmc the_bag
        has_a_vowel = '[aeiouyAEIOUY]'
        has_a_vowel_rulesub = p5regex_compile(has_a_vowel)
        long_enough = '[iaIA]|..'
        long_enough_rulesub = p5regex_compile(long_enough)
        non_alpha = "[^a-zA-Z]"
        non_alpha_rulesub = p5regex_compile(non_alpha)

next_line:
        readline one_line, infile_handle
        length I0, one_line
        if I0 == 0 goto write_cache

        chomp (one_line)
        downcase one_line
        match = has_a_vowel_rulesub (one_line)
        if match goto has_vowel
#         print one_line
#         print " doesn't have a vowel!\n"

has_vowel:
        match = long_enough_rulesub (one_line)
        if match goto nothing_weird
#         print one_line
#         print " isn't long enough\n"

nothing_weird:
        match = non_alpha_rulesub (one_line)
        unless match goto acceptable
#         print one_line
#         print " contains some weird non-alpha character\n"

goto next_line

acceptable:
        .local pmc existing_entry
        the_bag = make_bag (one_line)

        existing_entry = dict_hash[the_bag]
        unless_null existing_entry, adjoin
        .local pmc new_entry
        new new_entry, .ResizablePMCArray
        push new_entry, one_line
        dict_hash[the_bag] = new_entry
        goto next_line

adjoin: 
        adjoin (one_line, existing_entry)
        goto next_line

call_snarf_cache:
        say "Snarfing the cache"
        .local pmc entries
        entries = snarf_cache (cache_file)
        print entries
        print " entries\n"
        goto cleanup
write_cache:
        entries = write_cache (dict_hash, cache_file)
cleanup:
        close infile_handle
        .return (entries)
.end

.sub 'adjoin'
        .param string s
        .param pmc list
        .local pmc iterator

        new iterator, .Iterator, list
next:   
        unless iterator goto push
        .local string this_entry
        shift this_entry, iterator
        if this_entry == s goto done
        goto next
push:   push list, s
done:
.end

.sub 'test_adjoin'
        .local pmc list
        .local string s

        new list, .ResizableStringArray
        adjoin("foo", list)
        adjoin("bar", list)
        adjoin("zip", list)
        adjoin("foo", list)
        _dumper (list)
.end

.sub 'write_entry'
        .param pmc cache_fd
        .param pmc this_bag
        .param pmc these_words
        .local pmc iterator
        .local pmc word

        print cache_fd, this_bag
        
        new iterator, .Iterator, these_words
next_word:   
        unless iterator goto done
        shift word, iterator
        print cache_fd, ","
        print cache_fd, word
        goto next_word
done:
        print cache_fd, "\n"
.end

.sub 'write_cache'
        .param pmc hash
        .param string cache_file_name
        .local pmc iterator
        .local pmc cache_fd
        .local pmc entries_as_written
        cache_fd = open cache_file_name, ">"
        print hash
        new iterator, .Iterator, hash
        new entries_as_written, .ResizablePMCArray
next:   
        unless iterator goto cleanup
        .local pmc this_bag
        .local pmc these_words
        shift this_bag, iterator
        these_words = hash[this_bag]
        write_entry (cache_fd, this_bag, these_words)
        unshift these_words, this_bag
        push entries_as_written, these_words
        goto next
cleanup:
        close cache_fd
        .return (entries_as_written)
.end

.sub 'snarf_cache'
        .param string cache_file_name
        .local string one_line
        .local pmc cache_fd
        .local pmc rv
        .local pmc fields
        .local pmc chomp
        .local pmc bag
        new rv, .ResizablePMCArray
        new bag, .BigInt
        chomp = get_global ['String';'Utils'], 'chomp'
        cache_fd = open cache_file_name, "<"
next_line:       
        readline one_line, cache_fd
        unless one_line goto cleanup
        chomp (one_line)
        split fields, ",", one_line

        .local string digit_string
        shift digit_string, fields
        set bag, digit_string
        unshift fields, bag

        push rv, fields
        goto next_line
cleanup:
        close cache_fd
        .return (rv)
.end

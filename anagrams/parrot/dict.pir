# -*-pir-*-
.sub 'snarf_dict' :main
        load_bytecode 'PGE.pbc'
        load_bytecode 'bag.pbc'
        load_bytecode 'dumper.pir'
        load_bytecode 'String/Utils.pbc'
        .local pmc chomp
        .local string one_line
        .local pmc infile_handle
        chomp = get_global ['String';'Utils'], 'chomp'
        print chomp

        bag_init()
        infile_handle = open "words", "<"
#        infile_handle = open "/usr/share/dict/words", "<"

        .local pmc p5regex_compile
        p5regex_compile = compreg 'PGE::P5Regex'         # get the compiler
        .local string has_a_vowel, long_enough, non_alpha
        .local pmc has_a_vowel_rulesub, long_enough_rulesub, non_alpha_rulesub, match
        .local pmc the_bag
        has_a_vowel = '[aeiouyAEIOUY]'
        has_a_vowel_rulesub = p5regex_compile(has_a_vowel)
        long_enough = '[iaIA]|..'
        long_enough_rulesub = p5regex_compile(long_enough)
        non_alpha = "[^a-zA-Z\n]"
        non_alpha_rulesub = p5regex_compile(non_alpha)

        .local pmc dict_hash    # used to build up the final entries
        new dict_hash, .Hash
next_line:
        readline one_line, infile_handle
        length I0, one_line
        if I0 == 0 goto cleanup

        chomp (one_line)
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
        push new_entry, the_bag
        push new_entry, one_line
        dict_hash[the_bag] = new_entry
        goto next_line

adjoin: 
        push existing_entry, one_line # BUGBUG -- avoid duplicates.
        goto next_line
cleanup:
        _dumper(dict_hash)
        close infile_handle
.end

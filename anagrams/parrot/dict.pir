# -*-pir-*-
.sub 'snarf_dict' :main
        load_bytecode 'PGE.pbc'
        load_bytecode 'bag.pbc'
        load_bytecode 'dumper.pir'
        load_bytecode 'String/Utils.pbc'
        .local pmc chomp
        chomp = get_global ['String';'Utils'], 'chomp'
        print chomp

        bag_init()
        P0 = open "words", "<"
#        P0 = open "/usr/share/dict/words", "<"

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
        readline S0, P0
        length I0, S0
        if I0 == 0 goto cleanup

        chomp (S0)
        match = has_a_vowel_rulesub (S0)
        if match goto has_vowel
#         print S0
#         print " doesn't have a vowel!\n"

has_vowel:
        match = long_enough_rulesub (S0)
        if match goto nothing_weird
#         print S0
#         print " isn't long enough\n"

nothing_weird:
        match = non_alpha_rulesub (S0)
        unless match goto acceptable
#         print S0
#         print " contains some weird non-alpha character\n"

goto next_line

acceptable:
        .local pmc existing_entry
        the_bag = make_bag (S0)

        existing_entry = dict_hash[the_bag]
        unless_null existing_entry, adjoin
        .local pmc new_entry
        new new_entry, .ResizablePMCArray
        push new_entry, the_bag
        push new_entry, S0
        dict_hash[the_bag] = new_entry
        goto next_line

adjoin: 
        push existing_entry, S0 # BUGBUG -- avoid duplicates.
        goto next_line
cleanup:
        _dumper(dict_hash)
        close P0
.end

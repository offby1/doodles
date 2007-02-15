# -*-pir-*-
.sub 'example' :main
        load_bytecode 'PGE.pbc'
        load_bytecode 'bag.pbc'
        P0 = open "words", "<"
#        P0 = open "/usr/share/dict/words", "<"

        .local pmc p5regex_compile
        p5regex_compile = compreg 'PGE::P5Regex'         # get the compiler
        .local string has_a_vowel, long_enough, non_alpha
        .local pmc has_a_vowel_rulesub, long_enough_rulesub, non_alpha_rulesub, match
        has_a_vowel = '[aeiouyAEIOUY]'
        has_a_vowel_rulesub = p5regex_compile(has_a_vowel)
        long_enough = '[iaIA]|..'
        long_enough_rulesub = p5regex_compile(long_enough)
        non_alpha = "[^a-zA-Z\n]"
        non_alpha_rulesub = p5regex_compile(non_alpha)

next_line:
        readline S0, P0
        length I0, S0
        if I0 == 0 goto cleanup

        match = has_a_vowel_rulesub (S0)
        if match goto has_vowel
        print S0
        print " doesn't have a vowel!\n"

has_vowel:
        match = long_enough_rulesub (S0)
        if match goto nothing_weird
        print S0
        print " isn't long enough\n"

nothing_weird:
        match = non_alpha_rulesub (S0)
        unless match goto acceptable
        print S0
        print " contains some weird non-alpha character\n"

acceptable:
        print S0
        print " is acceptable!\n"
        goto next_line
cleanup:
        close P0
        make_bag("snorfly")
.end

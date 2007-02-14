.sub 'example' :main
    load_bytecode 'PGE.pbc'
    P0 = open "/usr/share/dict/words", "<"

    .local pmc p5regex_compile
    p5regex_compile = compreg 'PGE::P5Regex'         # get the compiler
    .local string has_a_vowel
    .local pmc has_a_vowel_rulesub, match
    has_a_vowel = '[aeiouyAEIOUY]'
    has_a_vowel_rulesub = p5regex_compile(has_a_vowel)

next_line:
    readline S0, P0
    length I0, S0
    if I0 <= 0 goto cleanup

    # Do some magic with the regexp
    match = has_a_vowel_rulesub (S0)
    if match goto match_ok
    print S0
    print " doesn't have a vowel!\n"
match_ok:
    goto next_line
cleanup:
    close P0
.end

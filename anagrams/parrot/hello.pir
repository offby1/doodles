.sub 'example' :main
    load_bytecode 'PGE.pbc'
    P0 = open "/usr/share/dict/words", "<"

    .local pmc p5regex_compile
    p5regex_compile = compreg 'PGE::P5Regex'         # get the compiler
    .local string has_a_vowel, long_enough
    .local pmc has_a_vowel_rulesub, long_enough_rulesub, match
    has_a_vowel = '[aeiouyAEIOUY]'
    has_a_vowel_rulesub = p5regex_compile(has_a_vowel)
    long_enough = '[iaIA]|..'
    long_enough_rulesub = p5regex_compile(long_enough)

next_line:
    readline S0, P0
    length I0, S0
    if I0 <= 0 goto cleanup

    # Do some magic with the regexp

    match = has_a_vowel_rulesub (S0)
    if match goto has_vowel
    print S0
    print " doesn't have a vowel!\n"

has_vowel:
    match = long_enough_rulesub (S0)
    if match goto acceptable
    print S0
    print " isn't long enough\n"

acceptable:
    goto next_line
cleanup:
    close P0
.end

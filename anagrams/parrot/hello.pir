# -*-asm-*-
.sub 'example' :main
    P0 = open "/usr/share/dict/words", "<"
next_line:
    readline S0, P0
    length I0, S0
    if I0 <= 0 goto cleanup
    print S0
    goto next_line
cleanup:
    close P0
.end

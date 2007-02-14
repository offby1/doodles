.sub 'example' :main
    .local string dict_fn 
    dict_fn = "/usr/share/dict/words"
    P0 = open dict_fn, "<"
yow:
    readline S0, P0
    length I0, S0
    if I0 <= 0 goto end
    print S0
    goto yow
end:
    close P0
.end

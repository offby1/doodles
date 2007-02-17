.sub 'main' :main
        load_bytecode 'dumper.pir'
        .local ResizablePMCArray fields
        split fields, ",", "hey,you"
        _dumper (fields)

        .local pmc big
        new big, .BigInt
        _dumper (big)

        set big, "1234567890987654321"
        _dumper (big)

        unshift fields, big
        _dumper (fields)
.end

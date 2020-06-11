help-me: {ARGS: [SOURCE [CMD [OPTS]]]
    SOURCE: INPUT_FILE  (default makefile.reb)
    CMD:    make | dump (default make)
OPTS: 
    make: OUT_FILE (default build/makefile)
    dump: OUT_FILE (default stdout)
}

do %tools/bootstrap-shim.r
if not void? :tighten [
    enfix: enfix adapt :enfix [action: tighten :action]
]

blockify: default [function [x] [
    either block? x [x] [reduce [x]]
]]

wchar: charset [
    #"A" - #"Z" "_"
]

expand: function [
    template [block! text! file! tag!]
][
    esc: #"$"
    t: r: _
    r: make block! 0
    if block? template [
        for-next t template [
            new-line tail r new-line? t
            switch type-of t/1 [
                block! [append/only r expand t/1]
                text! file! tag! [append r expand t/1]
            ] else [append r t/1]
        ]
        new-line tail r new-line? template
        return r
    ]
    parse as text! template [
        any [
            copy t to esc skip
            (if not empty? t [append r t])
            [ [ "(" copy t to #")" skip
                | copy t some wchar
	]
                ( t: blockify load t
	    append/only r to-group t
	)
            | opt esc (append r esc)
            ]
        ]
        copy t to end
        (if not empty? t [append r t])
    ]
    either text? template 
    [ reduce ['unspaced r] ]
    [ reduce ['to (type-of template) 'unspaced r] ]
]

expand-many: function [
    template [block! any-string!]
    'vars
    data [block!]
][ map-each :vars data expand template ]

&: enfix :join

dump: function [
    makefile [block!]
    target [any-string!]
][
    r: (mold makefile) & "^/; vim: set syn=rebol:"
    if empty? target [print r]
    else [write to-file targetr]
]

gmake: function [
    makefile [block!]
    target [any-string!]
][
    r: make text! 0
    for-each [t s c] makefile [
        if text? t [
            append r spaced [".PHONY:" t newline]
        ]
        append r unspaced [t ": " s newline]
        for-each c blockify c [
            append r tab
            append r c
            append r newline
        ]
        append r newline
    ]
    if empty? target [print r]
    else [write to-file target r]
]

=== MAIN ===
cd :system/options/path
args: system/script/args
makefile: any [pick args 1 "makefile.reb"]
cmd: any [pick args 2 "make"]
target: pick args 3
if not set? 'target [ target:
    switch cmd [
        "dump" [_] ; stdout
        "make" ["build/makefile"]
    ]
]

makefile: reduce do to-file makefile
m: makefile
while [not tail? m] [
    while [block? m/1] [
        insert m take m
    ]
    m/2: reduce m/2 
    if block? m/2 [m/2: flatten m/2]
    m/3: reduce m/3
    if block? m/3 [
        m/3: flatten m/3
        new-line/all m/3 true
    ]
    m: skip m 3
]

switch cmd [
    "dump" [dump makefile target]
    "make" [gmake makefile target]
] else [ print help-me ]

; vim: set et sw=2:

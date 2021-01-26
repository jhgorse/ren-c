; functions/control/apply.r

(did redbol-apply: func [
    return: [<opt> any-value!]
    action [action!]
    block [block!]
    /only
    <local> types arg key frame params mode
][
    types: as frame! :action  ; exemplar of types
    frame: make frame! :action  ; frame we are building
    params: parameters of :action  ; ordered list of parameters
    mode: <normal>

    ; Rebol2 and R3-Alpha APPLY would fill in NONE for any parameters that
    ; were not provided in the apply block:
    ;
    ;     rebol2/r3-alpha>> apply func [a b c] [reduce [a b c]] []
    ;     == [none none none]
    ;
    ; This means we need to enumerate and fill in the frame as long as there
    ; are parameters--not as long as there are block values.
    ;
    while [not tail? params] [
        case [
            not block [
                arg: null  ; could also do BLANK! if no more block data
            ]
            only [  ; /ONLY means do not evaluate arguments
                arg: get/any 'block/1
                block: next block
            ]
            true [  ; evaluate (skipping comments and other invisibles)
                until .not.quoted? [[block arg]: evaluate block]
            ]
        ]

        key: to word! dequote params/1
        all [
            refinement? params/1
            elide if not block [break]  ; done if refinements w/no more block
            mode = <normal>
        ] then [
            mode: if arg [#]  ; set mode to either use or don't use next arg
            if empty? second pick types key [  ; no-arg refine...
                set (in frame key) mode  ; ...must be # or NULL
            ] else [
                continue  ; keep param on the refinement, get next arg
            ]
        ] else [
            if mode [  ; normal or # case will set
                set (in frame key) get/any 'arg
            ]
        ]

        mode: <normal>
        params: next params
    ]

    ; Too many arguments was not a problem for R3-alpha's APPLY, it would
    ; evaluate them all even if not used by the function.  It may or may not
    ; be better to have it be an error.
    ;
    ; https://github.com/metaeducation/rebol-issues/issues/2237
    ;
    comment [
        all [block, not tail? block] then [
            fail "Too many arguments passed in REDBOL-APPLY block."
        ]
    ]

    print mold frame
    do frame
])

([a b c d e] = redbol-apply :append [[a b c] [d e f] true 2])

[#44 (
    error? trap [redbol-apply 'append/only [copy [a b] 'c]]
)]
(1 == redbol-apply :subtract [2 1])
(1 = (redbol-apply :- [2 1]))
(null = redbol-apply func [a] [a] [])
(null = redbol-apply/only func [a] [a] [])

[#2237  ; current choice is not to error on excess arguments
    (not error? trap [redbol-apply func [a] [a] [1 2]])
    (not error? trap [redbol-apply/only func [a] [a] [1 2]])
]

(error? redbol-apply :make [error! ""])

(# = redbol-apply func [/a] [a] [#[true]])
(null = redbol-apply func [/a] [a] [#[false]])
(null = redbol-apply func [/a] [a] [])
(# = redbol-apply/only func [/a] [a] [#[true]])

(
    comment {The WORD! false, not #[false], but allowed in Rebol2}

    # = redbol-apply/only func [/a] [a] [false]
)
(null == redbol-apply/only func [/a] [a] [])

(use [a] [a: true, # = redbol-apply func [/a] [a] [a]])
(use [a] [a: false, null == redbol-apply func [/a] [a] [a]])
(use [a] [a: false, # = redbol-apply func [/a] [a] [/a]])
(use [a] [a: false, /a = redbol-apply/only func [/a] [/a] [/a]])

(group! == redbol-apply/only (specialize :of [property: 'type]) [()])
([1] == head of redbol-apply :insert [copy [] [1] blank blank])
([1] == head of redbol-apply :insert [copy [] [1] blank false])
([1] == head of redbol-apply :insert [copy [] [1] blank true])
(action! == redbol-apply (specialize :of [property: 'type]) [:print])
(get-word! == redbol-apply/only (specialize :of [property: 'type]) [:print])

[
    #1760

    (2 == reeval func [] [redbol-apply does [] [return 1] 2])
    (1 == reeval func [] [redbol-apply func [a] [a] [return 1] 2])
    (null == reeval func [] [redbol-apply does [] [1]])
    (1 == reeval func [] [redbol-apply func [a] [a] [return 1]])
    (1 == reeval func [] [redbol-apply func [a b] [a] [return 1 2]])
    (1 == reeval func [] [redbol-apply func [a b] [a] [2 return 1]])
]

(
    null? redbol-apply func [
        return: [<opt> any-value!]
        x [<opt> any-value!]
    ][
        get 'x
    ][
        null
    ]
)
(
    null? redbol-apply func [
        return: [<opt> any-value!]
        'x [<opt> any-value!]
    ][
        get 'x
    ][
        null
    ]
)
(
    null? redbol-apply func [
        return: [<opt> any-value!]
        x [<opt> any-value!]
    ][
        return get 'x
    ][
        null
    ]
)
(
    void? redbol-apply func [
        return: [<opt> any-value!]
        'x [<opt> any-value!]
    ][
        return get/any 'x
    ][
        '~void~
    ]
)
(
    error? redbol-apply func ['x [<opt> any-value!]] [
        return get 'x
    ][
        make error! ""
    ]
)
(
    error? redbol-apply/only func [x [<opt> any-value!]] [
        return get 'x
    ] head of insert copy [] make error! ""
)
(
    error? redbol-apply/only func ['x [<opt> any-value!]] [
        return get 'x
    ] head of insert copy [] make error! ""
)
(use [x] [x: 1 strict-equal? 1 redbol-apply func ['x] [:x] [:x]])
(use [x] [x: 1 strict-equal? 1 redbol-apply func ['x] [:x] [:x]])
(
    use [x] [
        x: 1
        strict-equal? first [:x] redbol-apply/only func [:x] [:x] [:x]
    ]
)
(
    use [x] [
        unset 'x
        strict-equal? first [:x] redbol-apply/only func ['x [<opt> any-value!]] [
            return get 'x
        ] [:x]
    ]
)
(use [x] [x: 1 strict-equal? 1 redbol-apply func [:x] [:x] [x]])
(use [x] [x: 1 strict-equal? 'x redbol-apply func [:x] [:x] ['x]])
(use [x] [x: 1 strict-equal? 'x redbol-apply/only func [:x] [:x] [x]])
(use [x] [x: 1 strict-equal? 'x redbol-apply/only func [:x] [return :x] [x]])
(
    use [x] [
        unset 'x
        strict-equal? 'x redbol-apply/only func ['x [<opt> any-value!]] [
            return get 'x
        ] [x]
    ]
)

[(
    comment {MAKE FRAME! :RETURN should preserve binding in the frame}
    1 == reeval func [] [redbol-apply :return [1] 2]
)]

(null == redbol-apply/only func [/a] [a] [#[false]])
(group! == redbol-apply/only :type-of [()])

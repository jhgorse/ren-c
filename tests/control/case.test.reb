; functions/control/case.r

(true = case [true [true]])
(false = case [true [false]])
(
    success: false
    case [true [success: true]]
    success
)
(
    success: true
    case [false [success: false]]
    success
)

(
    null? case [false []]  ; null indicates no branch was taken
)
(
    null? case []  ; empty case block is legal (e.g. as COMPOSE product)
)
(
    void? case [true []]  ; void indicates branch was taken (vs. null)
)
(
    void? case [
        true []
        false [1 + 2]
    ]
)
[#2246 (
    void? case [true []]
)]

(
    'a = case [
        first [a b c]  ; no corresponding branch, means "case fallout"
    ]
)

(
    3 = case [true (reduce ['add 1 2])]
)
(
    null? case [false (reduce ['add 1 2])]
)

(
    error? trap [
        case [
            true add 1 2  ; branch slots must be BLOCK!, ACTION!, softquote
        ]
    ]
)

; Invisibles should be legal to mix with CASE.

(
    flag: false
    result: case [
        1 < 2 [1020]
        elide (flag: true)
        true [fail "shouldn't get here"]
    ]
    (not flag) and [result = 1020]
)



; RETURN, THROW, BREAK will stop case evaluation
(
    f1: func [] [case [return 1 2]]
    1 = f1
)
(
    1 = catch [
        case [throw 1 2]
        2
    ]
)
(
    null? loop 1 [
        case [break 2]
        2
    ]
)

[#86 (
    s1: false
    s2: false
    case/all [
        true [s1: true]
        true [s2: true]
    ]
    s1 and @s2
)]

; nested calls
(1 = case [true [case [true [1]]]])

; infinite recursion
(
    n: 0
    blk: [elide if 10000 = n: n + 1 [throw <finished>] case blk]
    <finished> = catch blk
)


; Work in progress new feature for specifying predicates with a refinement

(<a> = case/predicate [1 = 2 [<a>]] :not?)
(<b> = case/predicate [1 [<a>] 2 [<b>]] :even?)

; !!! This happens due to the trick that DEFAULT uses to work with fallout.
; It's not clear how to resolve it to get the correct behavior:
;
; https://forum.rebol.info/t/default-now-usable-in-case-switch/739/3
;
(#[false] = case/predicate [1 = 1 [<a>] default [<b>]] :not?)

; Errors on bad branches
(
    e: trap [case [true #bad]]
    e/id = 'bad-value
)

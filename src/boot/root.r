REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "Root context"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0.
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Purpose: {
        Root system values. This context is hand-made very early at boot time
        to allow it to hold key system values during boot up. Most of these
        are put here to prevent them from being garbage collected.
    }
    Note: "See Task Context for per-task globals"
]

system          ; system object
errobj          ; error object template
strings         ; low-level strings accessed via Boot_Strs[] (GC protection)
typesets        ; block of TYPESETs used by system; expandable
empty-block     ; a value that is an empty BLOCK!
empty-string    ; a value that is an empty STRING!

;; Tags used in the native-optimized versions of user-function-generators
;; FUNC and PROC

no-return-tag   ; func w/o definitional return, ignores non-definitional ones
no-leave-tag    ; func w/o definitional leave, ignores non-definitional ones
punctuates-tag  ; function's result cannot be used as a function argument
ellipsis-tag    ; FUNC+PROC use as alternative to [[]] to mark varargs
opt-tag         ; FUNC+PROC use as alternative to _ to mark optional void? args
end-tag         ; FUNC+PROC use as alternative to | to mark endable args
local-tag       ; marks the beginning of a list of "pure locals"
durable-tag     ; !!! In progress - argument word lookup survives call ending

;; !!! See notes on FUNCTION-META in %sysobj.r

function-meta

;; PRINT takes a /DELIMIT which can be a block specifying delimiters at each
;; level of depth in the recursion of blocks.  The default is [#" " |], which
;; is a signal to put spaces at the first level and then after that nothing.
;;
default-print-delimiter

boot            ; boot block defined in boot.r (GC'd after boot is done)


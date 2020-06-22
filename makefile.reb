REBOL_TOOL: system/options/boot
REBOL: "$REBOL_TOOL -qs"
BASE_DIR: %..
TOOLS: BASE_DIR/tools
SRC: BASE_DIR/src
EXTENSIONS: BASE_DIR/extensions
GIT_COMMIT: default ["unknown"]
OS_ID: 0.13.2
MKDIR: "mkdir -p"
DEFS: "-DNDEBUG -DOS_STACK_GROWS_DOWN -DENDIAN_LITTLE -DHAS_LL_CONSTS -D_FILE_OFFSET_BITS=64 -DTO_ANDROID -DTO_ANDROID5_ARM"
INCLUDE: "-I$SRC/include -Iprep/include"
COMPILER: 'gcc
CC: "gcc -c" ; Compile Command
CFLAGS: "-O2 -fvisibility=hidden -fPIC"
LINK: "gcc" ; Link command
LFLAGS: "-fvisibility=hidden -pie -rdynamic" 
LIBS: "-lm -ldl -llog"
STRIP: "strip -S -x -X"


;; OBJS
core-objs: [
    ; msc: /wd5045 => https://stackoverflow.com/q/50399940
    ; msc: /wd4146 => unary minus operator applied to unsigned type
    ; (A)???
    %a-constants _
    %a-globals _
    %a-lib _
    ; (B)oot
    %b-init _
    ; (C)ore
    %c-bind _
    %c-do _
    %c-context _
    %c-error _
    %c-eval [#prefer-O2-optimization]
        ; There are several good reasons to optimize the evaluator itself even
        ; if one is doing a "size-biased" build.  It's not just about wanting
        ; the critical code to be faster--but also, since it recurses, if
        ; stack frames aren't flattened out then they add up...and may blow
        ; internal limits (like in a web browser for JS/WASM calls)
        ;
    %c-function _
    %c-path _
    %c-port _
    %c-signal _
    %c-specialize _
    %c-value _
    %c-word _
    ; (D)ebug
    %d-crash _
    %d-dump _
    %d-eval _
    %d-gc _
    %d-print _
    %d-stack _
    %d-stats _
    %d-test _
    %d-trace _
    %d-winstack _
    ; (F)???
    %f-blocks _
    %f-deci _
        ; May 2018 update to MSVC 2017 added warnings for Spectre mitigation.
        ; %f-deci.c is a lot of twiddly custom C code for implementing a fixed
        ; precision math type, that was for some reason a priority in R3-Alpha
        ; but isn't very central to Ren-C.  It is not a priority to audit
        ; it for speed, so allow it to be slow if MSVC compiles with /Qspectre
        ;
    %f-device _
    %f-dtoa [
        gcc "-Wno-cast-qual -Wno-unused-const-variable -Wno-sign-compare -Wno-uninitialized -Wno-unknown-warning -Wno-implicit-fallthrough"
        msc "/wd5045 /wd4146"
    ] 
        ; f-dtoa.c comes from a third party and is an old file.  There is an
        ; updated package, but it is not a single-file...rather something with
        ; a complex build process.  If it were to be updated, it would need
        ; to be done through a process that extracted it in a way to fit into
        ; the ethos of the Rebol build process.
        ;
        ; Hence we add tolerance for warnings that the file has.
        ;
    %f-enbase [msc "/wd5045"]
        ; At time of writing there are 4 Spectre mitigations, which should
        ; be looked at and rewritten when there is time:
        ;
    %f-extension _
    %f-int _
    %f-math _
    %f-modify _
    %f-qsort _
    %f-random _
    %f-round _
    %f-series _
    %f-stubs _
    ; (L)exer
    %l-scan _
    %l-types _
    ; (M)emory
    %m-gc _
    %m-pools [gcc "-Wno-uninitialized"]
    %m-series _
    %m-stacks _
    ; (N)atives
    %n-control _
    %n-data _
    %n-do _
    %n-error _
    %n-function _
    %n-io _
    %n-loop _
    %n-math _
    %n-protect _
    %n-reduce _
    %n-sets _
    %n-strings _
    %n-system _
    ; (S)trings
    %s-cases _
    %s-crc _
    %s-find _
    %s-make _
    %s-mold _
    %s-ops _
    ; (T)ypes
    %t-binary _
    %t-bitset _
    %t-blank _
    %t-block _
    %t-char _
    %t-datatype _
    %t-date _
    %t-decimal _
    %t-function _
    %t-integer _
    %t-logic _
    %t-map _
    %t-money _
    %t-object _
    %t-pair _
    %t-port _
    %t-quoted _
    %t-string _
    %t-time _
    %t-tuple _
    %t-typeset _
    %t-varargs _
    %t-word _
    %u-compress _
    ; (U)??? (3rd-party code extractions)
    %u-parse _
    %u-zlib [
        gcc "-Wno-unknown-warning -Wno-implicit-fallthrough" msc "/wd5045" <no-make-header>
    ]
        ; Zlib is an active project so it would be worth it to check to see
        ; if minor patches for subverting Spectre mitigation would be taken.
]
generated-objs: [ ; Files created by the make-boot process
    %tmp-boot-block _
    %tmp-type-hooks _
]
extensions-objs: [
    %bmp/mod-bmp _
    %tmp-mod-bmp-init _
    %console/mod-console _
    %tmp-mod-console-init _
    %crypt/mod-crypt _
    %crypt/mbedtls/library/rsa _
    %crypt/mbedtls/library/rsa_internal _
    %crypt/mbedtls/library/oid _
    %crypt/mbedtls/library/platform _
    %crypt/mbedtls/library/platform_util _
    %crypt/mbedtls/library/bignum _
    %crypt/mbedtls/library/md _
    %crypt/mbedtls/library/cipher _
    %crypt/mbedtls/library/cipher_wrap _
    %crypt/mbedtls/library/sha256 _
    %crypt/mbedtls/library/sha512 _
    %crypt/mbedtls/library/ripemd160 _
    %crypt/mbedtls/library/md5 _
    %crypt/mbedtls/library/sha1 _
    %crypt/mbedtls/library/aes _
    %crypt/mbedtls/library/arc4 _
    %crypt/mbedtls/library/dhm _
    %crypt/mbedtls/library/ecdh _
    %crypt/mbedtls/library/ecp _
    %crypt/mbedtls/library/ecp_curves _
    %tmp-mod-crypt-init _
    %debugger/mod-debugger _
    %tmp-mod-debugger-init _
    %dns/mod-dns _
    %tmp-mod-dns-init _
    %event/mod-event _
    %event/t-event _
    %event/p-event _
    %event/event-posix _
    %tmp-mod-event-init _
    %filesystem/mod-filesystem _
    %filesystem/p-file _
    %filesystem/p-dir _
    %filesystem/file-posix _
    %tmp-mod-filesystem-init _
    %gif/mod-gif _
    %tmp-mod-gif-init _
    %gob/mod-gob _
    %gob/t-gob _
    %tmp-mod-gob-init _
    %image/mod-image _
    %image/t-image _
    %tmp-mod-image-init _
    %jpg/mod-jpg _
    %jpg/u-jpg _
    %tmp-mod-jpg-init _
    %library/mod-library _
    %library/library-posix _
    %tmp-mod-library-init _
    %locale/mod-locale _
    %tmp-mod-locale-init _
    %network/mod-network _
    %network/dev-net _
    %tmp-mod-network-init _
    %png/mod-png _
    %png/lodepng _
    %tmp-mod-png-init _
    %process/mod-process _
    %process/call-posix _
    %tmp-mod-process-init _
    %secure/mod-secure _
    %tmp-mod-secure-init _
    %serial/mod-serial _
    %serial/serial-posix _
    %tmp-mod-serial-init _
    %stdio/mod-stdio _
    %stdio/p-stdio _
    %stdio/stdio-posix _
    %stdio/readline-posix _
    %tmp-mod-stdio-init _
    %time/mod-time _
    %time/time-posix _
    %tmp-mod-time-init _
    %utf/mod-utf _
    %tmp-mod-utf-init _
    %uuid/mod-uuid _
    %tmp-mod-uuid-init _
    %vector/mod-vector _
    %vector/t-vector _
    %tmp-mod-vector-init _
    %view/mod-view _
    %tmp-mod-view-init _
]
OBJS: map-each [s o] core-objs & generated-objs & extensions-objs [s]


;; TARGETS
[

"clean" _ map-each X [
    %objs/
    %prep/
    %r3
    %libr3.*
] [
    {rm -fr $X}
] 
 
"folders" _ map-each X [
    %""
    %bmp/
    %console/
    %crypt/
    %crypt/mbedtls/library/
    %debugger/
    %dns/
    %event/
    %filesystem/
    %gif/
    %gob/
    %image/
    %jpg/
    %library/
    %locale/
    %main/
    %network/
    %png/
    %process/
    %secure/
    %serial/
    %stdio/
    %time/
    %utf/
    %uuid/
    %vector/
    %view/
] [
    {$MKDIR objs/$X}
]

"prep" REBOL_TOOL [
    reduce [
    {$REBOL $TOOLS/make-natives.r}
    {$REBOL $TOOLS/make-headers.r}
    {$REBOL $TOOLS/make-boot.r OS_ID: $OS_ID GIT_COMMIT: $GIT_COMMIT}
    {$REBOL $TOOLS/make-reb-lib.r OS_ID: $OS_ID}
    ]
    map-each [M N] [
        "BMP" %bmp
        "Console" %console
        "Crypt" %crypt
        "Debugger" %debugger
        "DNS" %dns
        "Event" %event
        "Filesystem" %filesystem
        "GIF" %gif
        "Gob" %gob
        "Image" %image
        "JPG" %jpg
        "Library" %library
        "Locale" %locale
        "Network" %network
        "PNG" %png
        "Process" %process
        "Secure" %secure
        "Serial" %serial
        "Stdio" %stdio
        "Time" %time
        "UTF" %utf
        "UUID" %uuid
        "Vector" %vector
        "View" %view
    ] [
        S: %extensions/$N/mod-$N.c
        {$REBOL $TOOLS/prep-extension.r MODULE: $M SRC: $S OS_ID: $OS_ID}
    ]

    reduce [
        {$REBOL $TOOLS/make-boot-ext-header.r EXTENSIONS: Image:Console:Crypt:BMP:DNS:Event:Filesystem:GIF:Vector:Time:JPG:Library:UUID:Network:PNG:UTF:Secure:Serial:Stdio:View:Process:Gob:Debugger:Locale}
        {$(REBOL) $SRC/main/prep-main.reb}
    ]
]

; crypt module
map-each N [
    %aes
    %arc4
    %bignum
    %cipher
    %cipher_wrap
    %dhm
    %ecdh
    %ecp
    %ecp_curves
    %md
    %md5
    %oid
    %platform
    %platform_util
    %ripemd160
    %rsa
    %rsa_internal
    %sha1
    %sha256
    %sha512
] [
    N: %mbedtls/library/$N
    T: %objs/crypt/$N.o
    S: %$EXTENSIONS/crypt/$N.c
    reduce [
      %$T %$S {$CC -I$EXTENSIONS/crypt -I$EXTENSIONS/crypt/mbedtls/include -DMBEDTLS_CONFIG_FILE=\"mbedtls-rebol-config.h\" -Iprep/extensions/crypt $INCLUDE -DREB_API $DEFS $CFLAGS -o $t $s}
    ]
]

map-each [N O] reduce [
    %bmp _
    %console _
    %crypt {-I$EXTENSIONS/crypt -I$EXTENSIONS/crypt/mbedtls/include -DMBEDTLS_CONFIG_FILE=\"mbedtls-rebol-config.h\" }
    %debugger _
    %dns _
    %event _
    %filesystem _
    %gif _
    %gob _
    %image _
    %jpg _
    %library _
    %locale _
    %network _
    %png {-DLODEPNG_NO_COMPILE_ZLIB -DLODEPNG_NO_COMPILE_ALLOCATORS -DLODEPNG_NO_COMPILE_CPP }
    %process _
    %secure _
    %serial _
    %stdio _
    %time _
    %utf _
    %uuid {-I$EXTENSIONS/uuid/libuuid }
    %vector _
    %view _
] [
    T: %objs/tmp-mod-$N-init.o
    S: %prep/extensions/$N/tmp-mod-$N-init.c
    reduce [
        %$T %$S {$CC $O -Iprep/extensions/$N $INCLUDE -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
]

map-each [A B O] reduce [
    %bmp %mod-bmp _
    %console %mod-console _
    %crypt %mod-crypt {-I$EXTENSIONS/crypt -I$EXTENSIONS/crypt/mbedtls/include -DMBEDTLS_CONFIG_FILE=\"mbedtls-rebol-config.h\" }
    %debugger %mod-debugger _
    %dns %mod-dns _
    %event %mod-event _
    %filesystem %mod-filesystem _
    %gif %mod-gif _
    %gob %mod-gob _
    %image %mod-image _
    %jpg %mod-jpg _
    %library %mod-library _
    %locale %mod-locale _
    %network %mod-network _
    %png %mod-png {-DLODEPNG_NO_COMPILE_ZLIB -DLODEPNG_NO_COMPILE_ALLOCATORS -DLODEPNG_NO_COMPILE_CPP }
    %process %mod-process _
    %secure %mod-secure _
    %serial %mod-serial _
    %stdio %mod-stdio _
    %time %mod-time _
    %utf %mod-utf _
    %uuid %mod-uuid {-I$EXTENSIONS/uuid/libuuid }
    %vector %mod-vector _
    %view %mod-view _
    %event %event-posix _
    %event %p-event _
    %event %t-event _
    %filesystem %file-posix _
    %filesystem %p-dir _
    %filesystem %p-file _
    %gob %t-gob _
    %image %t-image _
    %jpg %u-jpg {-Wno-unused-parameter -Wno-shift-negative-value }
    %library %library-posix _
    %network %dev-net _
    %png %lodepng {-DLODEPNG_NO_COMPILE_ZLIB -DLODEPNG_NO_COMPILE_ALLOCATORS -DLODEPNG_NO_COMPILE_CPP -Wno-cast-qual }
    %process %call-posix _
    %serial %serial-posix _
    %stdio %p-stdio _
    %stdio %readline-posix _
    %stdio %stdio-posix _
    %time %time-posix _
    %vector %t-vector _
] [
    T: %objs/$A/$B.o
    S: %$EXTENSIONS/$A/$B.c
    reduce [
      %$T %$S {$CC $O -Iprep/extensions/$A $INCLUDE -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
]

map-each [N O] core-objs [
    T: %objs/$N.o
    S: %$SRC/core/$N.c
    if O [O: try select O COMPILER] 
    reduce [
      %$T %$S {$CC $INCLUDE -Iprep/core -DREB_API $DEFS $CFLAGS $O -o $T $S}
    ]
]

map-each [N O] generated-objs [
    T: %objs/$N.o
    S: %prep/core/$N.c
    reduce [
      %$T %$S {$CC $INCLUDE -Iprep/core -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
]

%objs/main.o %$SRC/main/main.c 
{$CC $INCLUDE -Iprep/main -DREB_CORE $DEFS $CFLAGS -o objs/main.o $SRC/main/main.c}

%r3 map-each N (OBJS & %main) [
  %objs/$N.o
]
reduce [
    {$LINK -o r3 $LFLAGS }
    & (form map-each N OBJS [%objs/$N.o])
    & { $LIBS objs/main.o}
    {$STRIP r3}
] 

"check" %r3 reduce [{$STRIP r3}]

] ;;; END TARGETS ;;;

; vim: set et sw=4:

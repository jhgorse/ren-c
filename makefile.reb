REBOL-TOOL: system/options/boot
REBOL: spaced [REBOL-TOOL "-qs"]
BASE_DIR: %..
TOOLS: BASE_DIR/tools
SRC: BASE_DIR/src
EXTENSIONS: BASE_DIR/extensions
GIT_COMMIT: default ["unknown"]
OS_ID: 0.13.2
MKDIR: "mkdir -p"
DEFS: "-DNDEBUG -DOS_STACK_GROWS_DOWN -DENDIAN_LITTLE -DHAS_LL_CONSTS -D_FILE_OFFSET_BITS=64 -DTO_ANDROID -DTO_ANDROID5_ARM"
INCLUDE: do expand "-I$SRC/include -Iprep/include"
CC: "gcc -c"
CFLAGS: "-O2 -fvisibility=hidden -fPIC"
LINK: "gcc"
LFLAGS: "-fvisibility=hidden -pie -rdynamic" 
LIBS: "-lm -ldl -llog"
STRIP: "strip -S -x -X"

OBJS: [
  %a-constants
  %a-globals
  %a-lib
  %b-init
  %c-bind
  %c-do
  %c-context
  %c-error
  %c-eval
  %c-function
  %c-path
  %c-port
  %c-signal
  %c-specialize
  %c-value
  %c-word
  %d-crash
  %d-dump
  %d-eval
  %d-gc
  %d-print
  %d-stack
  %d-stats
  %d-test
  %d-trace
  %d-winstack
  %f-blocks
  %f-deci
  %f-device
  %f-dtoa
  %f-enbase
  %f-extension
  %f-int
  %f-math
  %f-modify
  %f-qsort
  %f-random
  %f-round
  %f-series
  %f-stubs
  %l-scan
  %l-types
  %m-gc
  %m-pools
  %m-series
  %m-stacks
  %n-control
  %n-data
  %n-do
  %n-error
  %n-function
  %n-io
  %n-loop
  %n-math
  %n-protect
  %n-reduce
  %n-sets
  %n-strings
  %n-system
  %s-cases
  %s-crc
  %s-find
  %s-make
  %s-mold
  %s-ops
  %t-binary
  %t-bitset
  %t-blank
  %t-block
  %t-char
  %t-datatype
  %t-date
  %t-decimal
  %t-function
  %t-integer
  %t-logic
  %t-map
  %t-money
  %t-object
  %t-pair
  %t-port
  %t-quoted
  %t-string
  %t-time
  %t-tuple
  %t-typeset
  %t-varargs
  %t-word
  %u-compress
  %u-parse
  %u-zlib
  %tmp-boot-block
  %tmp-type-hooks
  %bmp/mod-bmp
  %tmp-mod-bmp-init
  %console/mod-console
  %tmp-mod-console-init
  %crypt/mod-crypt
  %crypt/mbedtls/library/rsa
  %crypt/mbedtls/library/rsa_internal
  %crypt/mbedtls/library/oid
  %crypt/mbedtls/library/platform
  %crypt/mbedtls/library/platform_util
  %crypt/mbedtls/library/bignum
  %crypt/mbedtls/library/md
  %crypt/mbedtls/library/cipher
  %crypt/mbedtls/library/cipher_wrap
  %crypt/mbedtls/library/sha256
  %crypt/mbedtls/library/sha512
  %crypt/mbedtls/library/ripemd160
  %crypt/mbedtls/library/md5
  %crypt/mbedtls/library/sha1
  %crypt/mbedtls/library/aes
  %crypt/mbedtls/library/arc4
  %crypt/mbedtls/library/dhm
  %crypt/mbedtls/library/ecdh
  %crypt/mbedtls/library/ecp
  %crypt/mbedtls/library/ecp_curves
  %tmp-mod-crypt-init
  %debugger/mod-debugger
  %tmp-mod-debugger-init
  %dns/mod-dns
  %tmp-mod-dns-init
  %event/mod-event
  %event/t-event
  %event/p-event
  %event/event-posix
  %tmp-mod-event-init
  %filesystem/mod-filesystem
  %filesystem/p-file
  %filesystem/p-dir
  %filesystem/file-posix
  %tmp-mod-filesystem-init
  %gif/mod-gif
  %tmp-mod-gif-init
  %gob/mod-gob
  %gob/t-gob
  %tmp-mod-gob-init
  %image/mod-image
  %image/t-image
  %tmp-mod-image-init
  %jpg/mod-jpg
  %jpg/u-jpg
  %tmp-mod-jpg-init
  %library/mod-library
  %library/library-posix
  %tmp-mod-library-init
  %locale/mod-locale
  %tmp-mod-locale-init
  %network/mod-network
  %network/dev-net
  %tmp-mod-network-init
  %png/mod-png
  %png/lodepng
  %tmp-mod-png-init
  %process/mod-process
  %process/call-posix
  %tmp-mod-process-init
  %secure/mod-secure
  %tmp-mod-secure-init
  %serial/mod-serial
  %serial/serial-posix
  %tmp-mod-serial-init
  %stdio/mod-stdio
  %stdio/p-stdio
  %stdio/stdio-posix
  %stdio/readline-posix
  %tmp-mod-stdio-init
  %time/mod-time
  %time/time-posix
  %tmp-mod-time-init
  %utf/mod-utf
  %tmp-mod-utf-init
  %uuid/mod-uuid
  %tmp-mod-uuid-init
  %vector/mod-vector
  %vector/t-vector
  %tmp-mod-vector-init
  %view/mod-view
  %tmp-mod-view-init
]

[ ; targets

"clean" _ expand-many [
    {rm -fr $X}
] X [
    %objs/
    %prep/
    %r3
    %libr3.*
] 
 
"folders" _ expand-many [
    {$MKDIR objs/$X}
] X [
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
]

"prep" REBOL-TOOL [
    reduce expand [
    {$REBOL $TOOLS/make-natives.r}
    {$REBOL $TOOLS/make-headers.r}
    {$REBOL $TOOLS/make-boot.r OS_ID: $OS_ID GIT_COMMIT: $GIT_COMMIT}
    {$REBOL $TOOLS/make-reb-lib.r OS_ID: $OS_ID}
    ]
    expand-many [
        S: %extensions/$N/mod-$N.c
        {$REBOL $TOOLS/prep-extension.r MODULE: $M SRC: $S OS_ID: $OS_ID}
    ] [M N] [
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
    ]

    reduce expand [
        {$REBOL $TOOLS/make-boot-ext-header.r EXTENSIONS: Image:Console:Crypt:BMP:DNS:Event:Filesystem:GIF:Vector:Time:JPG:Library:UUID:Network:PNG:UTF:Secure:Serial:Stdio:View:Process:Gob:Debugger:Locale}
        {$(REBOL) $SRC/main/prep-main.reb}
    ]
]

; crypt module
expand-many [
    N: %mbedtls/library/$N
    T: %objs/crypt/$N.o
    S: %$EXTENSIONS/crypt/$N.c
    reduce [
      %$T %$S {$CC -I$EXTENSIONS/crypt -I$EXTENSIONS/crypt/mbedtls/include -DMBEDTLS_CONFIG_FILE=\"mbedtls-rebol-config.h\" -Iprep/extensions/crypt $INCLUDE -DREB_API $DEFS $CFLAGS -o $t $s}
    ]
] N [
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
]

expand-many [
    T: %objs/tmp-mod-$N-init.o
    S: %prep/extensions/$N/tmp-mod-$N-init.c
    reduce [
        %$T %$S {$CC $O -Iprep/extensions/$N $INCLUDE -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
] [N O] reduce expand [
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
]

expand-many [
    T: %objs/$A/$B.o
    S: %$EXTENSIONS/$A/$B.c
    reduce [
      %$T %$S {$CC $O -Iprep/extensions/$A $INCLUDE -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
] [A B O] reduce expand [
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
]

expand-many [
    T: %objs/$N.o
    S: %$SRC/core/$N.c
    reduce [
      %$T %$S {$CC $INCLUDE -Iprep/core -DREB_API $DEFS $CFLAGS $W -o $T $S}
    ]
] [N W] [
    %a-constants _
    %a-globals _
    %a-lib _
    %b-init _
    %c-bind _
    %c-do _
    %c-context _
    %c-error _
    %c-eval _
    %c-function _
    %c-path _
    %c-port _
    %c-signal _
    %c-specialize _
    %c-value _
    %c-word _
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
    %f-blocks _
    %f-deci _
    %f-device _
    %f-dtoa {-Wno-cast-qual -Wno-unused-const-variable -Wno-sign-compare -Wno-uninitialized -Wno-unknown-warning -Wno-implicit-fallthrough}
    %f-enbase _
    %f-extension _
    %f-int _
    %f-math _
    %f-modify _
    %f-qsort _
    %f-random _
    %f-round _
    %f-series _
    %f-stubs _
    %l-scan _
    %l-types _
    %m-gc _
    %m-pools {-Wno-uninitialized}
    %m-series _
    %m-stacks _
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
    %s-cases _
    %s-crc _
    %s-find _
    %s-make _
    %s-mold _
    %s-ops _
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
    %u-parse _
    %u-zlib {-Wno-unknown-warning -Wno-implicit-fallthrough}
]

expand-many [
    T: %objs/$N.o
    S: %prep/core/$N.c
    reduce [
      %$T %$S {$CC $INCLUDE -Iprep/core -DREB_API $DEFS $CFLAGS -o $T $S}
    ]
] N [
    %tmp-boot-block
    %tmp-type-hooks
]

reduce expand [
    %objs/main.o %$SRC/main/main.c 
    {$CC $INCLUDE -Iprep/main -DREB_CORE $DEFS $CFLAGS -o objs/main.o $SRC/main/main.c}
]

%r3 expand-many [
  %objs/$N.o
] N (OBJS & %main)
reduce [
    spaced [LINK "-o r3" LFLAGS] & (expand-many [
      %objs/$N.o
    ] N OBJS) & [LIBS %objs/main.o]
    do expand {$STRIP r3}
] 

"check" %r3 do expand {$STRIP r3}

] ; end targets

; vim: set et sw=4:

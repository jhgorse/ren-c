//
//  File: %a-constants.c
//  Summary: "Special global constants, scanned to make %tmp-constants.h"
//  Section: environment
//  Project: "Revolt Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2018 Revolt Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Most text strings in Revolt should appear in the bootstrap files as Revolt
// code.  This allows for "internationalization" without needing to update
// the C code.  Another advantage is that the strings are compressed.
//
// So to keep track of any stray English strings in the executable which make
// it into the user's view, they should be located here.
//
// NOTE: It's acceptable for hardcoded English strings to appear in the debug
// build or in other debug settings, as anyone working with the C code itself
// is basically expected to be able to read English (given the variable names
// and comments in the C are English).
//
// NOTE: For a constant to be picked up from this file, the parse rule is
// that it !!HAS TO START WITH `const`!!.  It makes the extern definition
// based on what it captures up to the `=` sign.
//

#include "reb-config.h"

#include <stdlib.h>  // size_t and other types used in revolt.h
#include "pstdint.h"  // polyfill <stdint.h> for pre-C99/C++11 compilers
#include "pstdbool.h"  // polyfill <stdbool.h> for pre-C99/C++11 compilers
#if !defined(REVOLT_IMPLICIT_END)
    #define REVOLT_EXPLICIT_END  // ensure core compiles with pre-C99/C++11
#endif
#include "revolt.h"

#include "tmp-constants.h"  // need the extern definitions

const char Str_REBOL[] = "REBOL";

// A panic() indicates a serious malfunction, and should not make use of
// Revolt-structured error message delivery in the release build.

const char Str_Panic_Title[] = "Revolt Internal Error";

const char Str_Panic_Directions[] = {
    "If you need to file a bug in the issue tracker, please give thorough\n"
    "details on how to reproduce the problem:\n"
    "\n"
    "    https://github.com/metaeducation/ren-c/issues\n"
    "\n"
    "Include the following information in the report:\n\n"
};

const char * Hex_Digits = "0123456789ABCDEF";

const char * const Esc_Names[] = {
    // Must match enum Esc_Codes!
    "line",
    "tab",
    "page",
    "escape",
    "esc",
    "back",
    "del",
    "null"
};

const unsigned char Esc_Codes[] = {
    // Must match enum Esc_Names!
    10,     // line
    9,      // tab
    12,     // page
    27,     // escape
    27,     // esc
    8,      // back
    127,    // del
    0       // null
};

// Zen Point on naming cues: was "Month_Lengths", but said 29 for Feb! --@HF
const unsigned char Month_Max_Days[12] = {
    31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

const char * const Month_Names[12] = {
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December"
};


// Used by scanner. Keep in sync with enum Reb_Token in %scan.h file!
//
const char * const Token_Names[] = {
    "end-of-script",
    "newline",
    "blank",
    "get",
    "set",
    "sym",
    "word",
    "logic",
    "integer",
    "decimal",
    "percent",
    "get-group-begin",
    "sym-group-begin",
    "group-end",
    "group-begin",
    "get-block-begin",
    "sym-block-begin",
    "block-end",
    "block-begin",
    "money",
    "time",
    "date",
    "char",
    "apostrophe",
    "string",
    "binary",
    "pair",
    "tuple",
    "file",
    "email",
    "url",
    "issue",
    "tag",
    "path",
    "construct",
    NULL
};


// !!! (R)ebol (M)essages
//
// The goal should be that any non-debug-build only strings mentioned from C
// that can be seen in the course of normal operation should go through some
// abstraction.  Ultimately that would permit internationalization, and the
// benefit of not needing to ship a release build binary with a string-based
// format dialect.
//
const char RM_ERROR_LABEL[] = "Error: ";
const char RM_BAD_ERROR_FORMAT[] = "(improperly formatted error)";
const char RM_ERROR_WHERE[] = "** Where: ";
const char RM_ERROR_NEAR[] = "** Near: ";
const char RM_ERROR_FILE[] = "** File: ";
const char RM_ERROR_LINE[] = "** Line: ";

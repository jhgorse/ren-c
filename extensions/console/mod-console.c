//
//  File: %mod-console.c
//  Summary: "Read/Eval/Print Loop (REPL) Skinnable Console for Revolt"
//  Section: Extension
//  Project: "Revolt Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2016-2019 Revolt Open Source Contributors
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

#include "sys-core.h"

#include "tmp-mod-console.h"

//
//  console-dummy: native [
//      {Every extension currently needs at least one native.  Review.}
//  ]
//
REBNATIVE(console_dummy)
{
    CONSOLE_INCLUDE_PARAMS_OF_CONSOLE_DUMMY;
    return Init_Void(D_OUT);
}

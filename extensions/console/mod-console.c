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
//  export console: native [
//
//  {Runs customizable Read-Eval-Print Loop}
//
//      return: "Exit code, RESUME instruction, or handle to evaluator hook"
//          [integer! sym-group! handle!]
//      /resumable "Allow RESUME instruction (will return a SYM-GROUP!)"
//      /skin "File containing console skin, or MAKE CONSOLE! derived object"
//          [file! object!]
//  ]
//
REBNATIVE(console)
//
// !!! The idea behind the console is that it can be called with skinning;
// so that if BREAKPOINT wants to spin up a console, it can...but with a
// little bit of injected information like telling you the current stack
// level it's focused on.  How that's going to work is still pretty up
// in the air.
//
// What it will return will be either an exit code (INTEGER!), a signal for
// cancellation (BLANK!), or a debugging instruction (BLOCK!).
{
    CONSOLE_INCLUDE_PARAMS_OF_CONSOLE;

    // !!! The initial usermode console implementation was geared toward a
    // single `system/console` object.  But the debugger raised the issue of
    // nested sessions which might have a different skin.  So save whatever
    // the console object was if it is being overridden.

    REBVAL *old_console = rebValue(":system/console", rebEND);
    if (REF(skin))
        rebElide("system/console: _", rebEND);  // !!! needed for now

    REBVAL *responses = rebValueQ("make-chan/capacity 1", rebEND);
    REBVAL *requests = rebValueQ("make-chan/capacity 1", rebEND);

    rebElideQ(
        "go/kernel [",
            "console-impl",
            requests,
            responses,
            rebL(did REF(resumable)),
            REF(skin),
        "]",
        rebEND
    );

    REBVAL *code;

    while (true) {
        //
        // This runs the HOST-CONSOLE, which returns *requests* to execute
        // arbitrary code by way of its return results.  The ENTRAP is thus
        // here to intercept bugs *in HOST-CONSOLE itself*.  Any evaluations
        // for the user (or on behalf of the console skin) are done in
        // Run_Sandboxed_Group().
        //
        code = rebValueQ("receive-chan", requests, rebEND);

        if (rebDidQ("integer?", code, rebEND))
            break;  // when HOST-CONSOLE returns INTEGER! it means exit code

        if (rebDidQ("match [sym-group! handle!]", code, rebEND)) {
            assert(REF(resumable));
            break;
        }

        bool debuggable = rebDidQ("block?", code, rebEND);

        REBVAL *block;
        if (debuggable)
            block = code;
        else
            block = rebValueQ("as block!", rebR(code), rebEND);

        REBVAL *result;
        if (debuggable)
            result = rebValue(
                "receive-chan go/channel", rebR(block), rebEND
            );
        else
            result = rebValue(
                "receive-chan go/channel/kernel", rebR(block), rebEND
            );

        rebElideQ("send-chan", responses, rebR(result), rebEND);
    }

    // Exit code is now an INTEGER! or a resume instruction PATH!

    rebElideQ("system/console:", rebR(old_console), rebEND);

    // !!! Go lore says "don't close a channel from the receiver side".  This
    // means we should not close `requests`?
    //
    rebElideQ("close-chan", responses, rebEND);

    rebRelease(responses);
    rebRelease(requests);

    return code;  // http://stackoverflow.com/q/1101957/
}

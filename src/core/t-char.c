//
//  File: %t-char.c
//  Summary: "character datatype"
//  Section: datatypes
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2016 Rebol Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//=////////////////////////////////////////////////////////////////////////=//
//

#include "sys-core.h"


//
//  CT_Char: C
//
REBINT CT_Char(const RELVAL *a, const RELVAL *b, REBINT mode)
{
    REBINT num;

    if (mode >= 0) {
        if (mode == 0)
            num = LO_CASE(VAL_CHAR(a)) - LO_CASE(VAL_CHAR(b));
        else
            num = VAL_CHAR(a) - VAL_CHAR(b);
        return (num == 0);
    }

    num = VAL_CHAR(a) - VAL_CHAR(b);
    if (mode == -1) return (num >= 0);
    return (num > 0);
}


//
//  REBTYPE: C
//
REBTYPE(Char)
{
    REBUNI chr = VAL_CHAR(D_ARG(1));
    REBINT  arg;
    REBVAL  *val;

    if (IS_BINARY_ACT(action)) {
        val = D_ARG(2);
        if (IS_CHAR(val))
            arg = VAL_CHAR(val);
        else if (IS_INTEGER(val))
            arg = VAL_INT32(val);
        else if (IS_DECIMAL(val))
            arg = (REBINT)VAL_DECIMAL(val);
        else
            fail (Error_Math_Args(REB_CHAR, action));
    }

    switch (action) {

    case A_ADD:
        chr += cast(REBUNI, arg);
        break;

    case A_SUBTRACT:
        chr -= cast(REBUNI, arg);
        if (IS_CHAR(D_ARG(2))) {
            SET_INTEGER(D_OUT, chr);
            return R_OUT;
        }
        break;

    case A_MULTIPLY:
        chr *= arg;
        break;

    case A_DIVIDE:
        if (arg == 0) fail (Error(RE_ZERO_DIVIDE));
        chr /= arg;
        break;

    case A_REMAINDER:
        if (arg == 0) fail (Error(RE_ZERO_DIVIDE));
        chr %= arg;
        break;

    case A_AND_T:
        chr &= cast(REBUNI, arg);
        break;

    case A_OR_T:
        chr |= cast(REBUNI, arg);
        break;

    case A_XOR_T:
        chr ^= cast(REBUNI, arg);
        break;

    case A_NEGATE:
        chr = cast(REBUNI, -chr);
        break;

    case A_COMPLEMENT:
        chr = cast(REBUNI, ~chr);
        break;

    case A_EVEN_Q:
        return (cast(REBUNI, ~chr) & 1) ? R_TRUE : R_FALSE;

    case A_ODD_Q:
        return (chr & 1) ? R_TRUE : R_FALSE;

    case A_RANDOM:  //!!! needs further definition ?  random/zero
        if (D_REF(2)) { // /seed
            Set_Random(chr);
            return R_VOID;
        }
        if (chr == 0) break;
        chr = (REBUNI)(1 + ((REBCNT)Random_Int(D_REF(3)) % chr)); // /secure
        break;

    case A_MAKE:
    case A_TO:
        val = D_ARG(2);

        switch(VAL_TYPE(val)) {
        case REB_CHAR:
            chr = VAL_CHAR(val);
            break;

        case REB_INTEGER:
        case REB_DECIMAL:
            arg = Int32(val);
            if (arg > MAX_UNI || arg < 0) goto bad_make;
            chr = arg;
            break;

        case REB_BINARY:
        {
            const REBYTE *bp = VAL_BIN(val);
            arg = VAL_LEN_AT(val);
            if (arg == 0) goto bad_make;
            if (*bp > 0x80) {
                // !!! This test is presumably redundant - temporarily left
                // in as a check to see if its presence here detected
                // anything differently that Scan_UTF8_Char wouldn't.
                REBOOL redundant_legal = Legal_UTF8_Char(bp, arg);

                if (!Back_Scan_UTF8_Char(&chr, bp, NULL)) {
                    assert(!redundant_legal);
                    goto bad_make;
                }
                if (!redundant_legal) {
                    assert(FALSE);
                    goto bad_make;
                }
            }
            else
                chr = *bp;
        }
            break;

#ifdef removed
//      case REB_ISSUE:
            // Scan 8 or 16 bit hex str, will throw on error...
            arg = Scan_Hex_Value(
                VAL_RAW_DATA_AT(val), VAL_LEN_AT(val), !VAL_BYTE_SIZE(val)
            );
            if (arg > MAX_UNI || arg < 0) goto bad_make;
            chr = arg;
            break;
#endif

        case REB_STRING:
            if (VAL_INDEX(val) >= VAL_LEN_HEAD(val))
                fail (Error_Bad_Make(REB_CHAR, val));
            chr = GET_ANY_CHAR(VAL_SERIES(val), VAL_INDEX(val));
            break;

        default:
bad_make:
        fail (Error_Bad_Make(REB_CHAR, val));
    }
        break;

    default:
        fail (Error_Illegal_Action(REB_CHAR, action));
    }

    if ((chr >> 16) != 0 && (chr >> 16) != 0xffff)
        fail (Error(RE_TYPE_LIMIT, Get_Type(REB_CHAR)));
    SET_CHAR(D_OUT, chr);
    return R_OUT;
}


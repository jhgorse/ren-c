/***********************************************************************
**
**  REBOL [R3] Language Interpreter and Run-time Environment
**
**  Copyright 2012 REBOL Technologies
**  REBOL is a trademark of REBOL Technologies
**
**  Licensed under the Apache License, Version 2.0 (the "License");
**  you may not use this file except in compliance with the License.
**  You may obtain a copy of the License at
**
**  http://www.apache.org/licenses/LICENSE-2.0
**
**  Unless required by applicable law or agreed to in writing, software
**  distributed under the License is distributed on an "AS IS" BASIS,
**  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
**  See the License for the specific language governing permissions and
**  limitations under the License.
**
************************************************************************
**
**  Module:  c-do.c
**  Summary: the core interpreter - the heart of REBOL
**  Section: core
**  Author:  Carl Sassenrath
**  Notes:
**    WARNING WARNING WARNING
**    This is highly tuned code that should only be modified by experts
**    who fully understand its design. It is very easy to create odd
**    side effects so please be careful and extensively test all changes!
**
***********************************************************************/

#include "sys-core.h"

#include "tmp-evaltypes.h"


//
//  Eval_Depth: C
//
REBINT Eval_Depth(void)
{
    REBINT depth = 0;
    struct Reb_Frame *frame = FS_TOP;

    for (; frame != NULL; frame = FRM_PRIOR(frame), depth++)
        NOOP;

    return depth;
}


//
//  Frame_At_Depth: C
//
struct Reb_Frame *Frame_At_Depth(REBCNT n)
{
    struct Reb_Frame *frame = FS_TOP;

    while (frame) {
        if (n == 0) return frame;

        --n;
        frame = FRM_PRIOR(frame);
    }

    return NULL;
}


//
//  trace: native [
//  
//  {Enables and disables evaluation tracing and backtrace.}
//  
//      mode [integer! logic!]
//      /back {Set mode ON to enable or integer for lines to display}
//      /function "Traces functions only (less output)"
//  ]
//
REBNATIVE(trace)
{
    REBVAL *arg = D_ARG(1);

    Check_Security(SYM_DEBUG, POL_READ, 0);

    // The /back option: ON and OFF, or INTEGER! for # of lines:
    if (D_REF(2)) { // /back
        if (IS_LOGIC(arg)) {
            Enable_Backtrace(VAL_LOGIC(arg));
        }
        else if (IS_INTEGER(arg)) {
            REBINT lines = Int32(arg);
            Trace_Flags = 0;
            if (lines < 0) {
                fail (Error_Invalid_Arg(arg));
                return R_UNSET;
            }

            Display_Backtrace(cast(REBCNT, lines));
            return R_UNSET;
        }
    }
    else Enable_Backtrace(FALSE);

    // Set the trace level:
    if (IS_LOGIC(arg)) {
        Trace_Level = VAL_LOGIC(arg) ? 100000 : 0;
    }
    else Trace_Level = Int32(arg);

    if (Trace_Level) {
        Trace_Flags = 1;
        if (D_REF(3)) SET_FLAG(Trace_Flags, 1); // function
        Trace_Depth = Eval_Depth() - 1; // subtract current TRACE frame
    }
    else Trace_Flags = 0;

    return R_UNSET;
}

static REBINT Init_Depth(void)
{
    // Check the trace depth is ok:
    REBINT depth = Eval_Depth() - Trace_Depth;
    if (depth < 0 || depth >= Trace_Level) return -1;
    if (depth > 10) depth = 10;
    Debug_Space(cast(REBCNT, 4 * depth));
    return depth;
}

#define CHECK_DEPTH(d) if ((d = Init_Depth()) < 0) return;\

void Trace_Line(
    union Reb_Frame_Source source,
    REBIXO indexor,
    const REBVAL *value
) {
    int depth;

    if (GET_FLAG(Trace_Flags, 1)) return; // function
    if (IS_FUNCTION(value)) return;

    CHECK_DEPTH(depth);

    if (indexor == END_FLAG) {
        Debug_Fmt_("END_FLAG...");
    }
    else if (indexor == VALIST_FLAG) {
        Debug_Fmt_("VALIST_FLAG...");
    }
    else {
        REBCNT index = cast(REBCNT, indexor);
        Debug_Fmt_(cs_cast(BOOT_STR(RS_TRACE,1)), index, value);
    }

    if (IS_WORD(value) || IS_GET_WORD(value)) {
        value = GET_OPT_VAR_MAY_FAIL(value);
        if (VAL_TYPE(value) < REB_FUNCTION)
            Debug_Fmt_(cs_cast(BOOT_STR(RS_TRACE,2)), value);
        else if (VAL_TYPE(value) == REB_FUNCTION) {
            REBARR *words = List_Func_Words(value);
            Debug_Fmt_(
                cs_cast(BOOT_STR(RS_TRACE,3)), Get_Type_Name(value), words
            );
            Free_Array(words);
        }
        else
            Debug_Fmt_(cs_cast(BOOT_STR(RS_TRACE,4)), Get_Type_Name(value));
    }
    /*if (ANY_WORD(value)) {
        word = value;
        if (IS_WORD(value)) value = GET_OPT_VAR_MAY_FAIL(word);
        Debug_Fmt_(cs_cast(BOOT_STR(RS_TRACE,2)), VAL_WORD_CONTEXT(word), VAL_WORD_INDEX(word), Get_Type_Name(value));
    }
    if (Trace_Stack) Debug_Fmt(cs_cast(BOOT_STR(RS_TRACE,3)), DSP, FS_TOP);
    else
    */
    Debug_Line();
}

void Trace_Func(REBCNT label_sym, const REBVAL *value)
{
    int depth;
    CHECK_DEPTH(depth);
    Debug_Fmt_(
        cs_cast(BOOT_STR(RS_TRACE,5)),
        Get_Sym_Name(label_sym),
        Get_Type_Name(value)
    );
    if (GET_FLAG(Trace_Flags, 1))
        Debug_Values(FRM_ARG(FS_TOP, 1), FRM_NUM_ARGS(FS_TOP), 20);
    else Debug_Line();
}

void Trace_Return(REBCNT label_sym, const REBVAL *value)
{
    int depth;
    CHECK_DEPTH(depth);
    Debug_Fmt_(cs_cast(BOOT_STR(RS_TRACE,6)), Get_Sym_Name(label_sym));
    Debug_Values(value, 1, 50);
}

void Trace_Arg(REBINT num, const REBVAL *arg, const REBVAL *path)
{
    int depth;
    if (IS_REFINEMENT(arg) && (!path || IS_END(path))) return;
    CHECK_DEPTH(depth);
    Debug_Fmt(cs_cast(BOOT_STR(RS_TRACE,6)), num+1, arg);
}


//
//  Trace_Value: C
//
void Trace_Value(REBINT n, const REBVAL *value)
{
    int depth;
    CHECK_DEPTH(depth);
    Debug_Fmt(cs_cast(BOOT_STR(RS_TRACE,n)), value);
}


//
//  Trace_String: C
//
void Trace_String(REBINT n, const REBYTE *str, REBINT limit)
{
    static char tracebuf[64];
    int depth;
    int len = MIN(60, limit);
    CHECK_DEPTH(depth);
    memcpy(tracebuf, str, len);
    tracebuf[len] = '\0';
    Debug_Fmt(cs_cast(BOOT_STR(RS_TRACE,n)), tracebuf);
}


//
//  Trace_Error: C
//
void Trace_Error(const REBVAL *value)
{
    int depth;
    CHECK_DEPTH(depth);
    Debug_Fmt(
        cs_cast(BOOT_STR(RS_TRACE, 11)),
        &VAL_ERR_VALUES(value)->type,
        &VAL_ERR_VALUES(value)->id
    );
}


//
//  Next_Path_Throws: C
// 
// Evaluate next part of a path.
//
REBOOL Next_Path_Throws(REBPVS *pvs)
{
    REBVAL *path;
    REBPEF func;

    REBVAL temp;
    VAL_INIT_WRITABLE_DEBUG(&temp);

    // Path must have dispatcher, else return:
    func = Path_Dispatch[VAL_TYPE_0(pvs->value)];
    if (!func) return FALSE; // unwind, then check for errors

    pvs->path++;

    //Debug_Fmt("Next_Path: %r/%r", pvs->path-1, pvs->path);

    // object/:field case:
    if (IS_GET_WORD(path = pvs->path)) {
        pvs->select = GET_MUTABLE_VAR_MAY_FAIL(path);
        if (IS_UNSET(pvs->select))
            fail (Error(RE_NO_VALUE, path));
    }
    // object/(expr) case:
    else if (IS_GROUP(path)) {
        if (DO_VAL_ARRAY_AT_THROWS(&temp, path)) {
            *pvs->value = temp;
            return TRUE;
        }

        pvs->select = &temp;
    }
    else // object/word and object/value case:
        pvs->select = path;

    // Uses selector on the value.
    // .path - must be advanced as path is used (modified by func)
    // .value - holds currently evaluated path value (modified by func)
    // .select - selector on value
    // .store - storage (usually TOS) for constructed values
    // .setval - non-zero for SET-PATH (set to zero after SET is done)
    // .orig - original path for error messages
    switch (func(pvs)) {
    case PE_OK:
        break;
    case PE_SET: // only sets if end of path
        if (pvs->setval && IS_END(pvs->path+1)) {
            *pvs->value = *pvs->setval;
            pvs->setval = 0;
        }
        break;
    case PE_NONE:
        SET_NONE(pvs->store);
    case PE_USE:
        pvs->value = pvs->store;
        break;
    case PE_BAD_SELECT:
        fail (Error(RE_INVALID_PATH, pvs->orig, pvs->path));
    case PE_BAD_SET:
        fail (Error(RE_BAD_PATH_SET, pvs->orig, pvs->path));
    case PE_BAD_RANGE:
        fail (Error_Out_Of_Range(pvs->path));
    case PE_BAD_SET_TYPE:
        fail (Error(RE_BAD_FIELD_SET, pvs->path, Type_Of(pvs->setval)));
    default:
        assert(FALSE);
    }

    if (NOT_END(pvs->path + 1)) return Next_Path_Throws(pvs);

    return FALSE;
}


//
//  Do_Path_Throws: C
// 
// Evaluate a path value, given the first value in that path's series.  This
// evaluator may throw because parens are evaluated, e.g. `foo/(throw 1020)`
//
// If label_sym is passed in as being non-null, then the caller is implying
// readiness to process a path which may be a function with refinements.
// These refinements will be left in order on the data stack in the case
// that `out` comes back as IS_FUNCTION().
//
// If a `val` is provided, it is assumed to be a set-path and is set to that
// value IF the path evaluation did not throw or error.  HOWEVER the set value
// is NOT put into `out`.  This provides more flexibility on performance in
// the evaluator, which may already have the `val` where it wants it, and
// so the extra assignment would just be overhead.
//
// !!! Path evaluation is one of the parts of R3-Alpha that has not been
// vetted very heavily by Ren-C, and needs a review and overhaul.
//
REBOOL Do_Path_Throws(
    REBVAL *out,
    REBSYM *label_sym,
    const REBVAL *path,
    REBVAL *val
) {
    REBPVS pvs;
    REBDSP dsp_orig = DSP;

    assert(ANY_PATH(path));

    // !!! There is a bug in the dispatch such that if you are running a
    // set path, it does not always assign the output, because it "thinks you
    // aren't going to look at it".  This presumably originated from before
    // parens were allowed in paths, and neglects cases like:
    //
    //     foo/(throw 1020): value
    //
    // We always have to check to see if a throw occurred.  Until this is
    // streamlined, we have to at minimum set it to something that is *not*
    // thrown so that we aren't testing uninitialized memory.  A safe trash
    // will do, which is unset in release builds.
    //
    if (val)
        SET_TRASH_SAFE(out);

    // None of the values passed in can live on the data stack, because
    // they might be relocated during the path evaluation process.
    //
    assert(!IN_DATA_STACK(out));
    assert(!IN_DATA_STACK(path));
    assert(!val || !IN_DATA_STACK(val));

    // Not currently robust for reusing passed in path or value as the output
    assert(out != path && out != val);

    assert(!val || !THROWN(val));

    pvs.setval = val;       // Set to this new value
    pvs.store = out;        // Space for constructed results

    // Get first block value:
    pvs.orig = path;
    pvs.path = VAL_ARRAY_AT(pvs.orig);

    // Lookup the value of the variable:
    if (IS_WORD(pvs.path)) {
        pvs.value = GET_MUTABLE_VAR_MAY_FAIL(pvs.path);
        if (IS_UNSET(pvs.value))
            fail (Error(RE_NO_VALUE, pvs.path));
    }
    else pvs.value = pvs.path;

    // Start evaluation of path:
    if (IS_END(pvs.path + 1)) {
        // If it was a single element path, return the value rather than
        // try to dispatch it (would cause a crash at time of writing)
        //
        // !!! Is this the desired behavior, or should it be an error?
    }
    else if (Path_Dispatch[VAL_TYPE_0(pvs.value)]) {
        REBOOL threw = Next_Path_Throws(&pvs);

        // !!! See comments about why the initialization of out is necessary.
        // Without it this assertion can change on some things:
        //
        //     t: now
        //     t/time: 10:20:03
        //
        // (It thinks pvs.value has its THROWN bit set when it completed
        // successfully.  It was a PE_USE case where pvs.value was reset to
        // pvs.store, and pvs.store has its thrown bit set.  Valgrind does not
        // catch any uninitialized variables.)
        //
        // There are other cases that do trip valgrind when omitting the
        // initialization, though not as clearly reproducible.
        //
        assert(threw == THROWN(pvs.value));

        if (threw) return TRUE;

        // Check for errors:
        if (NOT_END(pvs.path + 1) && !IS_FUNCTION(pvs.value)) {
            // Only function refinements should get by this line:
            fail (Error(RE_INVALID_PATH, pvs.orig, pvs.path));
        }
    }
    else if (!IS_FUNCTION(pvs.value))
        fail (Error(RE_BAD_PATH_TYPE, pvs.orig, Type_Of(pvs.value)));

    if (val) {
        // If SET then we don't return anything
        assert(IS_END(pvs.path) + 1);
        return FALSE;
    }

    // If storage was not used, then copy final value back to it:
    if (pvs.value != pvs.store) *pvs.store = *pvs.value;

    assert(!THROWN(out));

    // Return 0 if not function or is :path/word...
    if (!IS_FUNCTION(pvs.value)) {
        assert(IS_END(pvs.path) + 1);
        return FALSE;
    }

    if (label_sym) {
        REBVAL refinement;
        VAL_INIT_WRITABLE_DEBUG(&refinement);

        // When a function is hit, path processing stops as soon as the
        // processed sub-path resolves to a function. The path is still sitting
        // on the position of the last component of that sub-path. Usually,
        // this last component in the sub-path is a word naming the function.
        //
        if (IS_WORD(pvs.path)) {
            *label_sym = VAL_WORD_SYM(pvs.path);
        }
        else {
            // In rarer cases, the final component (completing the sub-path to
            // the function to call) is not a word. Such as when you use a path
            // to pick by index out of a block of functions:
            //
            //      functions: reduce [:add :subtract]
            //      functions/1 10 20
            //
            // Or when you have an immediate function value in a path with a
            // refinement. Tricky to make, but possible:
            //
            //      do reduce [
            //          to-path reduce [:append 'only] [a] [b]
            //      ]
            //

            // !!! When a function was not invoked through looking up a word
            // (or a word in a path) to use as a label, there were once three
            // different alternate labels used.  One was SYM__APPLY_, another
            // was ROOT_NONAME, and another was to be the type of the function
            // being executed.  None are fantastic, we do the type for now.

            *label_sym = SYM_FROM_KIND(VAL_TYPE(pvs.value));
        }

        // Move on to the refinements (if any)
        pvs.path = pvs.path + 1;

        // !!! Currently, the mainline path evaluation "punts" on refinements.
        // When it finds a function, it stops the path evaluation and leaves
        // the position pvs.path before the list of refinements.
        //
        // A more elegant solution would be able to process and notice (for
        // instance) that `:APPEND/ONLY` should yield a function value that
        // has been specialized with a refinement.  Path chaining should thus
        // be able to effectively do this and give the refined function object
        // back to the evaluator or other client.
        //
        // If a label_sym is passed in, we recognize that a function dispatch
        // is going to be happening.  We do not want to pay to generate the
        // new series that would be needed to make a temporary function that
        // will be invoked and immediately GC'd  So we gather the refinements
        // on the data stack.
        //
        // This code simulates that path-processing-to-data-stack, but it
        // should really be something in dispatch iself.  In any case, we put
        // refinements on the data stack...and caller knows refinements are
        // from dsp_orig to DSP (thanks to accounting, all other operations
        // should balance!)

        for (; NOT_END(pvs.path); pvs.path++) { // "the refinements"
            if (IS_NONE(pvs.path)) continue;

            if (IS_GROUP(pvs.path)) {
                // Note it is not legal to use the data stack directly as the
                // output location for a DO (might be resized)

                if (DO_VAL_ARRAY_AT_THROWS(&refinement, pvs.path)) {
                    *out = refinement;
                    DS_DROP_TO(dsp_orig);
                    return TRUE;
                }
                if (IS_NONE(&refinement)) continue;
                DS_PUSH(&refinement);
            }
            else if (IS_GET_WORD(pvs.path)) {
                DS_PUSH_TRASH;
                *DS_TOP = *GET_OPT_VAR_MAY_FAIL(pvs.path);
                if (IS_NONE(DS_TOP)) {
                    DS_DROP;
                    continue;
                }
            }
            else
                DS_PUSH(pvs.path);

            // Whatever we were trying to use as a refinement should now be
            // on the top of the data stack, and only words are legal ATM
            //
            if (!IS_WORD(DS_TOP))
                fail (Error(RE_BAD_REFINE, DS_TOP));

            // Go ahead and canonize the word symbol so we don't have to
            // do it each time in order to get a case-insenstive compare
            //
            INIT_WORD_SYM(DS_TOP, SYMBOL_TO_CANON(VAL_WORD_SYM(DS_TOP)));
        }

        // To make things easier for processing, reverse the refinements on
        // the data stack (we needed to evaluate them in forward order).
        // This way we can just pop them as we go, and know if they weren't
        // all consumed if it doesn't get back to `dsp_orig` by the end.

        if (dsp_orig != DSP) {
            REBVAL *bottom = DS_AT(dsp_orig + 1);
            REBVAL *top = DS_TOP;
            while (top > bottom) {
                refinement = *bottom;
                *bottom = *top;
                *top = refinement;

                top--;
                bottom++;
            }
        }
    }
    else {
        // !!! Historically this just ignores a result indicating this is a
        // function with refinements, e.g. ':append/only'.  However that
        // ignoring seems unwise.  It should presumably create a modified
        // function in that case which acts as if it has the refinement.
        //
        // If the caller did not pass in a label pointer we assume they are
        // likely not ready to process any refinements.
        //
        if (NOT_END(pvs.path + 1))
            fail (Error(RE_TOO_LONG)); // !!! Better error or add feature
    }

    return FALSE;
}


//
//  Pick_Path: C
// 
// Lightweight version of Do_Path used for A_PICK actions.
// Does not do GROUP! evaluation, hence not designed to throw.
//
void Pick_Path(REBVAL *out, REBVAL *value, REBVAL *selector, REBVAL *val)
{
    REBPVS pvs;
    REBPEF func;

    pvs.value = value;
    pvs.path = 0;
    pvs.select = selector;
    pvs.setval = val;
    pvs.store = out;        // Temp space for constructed results

    // Path must have dispatcher, else return:
    func = Path_Dispatch[VAL_TYPE_0(value)];
    if (!func) return; // unwind, then check for errors

    switch (func(&pvs)) {
    case PE_OK:
        break;
    case PE_SET: // only sets if end of path
        if (pvs.setval) *pvs.value = *pvs.setval;
        break;
    case PE_NONE:
        SET_NONE(pvs.store);
    case PE_USE:
        pvs.value = pvs.store;
        break;
    case PE_BAD_SELECT:
        fail (Error(RE_INVALID_PATH, pvs.value, pvs.select));
    case PE_BAD_SET:
        fail (Error(RE_BAD_PATH_SET, pvs.value, pvs.select));
    default:
        assert(FALSE);
    }
}


//
//  Do_Signals_Throws: C
// 
// Special events to process periodically during evaluation. Search for
// SET_SIGNAL to find them.  (Note: Not to be confused with SIGINT and unix
// signals, although possibly triggered by one.)
//
// Currently the ability of a signal to THROW comes from the processing of
// breakpoints.  The RESUME instruction is able to execute code with /DO,
// and that code may escape the
//
REBOOL Do_Signals_Throws(REBVAL *out)
{
    struct Reb_State state;
    REBCTX *error;

    REBCNT sigs;
    REBCNT mask;

    assert(Saved_State || PG_Boot_Phase < BOOT_MEZZ);

    // Accumulate evaluation counter and reset countdown:
    if (Eval_Count <= 0) {
        //Debug_Num("Poll:", (REBINT) Eval_Cycles);
        Eval_Cycles += Eval_Dose - Eval_Count;
        Eval_Count = Eval_Dose;
        if (Eval_Limit != 0 && Eval_Cycles > Eval_Limit)
            Check_Security(SYM_EVAL, POL_EXEC, 0);
    }

    if (!(Eval_Signals & Eval_Sigmask)) {
        SET_UNSET(out);
        return FALSE;
    }

    // Be careful of signal loops! EG: do not PRINT from here.
    sigs = Eval_Signals & (mask = Eval_Sigmask);
    Eval_Sigmask = 0;   // avoid infinite loop
    //Debug_Num("Signals:", Eval_Signals);

    // Check for recycle signal:
    if (GET_FLAG(sigs, SIG_RECYCLE)) {
        CLR_SIGNAL(SIG_RECYCLE);
        Recycle();
    }

#ifdef NOT_USED_INVESTIGATE
    if (GET_FLAG(sigs, SIG_EVENT_PORT)) {  // !!! Why not used?
        CLR_SIGNAL(SIG_EVENT_PORT);
        Awake_Event_Port();
    }
#endif

    // Breaking only allowed after MEZZ boot
    //
    if (GET_FLAG(sigs, SIG_INTERRUPT) && PG_Boot_Phase >= BOOT_MEZZ) {
        CLR_SIGNAL(SIG_INTERRUPT);
        Eval_Sigmask = mask;

        if (Do_Breakpoint_Throws(out, TRUE, UNSET_VALUE, FALSE))
            return TRUE;

        return FALSE;
    }

    // Halting only allowed after MEZZ boot
    //
    if (GET_FLAG(sigs, SIG_HALT) && PG_Boot_Phase >= BOOT_MEZZ) {
        CLR_SIGNAL(SIG_HALT);
        Eval_Sigmask = mask;

        fail (VAL_CONTEXT(TASK_HALT_ERROR));
    }

    Eval_Sigmask = mask;

    SET_UNSET(out);
    return FALSE;
}


#if !defined(NDEBUG)

#include "debugbreak.h"


//
//  In_Legacy_Function_Debug: C
//
// Determine if a legacy function is "in effect" currently.  To the extent
// that compatibility in debug builds or legacy mode with R3-Alpha is
// "important" this should be used sparingly, because code can be bound and
// passed around in blocks.  So you might be running a legacy function passed
// new code or new code passed legacy code (e.g. a mezzanine that uses DO)
//
REBOOL In_Legacy_Function_Debug(void)
{
    // Find the first bit of code that's actually running ordinarily in
    // the evaluator, and not just dispatching.
    //
    struct Reb_Frame *frame = FS_TOP;
    for (; frame != NULL; frame = frame->prior) {
        if (frame->flags & DO_FLAG_VALIST)
            return FALSE; // no source array to look at

        break; // whatever's dispatching here, there is a source array
    }

    if (frame == NULL)
        return FALSE;

    // Check the flag on the source series
    //
    if (GET_ARR_FLAG(frame->source.array, SERIES_FLAG_LEGACY))
        return TRUE;

    return FALSE;
}


//
// R3-Alpha and Rebol2 used TRUE for a refinement and NONE for the argument
// to a refinement which is not present.  Ren-C provides the name of the
// argument as a WORD! if for the refinement, and UNSET! for refinement
// args that are not there.  (This makes chaining work.)
//
// Could be woven in efficiently, but as it's a debug build only feature it's
// better to isolate it into a post-phase.  This improves the readability of
// the mainline code.
//
static void Legacy_Convert_Function_Args_Debug(struct Reb_Frame *f)
{
    REBVAL *param = FUNC_PARAMS_HEAD(f->func);
    REBVAL *arg = FRM_ARGS_HEAD(f);

    REBOOL set_none = FALSE;

    for (; NOT_END(param); ++param, ++arg) {
        if (GET_VAL_FLAG(param, TYPESET_FLAG_REFINEMENT)) {

            if (IS_WORD(arg)) {
                assert(VAL_WORD_SYM(arg) == VAL_TYPESET_SYM(param));
                SET_TRUE(arg);
                set_none = FALSE;
            }
            else if (IS_NONE(arg)) {
                set_none = TRUE;
            }
            else assert(FALSE);
        }
        else {
            if (set_none && !GET_VAL_FLAG(param, TYPESET_FLAG_HIDDEN)) {
                assert(IS_UNSET(arg));
                SET_NONE(arg);
            }
        }
    }
}


//
//  Trace_Fetch_Debug: C
//
// When down to the wire and wanting to debug the evaluator, it can be very
// useful to see the steps of the states it's going through to see what is
// wrong.  This routine hooks the individual fetch and writes at a more
// fine-grained level than a breakpoint at each DO/NEXT point.
//
void Trace_Fetch_Debug(const char* msg, struct Reb_Frame *f, REBOOL after) {
    Debug_Fmt(
        "%d - %s : %s",
        cast(REBCNT, f->indexor),
        msg,
        after ? "AFTER" : "BEFORE"
    );
    assert(f->value != NULL || (after && f->indexor == END_FLAG));
    if (f->value)
        PROBE(f->value);
}


//
// The entry checks to DO are for verifying that the setup of the Reb_Frame
// passed in was valid.  They run just once for each Do_Core() call, and
// are only in the debug build.
//
static REBCNT Do_Entry_Checks_Debug(struct Reb_Frame *f)
{
    // The caller must preload ->value with the first value to process.  It
    // may be resident in the array passed that will be used to fetch further
    // values, or it may not.
    //
    assert(f->value);

    // All callers should ensure that the type isn't an END marker before
    // bothering to invoke Do_Core().
    //
    assert(NOT_END(f->value));

    // Though we can protect the value written into the target pointer 'out'
    // from GC during the course of evaluation, we can't protect the
    // underlying value from relocation.  Technically this would be a problem
    // for any series which might be modified while this call is running, but
    // most notably it applies to the data stack--where output used to always
    // be returned.
    //
#ifdef STRESS_CHECK_DO_OUT_POINTER
    ASSERT_NOT_IN_SERIES_DATA(f->out);
#else
    assert(!IN_DATA_STACK(f->out));
#endif

    // The DO_FLAGs were decided to come in pairs for clarity, to make sure
    // that each callsite of the core routines was clear on what it was
    // asking for.  This may or may not be overkill long term, but helps now.
    //
    assert(
        LOGICAL(f->flags & DO_FLAG_NEXT)
        != LOGICAL(f->flags & DO_FLAG_TO_END)
    );
    assert(
        LOGICAL(f->flags & DO_FLAG_LOOKAHEAD)
        != LOGICAL(f->flags & DO_FLAG_NO_LOOKAHEAD)
    );
    assert(
        LOGICAL(f->flags & DO_FLAG_EVAL_NORMAL)
        != LOGICAL(f->flags & DO_FLAG_EVAL_ONLY)
    );

    // This flag is managed solely by the frame code; shouldn't come in set
    //
    assert(NOT(f->flags & DO_FLAG_FRAME_CONTEXT));

#if !defined(NDEBUG)
    //
    // This has to be nulled out in the debug build by the code itself inline,
    // because sometimes one stackvars call ends and then another starts
    // before the debug preamble is run.  Give it an initial NULL here though.
    //
    f->data.stackvars = NULL;
#endif

    // Snapshot the "tick count" to assist in showing the value of the tick
    // count at each level in a stack, so breakpoints can be strategically
    // set for that tick based on higher levels than the value you might
    // see during a crash.
    //
    f->do_count = TG_Do_Count;
    return f->do_count;
}


//
// The iteration preamble takes care of clearing out variables and preparing
// the state for a new "/NEXT" evaluation.  It's a way of ensuring in the
// debug build that one evaluation does not leak data into the next, and
// making the code shareable allows code paths that jump to later spots
// in the switch (vs. starting at the top) to reuse the work.
//
static REBCNT Do_Evaluation_Preamble_Debug(struct Reb_Frame *f) {
    //
    // The ->mode is examined by parts of the system as a sign of whether
    // the stack represents a function invocation or not.  If it is changed
    // from CALL_MODE_GUARD_ARRAY_ONLY during an evaluation step, it must
    // be changed back before a next step is to run.
    //
    assert(f->mode == CALL_MODE_GUARD_ARRAY_ONLY);

    // If running the evaluator, then this frame should be the topmost on the
    // frame stack.
    //
    assert(f == FS_TOP);

    // We checked for END when we entered Do_Core() and short circuited
    // that, but if we're running DO_FLAG_TO_END then the catch for that is
    // an index check.  We shouldn't go back and `do_at_index` on an end!
    //
    assert(f->value && NOT_END(f->value));
    assert(f->indexor != THROWN_FLAG);

    // Note that `f->indexor` *might* be END_FLAG in the case of an eval;
    // if you write `do [eval help]` then it will load help in as f->value
    // and retrigger, and `help` (for instance) is capable of handling a
    // prefetched input that is at end.  This is different from most cases
    // where END_FLAG directly implies prefetch input was exhausted and
    // f->value must be NULL.
    //
    assert(f->indexor != END_FLAG || IS_END(f->eval_fetched));

    // The value we are processing should not be THROWN() and any series in
    // it should be under management by the garbage collector.
    //
    // !!! THROWN() bit on individual values is in the process of being
    // deprecated, in favor of the evaluator being in a "throwing state".
    //
    assert(!THROWN(f->value));
    ASSERT_VALUE_MANAGED(f->value);

    // Trash call variables in debug build to make sure they're not reused.
    // Note that this call frame will *not* be seen by the GC unless it gets
    // chained in via a function execution, so it's okay to put "non-GC safe"
    // trash in at this point...though by the time of that call, they must
    // hold valid values.
    //
    f->func = cast(REBFUN*, 0xDECAFBAD);
    f->label_sym = SYM_0;
    f->label_str = "(no current label)";
    f->param = cast(REBVAL*, 0xDECAFBAD);
    f->arg = cast(REBVAL*, 0xDECAFBAD);
    f->refine = cast(REBVAL*, 0xDECAFBAD);

    // This counter is helpful for tracking a specific invocation.
    // If you notice a crash, look on the stack for the topmost call
    // and read the count...then put that here and recompile with
    // a breakpoint set.  (The 'TG_Do_Count' value is captured into a
    // local 'count' so you still get the right count after recursion.)
    //
    // We bound it at the max unsigned 32-bit because otherwise it would
    // roll over to zero and print a message that wasn't asked for, which
    // is annoying even in a debug build.
    //
    if (TG_Do_Count < MAX_U32) {
        f->do_count = ++TG_Do_Count;
        if (f->do_count ==
            // *** DON'T COMMIT THIS v-- KEEP IT AT ZERO! ***
                                      0
            // *** DON'T COMMIT THIS --^ KEEP IT AT ZERO! ***
        ) {
            if (f->indexor == VALIST_FLAG) {
                //
                // !!! Can't fetch the next value here without destroying the
                // forward iteration.  Destructive debugging techniques could
                // be added here on demand, or non-destructive ones that
                // logged the va_list into a dynamically allocated array
                // could be put in the debug build, etc.  Add when necessary.
                //
                Debug_Fmt(
                    "Do_Core() count trap (va_list, no nondestructive fetch)"
                );
            }
            else if (f->indexor == END_FLAG) {
                assert(f->value != NULL);
                Debug_Fmt("Performing EVAL at end of array (no args)");
                PROBE_MSG(f->value, "Do_Core() count trap");
            }
            else {
                REBVAL dump;
                VAL_INIT_WRITABLE_DEBUG(&dump);

                PROBE_MSG(f->value, "Do_Core() count trap");
                Val_Init_Block_Index(
                    &dump, f->source.array, cast(REBCNT, f->indexor)
                );
                PROBE_MSG(&dump, "Do_Core() next up...");
            }
        }
    }

    return f->do_count;
}

// Needs a forward declaration, but makes more sense to locate after Do_Core()
//
static void Do_Exit_Checks_Debug(struct Reb_Frame *f);

#endif


// Simple macro for wrapping (but not obscuring) a `goto` in the code below
//
#define NOTE_THROWING(g) \
    do { \
        assert(f->indexor == THROWN_FLAG); \
        assert(THROWN(f->out)); \
        g; /* goto statement left at callsite for readability */ \
    } while(0)


//
//  Do_Core: C
// 
// Evaluate the code block until we have:
//
//     1. An irreducible value (return next index)
//     2. Reached the end of the block (return END_FLAG)
//     3. Encountered an error
//
// For comprehensive notes on the input parameters, output parameters, and
// internal state variables...see %sys-do.h and `struct Reb_Frame`.
// 
// NOTES:
//
// 1. This is a very long routine.  That is largely on purpose, because it
//    does not contain repeated portions...and is a critical performance
//    bottleneck in the system.  So dividing it for the sake of "having
//    more functions" wouldn't be a good idea, especially since the target
//    is compilers that are so simple they may not have proper inlining
//    (which is only a "hint" to the compiler even if it's supported).
//
// 2. Changing the behavior of the parameter fulfillment in this core routine
//    generally also means changes to two other semi-parallel routines:
//    `Apply_Block_Throws()` and `Redo_Func_Throws().`  Review the impacts
//    of any changes on all three.
//
// The evaluator only moves forward, and it consumes exactly one element from
// the input at a time.  This input may be a source where the index needs
// to be tracked and care taken to contain the index within its boundaries
// in the face of change (e.g. a mutable ARRAY).  Or it may be an entity
// which tracks its own position on each fetch and where it is immutable,
// where the "index" is serving as a flag and should be left static.
//
// !!! There is currently no "locking" or other protection on the arrays that
// are in the call stack and executing.  Each iteration must be prepared for
// the case that the array has been modified out from under it.  The code
// evaluator will not crash, but re-fetches...ending the evaluation if the
// array has been shortened to before the index, and using possibly new
// values.  The benefits of this self-modifying lenience should be reviewed
// to inform a decision regarding the locking of arrays during evaluation.
//
void Do_Core(struct Reb_Frame * const f)
{
#if !defined(NDEBUG)
    //
    // Debug reuses PUSH_TRAP's snapshotting to check for leaks on each step
    //
    struct Reb_State state;

    // Cache of `f->do_count` (to make it quicker to see in the debugger)
    //
    REBCNT do_count;
#endif

    // Definitional Return gives back a "corrupted" REBVAL of a return native,
    // whose body is actually an indicator of the return target.  The
    // Reb_Frame only stores the FUNC so we must extract this body from the
    // value if it represents a exit_from
    //
    REBARR *exit_from;

    // See notes below on reference for why this is needed to implement eval.
    //
    REBOOL eval_normal; // EVAL/ONLY can trigger this to FALSE

    // Check just once (stack level would be constant if checked in a loop)
    //
    if (C_STACK_OVERFLOWING(&f)) Trap_Stack_Overflow();

    // Chain the call state into the stack, and mark it as generally not
    // having valid fields to GC protect (more valid during function calls).
    //
    f->prior = TG_Frame_Stack;
    TG_Frame_Stack = f;
    f->mode = CALL_MODE_GUARD_ARRAY_ONLY;

#if !defined(NDEBUG)
    SNAP_STATE(&state); // for comparison to make sure stack balances, etc.
    do_count = Do_Entry_Checks_Debug(f); // checks that run once per Do_Core()
#endif

    // Capture the data stack pointer on entry (used by debug checks, but
    // also refinements are pushed to stack and need to be checked if there
    // are any that are not processed)
    //
    f->dsp_orig = DSP;

    // Indicate that we do not have a value already fetched by eval which is
    // pending to be the next fetch (after the eval's "slipstreamed" f->value
    // is done processing).
    //
    f->eval_fetched = NULL;

    // The f->out slot is GC protected while the natives or user code runs.
    // To keep it from crashing the GC, we put in "safe trash" that will be
    // acceptable to the GC but raise alerts if any other code reads it.
    //
    SET_TRASH_SAFE(f->out);

value_ready_for_do_next:
    //
    // f->value is expected to be set here, as is f->index
    //
    // !!! are there more rules for the locations value can't point to?
    // Note that a fetched value pointer may be within a va_arg list.  Also
    // consider the GC implications of running ANY non-EVAL/ONLY scenario;
    // how do you know the values are safe?  (See ideas in %sys-do.h)
    //
    assert(f->value && !IS_END(f->value) && f->value != f->out);
    assert(f->indexor != END_FLAG && f->indexor != THROWN_FLAG);

    if (Trace_Flags) Trace_Line(f->source, f->indexor, f->value);

    // Save the index at the start of the expression in case it is needed
    // for error reporting.  FRM_INDEX can account for prefetching, but it
    // cannot know what a preloaded head value was unless it was saved
    // under a debug> mode.
    //
    if (f->indexor != VALIST_FLAG) f->expr_index = f->indexor;

    // Make sure `eval` is trash in debug build if not doing a `reevaluate`.
    // It does not have to be GC safe (for reasons explained below).  We
    // also need to reset evaluation to normal vs. a kind of "inline quoting"
    // in case EVAL/ONLY had enabled that.
    //
    VAL_INIT_WRITABLE_DEBUG(&(f->cell.eval)); // in union, always reinit
    SET_TRASH_IF_DEBUG(&(f->cell.eval));
    eval_normal = LOGICAL(f->flags & DO_FLAG_EVAL_NORMAL);

    // If we're going to jump to the `reevaluate:` label below we should not
    // consider it a Recycle() opportunity.  The value residing in `eval`
    // is a local variable unseen by the GC *by design*--to avoid having
    // to initialize it or GC-safe de-initialize it each time through
    // the evaluator loop.  It will only be protected by the GC under
    // circumstances that wind up extracting its properties during a needed
    // evaluation (hence protected indirectly via `f->array` or `f->func`.)
    //
    if (--Eval_Count <= 0 || Eval_Signals) {
        //
        // Note that Do_Signals_Throws() may do a recycle step of the GC, or
        // it may spawn an entire interactive debugging session via
        // breakpoint before it returns.  It may also FAIL and longjmp out.
        //
        if (Do_Signals_Throws(f->out)) {
            f->indexor = THROWN_FLAG;
            NOTE_THROWING(goto return_indexor);
        }

        if (!IS_UNSET(f->out)) {
            //
            // !!! What to do with something like a Ctrl-C-based breakpoint
            // session that does something like `resume/with 10`?  We are
            // "in-between" evaluations, so that 10 really has no meaning
            // and is just going to get discarded.  FAIL for now to alert
            // the user that something is off, but perhaps the failure
            // should be contained in a sandbox and restart the break?
            //
            fail (Error(RE_MISC));
        }
    }

reevaluate:
    //
    // ^-- See why reevaluate must jump to be *after* a potentical GC point.
    // (We also want the debugger to consider the triggering EVAL as the
    // start of the expression, and don't want to advance `expr_index`).

    // On entry we initialized `f->out` to a GC-safe value, and no evaluations
    // should write END markers or unsafe trash in the slot.  As evaluations
    // proceed the value they wrote in `f->out` should be fine to leave there
    // as it won't crash the GC--and is cheaper than overwriting.  But in the
    // debug build, throw safe trash in the slot half the time to catch stray
    // reuses of irrelevant data...and test the release path the other half.
    //
    if (SPORADICALLY(2)) SET_TRASH_SAFE(f->out);

#if !defined(NDEBUG)
    do_count = Do_Evaluation_Preamble_Debug(f); // per-DO/NEXT debug checks
    cast(void, do_count); // suppress unused warning
    exit_from = cast(REBARR*, 0xDECAFBAD); // trash local to alert on reuse
#endif

    //==////////////////////////////////////////////////////////////////==//
    //
    // BEGIN MAIN SWITCH STATEMENT
    //
    //==////////////////////////////////////////////////////////////////==//

    switch (Eval_Table[VAL_TYPE(f->value)]) { // see ET_XXX, RE: jump table

//==//////////////////////////////////////////////////////////////////////==//
//
// [no evaluation] (REB_BLOCK, REB_INTEGER, REB_STRING, etc.)
//
// Copy the value's bits to f->out and fetch the next value.  (Infix behavior
// may kick in for this same "DO/NEXT" step--see processing after switch.)
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_NONE:
        DO_NEXT_REFETCH_QUOTED(f->out, f);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [BAR! and LIT-BAR!]
//
// If an expression barrier is seen in-between expressions (as it will always
// be if hit in this switch), it becomes UNSET!.  It only errors in argument
// fulfillment during the switch case for ANY-FUNCTION!.
//
// LIT-BAR! decays into an ordinary BAR! if seen here by the evaluator.
//
// Note that natives and dialects frequently do their own interpretation of
// BAR!--rather than just evaluate it and let it mean something equivalent
// to an unset.  For instance:
//
//     case [false [print "F"] | true [print ["T"]]
//
// If CASE did not specially recognize BAR!, it would complain that the
// "second condition" was UNSET!.  So if you are looking for a BAR! behavior
// and it's not passing through here, check the construct you are using.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_BAR:
        SET_UNSET(f->out);
        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

    case ET_LIT_BAR:
        SET_BAR(f->out);
        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [WORD!]
//
// A plain word tries to fetch its value through its binding.  It will fail
// and longjmp out of this stack if the word is unbound (or if the binding is
// to a variable which is not set).  Should the word look up to a function,
// then that function will be called by jumping to the ANY-FUNCTION! case.
//
// Note: Infix functions cannot be dispatched from this point, as there is no
// "Left-Hand-Side" computed to use.  Infix dispatch happens on words during
// a lookahead *after* this switch statement, when a omputed value in f->out
// is available.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_WORD:
        *(f->out) = *GET_OPT_VAR_MAY_FAIL(f->value);

    dispatch_the_word_in_out:
        if (IS_FUNCTION(f->out)) { // check before checking unset, for speed
            if (GET_VAL_FLAG(f->out, FUNC_FLAG_INFIX))
                fail (Error(RE_NO_OP_ARG, f->value)); // see Note above

            f->label_sym = VAL_WORD_SYM(f->value);

        #if !defined(NDEBUG)
            f->label_str = cast(const char*, Get_Sym_Name(f->label_sym));
        #endif

            if (Trace_Flags) Trace_Line(f->source, f->indexor, f->value);

            f->value = f->out;
            goto do_function_in_value;
        }

        if (IS_UNSET(f->out))
            fail (Error(RE_NO_VALUE, f->value)); // need `:x` if `x` is unset

    #if !defined(NDEBUG)
        if (LEGACY(OPTIONS_LIT_WORD_DECAY) && IS_LIT_WORD(f->out))
            VAL_SET_TYPE_BITS(f->out, REB_WORD); // don't reset full header!
    #endif

        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [SET-WORD!]
//
// Does the evaluation into `out`, then gets the variable indicated by the
// word and writes the result there as well.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_SET_WORD:
        f->param = f->value; // fetch writes f->value, so save SET-WORD! ptr

        FETCH_NEXT_ONLY_MAYBE_END(f);
        if (f->indexor == END_FLAG)
            fail (Error(RE_NEED_VALUE, f->param)); // e.g. `do [foo:]`

        if (eval_normal) { // not using EVAL/ONLY
            DO_NEXT_REFETCH_MAY_THROW(f->out, f, DO_FLAG_LOOKAHEAD);
            if (f->indexor == THROWN_FLAG)
                NOTE_THROWING(goto return_indexor);
        }
        else
            DO_NEXT_REFETCH_QUOTED(f->out, f);

        if (IS_UNSET(f->out))
            fail (Error(RE_NEED_VALUE, f->param)); // e.g. `foo: ()`

        *GET_MUTABLE_VAR_MAY_FAIL(f->param) = *(f->out);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [GET-WORD!]
//
// A GET-WORD! does no checking for unsets, no dispatch on functions, and
// will return an UNSET! if that is what the variable is.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_GET_WORD:
        *(f->out) = *GET_OPT_VAR_MAY_FAIL(f->value);
        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

//==/////////////////////////////////////////////////////////////////////==//
//
// [LIT-WORD!]
//
// Note we only want to reset the type bits in the header, not the whole
// header--because header bits contain information like WORD_FLAG_BOUND.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_LIT_WORD:
        DO_NEXT_REFETCH_QUOTED(f->out, f);
        VAL_SET_TYPE_BITS(f->out, REB_WORD);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [GROUP!]
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_GROUP:
        if (DO_VAL_ARRAY_AT_THROWS(f->out, f->value)) {
            f->indexor = THROWN_FLAG;
            NOTE_THROWING(goto return_indexor);
        }
        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [PATH!]
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_PATH:
        if (Do_Path_Throws(f->out, &f->label_sym, f->value, NULL)) {
            f->indexor = THROWN_FLAG;
            NOTE_THROWING(goto return_indexor);
        }

        if (IS_FUNCTION(f->out)) {
            //
            // object/func or func/refinements or object/func/refinement
            //
            // Because we passed in a label symbol, the path evaluator was
            // willing to assume we are going to invoke a function if it
            // is one.  Hence it left any potential refinements on data stack.
            //
            assert(DSP >= f->dsp_orig);

            // Cannot handle infix because prior value is wiped out above
            // (Theoretically we could save it if we are DO-ing a chain of
            // values, and make it work.  But then, a loop of DO/NEXT
            // may not behave the same as DO-ing the whole block.  Bad.)
            //
            if (GET_VAL_FLAG(f->out, FUNC_FLAG_INFIX))
                fail (Error_Has_Bad_Type(f->out));

            f->value = f->out;
            goto do_function_in_value;
        }
        else {
            // Path should have been fully processed, no refinements on stack
            //
            assert(DSP == f->dsp_orig);
            FETCH_NEXT_ONLY_MAYBE_END(f);
        }
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [SET-PATH!]
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_SET_PATH:
        f->param = f->value; // fetch writes f->value, save SET-WORD! pointer

        FETCH_NEXT_ONLY_MAYBE_END(f);

        // `do [a/b/c:]` is not legal
        //
        if (f->indexor == END_FLAG)
            fail (Error(RE_NEED_VALUE, f->param));

        // We want the result of the set path to wind up in `out`, so go
        // ahead and put the result of the evaluation there.  Do_Path_Throws
        // will *not* put this value in the output when it is making the
        // variable assignment!
        //
        if (eval_normal) {
            DO_NEXT_REFETCH_MAY_THROW(f->out, f, DO_FLAG_LOOKAHEAD);

            if (f->indexor == THROWN_FLAG)
                NOTE_THROWING(goto return_indexor);
        }
        else {
            *(f->out) = *(f->value);
            FETCH_NEXT_ONLY_MAYBE_END(f);
        }

        // `a/b/c: ()` is not legal (cannot assign path from unset)
        //
        if (IS_UNSET(f->out))
            fail (Error(RE_NEED_VALUE, f->param));

        // !!! The evaluation ordering of SET-PATH! evaluation seems to break
        // the "left-to-right" nature of the language:
        //
        //     >> foo: make object! [bar: 10]
        //
        //     >> foo/(print "left" 'bar): (print "right" 20)
        //     right
        //     left
        //     == 20
        //
        // In addition to seeming "wrong" it also necessitates an extra cell
        // of storage.  This should be reviewed along with Do_Path generally.
        {
            REBVAL temp;
            VAL_INIT_WRITABLE_DEBUG(&temp);
            if (Do_Path_Throws(&temp, NULL, f->param, f->out)) {
                f->indexor = THROWN_FLAG;
                *(f->out) = temp;
                NOTE_THROWING(goto return_indexor);
            }
        }

        // We did not pass in a symbol, so not a call... hence we cannot
        // process refinements.  Should not get any back.
        //
        assert(DSP == f->dsp_orig);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [GET-PATH!]
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_GET_PATH:
        //
        // returns in word the path item, DS_TOP has value
        //
        if (Do_Path_Throws(f->out, NULL, f->value, NULL)) {
            f->indexor = THROWN_FLAG;
            NOTE_THROWING(goto return_indexor);
        }

        // We did not pass in a symbol ID
        //
        assert(DSP == f->dsp_orig);
        FETCH_NEXT_ONLY_MAYBE_END(f);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [LIT-PATH!]
//
// We only set the type, in order to preserve the header bits... (there
// currently aren't any for ANY-PATH!, but there might be someday.)
//
// !!! Aliases a REBSER under two value types, likely bad, see #2233
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_LIT_PATH:
        DO_NEXT_REFETCH_QUOTED(f->out, f);
        VAL_SET_TYPE_BITS(f->out, REB_PATH);
        break;

//==//////////////////////////////////////////////////////////////////////==//
//
// [FUNCTION!]
//
// If a function makes it to the SWITCH statement, that means it is either
// literally a function value in the array (`do compose [(:+) 1 2]`) or is
// being retriggered via EVAL.  Note that infix functions that are
// encountered in this way will behave as prefix--their infix behavior
// is only triggered when they are looked up from a word.  See #1934.
//
// Most function evaluations are triggered from a SWITCH on a WORD! or PATH!,
// which jumps in at the `do_function_in_value` label.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_FUNCTION:
        //
        // Note: Because this is a function value being hit literally in
        // a block, it does not have a name.  Use symbol of its VAL_TYPE
        //
        f->label_sym = SYM_FROM_KIND(VAL_TYPE(f->value));

    #if !defined(NDEBUG)
        f->label_str = cast(const char*, Get_Sym_Name(f->label_sym));
    #endif

    do_function_in_value:
        //
        // `do_function_in_value` expects the function to be in f->value,
        // and if it's a definitional return we need to extract its target.
        // (the REBVAL you get from FUNC_VALUE() does not have the exit_from
        // poked into it.)
        //
        // Note that you *can* have a 'literal' definitional return value,
        // because the user can compose it into a block like any function.
        //
        assert(IS_FUNCTION(f->value));
        f->func = VAL_FUNC(f->value);

        // There may be refinements pushed to the data stack to process, if
        // the call originated from a path dispatch.
        //
        assert(DSP >= f->dsp_orig);

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! EVAL HANDLING
    //
    //==////////////////////////////////////////////////////////////////==//

        // The EVAL "native" is unique because it cannot be a function that
        // runs "under the evaluator"...because it *is the evaluator itself*.
        // Hence it is handled in a special way.
        //
        if (f->func == PG_Eval_Func) {
            FETCH_NEXT_ONLY_MAYBE_END(f);

            if (f->indexor == END_FLAG) // e.g. `do [eval]`
                fail (Error_No_Arg(f->label_sym, FUNC_PARAM(PG_Eval_Func, 1)));

            // "DO/NEXT" full expression into the `eval` REBVAR slot
            // (updates index...).  (There is an /ONLY switch to suppress
            // normal evaluation but it does not apply to the value being
            // retriggered itself, just any arguments it consumes.)
            //
            VAL_INIT_WRITABLE_DEBUG(&(f->cell.eval));
            DO_NEXT_REFETCH_MAY_THROW(&(f->cell.eval), f, DO_FLAG_LOOKAHEAD);

            if (f->indexor == THROWN_FLAG)
                NOTE_THROWING(goto return_indexor);

            // There's only one refinement to EVAL and that is /ONLY.  It can
            // push one refinement to the stack or none.  The state will
            // twist up the evaluator for the next evaluation only.
            //
            if (DSP > f->dsp_orig) {
                assert(DSP == f->dsp_orig + 1);
                assert(VAL_WORD_SYM(DS_TOP) == SYM_ONLY); // canonized on push
                DS_DROP;
                eval_normal = FALSE;
            }
            else
                eval_normal = TRUE;

            // Jumping to the `reevaluate:` label will skip the fetch from the
            // array to get the next `value`.  So seed it with the address of
            // eval result, and step the index back by one so the next
            // increment will get our position sync'd in the block.
            //
            // If there's any reason to be concerned about the temporary
            // item being GC'd, it should be taken care of by the implicit
            // protection from the Do Stack.  (e.g. if it contains a function
            // that gets evaluated it will wind up in f->func, if it's a
            // GROUP! or PATH!-containing-GROUP! it winds up in f->array...)
            //
            // Note that we may be at the end (which would usually be a NULL
            // case for f->value) but we are splicing in eval over that,
            // which keeps the switch from crashing.
            //
            if (f->value)
                f->eval_fetched = f->value;
            else
                f->eval_fetched = END_VALUE; // NULL means no eval_fetched :-/

            f->value = &(f->cell.eval);
            goto reevaluate; // we don't move index!
        }

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! "VARLESS" CALL DISPATCH
    //
    //==////////////////////////////////////////////////////////////////==//

        // If a native has no refinements to process, it is feasible to
        // allow it to run "varless".  Even though the chunk stack is a
        // very cheap abstraction, it is not zero cost...and some functions
        // are better implemented as essentially inline hooks to the DO
        // evaluator.
        //
        // All varless functions must still be able to run with args stored
        // as vars if requested, because debug scenarios would expect those
        // cells to be inspectable on the stack.  Hence, if there are any
        // trace flags set we fall back upon that implementation.
        //
        // (EVAL/ONLY also suppresses varless abilities, because the
        // burden of the flag would be too much to pass through.)
        //
        if ( // check from most likely to be false to least likely...
            GET_VAL_FLAG(FUNC_VALUE(f->func), FUNC_FLAG_VARLESS)
            && DSP == f->dsp_orig
            && !Trace_Flags
            && eval_normal // avoid framelessness if EVAL/ONLY used
            && !SPORADICALLY(2) // run it framed in DEBUG 1/2 of the time
        ) {
            REB_R ret;

            FETCH_NEXT_ONLY_MAYBE_END(f);

            // A NULL arg signifies to the called function that it is being
            // run varless.  If it had arg values, then it would be non-NULL
            // (and be the pointer to the values in memory).
            //
            f->arg = NULL;

            // We might wind up invoking the GC, and we need to make sure
            // the reusable variables aren't bad data.  `value` should be
            // good but we don't know what's in the others.
            //
            f->param = NULL;
            f->refine = NULL;
            f->cell.subfeed = NULL;

            SET_TRASH_SAFE(f->out);

            f->mode = CALL_MODE_FUNCTION; // !!! separate "varless" mode?

            if (VAL_FUNC_CLASS(FUNC_VALUE(f->func)) == FUNC_CLASS_ACTION) {
                //
                // At the moment, the type checking actions run framelessly,
                // while no other actions do.  These are things like STRING?
                // and INTEGER?

                assert(FUNC_ACT(f->func) < REB_MAX_0);
                assert(FUNC_NUM_PARAMS(f->func) == 1);

                if (f->indexor == END_FLAG)
                    fail (Error_No_Arg(
                        f->label_sym, FUNC_PARAM(f->func, 1)
                    ));

                DO_NEXT_REFETCH_MAY_THROW(f->out, f, DO_FLAG_LOOKAHEAD);

                if (f->indexor == THROWN_FLAG)
                    ret = R_OUT_IS_THROWN;
                else {
                    if (VAL_TYPE_0(f->out) == FUNC_ACT(f->func))
                        SET_TRUE(f->out);
                    else
                        SET_FALSE(f->out);
                    ret = R_OUT;
                }
            }
            else {
                // Beyond the type-checking actions, only NATIVE! can be
                // varless...
                //
                assert(
                    VAL_FUNC_CLASS(FUNC_VALUE(f->func))
                    == FUNC_CLASS_NATIVE
                );
                ret = (*FUNC_CODE(f->func))(f);
            }

            // If varless, use SET_UNSET(D_OUT) instead of R_UNSET, etc.
            //
            assert(ret == R_OUT || ret == R_OUT_IS_THROWN);

            if (ret == R_OUT_IS_THROWN) {
                assert(THROWN(f->out));

                // There are actually "two kinds of throws"...one that can't
                // be resumed (such as that which might happen during a
                // parameter fulfillment) and one that might be resumable
                // (like a throw during a DO_ARRAY of a fulfilled parameter).
                // A varless native must make this distinction to line up
                // with the distinction from normal evaluation.
                //
                if (f->mode == CALL_MODE_THROW_PENDING) {
                    assert(f->indexor != THROWN_FLAG);
                    goto handle_possible_exit_thrown;
                }

                assert(f->indexor == THROWN_FLAG);
                NOTE_THROWING(goto return_indexor);
            }

            f->mode = CALL_MODE_GUARD_ARRAY_ONLY;

            // We're done!
            break;
        }

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! NORMAL ARGUMENT FULFILLMENT PROCESS
    //
    //==////////////////////////////////////////////////////////////////==//

        // At this point `f->value` is still good because we have not
        // advanced the input.  We extract the special exit_from property
        // contained in optimized definitional returns.
        //
        if (f->func == PG_Leave_Func) {
            exit_from = VAL_FUNC_EXIT_FROM(f->value);
            goto do_definitional_exit_from;
        }
        if (f->func == PG_Return_Func)
            exit_from = VAL_FUNC_EXIT_FROM(f->value);
        else
            exit_from = NULL;

        // Note: Although we create the call frame here, we cannot "put
        // it into effect" until all the arguments have been computed.
        // This is because recursive stack-relative bindings would wind
        // up reading variables out of the frame while it is still
        // being built, and that would be bad.

        // Depending on the <durable> settings of a function's arguments, they
        // may wind up resident in stack space or in dynamically allocated
        // space.  This sets up the memory as appropriate for the flags.
        //
        Push_Or_Alloc_Vars_For_Call(f);

        // If it's a specialization, we've already taken care of what we
        // needed to know from that specialization--all further references
        // will need to talk about the function which is being called.
        //
        // !!! For debugging, it would probably be desirable to indicate
        // that this call of the function originated from a specialization.
        // So that would mean saving the specialization's f->func somewhere.
        //
        if (FUNC_CLASS(f->func) == FUNC_CLASS_SPECIAL) {
            f->func = CTX_FRAME_FUNC(
                FUNC_VALUE(f->func)->payload.function.impl.special
            );
            f->flags |= DO_FLAG_EXECUTE_FRAME;
        }

        // Advance the input, which loses our ability to inspect the function
        // value further.  Note we are allowed to be at a END_FLAG (such
        // as if the function has no arguments, or perhaps its first argument
        // is hard quoted as HELP's is and it can accept that.)
        //
        FETCH_NEXT_ONLY_MAYBE_END(f);

        // We assume you can enumerate both the formal parameters (in the
        // spec) and the actual arguments (in the call frame) using pointer
        // incrementation, that they are both terminated by END, and
        // that there are an equal number of values in both.
        //
        f->param = FUNC_PARAMS_HEAD(f->func);

        if (IS_END(f->param)) {
            //
            // There are no arguments, so just skip the next section.  We
            // know that `param` contains an END marker so the GC
            // won't crash on it.  The Dispatch_Call() will ovewrite both
            // `arg` and `refine`.
            //
            goto function_ready_to_call;
        }

        // Since we know we're not going to just overwrite it, go ahead and
        // grab the arg head.  While fulfilling arguments the GC might be
        // invoked, so we have to initialize `refine` to something too...
        //
        f->arg = FRM_ARGS_HEAD(f);

    do_function_arglist_in_progress:
        //
        // f->out may have either contained the infix argument (if jumped in)
        // or if this was a fresh loop iteration, the debug build had
        // set f->out to a safe trash.  Using the statistical technique
        // again, we mimic the release build behavior of trust *half* the
        // time, and put in a trapping trash the other half...
        //
    #if !defined(NDEBUG)
        if (SPORADICALLY(2))
            SET_TRASH_SAFE(f->out);
    #endif

        // While fulfilling arguments the GC might be invoked, so we have to
        // initialize `refine` to something too.  The GC sees subfeed too.
        //
        f->refine = NULL;
        f->cell.subfeed = NULL;

        // This loop goes through the parameter and argument slots, filling in
        // the arguments via recursive calls to the evaluator.
        //
        // Note that Make_Call initialized them all to UNSET.  This is needed
        // in order to allow skipping around, in particular so that a
        // refinement slot can be marked as processed or not processed, but
        // also because the garbage collector has to consider the slots "live"
        // as arguments are progressively fulfilled.
        //
        f->mode = CALL_MODE_ARGS;

        for (; NOT_END(f->param); f->param++, f->arg++) {
        no_advance:
            assert(IS_TYPESET(f->param));

            // If the frame was pre-built then we want to skip most parameter
            // fulfillment logic.  However, refinements throw a wrench in
            // it because of specialization, because a refinement may be
            // specialized but an argument to it not, or vice-versa.  The
            // `f->mode` and `f->refine` must be updated to allow fallthrough
            // to normal fulfillment.
            //
            if ((f->flags & DO_FLAG_EXECUTE_FRAME)) {
                //
                // !!! Currently there is no support for calling a FRAME! with
                // additional refinements beyond the specialization.  So you
                // can't specialize `frame: :APPEND/ONLY` and then call it as
                // `frame/DUP/ONLY` which would require checking for duplicate
                // refinements and other things.  In the long term that should
                // be implemented.
                //
                assert(f->mode != CALL_MODE_SEEK_REFINE_WORD);

                if (IS_BAR(f->arg)) {
                    if (GET_VAL_FLAG(f->param, TYPESET_FLAG_HIDDEN)) {
                        //
                        // Pure local, so if told to fulfill ordinarily then
                        // that would just be an UNSET!
                        //
                        SET_UNSET(f->arg);
                        continue;
                    }

                    if (GET_VAL_FLAG(f->param, TYPESET_FLAG_REFINEMENT)) {
                        //
                        // With a BAR! in a refinement slot, we take that to
                        // mean that the refinement is not supplied.
                        //
                        SET_NONE(f->arg);
                        f->refine = f->arg;
                        f->mode = CALL_MODE_REFINE_SKIP;
                        continue;
                    }

                    if (
                        f->mode == CALL_MODE_REFINE_REVOKE
                        || f->mode == CALL_MODE_REFINE_SKIP
                    ) {
                        //
                        // A BAR! in a refinement slot where the refinement is
                        // not being taken...either because there was a BAR!
                        // or a FALSE in the refinement slot or it's being
                        // revoked due to an UNSET! being seen...will just
                        // act like an UNSET!
                        //
                        SET_UNSET(f->arg);
                        continue;
                    }

                    // Any other kind of argument we fall through to normal
                    // processing to either fulfill the argument or deliver
                    // the appropriate error.
                    //
                    // !!! Currently ordinary dispatch is written to expect
                    // that the frame was filled with UNSET!  Since ordinary
                    // dispatch is going to be more common than fulfilling a
                    // partially specialized arg, we restore that invariant.
                    //
                    SET_UNSET(f->arg);
                    goto fulfill_non_refinement_non_local;
                }

                // Otherwise, it's not a BAR!, but a value to use...

                if (GET_VAL_FLAG(f->param, TYPESET_FLAG_HIDDEN)) {
                    //
                    // "Pure locals" are expected by functions to be unset.
                    // If frame dispatch were allowed to poke values into
                    // that locals it is "locals injection" and would
                    // undermine the function's ability to assume that the
                    // local was UNSET! at function start.
                    //
                    // !!! This should be reviewed in terms of potential
                    // "continuation"--an issue that would also need to
                    // suppress type checking.  What if a FRAME! could be
                    // thrown and then later resumed with a memory of an
                    // intermediate state?  Such frames would need the ability
                    // to have any value in a locals slot.
                    //
                    if (!IS_UNSET(f->arg))
                        fail (Error_Local_Injection(f->label_sym, f->param));

                    continue;
                }

                // Refinements that are considered to be specified must be
                // coerced into the proper WORD! value of that refinement.
                // An UNSET! is not considered to be coercible.
                //
                if (GET_VAL_FLAG(f->param, TYPESET_FLAG_REFINEMENT)) {
                    if (IS_UNSET(f->arg)) {
                        //
                        // !!! More specific "refinement unset" error?
                        //
                        fail (Error_Arg_Type(
                            f->label_sym, f->param, Type_Of(f->arg))
                        );
                    }
                    else if (IS_CONDITIONAL_TRUE(f->arg)) {
                        Val_Init_Word(
                            f->arg, REB_WORD, VAL_TYPESET_SYM(f->param)
                        );
                        f->mode = CALL_MODE_REFINE_PENDING;
                        continue;
                    }
                    else {
                        SET_NONE(f->arg);
                        f->mode = CALL_MODE_REFINE_SKIP;
                        continue;
                    }
                }

                // If it's an ordinary arg then the revoking/pending logic
                // is repeated here.  This repetition is unfortunate, but if
                // control were to be passed to normal argument fulfillment
                // it would mean stuffing `f->value` from the `f->arg` (which
                // is dubious) as well as doing a mode check to know not to
                // do full evaluation.
                //
                if (
                    f->mode == CALL_MODE_REFINE_REVOKE
                    || f->mode == CALL_MODE_REFINE_SKIP
                ) {
                    //
                    // !!! Technically should it be different errors when you
                    // are given a conditionally false refinement and then
                    // a frame value, vs a conditionally true one and not all
                    // unset values in a revoke?
                    //
                    if (!IS_UNSET(f->arg))
                        fail (Error(RE_BAD_REFINE_REVOKE));

                    continue;
                }
                else if (f->mode == CALL_MODE_REFINE_PENDING) {
                    f->mode = CALL_MODE_REFINE_ARGS;
                    continue;
                }

                goto type_check_arg;
            }

            // *** PURE LOCALS => continue ***

            if (GET_VAL_FLAG(f->param, TYPESET_FLAG_HIDDEN)) {
                //
                // When the spec contained a SET-WORD!, that was a "pure
                // local".  It corresponds to no argument and will not
                // appear in WORDS-OF.  Unlike /local, it cannot be used
                // for "locals injection".  Helpful when writing generators
                // because you don't have to go find /local (!), you can
                // really put it wherever is convenient--no position rule.
                //
                // A trick for functions marked FUNC_FLAG_LEAVE_OR_RETURN
                // puts a "magic" REBNATIVE(return) value into the arg slot
                // for pure locals named RETURN: ....used by FUNC and CLOS
                //
                // Leave this arg value as an UNSET!
                //
                continue;
            }

            if (!GET_VAL_FLAG(f->param, TYPESET_FLAG_REFINEMENT)) {
                //
                // Hunting a refinement?  Quickly disregard this if we are
                // doing such a scan and it isn't a refinement.
                //
                if (f->mode == CALL_MODE_SEEK_REFINE_WORD)
                    continue;
            }
            else {
                // *** REFINEMENTS => continue ***

                // Refinements are tricky because users can write:
                //
                //     foo: func [a /b c /d e] [...]
                //
                //     foo/b/d (1 + 2) (3 + 4) (5 + 6)
                //     foo/d/b (1 + 2) (3 + 4) (5 + 6)
                //
                // But we are marching across the parameters in order of their
                // *definition*.  Hence we may have to seek refinements ahead
                // or behind to know where to put the results we evaluate.
                //
                if (f->mode == CALL_MODE_SEEK_REFINE_WORD) {
                    //
                    // Note that we have already canonized the path words for
                    // a case-insensitive-comparison to the symbol in the
                    // function's paramlist.  While it might be tempting to
                    // canonize those too, they should retain their original
                    // case for when that symbol is given back to the user to
                    // indicate a used refinement.
                    //
                    if (
                        VAL_WORD_SYM(DS_TOP) // ...was canonized when pushed
                        == SYMBOL_TO_CANON(VAL_TYPESET_SYM(f->param))
                    ) {
                        // If we seeked backwards to find a refinement and it's
                        // the one we are looking for, "consume" it off the
                        // data stack to say we found it in the frame
                        //
                        DS_DROP;

                        // Now as we switch to pending mode, change refine to
                        // point at the arg slot so we can revoke it if needed
                        //
                        f->mode = CALL_MODE_REFINE_PENDING;
                        f->refine = f->arg;

                        Val_Init_Word(
                            f->refine, REB_WORD, VAL_TYPESET_SYM(f->param)
                        );

                        continue;
                    }

                    // ...else keep scanning, but if it's unset then set it
                    // to none because we *might* not revisit this spot again.
                    //
                    if (IS_UNSET(f->arg))
                        SET_NONE(f->arg);

                    continue;
                }

                if (f->dsp_orig == DSP) {
                    //
                    // No refinements are left on the data stack, so if this
                    // refinement slot is still unset, skip the args and leave
                    // them as unsets (or set nones in legacy mode)
                    //
                    f->mode = CALL_MODE_REFINE_SKIP;
                    if (IS_UNSET(f->arg))
                        SET_NONE(f->arg);

                    continue;
                }

                // Should have only pushed words to the stack (or there would
                // have been an earlier error)
                //
                assert(IS_WORD(DS_TOP));

                if (
                    VAL_WORD_SYM(DS_TOP) // ...was canonized when pushed
                    == SYMBOL_TO_CANON(VAL_TYPESET_SYM(f->param))
                ) {
                    // We were lucky and the next refinement we wish to
                    // process lines up with this parameter slot.

                    f->mode = CALL_MODE_REFINE_PENDING;
                    f->refine = f->arg;

                    DS_DROP;

                    Val_Init_Word(
                        f->refine, REB_WORD, VAL_TYPESET_SYM(f->param)
                    );

                    continue;
                }

                // We weren't lucky and need to scan

                f->mode = CALL_MODE_SEEK_REFINE_WORD;
                assert(IS_WORD(DS_TOP));

                // We have to reset to the beginning if we are going to scan,
                // because we might have gone past the refinement on a prior
                // scan.  (Review if a bit might inform us if we've ever done
                // a scan before to get us started going out of order.  If not,
                // we would only need to look ahead.)
                //
                f->param = FRM_PARAMS_HEAD(f);
                f->arg = FRM_ARGS_HEAD(f);

                // We might have a function with no normal args, where a
                // refinement is the first parameter...and we don't want to
                // run the loop's arg++/param++ we get if we `continue`
                //
                goto no_advance;
            }

            if (f->mode == CALL_MODE_REFINE_SKIP) {
                //
                // Just skip because the args are already UNSET! (or NONE! if
                // we are in LEGACY_OPTIONS_REFINEMENT_TRUE mode
                //
                continue;
            }

        fulfill_non_refinement_non_local:
            assert(
                f->mode == CALL_MODE_ARGS
                || f->mode == CALL_MODE_REFINE_PENDING
                || f->mode == CALL_MODE_REFINE_ARGS
                || f->mode == CALL_MODE_REFINE_REVOKE
            );

            // Evaluation argument "hook" parameters (signaled in MAKE FUNCION!
            // by a `|` in the typeset, and in FUNC by `<...>`).  They point
            // back to this call through a reified FRAME!, and are able to
            // consume additional arguments during the function run.
            //
            if (GET_VAL_FLAG(f->param, TYPESET_FLAG_VARIADIC)) {
                assert(eval_normal); // !!! Can EVAL/ONLY be supported?
                /*assert(DSP == f->dsp_orig);*/ // !!! *after* refinement args?

                VAL_RESET_HEADER(f->arg, REB_VARARGS);

                VAL_VARARGS_FRAME_CTX(f->arg)
                    = Context_For_Frame_May_Reify(f, NULL, FALSE);
                ENSURE_ARRAY_MANAGED(
                    CTX_VARLIST(VAL_VARARGS_FRAME_CTX(f->arg))
                );

                VAL_VARARGS_PARAM(f->arg) = f->param; // type checks on TAKE

                assert(f->cell.subfeed == NULL); // NULL earlier in switch case
                continue;
            }

            // No argument--quoted or otherwise--is allowed to be directly
            // filled by a literal expression barrier.  Not even if it is able
            // to accept the type BAR! (other means must be used, e.g.
            // LIT-BAR! decaying to a BAR! in the slot).
            //
            // Since we prefetched, this can look before a possible DO/NEXT.
            //
            // !!! Technically this is an additional check of f->indexor for
            // the END_FLAG, so it would suggest better performance if the
            // check were done on each non-END-flag-checked branch, but for
            // now it is checked in one place for simplicity.
            //
            if (f->indexor != END_FLAG && IS_BAR(f->value))
                fail (Error(RE_EXPRESSION_BARRIER));

            // *** QUOTED OR EVALUATED ITEMS ***

            if (GET_VAL_FLAG(f->param, TYPESET_FLAG_QUOTE)) {
                if (f->indexor == END_FLAG) {
                    //
                    // If a function has a quoted argument whose types permit
                    // unset, then that specific case is allowed, in order to
                    // implement console commands like HELP (which acts as
                    // arity 1 or 0, using this trick)
                    //
                    //     >> foo: func [:a [unset!]] [
                    //         if unset? :a ["special allowance"]
                    //     ]
                    //
                    //     >> do [foo]
                    //     == "special allowance"
                    //
                    SET_UNSET(f->arg);

                    // Pre-empt the later type checking in order to inject a
                    // more specific message than "doesn't take UNSET!"
                    //
                    if (!TYPE_CHECK(f->param, REB_UNSET))
                        fail (Error_No_Arg(f->label_sym, f->param));
                }
                else if (
                    GET_VAL_FLAG(f->param, TYPESET_FLAG_EVALUATE) // soft quote
                    && eval_normal // we're not in never-evaluating EVAL/ONLY
                    && (
                        IS_GROUP(f->value)
                        || IS_GET_WORD(f->value)
                        || IS_GET_PATH(f->value)
                    )
                ) {
                    // These cases are "soft quoted", because both the flags
                    // TYPESET_FLAG_QUOTE and TYPESET_FLAG_EVALUATE are set.
                    //
                    //     >> foo: function ['a] [print [{a is} a]
                    //
                    //     >> foo 1 + 2
                    //     a is 1
                    //
                    //     >> foo (1 + 2)
                    //     a is 3
                    //
                    // This provides a convenient escape mechanism to allow
                    // callers to subvert quoting if they need to.
                    //
                    // These are "no-arg" evals so we do them isolated.  The
                    // `arg` slot is the input, and can't be output for the
                    // DO also...so use `out` instead.  (`out` is a decent
                    // choice because if a throw were to happen, that's where
                    // the thrown value would have to wind up in that case.)
                    //
                    if (DO_VALUE_THROWS(f->out, f->value)) {
                        //
                        // If we have refinements pending on the data
                        // stack we need to balance those...
                        //
                        DS_DROP_TO(f->dsp_orig);

                        f->indexor = THROWN_FLAG;
                        NOTE_THROWING(goto drop_call_and_return_thrown);
                    }

                    *(f->arg) = *(f->out);

                    FETCH_NEXT_ONLY_MAYBE_END(f);
                }
                else {
                    // This is either not one of the "soft quoted" cases, or
                    // "hard quoting" was explicitly used with GET-WORD!:
                    //
                    //     >> foo: function [:a] [print [{a is} a]
                    //
                    //     >> foo 1 + 2
                    //     a is 1
                    //
                    //     >> foo (1 + 2)
                    //     a is (1 + 2)
                    //
                    DO_NEXT_REFETCH_QUOTED(f->arg, f);
                }
            }
            else {
                // !!! Note: ROUTINE! does not set any bits on the symbols
                // and will need to be made to...
                //
                // assert(GET_VAL_FLAG(param, TYPESET_FLAG_EVALUATE));

                if (f->indexor == END_FLAG)
                    fail (Error_No_Arg(FRM_LABEL_SYM(f), f->param));

                // An ordinary WORD! in the function spec indicates that you
                // would like that argument to be evaluated normally.
                //
                //     >> foo: function [a] [print [{a is} a]
                //
                //     >> foo 1 + 2
                //     a is 3
                //
                // Special outlier EVAL/ONLY can be used to subvert this:
                //
                //     >> eval/only :foo 1 + 2
                //     a is 1
                //     ** Script error: + operator is missing an argument
                //
                if (eval_normal) {
                    DO_NEXT_REFETCH_MAY_THROW(
                        f->arg,
                        f,
                        GET_VAL_FLAG(FUNC_VALUE(f->func), FUNC_FLAG_INFIX)
                            ? DO_FLAG_NO_LOOKAHEAD
                            : DO_FLAG_LOOKAHEAD
                    );

                    if (f->indexor == THROWN_FLAG) {
                        *(f->out) = *(f->arg);

                        // If we have refinements pending on the data
                        // stack we need to balance those...
                        //
                        DS_DROP_TO(f->dsp_orig);

                        NOTE_THROWING(goto drop_call_and_return_thrown);
                    }
                }
                else
                    DO_NEXT_REFETCH_QUOTED(f->arg, f);
            }

            ASSERT_VALUE_MANAGED(f->arg);

            if (IS_UNSET(f->arg)) {
                if (f->mode == CALL_MODE_REFINE_ARGS)
                    fail (Error(RE_BAD_REFINE_REVOKE));
                else if (f->mode == CALL_MODE_REFINE_PENDING) {
                    f->mode = CALL_MODE_REFINE_REVOKE;
                    assert(IS_WORD(f->refine));
                    assert(IS_UNSET(f->arg));

                    SET_NONE(f->refine); // ...revoke the refinement.
                }
                else if (f->mode == CALL_MODE_REFINE_REVOKE) {
                    //
                    // We are revoking arguments to a refinement that have
                    // never been filled, so they should be vacant.
                    //
                    assert(IS_UNSET(f->arg));
                }
            }
            else {
                if (f->mode == CALL_MODE_REFINE_REVOKE)
                    fail (Error(RE_BAD_REFINE_REVOKE));
                else if (f->mode == CALL_MODE_REFINE_PENDING)
                    f->mode = CALL_MODE_REFINE_ARGS;
            }

            // Don't type check the argument if it's being revoked.
            //
            if (f->mode == CALL_MODE_REFINE_REVOKE) {
                assert(IS_UNSET(f->arg));
                continue;
            }

        type_check_arg:
            if (!TYPE_CHECK(f->param, VAL_TYPE(f->arg)))
                fail (Error_Arg_Type(f->label_sym, f->param, Type_Of(f->arg)));
        }

        // If we were scanning and didn't find the refinement we were looking
        // for, then complain with an error.
        //
        // !!! This will complain differently than a proper "REFINED!" strategy
        // would complain, because if you do:
        //
        //     append/(second [only asdhjas])/(print "hi") [a b c] [d]
        //
        // ...it would never make it to the print.  Here we do all the PATH!
        // and GROUP! evals up front and check that things are words or NONE,
        // not knowing if a refinement isn't on the function until the end.
        //
        if (f->mode == CALL_MODE_SEEK_REFINE_WORD)
            fail (Error(RE_BAD_REFINE, DS_TOP));

        // In the case where the user has said foo/bar/baz, and bar was later
        // in the spec than baz, then we will have passed it.  We need to
        // restart the scan (which may wind up failing)
        //
        if (DSP != f->dsp_orig) {
            f->mode = CALL_MODE_SEEK_REFINE_WORD;
            f->param = FRM_PARAMS_HEAD(f);
            f->arg = FRM_ARGS_HEAD(f);
            goto no_advance;
        }

    function_ready_to_call:
        //
        // Execute the function with all arguments ready.
        //
    #if !defined(NDEBUG)
        if (GET_VAL_FLAG(FUNC_VALUE(f->func), FUNC_FLAG_LEGACY)) {
            //
            // OPTIONS_REFINEMENTS_TRUE was set when this particular function
            // was created.  Use the debug-build's legacy post-processing
            // so refinements and their args work like in Rebol2/R3-Alpha.
            //
            Legacy_Convert_Function_Args_Debug(f);
        }
    #endif

    #if !defined(NDEBUG)
        //
        // R3-Alpha DO acted like an "EVAL" when passed a function, hence it
        // would have an effective arity greater than 1 if that were the case.
        // It was the only function that could do this.  Ren-C isolates the
        // concept of a "re-evaluator" into a full-spectrum native that does
        // *only* that, and is known to be "un-wrappable":
        //
        // https://trello.com/c/YMAb89dv
        //
        // With the VALUE_FLAG_REEVALUATE bit (which had been a cost on every
        // REBVAL) now gone, we must hook the evaluator to implement the
        // legacy feature for DO.
        //
        if (
            LEGACY(OPTIONS_DO_RUNS_FUNCTIONS)
            && IS_FUNCTION_AND(FUNC_VALUE(f->func), FUNC_CLASS_NATIVE)
            && FUNC_CODE(f->func) == &N_do
            && IS_FUNCTION(FRM_ARGS_HEAD(f))
        ) {
            // Grab the argument into the eval storage slot before abandoning
            // the arglist.
            //
            f->cell.eval = *FRM_ARGS_HEAD(f);

            f->eval_fetched = f->value;
            f->value = &f->cell.eval;

            f->mode = CALL_MODE_GUARD_ARRAY_ONLY;
            goto drop_call_for_legacy_do_function;
        }
    #endif

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! THROWING OF "RETURN" + "LEAVE" DEFINITIONAL EXITs
    //
    //==////////////////////////////////////////////////////////////////==//

        if (exit_from) {
        do_definitional_exit_from:
            //
            // If it's a definitional return, then we need to do the throw
            // for the return, named by the value in the exit_from.  This
            // should be the RETURN native with 1 arg as the function, and
            // the native code pointer should have been replaced by a
            // REBFUN (if function) or REBCTX (if durable) to jump to.
            //
            // !!! Long term there will always be frames for user functions
            // where definitional returns are possible, but for now they
            // still only make them by default if <durable> requested)
            //
            // LEAVE jumps directly here, because it doesn't need to go
            // through any parameter evaluation.  (Note that RETURN can't
            // simply evaluate the next item without inserting an opportunity
            // for the debugger, e.g. `return (breakpoint)`...)
            //
            ASSERT_ARRAY(exit_from);

            // We only have a REBARR*, but want to actually THROW a full
            // REBVAL (FUNCTION! or FRAME! if it has a context) which matches
            // the paramlist.  In either case, the value comes from slot [0]
            // of the RETURN_FROM array, but in the debug build do an added
            // sanity check.
            //
            if (GET_ARR_FLAG(exit_from, SERIES_FLAG_CONTEXT)) {
                //
                // Request to exit from a specific FRAME!
                //
                *(f->out) = *CTX_VALUE(AS_CONTEXT(exit_from));
                assert(IS_FRAME(f->out));
                assert(CTX_VARLIST(VAL_CONTEXT(f->out)) == exit_from);
            }
            else {
                // Request to dynamically exit from first ANY-FUNCTION! found
                // that has a given parameter list
                //
                *(f->out) = *FUNC_VALUE(AS_FUNC(exit_from));
                assert(IS_FUNCTION(f->out));
                assert(VAL_FUNC_PARAMLIST(f->out) == exit_from);
            }

            f->indexor = THROWN_FLAG;

            if (f->func == PG_Leave_Func) {
                //
                // LEAVE never created an arglist, so it doesn't have to
                // free one.  Also, it wants to just return UNSET!
                //
                CONVERT_NAME_TO_THROWN(f->out, UNSET_VALUE, TRUE);
                NOTE_THROWING(goto return_indexor);
            }

            // On the other hand, RETURN did make an arglist that has to be
            // dropped from the chunk stack.
            //
            assert(FUNC_NUM_PARAMS(f->func) == 1);
            CONVERT_NAME_TO_THROWN(f->out, FRM_ARGS_HEAD(f), TRUE);
            NOTE_THROWING(goto drop_call_and_return_thrown);
        }

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! ARGUMENTS NOW GATHERED, DISPATCH CALL
    //
    //==////////////////////////////////////////////////////////////////==//

        assert(DSP == f->dsp_orig);

        // Although the Make_Call wrote safe trash into the output slot, we
        // need to do it again for the dispatch, since the spots are used to
        // do argument fulfillment into.
        //
        SET_TRASH_SAFE(f->out);

        assert(IS_END(f->param));
        // f->arg may be uninitialized if there were no args...
        // refine should be either NULL or in the args (valid during call)

        // If the function has a native-optimized version of definitional
        // return, the local for this return should so far have just been
        // ensured in last slot...and left unset by the arg filling.
        //
        // Now fill in the var for that local with a "hacked up" native
        // Note that FUNCTION! uses its PARAMLIST as the RETURN_FROM
        // usually, but not if it's reusing a frame.
        //
        if (GET_VAL_FLAG(FUNC_VALUE(f->func), FUNC_FLAG_LEAVE_OR_RETURN)) {
            assert(IS_END(f->arg)); // not uninitialized if we get here...

            --(f->param);
            --(f->arg);

            assert(GET_VAL_FLAG(f->param, TYPESET_FLAG_HIDDEN));
            assert(IS_UNSET(f->arg));

            if (VAL_TYPESET_CANON(f->param) == SYM_RETURN)
                *(f->arg) = *ROOT_RETURN_NATIVE;
            else {
                assert(VAL_TYPESET_CANON(f->param) == SYM_LEAVE);
                *(f->arg) = *ROOT_LEAVE_NATIVE;
            }

            // !!! Having to pick a function paramlist or a context for
            // definitional return (and doubly testing this flag) is a likely
            // temporary state of affairs, as all functions able to have a
            // definitional return will have contexts in NewFunction.
            //
            if (f->flags & DO_FLAG_FRAME_CONTEXT)
                VAL_FUNC_EXIT_FROM(f->arg) = CTX_VARLIST(f->data.context);
            else
                VAL_FUNC_EXIT_FROM(f->arg) = FUNC_PARAMLIST(f->func);
        }

        // Now we reset arg to the head of the argument list.  This provides
        // fast access for the callees, so they don't have to go through an
        // indirection further than just f->arg to get it.
        //
        // !!! When hybrid frames are introduced, review the question of
        // which pointer "wins".  Might more than one be used?
        //
        if (f->flags & DO_FLAG_FRAME_CONTEXT) {
            //
            // !!! Here this caches a dynamic series data pointer in arg.
            // For arbitrary series this is not legal to do, because a
            // resize could relocate it...but we know the argument list will
            // not expand in the current implementation.  However, a memory
            // compactor would likely prefer that there not be too many
            // locked series in order to more freely rearrange memory, so
            // this is a tradeoff.
            //
            assert(GET_ARR_FLAG(
                AS_ARRAY(f->data.context), SERIES_FLAG_FIXED_SIZE
            ));
            f->arg = CTX_VARS_HEAD(f->data.context);
        }
        else {
            // We cache the stackvars data pointer in the stack allocated
            // case.  Note that even if the frame becomes "reified" as a
            // context, the data pointer will be the same over the stack
            // level lifetime.
            //
            f->arg = &f->data.stackvars[0];
        }

        if (Trace_Flags) Trace_Func(f->label_sym, FUNC_VALUE(f->func));

        assert(f->indexor != THROWN_FLAG);

        f->mode = CALL_MODE_FUNCTION;

        // If the Do_XXX_Core function dispatcher throws, we can't let it
        // write `f->indexor` directly to become THROWN_FLAG because we may
        // "recover" from the throw by realizing it was a RETURN.  If that
        // is the case, the function we called is the one that returned...
        // so there could still be code after it to execute, and that index
        // will be needed.
        //
        // Rather than have a separate `REBOOL threw`, this goes ahead and
        // overwrites `f->mode` with a special state DO_MODE_THROWN.  It was
        // going to need to be updated anyway back to DO_MODE_0, so no harm
        // in reusing it for the indicator.
        //
        switch (VAL_FUNC_CLASS(FUNC_VALUE(f->func))) {
        case FUNC_CLASS_NATIVE:
            Do_Native_Core(f);
            break;

        case FUNC_CLASS_ACTION:
            Do_Action_Core(f);
            break;

        case FUNC_CLASS_COMMAND:
            Do_Command_Core(f);
            break;

        case FUNC_CLASS_CALLBACK:
        case FUNC_CLASS_ROUTINE:
            Do_Routine_Core(f);
            break;

        case FUNC_CLASS_USER:
            Do_Function_Core(f);
            break;

        case FUNC_CLASS_SPECIAL:
            //
            // Shouldn't get here--the specific function type should have been
            // extracted from the frame to use.
            //
            assert(FALSE);
            break;

        default:
            fail (Error(RE_MISC));
        }

    #if !defined(NDEBUG)
        assert(
            f->mode == CALL_MODE_FUNCTION
            || f->mode == CALL_MODE_THROW_PENDING
        );
        assert(THROWN(f->out) == LOGICAL(f->mode == CALL_MODE_THROW_PENDING));
    #endif

#if !defined(NDEBUG)
    drop_call_for_legacy_do_function:
#endif

    drop_call_and_return_thrown:
        //
        // The same label is currently used for both these outcomes, and
        // which happens depends on whether eval_fetched is NULL or not
        //
        if (f->flags & DO_FLAG_FRAME_CONTEXT) {
            if (CTX_STACKVARS(f->data.context) != NULL)
                Drop_Chunk(CTX_STACKVARS(f->data.context));

            if (GET_ARR_FLAG(
                CTX_VARLIST(f->data.context), SERIES_FLAG_MANAGED
            )) {
                // Context at some point became managed and hence may still
                // have outstanding references.  The accessible flag should
                // have been cleared by the drop chunk above.
                //
                assert(
                    !GET_ARR_FLAG(
                        CTX_VARLIST(f->data.context), SERIES_FLAG_ACCESSIBLE
                    )
                );
            }
            else {
                // If nothing happened that might have caused the context to
                // become managed (e.g. Val_Init_Word() using it or a
                // Val_Init_Object() for the frame) then the varlist can just
                // go away...
                //
                Free_Array(CTX_VARLIST(f->data.context));
                //
                // NOTE: Even though we've freed the pointer, we still compare
                // it for identity below when checking to see if this was the
                // stack level being thrown to!
            }
        }
        else
            Drop_Chunk(f->data.stackvars);

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! CATCHING OF EXITs (includes catching RETURN + LEAVE)
    //
    //==////////////////////////////////////////////////////////////////==//

        // A definitional return should only be intercepted if it was for this
        // particular function invocation.  Definitional return abilities have
        // been extended to natives and actions, in order to permit stack
        // control in debug situations (and perhaps some non-debug capabilities
        // will be discovered as well).
        //
    handle_possible_exit_thrown:
        if (
            f->mode == CALL_MODE_THROW_PENDING
            && GET_VAL_FLAG(f->out, VALUE_FLAG_EXIT_FROM)
        ) {
            if (IS_FRAME(f->out)) {
                //
                // This identifies an exit from a *specific* functiion
                // invocation.  We can only match it if we have a reified
                // frame context.
                //
                if (
                    (f->flags & DO_FLAG_FRAME_CONTEXT) &&
                    VAL_CONTEXT(f->out) == AS_CONTEXT(f->data.context)
                ) {
                    CATCH_THROWN(f->out, f->out);
                    f->mode = CALL_MODE_GUARD_ARRAY_ONLY;
                }
            }
            else if (IS_FUNCTION(f->out)) {
                //
                // This identifies an exit from whichever instance of the
                // function is most recent on the stack.  This can be used
                // to exit without reifying a frame.  If exiting dynamically
                // when all that was named was a function, but definitionally
                // scoped returns should ideally have a trick for having
                // the behavior of a reified frame without needing to do
                // so (for now, they use this path in FUNCTION!)
                //
                if (VAL_FUNC_PARAMLIST(f->out) == FUNC_PARAMLIST(f->func)) {
                    CATCH_THROWN(f->out, f->out);
                    f->mode = CALL_MODE_GUARD_ARRAY_ONLY;
                }
            }
            else if (IS_INTEGER(f->out)) {
                //
                // If it's an integer, we drop the value at each stack level
                // until 1 is reached...
                //
                if (VAL_INT32(f->out) == 1) {
                    CATCH_THROWN(f->out, f->out);
                    f->mode = CALL_MODE_GUARD_ARRAY_ONLY;
                }
                else {
                    // don't reset header (keep thrown flag as is), just bump
                    // the count down by one...
                    //
                    --f->out->payload.integer;
                    //
                    // ...and stay in thrown mode...
                }
            }
            else {
                assert(FALSE); // no other low-level EXIT/FROM supported
            }
        }

    //==////////////////////////////////////////////////////////////////==//
    //
    // FUNCTION! CALL COMPLETION (Type Check Result, Throw If Needed)
    //
    //==////////////////////////////////////////////////////////////////==//

        // Here we know the function finished.  If it has a definitional
        // return we need to type check it--and if it has a leave we have
        // to squash whatever the last evaluative result was and replace it
        // with an UNSET!
        //
        if (GET_VAL_FLAG(FUNC_VALUE(f->func), FUNC_FLAG_LEAVE_OR_RETURN)) {
            REBVAL *last_param = FUNC_PARAM(f->func, FUNC_NUM_PARAMS(f->func));
            if (VAL_TYPESET_CANON(last_param) == SYM_LEAVE) {
                SET_UNSET(f->out);
            }
            else {
                // The type bits of the definitional return are not applicable
                // to the `return` word being associated with a FUNCTION!
                // vs. an INTEGER! (for instance).  It is where the type
                // information for the non-existent return function specific
                // to this call is hidden.
                //
                assert(VAL_TYPESET_CANON(last_param) == SYM_RETURN);
                if (!TYPE_CHECK(last_param, VAL_TYPE(f->out)))
                    fail (Error_Arg_Type(
                        SYM_RETURN, last_param, Type_Of(f->out))
                    );
            }
        }

        // If running a frame execution then clear that flag out.
        //
        f->flags &= ~DO_FLAG_EXECUTE_FRAME;

    #if !defined(NDEBUG)
        //
        // No longer need to check f->data.context for thrown status if it
        // was used, so overwrite the dead pointer in the union.  Note there
        // are two entry points to Push_Or_Alloc_Vars_For_Call at the moment,
        // so this clearing can't be done by the debug routine at top of loop.
        //
        f->data.stackvars = NULL;
    #endif

    #if !defined(NDEBUG)
        if (f->eval_fetched) {
            //
            // All the eval wanted to do was get the call frame cleaned up.
            //
            // !!! This is only needed by the legacy implementation of DO
            // for EVAL of functions, because it has to drop the pushed
            // call with arguments.  Is there a cleaner way?
            //
            assert(LEGACY(OPTIONS_DO_RUNS_FUNCTIONS));
            assert(f->mode == CALL_MODE_GUARD_ARRAY_ONLY);
            assert(f->indexor != THROWN_FLAG);
            goto reevaluate;
        }
    #endif

        // If the throw wasn't intercepted as an exit from this function call,
        // accept the throw.  We only care about the mode getting set cleanly
        // back to CALL_MODE_GUARD_ARRAY_ONLY if evaluation continues...
        //
        if (f->mode == CALL_MODE_THROW_PENDING) {
            f->indexor = THROWN_FLAG;
            NOTE_THROWING(goto return_indexor);
        }
        else if (f->indexor == THROWN_FLAG)
            NOTE_THROWING(goto return_indexor);
        else
            f->mode = CALL_MODE_GUARD_ARRAY_ONLY;

        if (Trace_Flags) Trace_Return(f->label_sym, f->out);
        break;


//==//////////////////////////////////////////////////////////////////////==//
//
// [FRAME!]
//
// If a literal FRAME! is hit in the source, then its associated function
// will be executed with the data.  Any BAR! in the frame will be treated
// as if it is an unfulfilled parameter and go through ordinary parameter
// fulfillment logic.
//
//==//////////////////////////////////////////////////////////////////////==//

    case ET_FRAME:
        //
        // While *technically* possible that a context could be in use by more
        // than one function at a time, this is a dangerous enough idea to
        // prohibit unless some special situation arises and it's explicitly
        // said what is meant to be done.
        //
        /*if (GET_VAL_FLAG(f->value, EXT_CONTEXT_RUNNING))
           fail (Error(RE_FRAME_ALREADY_USED, f->value)); */

        assert(f->data.stackvars == NULL);
        f->data.context = VAL_CONTEXT(f->value);
        f->func = CTX_FRAME_FUNC(VAL_CONTEXT(f->value));

        if (GET_ARR_FLAG(
            CTX_VARLIST(VAL_CONTEXT(f->value)), CONTEXT_FLAG_STACK)
        ) {
            f->arg = VAL_CONTEXT_STACKVARS(f->value);
        }
        else
            f->arg = CTX_VARS_HEAD(VAL_CONTEXT(f->value));

        f->param = CTX_KEYS_HEAD(VAL_CONTEXT(f->value));

        f->flags |= DO_FLAG_FRAME_CONTEXT | DO_FLAG_EXECUTE_FRAME;

        FETCH_NEXT_ONLY_MAYBE_END(f);
        exit_from = NULL;
        goto do_function_arglist_in_progress;

//==//////////////////////////////////////////////////////////////////////==//
//
// [ ??? ] => panic
//
// All types must match a case in the switch.  This shouldn't happen.
//
//==//////////////////////////////////////////////////////////////////////==//

    default:
        panic (Error(RE_MISC));
    }

    //==////////////////////////////////////////////////////////////////==//
    //
    // END MAIN SWITCH STATEMENT
    //
    //==////////////////////////////////////////////////////////////////==//

    // There shouldn't have been any "accumulated state", in the sense that
    // we should be back where we started in terms of the data stack, the
    // mold buffer position, the outstanding manual series allocations, etc.
    //
    ASSERT_STATE_BALANCED(&state);

    // It's valid for the operations above to fall through after a fetch or
    // refetch that could have reached the end.
    //
    if (f->indexor == END_FLAG)
        goto return_indexor;

    // Throws should have already returned at the time of throw, by jumping
    // to the `thrown_index` label.
    //
    assert(f->indexor != THROWN_FLAG && !THROWN(f->out));

    if (f->flags & DO_FLAG_NO_LOOKAHEAD) {
        //
        // Don't do infix lookahead if asked *not* to look.  It's not typical
        // to be requested by callers (there is already no infix lookahead
        // by using DO_FLAG_EVAL_ONLY, so those cases don't need to ask.)
        //
        // However, recursive cases of DO disable infix dispatch if they are
        // currently processing an infix operation.  The currently processing
        // operation is thus given "higher precedence" by this disablement.
    }
    else {
        // Since we're not at an END, we know f->value has been prefetched,
        // so we can "peek" at it.
        //
        // If it is a WORD! that looks up to an infix function, we will use
        // the value sitting in `out` as the "left-hand-side" (parameter 1)
        // of that invocation.  (See #1934 for the resolution that literal
        // function values in the source will act as if they were prefix,
        // so word lookup is the only way to get infix behavior.)
        //
        if (IS_WORD(f->value)) {
            f->param = GET_OPT_VAR_MAY_FAIL(f->value);

            if (
                IS_FUNCTION(f->param)
                && GET_VAL_FLAG(f->param, FUNC_FLAG_INFIX)
            ) {
                f->label_sym = VAL_WORD_SYM(f->value);

            #if !defined(NDEBUG)
                f->label_str = cast(const char*, Get_Sym_Name(f->label_sym));
            #endif

                f->func = VAL_FUNC(f->param);

                // The warped function values used for definitional return
                // usually need their EXIT_FROMs extracted, but here we should
                // not worry about it as neither RETURN nor LEAVE are infix
                //
                assert(f->func != PG_Leave_Func);
                assert(f->func != PG_Return_Func);
                exit_from = NULL;

                if (Trace_Flags) Trace_Line(f->source, f->indexor, f->param);

                // We go ahead and start the vars, and put our evaluated
                // result into it as the "left-hand-side" before calling into
                // the rest of function's behavior.
                //
                Push_Or_Alloc_Vars_For_Call(f);

                // Infix functions must have at least arity 1 (exactly 2?)
                //
                assert(FUNC_NUM_PARAMS(f->func) >= 1);
                f->param = FUNC_PARAMS_HEAD(f->func);
                if (!TYPE_CHECK(f->param, VAL_TYPE(f->out)))
                    fail (Error_Arg_Type(
                        f->label_sym, f->param, Type_Of(f->out))
                    );

                // Use current `out` as first argument of the infix function
                //
                f->arg = FRM_ARGS_HEAD(f);
                *(f->arg) = *(f->out);

                ++f->param;
                ++f->arg;

                FETCH_NEXT_ONLY_MAYBE_END(f);
                goto do_function_arglist_in_progress;
            }

            // Perhaps not an infix function, but we just paid for a variable
            // lookup.  If this isn't just a DO/NEXT, use the work!
            //
            if (f->flags & DO_FLAG_TO_END) {
                //
                // We need to update the `expr_index` since we are skipping
                // the whole `do_at_index` preparation for the next cycle,
                // and also need to run the "Preamble" in debug builds to
                // properly update the tick count and clear out state.
                //
                f->expr_index = f->indexor;
                *(f->out) = *(f->param); // param is trashed by Preamble_Debug!

            #if !defined(NDEBUG)
                do_count = Do_Evaluation_Preamble_Debug(f);
            #endif

                goto dispatch_the_word_in_out; // will handle the FETCH_NEXT
            }
        }

        // Note: PATH! may contain parens, which would need to be evaluated
        // during lookahead.  This could cause side-effects if the lookahead
        // fails.  Consequently, PATH! should not be a candidate for doing
        // an infix dispatch.
    }

    // Continue evaluating rest of block if not just a DO/NEXT
    //
    if (f->flags & DO_FLAG_TO_END) goto value_ready_for_do_next;

return_indexor:
    //
    // Jumping here skips the natural check that would be done after the
    // switch on the value being evaluated, so we assert balance here too.
    //
    ASSERT_STATE_BALANCED(&state);

#if !defined(NDEBUG)
    Do_Exit_Checks_Debug(f);
#endif

    // Restore the top of stack (if there is a fail() and associated longjmp,
    // this restoration will be done by the Drop_Trap helper.)
    //
    TG_Frame_Stack = f->prior;

    // Caller needs to inspect `index`, at minimum to know if it's THROWN_FLAG
}


#if !defined(NDEBUG)

//
// Putting the exit checks in their own routines makes a small attempt to
// pare down the total amount of code in Do_Core() for readability, while
// still having a place to put as many checks that might help verify that
// things are working properly.
//
static void Do_Exit_Checks_Debug(struct Reb_Frame *f) {
    if (
        f->indexor != END_FLAG && f->indexor != THROWN_FLAG
        && f->indexor != VALIST_FLAG
    ) {
        // If we're at the array's end position, then we've prefetched the
        // last value for processing (and not signaled end) but on the
        // next fetch we *will* signal an end.
        //
        assert(f->indexor <= ARR_LEN(f->source.array));
    }

    if (f->flags & DO_FLAG_TO_END)
        assert(f->indexor == THROWN_FLAG || f->indexor == END_FLAG);

    if (f->indexor == END_FLAG) {
        assert(f->value == NULL); // NULLing out value may become debug-only
        assert(NOT_END(f->out)); // series END marker shouldn't leak out
    }

    if (f->indexor == THROWN_FLAG)
        assert(THROWN(f->out));

    // Function execution should have written *some* actual output value
    // over the trash that we put in the return slot before the call.
    //
    assert(!IS_TRASH_DEBUG(f->out));
    assert(VAL_TYPE(f->out) < REB_MAX); // cheap check

    ASSERT_VALUE_MANAGED(f->out);
}

#endif


//
//  Do_Array_At_Core: C
//
// Most common case of evaluator invocation in Rebol: the data lives in an
// array series.  Generic routine takes flags and may act as either a DO
// or a DO/NEXT at the position given.  Option to provide an element that
// may not be resident in the array to kick off the execution.
//
REBIXO Do_Array_At_Core(
    REBVAL *out,
    const REBVAL *opt_first,
    REBARR *array,
    REBCNT index,
    REBFLGS flags
) {
    struct Reb_Frame f;

    if (opt_first) {
        f.value = opt_first;
        f.indexor = index;
    }
    else {
        // Do_Core() requires caller pre-seed first value, always
        //
        f.value = ARR_AT(array, index);
        f.indexor = index + 1;
    }

    if (IS_END(f.value)) {
        SET_UNSET(out);
        return END_FLAG;
    }

    f.out = out;
    f.source.array = array;
    f.flags = flags;

    Do_Core(&f);

    return f.indexor;
}


//
//  Do_Va_Core: C
//
// (va_list by pointer: http://stackoverflow.com/a/3369762/211160)
//
// Central routine for doing an evaluation of an array of values by calling
// a C function with those parameters (e.g. supplied as arguments, separated
// by commas).  Uses same method to do so as functions like printf() do.
//
// In R3-Alpha this style of invocation was specifically used to call single
// Rebol functions.  It would use a list of REBVAL*s--each of which could
// come from disjoint memory locations and be passed directly with no
// evaluation.  Ren-C replaced this entirely by adapting the evaluator to
// use va_arg() lists for the same behavior as a DO of an ARRAY.
//
// The previously accomplished style of execution with a function which may
// not be in the arglist can be accomplished using `opt_first` to put that
// function into the optional first position.  To instruct the evaluator not
// to do any evaluation on the values supplied as arguments after that
// (corresponding to R3-Alpha's APPLY/ONLY) then DO_FLAG_EVAL_ONLY should be
// used--otherwise they will be evaluated normally.
//
// NOTE: Ren-C no longer supports the built-in ability to supply refinements
// positionally, due to the brittleness of this approach (for both system
// and user code).  The `opt_head` value should be made a path with the
// function at the head and the refinements specified there.  Future
// additions could do this more efficiently by allowing the refinement words
// to be pushed directly to the data stack.
//
// !!! C's va_lists are very dangerous, there is no type checking!  The
// C++ build should be able to check this for the callers of this function
// *and* check that you ended properly.  It means this function will need
// two different signatures (and so will each caller of this routine).
//
// Returns THROWN_FLAG, END_FLAG--or if DO_FLAG_NEXT is used it may return
// VALIST_INCOMPLETE_FLAG.
//
REBIXO Do_Va_Core(
    REBVAL *out,
    const REBVAL *opt_first,
    va_list *vaptr,
    REBFLGS flags
) {
    struct Reb_Frame f;

    if (opt_first)
        f.value = opt_first;
    else {
        // Do_Core() requires caller pre-seed first value, always
        //
        f.value = va_arg(*vaptr, const REBVAL*);
    }

    if (IS_END(f.value)) {
        SET_UNSET(out);
        return END_FLAG;
    }

    f.out = out;
    f.indexor = VALIST_FLAG;
    f.source.vaptr = vaptr;

    // !!! See notes in %m-gc.c about what needs to be done before it can
    // be safe to let arbitrary evaluations happen in variadic scenarios.
    // (This functionality coming soon, but it requires reifying the va_list
    // into an array if a GC incidentally happens during any va_list DOs.)
    //
    assert(flags & DO_FLAG_EVAL_ONLY);
    f.flags = flags | DO_FLAG_VALIST; // see notes in %sys-do.h on why needed

    Do_Core(&f);

    if (flags & DO_FLAG_NEXT) {
        //
        // Infix lookahead causes a fetch that cannot be undone.  Hence
        // va_list DO/NEXT can't be resumed -- see VALIST_INCOMPLETE_FLAG.
        // For a resumable interface on va_list, see the lower level
        // varless API.
        //
        if (f.indexor == VALIST_FLAG) {
            //
            // Try one more fetch and see if it's at the end.  If not, we did
            // not consume all the input.
            //
            FETCH_NEXT_ONLY_MAYBE_END(&f);
            if (f.indexor != END_FLAG) {
                assert(f.indexor == VALIST_FLAG); // couldn't throw!!
                return VALIST_INCOMPLETE_FLAG;
            }
        }

        assert(f.indexor == THROWN_FLAG || f.indexor == END_FLAG);
    }

    return f.indexor;
}


//
//  Do_Values_At_Core: C
//
// !!! Not yet implemented--concept is to accept a REBVAL[] array, rather
// than a REBARR of values.
//
// !!! Considerations of this core interface are to see the values as being
// potentially in non-contiguous points in memory, and advanced with some
// skip length between them.  Additionally the idea of some kind of special
// Rebol value or "REB_INSTRUCTION" to say how far to skip is a possibility,
// which would be more general in the sense that it would allow the skip
// distances to be generalized, though this would cost a pointer size
// entity at each point.  The advantage of REB_INSTRUCTION is that only the
// clients using the esoteric ability would be paying anything for it or
// the API complexity, but if an important client like Ren-C++ it might
// be worth the savings.
//
// Note: Functionally it would be possible to assume a 0 index and require
// the caller to bump the value pointer as necessary.  But an index-based
// interface is likely useful to avoid the bookkeeping required for the caller.
//
REBIXO Do_Values_At_Core(
    REBVAL *out,
    REBFLGS flags,
    const REBVAL *opt_head,
    const REBVAL values[],
    REBCNT index
) {
    fail (Error(RE_MISC));
}


//
//  Sys_Func: C
//
// Gets a system function with tolerance of it not being a function.
//
// (Extraction of a feature that formerly was part of a dedicated dual
// function to Apply_Func_Throws (Do_Sys_Func_Throws())
//
REBVAL *Sys_Func(REBCNT inum)
{
    REBVAL *value = CTX_VAR(Sys_Context, inum);

    if (!IS_FUNCTION(value)) fail (Error(RE_BAD_SYS_FUNC, value));

    return value;
}


//
//  Apply_Only_Throws: C
//
// Takes a list of arguments terminated by END_VALUE (or any IS_END) and
// will do something similar to R3-Alpha's "apply/only" with a value.  If
// that value is a function, it will be called...if it is a SET-WORD! it
// will be assigned, etc.
//
// This is equivalent to putting the value at the head of the input and
// then calling EVAL/ONLY on it.  If all the inputs are not consumed, an
// error will be thrown.
//
// The boolean result will be TRUE if an argument eval or the call created
// a THROWN() value, with the thrown value in `out`.
//
REBOOL Apply_Only_Throws(REBVAL *out, const REBVAL *applicand, ...)
{
    REBIXO indexor;
    va_list va;

#ifdef VA_END_IS_MANDATORY
    struct Reb_State state;
    REBCTX *error;
#endif

    va_start(va, applicand); // must mention last param before the "..."

#ifdef VA_END_IS_MANDATORY
    PUSH_TRAP(&error, &state);

// The first time through the following code 'error' will be NULL, but...
// `fail` can longjmp here, so 'error' won't be NULL *if* that happens!

    if (error) {
        va_end(va); // interject cleanup of whatever va_start() set up...
        fail (error); // ...then just retrigger error
    }
#endif

    indexor = Do_Va_Core(
        out,
        applicand, // opt_first
        &va,
        DO_FLAG_NEXT | DO_FLAG_LOOKAHEAD | DO_FLAG_EVAL_ONLY
    );

    if (indexor == VALIST_INCOMPLETE_FLAG) {
        //
        // Not consuming all the arguments given suggests a problem as far
        // as this interface is concerned.  To tolerate incomplete states,
        // use Do_Va_Core() directly.
        //
        fail (Error(RE_APPLY_TOO_MANY));
    }

    va_end(va);
    //
    // ^-- This va_end() will *not* be called if a fail() happens to longjmp
    // during the apply.  But is that a problem, you ask?  No survey has
    // turned up an existing C compiler where va_end() isn't a NOOP.
    //
    // But there's implementations we know of, then there's the Standard...
    //
    //    http://stackoverflow.com/a/32259710/211160
    //
    // The Standard is explicit: an implementation *could* require calling
    // va_end() if it wished--it's undefined behavior if you skip it.
    //
    // In the interests of efficiency and not needing to set up trapping on
    // each apply, our default is to assume the implementation does not
    // need the va_end() call.  But for thoroughness, VA_END_IS_MANDATORY is
    // outlined here to show the proper bracketing if it were ever needed.

#ifdef VA_END_IS_MANDATORY
    DROP_TRAP_SAME_STACKLEVEL_AS_PUSH(&state);
#endif

    assert(indexor == THROWN_FLAG || indexor == END_FLAG);
    return LOGICAL(indexor == THROWN_FLAG);
}



//
// ??? ======================================= MOVE THE BELOW TO %?????????
//


//
//  Do_Construct: C
// 
// Do a block with minimal evaluation and no evaluation of
// functions. Used for things like script headers where security
// is important.
// 
// Handles cascading set words:  word1: word2: value
//
void Do_Construct(REBVAL value[])
{
    REBDSP dsp_orig = DSP;

    REBVAL temp;
    VAL_INIT_WRITABLE_DEBUG(&temp);
    SET_NONE(&temp);

    // This routine reads values from the start to the finish, which means
    // that if it wishes to do `word1: word2: value` it needs to have some
    // way of getting to the value and then going back across the words to
    // set them.  One way of doing it would be to start from the end and
    // work backward, but this uses the data stack instead to gather set
    // words and then go back and set them all when a value is found.
    //
    // !!! This could also just remember the pointer of the first set
    // word in a run, but at time of writing this is just patching a bug.
    //
    for (; NOT_END(value); value++) {
        if (IS_SET_WORD(value)) {
            //
            // Remember this SET-WORD!.  Come back and set what it is
            // bound to, once a non-SET-WORD! value is found.
            //
            DS_PUSH(value);
            continue;
        }

        // If not a SET-WORD! then consider the argument to represent some
        // kind of value.
        //
        // !!! The historical default is to NONE!, and also to transform
        // what would be evaluative into non-evaluative.  So:
        //
        //     >> construct [a: b/c: d: append "Strange" <defaults>]
        //     == make object! [
        //         a: b/c:
        //         d: 'append
        //     ]
        //
        // A differing philosophy might be that the construction process
        // only tolerate input that would yield the same output if used
        // in an evaulative object creation.
        //
        if (IS_WORD(value)) {
            switch (VAL_WORD_CANON(value)) {
            case SYM_NONE:
                SET_NONE(&temp);
                break;

            case SYM_TRUE:
            case SYM_ON:
            case SYM_YES:
                SET_TRUE(&temp);
                break;

            case SYM_FALSE:
            case SYM_OFF:
            case SYM_NO:
                SET_FALSE(&temp);
                break;

            default:
                temp = *value;
                VAL_SET_TYPE_BITS(&temp, REB_WORD);
            }
        }
        else if (IS_LIT_WORD(value)) {
            temp = *value;
            VAL_SET_TYPE_BITS(&temp, REB_WORD);
        }
        else if (IS_LIT_PATH(value)) {
            temp = *value;
            VAL_SET_TYPE_BITS(&temp, REB_PATH);
        }
        else if (VAL_TYPE(value) >= REB_NONE) { // all valid values
            temp = *value;
        }
        else
            SET_NONE(&temp);

        // Set prior set-words:
        while (DSP > dsp_orig) {
            *GET_MUTABLE_VAR_MAY_FAIL(DS_TOP) = temp;
            DS_DROP;
        }
    }

    // All vars in the frame should have a default value if not set, so if
    // we reached the end with something like `[a: 10 b: c: d:]` just leave
    // the trailing words to that default.  However, we must balance the
    // stack to please the evaluator, so let go of the set-words that we
    // did not set.
    //
    DS_DROP_TO(dsp_orig);
}


//
//  Do_Min_Construct: C
// 
// Do no evaluation of the set values.
//
void Do_Min_Construct(REBVAL value[])
{
    REBVAL *temp;
    REBDSP ssp;  // starting stack pointer

    DS_PUSH_NONE;
    temp = DS_TOP;
    ssp = DSP;

    for (; NOT_END(value); value++) {
        if (IS_SET_WORD(value)) {
            // Next line not needed, because SET words are ALWAYS in frame.
            //if (VAL_WORD_INDEX(value) > 0 && VAL_WORD_CONTEXT(value) == frame)
                DS_PUSH(value);
        } else {
            // Get value:
            *temp = *value;
            // Set prior set-words:
            while (DSP > ssp) {
                *GET_MUTABLE_VAR_MAY_FAIL(DS_TOP) = *temp;
                DS_DROP;
            }
        }
    }
    DS_DROP; // temp
}


//
// ====================> MOVE THE BELOW TO %??? ?
//

//
//  Get_Simple_Value_Into: C
// 
// Does easy lookup, else just returns the value as is.
//
void Get_Simple_Value_Into(REBVAL *out, const REBVAL *val)
{
    if (IS_WORD(val) || IS_GET_WORD(val)) {
        *out = *GET_OPT_VAR_MAY_FAIL(val);
    }
    else if (IS_PATH(val) || IS_GET_PATH(val)) {
        if (Do_Path_Throws(out, NULL, val, NULL))
            fail (Error_No_Catch_For_Throw(out));
    }
    else {
        *out = *val;
    }
}


//
//  Resolve_Path: C
// 
// Given a path, return a context and index for its terminal.
//
REBCTX *Resolve_Path(REBVAL *path, REBCNT *index)
{
    REBVAL *sel; // selector
    const REBVAL *val;
    REBARR *blk;
    REBCNT i;

    if (VAL_LEN_HEAD(path) < 2) return 0;
    blk = VAL_ARRAY(path);
    sel = ARR_HEAD(blk);
    if (!ANY_WORD(sel)) return 0;
    val = GET_OPT_VAR_MAY_FAIL(sel);

    sel = ARR_AT(blk, 1);
    while (TRUE) {
        if (!ANY_CONTEXT(val) || !IS_WORD(sel)) return 0;
        i = Find_Word_In_Context(VAL_CONTEXT(val), VAL_WORD_SYM(sel), FALSE);
        sel++;
        if (IS_END(sel)) {
            *index = i;
            return VAL_CONTEXT(val);
        }
    }

    return 0; // never happens
}

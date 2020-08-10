//
//  File: %c-path.h
//  Summary: "Core Path Dispatching and Chaining"
//  Project: "Revolt Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2017 Revolt Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=////////////////////////////////////////////////////////////////////////=//
//
// !!! See notes in %sys-path.h regarding the R3-Alpha path dispatch concept
// and regarding areas that need improvement.
//

#include "sys-core.h"


//
//  Path_Executor: C
//
REB_R Path_Executor(REBFRM *f)
{
    if (GET_EVAL_FLAG(f, ABRUPT_FAILURE))  // fail() from in this executor
        return R_THROWN;  // just pass it up

    enum {
        ST_PATH_INITIAL_ENTRY = 0,
        ST_PATH_FIRST_GROUP_EVALUATING,
        ST_PATH_PICKER_GROUP_EVALUATING
    };

    switch (STATE_BYTE(f)) {
      case ST_PATH_INITIAL_ENTRY: goto initial_entry;
      case ST_PATH_FIRST_GROUP_EVALUATING: goto out_preloaded;
      case ST_PATH_PICKER_GROUP_EVALUATING: goto picker_ready;      
      default: assert(false);
    }

  initial_entry: {
    assert(
        not PVS_OPT_SETVAL(f)
        or not IN_DATA_STACK_DEBUG(PVS_OPT_SETVAL(f))
            // ^-- evaluation might relocate it
    );
    assert(f->out != PVS_OPT_SETVAL(f) and f->out != PVS_PICKER(f));

    // !!! For now, we allow `/a` to act as a GET of A and `/a:` to act as
    // a SET of A.  Review semantics.
    //
    while (NOT_END(f_value) and IS_BLANK(f_value))
        Fetch_Next_Forget_Lookback(f);

    // Seed the path evaluation process by looking up the first item (to
    // get a datatype to dispatch on for the later path items)
    //
    if (IS_WORD(f_value)) {
        //
        // Remember the actual location of this variable, not just its value,
        // in case we need to do R_IMMEDIATE writeback (e.g. month/day: 1)
        //
        f->u.path.ref = Lookup_Mutable_Word_May_Fail(f_value, f_specifier);
        f->u.path.ref_specifier = SPECIFIED;

        Move_Value(f->out, SPECIFIC(f->u.path.ref));

        if (IS_ACTION(f->out))
            PVS_OPT_LABEL(f) = VAL_WORD_SPELLING(f_value);
    }
    else if (
        IS_GROUP(f_value)
        and NOT_EVAL_FLAG(f, PATH_HARD_QUOTE)  // not precomposed
    ){
        if (GET_EVAL_FLAG(f, NO_PATH_GROUPS))
            fail ("GROUP! in PATH! used with GET or SET (use REDUCE/EVAL)");

        f->u.path.ref = nullptr;  // nowhere to R_IMMEDIATE write back to

        DECLARE_FRAME_AT_CORE (
            blockframe,
            f_value,
            f_specifier,
            EVAL_MASK_DEFAULT | EVAL_FLAG_TO_END
        );

        Init_Void(f->out);  // in case all invisibles, as usual
        Push_Frame(f->out, blockframe, &Evaluator_Executor);

        STATE_BYTE(f) = ST_PATH_FIRST_GROUP_EVALUATING;  // => first_is_in_out
        return R_CONTINUATION;
    }
    else {
        f->u.path.ref = nullptr;  // nowhere to R_IMMEDIATE write back to
        Derelativize(f->out, f_value, f_specifier);
    }
    goto out_preloaded;
  }

  out_preloaded: {
    if (Is_Throwing(f)) {
        TRASH_POINTER_IF_DEBUG(f_opt_label);  // !!! an assert checks--review
        return R_THROWN;
    }

    const RELVAL *lookback;
    lookback = Lookback_While_Fetching_Next(f);

    // !!! For the moment, we allow /foo to fetch foo normally.
    //
    if (IS_END(f_value)) {
        if (PVS_OPT_SETVAL(f)) {
            if (not f->u.path.ref)
                fail ("Can't set immediate location");
            Move_Value(f->u.path.ref, PVS_OPT_SETVAL(f));
        }
        goto finished;
    }

    // !!! NULL pickers are legal for those pathers that can take it, but
    // not for the output cell if not at the end.  (Could this be folded in
    // with general null dispatch and still give a good error on the partial
    // path so far?)
    //
    if (IS_NULLED(f->out))
        fail (Error_No_Value_Core(lookback, f_specifier));

    goto next_picker;
  }

  next_picker: {
    if (IS_GET_WORD(f_value)) {  // e.g. object/:field
        Get_Word_May_Fail(PVS_PICKER(f), f_value, f_specifier);
    }
    else if (
        IS_GROUP(f_value)  // object/(expr) case:
        and NOT_EVAL_FLAG(f, PATH_HARD_QUOTE)  // not precomposed
    ){
        if (GET_EVAL_FLAG(f, NO_PATH_GROUPS))
            fail ("GROUP! in PATH! used with GET or SET (use REDUCE/EVAL)");

        DECLARE_FRAME_AT_CORE (
            block_frame,
            f_value,
            f_specifier,
            EVAL_MASK_DEFAULT | EVAL_FLAG_TO_END
        );

        Init_Void(PVS_PICKER(f));  // in case all invisibles, as usual
        Push_Frame(PVS_PICKER(f), block_frame, &Evaluator_Executor);

        STATE_BYTE(f) = ST_PATH_PICKER_GROUP_EVALUATING;  // => picker_ready
        return R_CONTINUATION;
    }
    else { // object/word and object/value case:
        Derelativize(PVS_PICKER(f), f_value, f_specifier);
    }

    goto picker_ready;
  }

  picker_ready: {
    Fetch_Next_Forget_Lookback(f);

    // Note: NULLED "pickers" are accepted by some "pathers" (e.g. ACTION!
    // allows you to opt out of refinements this way)
  }

  pick_or_poke: {
    if (Is_Throwing(f)) {
        TRASH_POINTER_IF_DEBUG(f_opt_label);  // !!! an assert checks--review
        Move_Value(f->out, PVS_PICKER(f));  // eval was into out
        return R_THROWN;
    }

    bool was_custom = (KIND_BYTE(f->out) == REB_CUSTOM);  // !!! for hack
    PATH_HOOK *hook = Path_Hook_For_Type_Of(f->out);

    if (IS_END(f_value) and PVS_IS_SET_PATH(f)) {  // poke
        const REBVAL *r = hook(f, PVS_PICKER(f), PVS_OPT_SETVAL(f));

        switch (KIND_BYTE(r)) {
          case REB_0_END:  // unhandled
            assert(r == R_UNHANDLED);  // shouldn't be other ends
            fail (Error_Bad_Path_Poke_Raw(PVS_PICKER(f)));

          case REB_R_THROWN:
            panic ("Path dispatch isn't allowed to throw, only GROUP!s");

          case REB_R_INVISIBLE:  // dispatcher assigned target with opt_setval
            break;  // nothing left to do, have to take the dispatcher's word

          case REB_R_REFERENCE:  { // dispatcher wants set *if* at end of path
            Move_Value(f->u.path.ref, PVS_OPT_SETVAL(f));
            break; }

          case REB_R_IMMEDIATE: {
            //
            // Imagine something like:
            //
            //      month/year: 1
            //
            // First month is written into the out slot as a reference to the
            // location of the month DATE! variable.  But because we don't
            // pass references from the previous steps *in* to the path
            // picking material, it only has the copied value in pvs->out.
            //
            // If we had a reference before we called in, we saved it in
            // pvs->u.ref.  So in the example case of `month/year:`, that
            // would be the CTX_VAR() where month was found initially, and so
            // we write the updated bits from pvs->out there.

            if (not f->u.path.ref)
                fail ("Can't update temporary immediate value via SET-PATH!");

            Move_Value(f->u.path.ref, f->out);
            break; }

          case REB_R_REDO: // e.g. used by REB_QUOTED to retrigger, sometimes
            goto pick_or_poke;

          default:
            //
            // Something like a generic D_OUT.  We could in theory take those
            // to just be variations of R_IMMEDIATE, but it's safer to break
            // that out as a separate class.
            //
            fail ("Path evaluation produced temporary value, can't POKE it");
        }
        TRASH_POINTER_IF_DEBUG(PVS_OPT_SETVAL(f));
    }
    else {  // pick
        f->u.path.ref = nullptr;  // clear status of the reference

        const REBVAL *r = hook(f, PVS_PICKER(f), nullptr);  // no "setval"

        if (r and r != END_NODE) {
            assert(r->header.bits & NODE_FLAG_CELL);
            /* assert(not (r->header.bits & NODE_FLAG_ROOT)); */
        }

        if (r == f->out) {
            // Common case... result where we expect it
        }
        else if (not r) {
            Init_Nulled(f->out);
        }
        else if (r == R_UNHANDLED) {
            if (IS_NULLED(PVS_PICKER(f)))
                fail ("NULL used in path picking but was not handled");

            fail (Error_Bad_Path_Pick_Raw(PVS_PICKER(f)));
        }
        else if (GET_CELL_FLAG(r, ROOT)) { // API, from Alloc_Value()
            Handle_Api_Dispatcher_Result(f, r);
        }
        else switch (KIND_BYTE(r)) {
          case REB_R_THROWN:
            panic ("Path dispatch isn't allowed to throw, only GROUP!s");

          case REB_R_INVISIBLE:
            assert(PVS_IS_SET_PATH(f));
            if (not was_custom)
                panic("SET-PATH! evaluation ran assignment before path end");

            // !!! All REB_CUSTOM types do not do this check at the moment
            // But the exemption was made for STRUCT! and GOB!, due to the
            // dispatcher hack to do "sub-value addressing" is to call
            // Next_Path_Throws inside of them, to be able to do a write
            // while they still have memory of what the struct and variable
            // are (which would be lost in this protocol otherwise).
            //
            assert(IS_END(f_value));
            break;

          case REB_R_REFERENCE: {
            bool was_const = GET_CELL_FLAG(f->out, CONST);
            Derelativize(
                f->out,
                f->u.path.ref,
                f->u.path.ref_specifier
            );
            if (was_const)  // can't Inherit_Const(), would overwrite flag
                SET_CELL_FLAG(f->out, CONST);

            // Leave the f->u.ref as-is in case the next update turns out
            // to be R_IMMEDIATE, and it is needed.
            break; }

          case REB_R_REDO:  // e.g. used by REB_QUOTED to retrigger, sometimes
            goto pick_or_poke;

          default:
            panic ("REB_R value not supported for path dispatch");
        }
    }

    // A function being refined does not actually update pvs->out with
    // a "more refined" function value, it holds the original function and
    // accumulates refinement state on the stack.  The label should only
    // be captured the first time the function is seen, otherwise it would
    // capture the last refinement's name, so check label for non-NULL.
    //
    if (IS_ACTION(f->out) and IS_WORD(PVS_PICKER(f))) {
        if (not PVS_OPT_LABEL(f))
            PVS_OPT_LABEL(f) = VAL_WORD_SPELLING(PVS_PICKER(f));
    }

    if (IS_END(f_value))
        goto finished;

    goto next_picker;  // did not throw
  }

  finished: {
    if (PVS_OPT_SETVAL(f))
        return f->out;  // If SET we don't change output (out would be input)

    if (DSP != f->baseline.dsp) {
        //
        // To make things easier for processing, reverse any refinements
        // pushed as ISSUE!s (we needed to evaluate them in forward order).
        // This way we can just pop them as we go, and know if they weren't
        // all consumed if not back to `f->baseline.dsp` by the end.

        REBVAL *bottom = DS_AT(f->baseline.dsp + 1);
        REBVAL *top = DS_TOP;

        while (top > bottom) {
            assert(IS_SYM_WORD(bottom) and not IS_WORD_BOUND(bottom));
            assert(IS_SYM_WORD(top) and not IS_WORD_BOUND(top));

            // It's faster to just swap the spellings.  (If binding
            // mattered, we'd need to swap the whole cells).
            //
            REBSTR *temp = VAL_WORD_SPELLING(bottom);
            INIT_VAL_NODE(bottom, VAL_WORD_SPELLING(top));
            INIT_VAL_NODE(top, temp);

            top--;
            bottom++;
        }

        assert(IS_ACTION(f->out));

        if (GET_EVAL_FLAG(f, PUSH_PATH_REFINES)) {
            //
            // The caller knows how to handle the refinements-pushed-to-stack
            // in-reverse-order protocol, and doesn't want to pay for making
            // a new ACTION!.
        }
        else {
            // The caller actually wants an ACTION! value to store or use
            // for later, as opposed to just calling it once.  It costs a
            // bit to do this, but unlike in R3-Alpha, it's possible to do!
            //
            // Code for specialization via refinement order works from the
            // data stack.  (It can't use direct value pointers because it
            // pushes to the stack itself, hence may move it on expansion.)
            //
            if (Specialize_Action_Throws(
                PVS_PICKER(f),
                f->out,
                PVS_OPT_LABEL(f),
                nullptr,  // opt_def
                f->baseline.dsp  // first_refine_dsp
            )){
                panic ("REFINE-only specializations should not THROW");
            }

            Move_Value(f->out, PVS_PICKER(f));
        }
    }

    return f->out;
  }
}


//
//  Init_Any_Path_At_Core: C
//
REBVAL *Init_Any_Path_At_Core(
    RELVAL *out,
    enum Reb_Kind kind,
    REBARR *a,
    REBLEN index,
    REBNOD *binding
){
    assert(ANY_PATH_KIND(kind));
    Force_Series_Managed(SER(a));
    ASSERT_SERIES_TERM(SER(a));
    assert(index == 0);  // !!! current rule

    RESET_CELL(out, kind, CELL_FLAG_FIRST_IS_NODE);
    INIT_VAL_NODE(out, a);
    VAL_INDEX(out) = index;
    INIT_BINDING(out, binding);

    if (ARR_LEN(a) < 2)
        panic (a);

    return SPECIFIC(out);
}


//
//  PD_Fail: C
//
// In order to avoid having to pay for a check for NULL in the path dispatch
// table for types with no path dispatch, a failing handler is in the slot.
//
REB_R PD_Fail(
    REBPVS *pvs,
    const REBVAL *picker,
    const REBVAL *opt_setval
){
    UNUSED(picker);
    UNUSED(opt_setval);

    fail (pvs->out);
}


//
//  PD_Unhooked: C
//
// As a temporary workaround for not having real user-defined types, an
// extension can overtake an "unhooked" type slot to provide behavior.
//
REB_R PD_Unhooked(
    REBPVS *pvs,
    const REBVAL *picker,
    const REBVAL *opt_setval
){
    UNUSED(picker);
    UNUSED(opt_setval);

    const REBVAL *type = Datatype_From_Kind(VAL_TYPE(pvs->out));
    UNUSED(type); // !!! put in error message?

    fail ("Datatype is provided by an extension which is not loaded.");
}


//
//  Eval_Path_Throws_Core: C
//
// Evaluate an ANY_PATH! REBVAL, starting from the index position of that
// path value and continuing to the end.
//
// The evaluator may throw because GROUP! is evaluated, e.g. `foo/(throw 1020)`
//
// If label_sym is passed in as being non-null, then the caller is implying
// readiness to process a path which may be a function with refinements.
// These refinements will be left in order on the data stack in the case
// that `out` comes back as IS_ACTION().  If it is NULL then a new ACTION!
// will be allocated, in the style of the REFINE native, which will have the
// behavior of refinement partial specialization.
//
// If `opt_setval` is given, the path operation will be done as a "SET-PATH!"
// if the path evaluation did not throw or error.  HOWEVER the set value
// is NOT put into `out`.  This provides more flexibility on performance in
// the evaluator, which may already have the `val` where it wants it, and
// so the extra assignment would just be overhead.
//
// !!! Path evaluation is one of the parts of R3-Alpha that has not been
// vetted very heavily by Revolt, and needs a review and overhaul.
//
bool Eval_Path_Throws_Core(
    REBVAL *out, // if opt_setval, this is only used to return a thrown value
    REBSTR **label_out,
    REBARR *array,
    REBLEN index,
    REBSPC *specifier,
    const REBVAL *opt_setval, // Note: may be the same as out!
    REBFLGS flags
){
    assert(index == 0); // !!! current rule, immutable proxy w/AS may relax it

    while (KIND_BYTE(ARR_AT(array, index)) == REB_BLANK)
        ++index; // pre-feed any blanks

    assert(NOT_END(ARR_AT(array, index)));

    DECLARE_ARRAY_FEED (feed, array, index, specifier);
    DECLARE_FRAME (
        pvs,
        feed,
        flags | EVAL_FLAG_ALLOCATED_FEED | EVAL_FLAG_ROOT_FRAME
    );

    SET_END(out);  // !!! Necessary?
    Push_Frame(out, pvs, &Path_Executor);

    PVS_OPT_SETVAL(pvs) = opt_setval;
    PVS_OPT_LABEL(pvs) = nullptr;

    if (Trampoline_Throws(pvs)) {
        assert(Is_Throwing(pvs));
        Abort_Frame(pvs);

        if (IS_ERROR(VAL_THROWN_LABEL(out)))
            fail (Error_No_Catch_For_Throw(out));  // propagates error

        return true;  // thrown
    }
    assert(not Is_Throwing(pvs));

    if (label_out)
        *label_out = PVS_OPT_LABEL(pvs);

    Drop_Frame_Unbalanced(pvs);  // refinements may accrue on stack
    return false;  // not thrown
}


//
//  Get_Simple_Value_Into: C
//
// "Does easy lookup, else just returns the value as is."
//
// !!! This is a questionable service, reminiscent of old behaviors of GET,
// were `get x` would look up a variable but `get 3` would give you 3.
// At time of writing it seems to appear in only two places.
//
void Get_Simple_Value_Into(REBVAL *out, const RELVAL *val, REBSPC *specifier)
{
    if (IS_WORD(val) or IS_GET_WORD(val))
        Get_Word_May_Fail(out, val, specifier);
    else if (IS_PATH(val) or IS_GET_PATH(val))
        Get_Path_Core(out, val, specifier);
    else
        Derelativize(out, val, specifier);
}


//
//  Resolve_Path: C
//
// Given a path, determine if it is ultimately specifying a selection out
// of a context...and if it is, return that context.  So `a/obj/key` would
// return the object assocated with obj, while `a/str/1` would return
// NULL if `str` were a string as it's not an object selection.
//
// !!! This routine overlaps the logic of Eval_Path, and should potentially
// be a mode of that instead.  It is not very complete, considering that it
// does not execute GROUP! (and perhaps shouldn't?) and only supports a
// path that picks contexts out of other contexts, via word selection.
//
REBCTX *Resolve_Path(const REBVAL *path, REBLEN *index_out)
{
    REBARR *array = VAL_ARRAY(path);
    RELVAL *picker = ARR_HEAD(array);

    if (IS_END(picker) or not ANY_WORD(picker))
        return NULL; // !!! only handles heads of paths that are ANY-WORD!

    const RELVAL *var = Lookup_Word_May_Fail(picker, VAL_SPECIFIER(path));

    ++picker;
    if (IS_END(picker))
        return NULL; // !!! does not handle single-element paths

    while (ANY_CONTEXT(var) and IS_WORD(picker)) {
        REBLEN i = Find_Canon_In_Context(
            VAL_CONTEXT(var), VAL_WORD_CANON(picker), false
        );
        ++picker;
        if (IS_END(picker)) {
            *index_out = i;
            return VAL_CONTEXT(var);
        }

        var = CTX_VAR(VAL_CONTEXT(var), i);
    }

    return NULL;
}


//
//  pick: native [
//
//  {Perform a path picking operation, same as `:(:location)/(:picker)`}
//
//      return: [<opt> any-value!]
//          {Picked value, or null if picker can't fulfill the request}
//      location [any-value!]
//      picker [any-value!]
//          {Index offset, symbol, or other value to use as index}
//  ]
//
REBNATIVE(pick)
//
// In R3-Alpha, PICK was an "action", which dispatched on types through the
// "action mechanic" for the following types:
//
//     [any-series! map! gob! pair! date! time! tuple! bitset! port! varargs!]
//
// In Revolt, PICK is rethought to use the same dispatch mechanic as paths,
// to cut down on the total number of operations the system has to define.
{
    INCLUDE_PARAMS_OF_PICK;

    REBVAL *location = ARG(location);

    // PORT!s are kind of a "user defined type" which historically could
    // react to PICK and POKE, but which could not override path dispatch.
    // Use a symbol-based call to bounce the frame to the port, which should
    // be a compatible frame with the historical "action".
    //
    if (IS_PORT(location)) {
        DECLARE_LOCAL (word);
        Init_Word(word, Canon(SYM_PICK));
        return Do_Port_Action(frame_, location, word);
    }

    DECLARE_END_FRAME (pvs, EVAL_MASK_DEFAULT);
    Push_Frame(D_OUT, pvs, &Path_Executor);

    Move_Value(D_OUT, location);
    Move_Value(PVS_PICKER(pvs), ARG(picker));

    PVS_OPT_LABEL(pvs) = nullptr;  // e.g. :append/only returning APPEND
    PVS_OPT_SETVAL(pvs) = nullptr;

  redo: ;  // semicolon is intentional, next line is declaration

    PATH_HOOK *hook = Path_Hook_For_Type_Of(D_OUT);

    REB_R r = hook(pvs, PVS_PICKER(pvs), NULL);

    if (not r or r == pvs->out) {
        // Do nothing, let caller handle
    }
    else if (IS_END(r)) {
        assert(r == R_UNHANDLED);
        Abort_Frame(pvs);
        fail (Error_Bad_Path_Pick_Raw(PVS_PICKER(pvs)));
    }
    else if (GET_CELL_FLAG(r, ROOT)) {  // API value
        //
        // It was parented to the PVS frame, we have to read it out.
        //
        Move_Value(D_OUT, r);
        rebRelease(r);
        r = D_OUT;
    }
    else switch (CELL_KIND_UNCHECKED(r)) {
      case REB_R_INVISIBLE:
        assert(false); // only SETs should do this
        break;

      case REB_R_REFERENCE: {
        assert(pvs->out == D_OUT);
        bool was_const = GET_CELL_FLAG(D_OUT, CONST);
        Derelativize(
            D_OUT,
            pvs->u.path.ref,
            pvs->u.path.ref_specifier
        );
        if (was_const) // can't Inherit_Const(), flag would be overwritten
            SET_CELL_FLAG(D_OUT, CONST);
        r = D_OUT;
        break; }

      case REB_R_REDO:
        goto redo;

      default:
        panic ("Unsupported return value in Path Dispatcher");
    }

    TRASH_POINTER_IF_DEBUG(pvs->executor);
    Drop_Frame(pvs);
    return r;
}


//
//  poke: native [
//
//  {Perform a path poking operation, same as `(:location)/(:picker): :value`}
//
//      return: [<opt> any-value!]
//          {Same as value}
//      location [any-value!]
//          {(modified)}
//      picker
//          {Index offset, symbol, or other value to use as index}
//      value [<opt> any-value!]
//          {The new value}
//  ]
//
REBNATIVE(poke)
//
// As with PICK*, POKE is changed in Revolt from its own action to "whatever
// path-setting (now path-poking) would do".
{
    INCLUDE_PARAMS_OF_POKE;

    REBVAL *location = ARG(location);

    // PORT!s are kind of a "user defined type" which historically could
    // react to PICK and POKE, but which could not override path dispatch.
    // Use a symbol-based call to bounce the frame to the port, which should
    // be a compatible frame with the historical "action".
    //
    if (IS_PORT(location)) {
        DECLARE_LOCAL (word);
        Init_Word(word, Canon(SYM_POKE));
        return Do_Port_Action(frame_, location, word);
    }

    DECLARE_END_FRAME (pvs, EVAL_MASK_DEFAULT);
    Push_Frame(D_OUT, pvs, &Path_Executor);

    Move_Value(D_OUT, location);

    Move_Value(PVS_PICKER(pvs), ARG(picker));

    PVS_OPT_LABEL(pvs) = nullptr;  // e.g. :append/only returning APPEND
    PVS_OPT_SETVAL(pvs) = ARG(value);

    PATH_HOOK *hook = Path_Hook_For_Type_Of(location);

    const REBVAL *r = hook(pvs, PVS_PICKER(pvs), ARG(value));
    switch (KIND_BYTE(r)) {
      case REB_0_END:
        assert(r == R_UNHANDLED);
        Abort_Frame(pvs);  // must be topmost frame when failing
        fail (Error_Bad_Path_Poke_Raw(PVS_PICKER(pvs)));

      case REB_R_INVISIBLE:  // is saying it did the write already
        break;

      case REB_R_REFERENCE:  // wants us to write it
        Move_Value(pvs->u.path.ref, ARG(value));
        break;

      default:
        assert(false);  // shouldn't happen, complain in the debug build
        Abort_Frame(pvs);  // must be topmost frame when failing
        fail (PVS_PICKER(pvs));  // raise error in release build
    }

    TRASH_POINTER_IF_DEBUG(pvs->executor);
    Drop_Frame(pvs);

    RETURN (ARG(value)); // return the value we got in
}


//
//  -slash-1-: enfix native [
//
//  {Default implementation for `/` in the evaluator}
//
//      left [<opt> any-value!]
//      right [<opt> any-value!]
//  ]
//
REBNATIVE(_slash_1_)
//
// It's very desirable to have `/`, `/foo`, `/foo/`, `/foo/(bar)` etc. be
// instances of the same datatype of PATH!.  In this scheme, `/` would act
// like a "root path" and be achieved with `to path! [_ _]`.
//
// But with limited ASCII symbols, there is strong demand for `/` to be able
// to act like division in evaluative contexts, or to be overrideable for
// other things in a way not too dissimilar from `+`.
//
// The compromise used is to make `/` be a cell whose VAL_TYPE() is REB_PATH,
// but whose CELL_KIND() is REB_WORD with the special spelling `-1-SLASH-`.
// Binding mechanics and evaluator behavior are based on this unusual name.
// But when inspected by the user, it appears to be a PATH! with 2 blanks.
//
// This duality is imperfect, as any routine with semantics like COLLECT-WORDS
// would have to specifically account for it, or just see an empty path.
// But it is intended to give some ability to configure the behavior easily.
{
    INCLUDE_PARAMS_OF__SLASH_1_;

    REBVAL *left = ARG(left);
    REBVAL *right = ARG(right);

    // !!! Somewhat whimsically, this goes ahead and guesses at a possible
    // behavior for "dividing" strings using SPLIT.  This is a placeholder
    // for the idea that the left hand type gets to dispatch a choice of
    // what it means, as with ordinary path dispatch.
    //
    // Uses the /INTO refinement so that `"abcdef" / 2` divides the string
    // into two pieces, as opposed to pieces of length 2.
    //
    if (ANY_STRING(left) or ANY_ARRAY(left))
        return rebValueQ("split/into", left, right, rebEND);

    // Note: DIVIDE is historically a "type action", so technically it is the
    // left hand side type which gets to pick the behavior--consistent with
    // the plan for how 0-length paths would work.
    //
    return rebValueQ("divide", left, right, rebEND);
}


//
//  PD_Path: C
//
// A PATH! is not an array, but if it is implemented as one it may choose to
// dispatch path handling to its array.
//
REB_R PD_Path(
    REBPVS *pvs,
    const REBVAL *picker,
    const REBVAL *opt_setval
){
    if (opt_setval)
        fail ("PATH!s are immutable (convert to GROUP! or BLOCK! to mutate)");

    Init_Block(pvs->out, VAL_PATH(pvs->out));

    return PD_Array(pvs, picker, opt_setval);
}


//
//  REBTYPE: C
//
// The concept of PATH! is now that it is an immediate value.  While it
// permits picking and enumeration, it may or may not have an actual REBARR*
// node backing it.
//
REBTYPE(Path)
{
    REBVAL *path = D_ARG(1);

    switch (VAL_WORD_SYM(verb)) {
      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;
        UNUSED(ARG(value));

        switch (VAL_WORD_SYM(ARG(property))) {
          case SYM_LENGTH:
            if (MIRROR_BYTE(path) == REB_WORD) {
                assert(VAL_WORD_SYM(path) == SYM__SLASH_1_);
                return Init_Integer(frame_->out, 2);
            }
            return Series_Common_Action_Maybe_Unhandled(frame_, verb);

          // !!! Any other interesting reflectors?

          case SYM_INDEX: // not legal, paths always at head, no index
          default:
            break;
        }
        break; }

        // Since ANY-PATH! is immutable, a shallow copy should be cheap, but
        // it should be cheap for any similarly marked array.  Also, a /DEEP
        // copy of a path may copy groups that are mutable.
        //
      case SYM_COPY:
        if (MIRROR_BYTE(path) == REB_WORD) {
            assert(VAL_WORD_SYM(path) == SYM__SLASH_1_);
            return Move_Value(frame_->out, path);
        }

        goto retrigger;

      default:
        break;
    }

    return R_UNHANDLED;

  retrigger:

    return T_Array(frame_, verb);
}


//
//  MF_Path: C
//
void MF_Path(REB_MOLD *mo, const REBCEL *v, bool form)
{
    UNUSED(form);

    enum Reb_Kind kind = CELL_TYPE(v);  // Note: CELL_KIND() might be WORD!

    if (kind == REB_GET_PATH)
        Append_Codepoint(mo->series, ':');
    else if (kind == REB_SYM_PATH)
        Append_Codepoint(mo->series, '@');

    if (MIRROR_BYTE(v) == REB_WORD) {  // optimized for `/`, allows binding
        assert(VAL_WORD_SYM(v) == SYM__SLASH_1_);
        Append_Ascii(mo->series, "/");
    }
    else {
        REBARR *a = VAL_ARRAY(v);

        // Recursion check:
        if (Find_Pointer_In_Series(TG_Mold_Stack, a) != NOT_FOUND) {
            Append_Ascii(mo->series, ".../...");
            return;
        }
        Push_Pointer_To_Series(TG_Mold_Stack, a);

        assert(VAL_INDEX(v) == 0);  // new rule, not ANY-ARRAY!, always head
        assert(ARR_LEN(a) >= 2); // another rule, even / is `make path! [_ _]`

        RELVAL *item = ARR_HEAD(a);
        while (NOT_END(item)) {
            assert(not ANY_PATH(item)); // another new rule

            if (not IS_BLANK(item)) { // no blank molding; slashes convey it
                //
                // !!! Molding of items in paths which have slashes in them,
                // like URL! or FILE! (or some historical date formats) need
                // some kind of escaping, otherwise they have to be outlawed
                // too.  FILE! has the option of `a/%"dir/file.txt"/b` to put
                // the file in quotes, but URL does not.
                //
                Mold_Value(mo, item);

                // Note: Ignore VALUE_FLAG_NEWLINE_BEFORE here for ANY-PATH,
                // but any embedded BLOCK! or GROUP! which do have newlines in
                // them can make newlines, e.g.:
                //
                //     a/[
                //        b c d
                //     ]/e
            }

            ++item;
            if (IS_END(item))
                break;

            Append_Codepoint(mo->series, '/');
        }

        Drop_Pointer_From_Series(TG_Mold_Stack, a);
    }

    if (kind == REB_SET_PATH)
        Append_Codepoint(mo->series, ':');
}


//
//  MAKE_Path: C
//
// A MAKE of a PATH! is experimentally being thought of as evaluative.  This
// is in line with the most popular historical interpretation of MAKE, for
// MAKE OBJECT!--which evaluates the object body block.
//
REB_R MAKE_Path(
    REBVAL *out,
    enum Reb_Kind kind,
    const REBVAL *opt_parent,
    const REBVAL *arg
){
    if (opt_parent)
        fail (Error_Bad_Make_Parent(kind, opt_parent));

    if (not IS_BLOCK(arg))
        fail (Error_Bad_Make(kind, arg)); // "make path! 0" has no meaning

    DECLARE_FRAME_AT (f, arg, EVAL_MASK_DEFAULT | EVAL_FLAG_ROOT_FRAME);

    Push_Frame_Core(nullptr, f);

    REBDSP dsp_orig = DSP;

    while (NOT_END(f->feed->value)) {
        if (Eval_Step_Throws(out, f)) {
            Abort_Frame(f);
            return R_THROWN;
        }

        if (IS_END(out))
            break;
        if (IS_NULLED(out))
            continue;

        if (not ANY_PATH(out)) {
            if (DSP != dsp_orig and IS_BLANK(DS_TOP))
                DS_DROP(); // make path! ['a/ 'b] => a/b, not a//b
            Move_Value(DS_PUSH(), out);
        }
        else { // Splice any generated paths, so there are no paths-in-paths.

            RELVAL *item = VAL_ARRAY_AT(out);
            if (IS_BLANK(item) and DSP != dsp_orig) {
                if (IS_BLANK(DS_TOP)) // make path! ['a/b/ `/c`]
                    fail ("Cannot merge slashes in MAKE PATH!");
                ++item;
            }
            else if (DSP != dsp_orig and IS_BLANK(DS_TOP))
                DS_DROP(); // make path! ['a/ 'b/c] => a/b/c, not a//b/c

            for (; NOT_END(item); ++item)
                Derelativize(DS_PUSH(), item, VAL_SPECIFIER(out));
        }
    }

    REBARR *arr = Pop_Stack_Values_Core(dsp_orig, NODE_FLAG_MANAGED);
    Drop_Frame_Unbalanced(f); // !!! f->baseline.dsp got captured each loop

    if (ARR_LEN(arr) < 2) // !!! Should pass produced array as BLOCK! to error
        fail ("MAKE PATH! must produce path of at least length 2");

    // Need special case code if the array needs to be a disguised WORD!
    // (See -SLASH-1- for details)
    //
    if (ARR_LEN(arr) == 2)
        if (IS_BLANK(ARR_AT(arr, 0)) and IS_BLANK(ARR_AT(arr, 1))) {
            Free_Unmanaged_Array(arr);
            Init_Word(out, PG_Slash_1_Canon);
            mutable_KIND_BYTE(out) = REB_PATH;
            return out;
        }

    return Init_Any_Path(out, kind, arr);
}


static void Push_Path_Recurses(RELVAL *path, REBSPC *specifier)
{
    RELVAL *item = ARR_HEAD(VAL_PATH(path));
    for (; NOT_END(item); ++item) {
        if (IS_PATH(item)) {
            if (IS_SPECIFIC(item))
                Push_Path_Recurses(item, VAL_SPECIFIER(item));
            else
                Push_Path_Recurses(item, specifier);
        }
        else
            Derelativize(DS_PUSH(), item, specifier);
    }
}


//
//  TO_Path: C
//
REB_R TO_Path(REBVAL *out, enum Reb_Kind kind, const REBVAL *arg) {
    if (ANY_PATH(arg)) {  // e.g. `to set-path! 'a/b/c`
        assert(kind != VAL_TYPE(arg));  // TO should have called COPY

        Move_Value(out, arg);
        mutable_KIND_BYTE(out) = mutable_MIRROR_BYTE(out) = kind;
        return out;
    }

    if (not ANY_ARRAY(arg)) {  // e.g. `to path! foo` becomes `/foo`
        //
        // !!! This is slated to be able to fit into a single cell, with no
        // array allocation.
        //
        REBARR *a = Make_Array(2);
        Init_Blank(ARR_AT(a, 0));
        Move_Value(ARR_AT(a, 1), arg);
        TERM_ARRAY_LEN(a, 2);
        return Init_Any_Path(out, kind, a);
    }

    REBDSP dsp_orig = DSP;
    RELVAL *item = VAL_ARRAY_AT(arg);
    for (; NOT_END(item); ++item) {
        if (IS_PATH(item))
            Push_Path_Recurses(item, VAL_SPECIFIER(arg));
        else
            Derelativize(DS_PUSH(), item, VAL_SPECIFIER(arg));
    }

    if (DSP - dsp_orig < 2)
        fail ("TO PATH! must produce a path of at least length 2");

    if (DSP - dsp_orig == 2)
        if (IS_BLANK(DS_TOP) and IS_BLANK(DS_TOP - 1)) {
            DS_DROP_TO(dsp_orig);
            Init_Word(out, PG_Slash_1_Canon);
            mutable_KIND_BYTE(out) = REB_PATH;
            return out;
        }

    return Init_Any_Path(out, kind, Pop_Stack_Values(dsp_orig));
}


//
//  CT_Path: C
//
// "Compare Type" dispatcher for the following types: (list here to help
// text searches)
//
//     CT_Set_Path()
//     CT_Get_Path()
//     CT_Lit_Path()
//
REBINT CT_Path(const REBCEL *a, const REBCEL *b, REBINT mode)
{
    REBINT num;
    if (MIRROR_BYTE(a) == REB_WORD and MIRROR_BYTE(b) == REB_WORD)
        num = Compare_Word(a, b, mode == 1);
    else if (MIRROR_BYTE(a) != REB_WORD and MIRROR_BYTE(b) != REB_WORD)
        num = Cmp_Array(a, b, mode == 1);
    else
        num = -1;  // !!! what is the right answer here?

    if (mode >= 0)
        return (num == 0);
    if (mode == -1)
        return (num >= 0);
    return (num > 0);
}

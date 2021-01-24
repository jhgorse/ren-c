//
//  File: %sys-word.h
//  Summary: {Definitions for the ANY-WORD! Datatypes}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2019 Ren-C Open Source Contributors
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
// ANY-WORD! is the fundamental symbolic concept of Rebol.  Rather than
// storing a pointer to a mutable string, it stores a pointer to a read-only
// symbol (see %sys-symbol.h) that can be quickly looked up and compared.
//
// Words can act as a variable when bound specifically to a context
// (see %sys-context.h) or bound relatively to an action (see %sys-action.h).
//
// For routines that manage binding, see %sys-bind.h.
//


inline static REBLEN VAL_WORD_PRIMARY_INDEX(RELVAL *v) {
    assert(ANY_WORD_KIND(CELL_KIND(VAL_UNESCAPED(v))));
    return VAL_WORD_PRIMARY_INDEX_UNCHECKED(v);
}

inline static void INIT_VAL_WORD_PRIMARY_INDEX(RELVAL *v, REBLEN i) {
    assert(ANY_WORD_KIND(CELL_HEART(VAL_UNESCAPED(v))));
    assert(i < 1048576);  // 20 bit number for physical indices
    VAL_WORD_INDEXES_U32(v) &= 0xFFF00000;
    VAL_WORD_INDEXES_U32(v) |= i;
}

inline static void INIT_VAL_WORD_VIRTUAL_MONDEX(
    REBCEL(const*) v,  // mutation allowed on cached property
    REBLEN mondex  // index mod 4095 (hence invented name "mondex")
){
    assert(ANY_WORD_KIND(HEART3X_BYTE(v)));
    assert(mondex <= MONDEX_MOD);  // 12 bit number for virtual indices
    VAL_WORD_INDEXES_U32(m_cast(RELVAL*, cast(const RELVAL*, v))) &= 0x000FFFFF;
    VAL_WORD_INDEXES_U32(m_cast(RELVAL*, cast(const RELVAL*, v))) |= mondex << 20;
}

#ifdef CPLUSPLUS_11
    inline static void INIT_VAL_WORD_VIRTUAL_MONDEX(
        const RELVAL *v,  // virtual binding only in unescaped forms
        REBLEN mondex
    ) = delete;
#endif

inline static REBVAL *Init_Any_Word_Untracked(
    RELVAL *out,
    enum Reb_Kind kind,
    const REBSYM *symbol
){
    RESET_VAL_HEADER(out, kind, CELL_FLAG_FIRST_IS_NODE);
    mutable_BINDING(out) = symbol;
    VAL_WORD_INDEXES_U32(out) = 0;
    INIT_VAL_WORD_CACHE(cast(REBCEL(const*), out), SPECIFIED);

    // For each word there is a lowercase spelling, and up to two alternate
    // case variations that are encoded in the heart byte (TBD).  But if you
    // use a third spelling, that gives the cell a heart byte that counts in
    // the "escaped" range...while still encoding the cell is an ANY-WORD!.
    //
    // !!! The two alternate spellings concept is pending, but close.
    //
    // !!! Technically, the escaping is not necessary unless the word is
    // bound.  However, bouncing things from expanded to unexpanded makes
    // the code more complicated...and requires dereferencing the symbol
    // often.  This allows the heart byte to be set once and cue the necessary
    // behaviors from then on.
    //
    if (NOT_SUBCLASS_FLAG(SYMBOL, symbol, IS_CANON)) {
        Quotify_Core(out, 4);  // !!! trigger expansion, reuse code for now
        mutable_KIND3Q_BYTE(out) = kind;
        mutable_HEART3X_BYTE(out) = kind + (REB_64 * 3);  // signal escaping
        assert(VAL_WORD_VIRTUAL_MONDEX_UNCHECKED(out) == 4);  // quote level
        VAL_WORD_INDEXES_U32(out) &= 0x000FFFFF;  // zero out quote level
        assert(VAL_WORD_VIRTUAL_MONDEX_UNCHECKED(out) == 0);  // non quoted
    }

    return cast(REBVAL*, out);
}

#define Init_Any_Word(out,kind,spelling) \
    Init_Any_Word_Untracked(TRACK_CELL_IF_DEBUG(out), (kind), (spelling))

#define Init_Word(out,str)          Init_Any_Word((out), REB_WORD, (str))
#define Init_Get_Word(out,str)      Init_Any_Word((out), REB_GET_WORD, (str))
#define Init_Set_Word(out,str)      Init_Any_Word((out), REB_SET_WORD, (str))
#define Init_Sym_Word(out,str)      Init_Any_Word((out), REB_SYM_WORD, (str))

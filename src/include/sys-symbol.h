//
//  File: %sys-symbol.h
//  Summary: {Definitions for Symbols and Symbol IDs}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012-2021 Ren-C Open Source Contributors
// Copyright 2012 REBOL Technologies
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
// A "Symbol" (REBSYM) is a subclass of read-only string series, whose
// character sequences are legal for use in an ANY-WORD!.  These strings are
// "interned" such that each unique spelling exists only once in memory:
//
// https://en.wikipedia.org/wiki/String_interning
//
// If all the characters in a symbol are lowercase, then it is considered to
// be a "Canon" symbol, which has its own subclass type (REBCAN).  Since the
// binding in Redbol languages is case-insensitive, many functions require
// the canon form.  There are fast operations for navigating from any
// casing variation of a symbol (its "synonyms") to the canon.
//
// Some common symbols are listed in a file %words.r, and loaded during boot.
// These are given fixed integer IDs (SYMID_XXX) which make it convenient to
// use them in places like C switch() statements.  If a symbol does not
// have an ID number, it will report `SYMID_0`--not to be confused with a
// symbol of the text "0", which shouldn't exist because "0" is not a legal
// word spelling.
//
//=//// NOTES /////////////////////////////////////////////////////////////=//
//
// * R3-Alpha did not have a facility for garbage collecting symbols when they
//   were no longer used.  In Ren-C they are series, and GC'd like strings
//   and other REBSER variants.  Special handling is needed when they are
//   freed to clean up references in the hash tables used for interning.
//
// * Words that do not appear in %words.r will report their SYMID as SYMID_0.
//   This creates the risk of incorrectly thinking two different symbols are
//   the same with code like `VAL_WORD_ID(a) == VAL_WORD_ID(b)`.  The C++
//   build does some trickery to catch such comparisons at compile-time.


#if defined(NDEBUG) || !defined(CPLUSPLUS_11)
    //
    // Trivial definition for C build or release builds: symbols are just a C
    // enum value and an OPT_SYMID acts just like a SYMID.
    //
    typedef enum Reb_Symbol_Id SYMID;
    typedef enum Reb_Symbol_Id OPT_SYMID;
#else
    // The C++ build protects against the situation of:
    //
    //    VAL_WORD_ID(a) == VAL_WORD_ID(b)
    //
    // This could misleadingly make it seem like two words have the same
    // spelling, when in actuality neither has an ID and reported SYMID_0.
    //
    // It's avoided by creating an optional variant of SYMID called OPT_SYMID,
    // which disrupts comparisons with other OPT_SYMID.
    //
    // Defeating the comparison blocks must be done explicitly with functions
    // that demonstrate callsite awareness, like `Same_Nonzero_Symid()`.

    struct SYMID;

    struct OPT_SYMID {  // may only be converted to SYMID, no comparisons
        enum Reb_Symbol_Id n;
        OPT_SYMID (const SYMID& sym);
        bool operator==(enum Reb_Symbol_Id other) const
          { return n == other; }
        bool operator!=(enum Reb_Symbol_Id other) const
          { return n != other; }

        bool operator==(OPT_SYMID &&other) const = delete;
        bool operator!=(OPT_SYMID &&other) const = delete;

        operator unsigned int() const  // so it works in switch() statements
          { return cast(unsigned int, n); }

        explicit operator enum Reb_Symbol_Id()  // must be an *explicit* cast
          { return n; }
    };

    struct SYMID {  // acts like a REBOL_Symbol with no OPT_SYMID compares
        enum Reb_Symbol_Id n;
        SYMID () {}
        SYMID (int n) : n (cast(enum Reb_Symbol_Id, n)) {}
        SYMID (OPT_SYMID opt_sym) : n (opt_sym.n) {}

        operator unsigned int() const  // so it works in switch() statements
          { return cast(unsigned int, n); }

        explicit operator enum Reb_Symbol_Id() {  // must be an *explicit* cast
            assert(n != SYM_0);
            return n;
        }

        bool operator>=(enum Reb_Symbol_Id other) const {
            assert(other != SYM_0);
            return n >= other;
        }
        bool operator<=(enum Reb_Symbol_Id other) const {
            assert(other != SYM_0);
            return n <= other;
        }
        bool operator>(enum Reb_Symbol_Id other) const {
            assert(other != SYM_0);
            return n > other;
        }
        bool operator<(enum Reb_Symbol_Id other) const {
            assert(other != SYM_0);
            return n < other;
        }
        bool operator==(enum Reb_Symbol_Id other) const
          { return n == other; }
        bool operator!=(enum Reb_Symbol_Id other) const
          { return n != other; }

        bool operator==(SYMID &other) const = delete;  // may be SYM_0
        void operator!=(SYMID &other) const = delete;  // ...same
        bool operator==(const OPT_SYMID &other) const = delete;  // ...same
        void operator!=(const OPT_SYMID &other) const = delete;  // ...same
    };

    inline OPT_SYMID::OPT_SYMID(const SYMID &sym) : n (sym.n) {}
#endif

inline static bool Same_Nonzero_Symid(SYMID a, SYMID b) {
    assert(a != SYM_0 and b != SYM_0);
    return cast(REBLEN, a) == cast(REBLEN, b);
}

inline static OPT_SYMID ID_OF_CANON(const REBCAN *canon)
  { return cast(SYMID, SECOND_UINT16(canon->info)); }

inline static const REBCAN *Canon(SYMID symid) {
    assert(cast(REBLEN, symid) != 0);
    assert(cast(REBLEN, symid) < SER_USED(PG_Symbol_Canons));  // null if boot
    return *SER_AT(const REBCAN*, PG_Symbol_Canons, cast(REBLEN, symid));
}

inline static bool Are_Synonyms(const REBSYM *s1, const REBSYM *s2) {
    const REBSTR *temp = s1;
    do {
        if (temp == s2)
            return true;
    } while ((temp = LINK(NextSynonym, temp)) != s1);

    return false;  // stopped when circularly linked list loops back to self
}

inline static const REBCAN *Canon_Of_Symbol(const REBSYM *s) {
    if (GET_SUBCLASS_FLAG(SYMBOL, s, IS_CANON))
        return CAN(s);

    return MISC(CanonOfSynonym, s);
}

// Helper calls strsize() so you can more easily use literals at callsite.
// (Better to call Intern_UTF8_Managed() with the size if you know it.)
//
inline static const REBSTR *Intern_Unsized_Managed(const char *utf8)
  { return Intern_UTF8_Managed(cb_cast(utf8), strsize(utf8)); }

//
//  File: %main.c
//  Summary: "Console application main entry point"
//  Project: "Revolt Language Interpreter and Run-time Environment"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2020 Revolt Open Source Contributors
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
// This contains the main() routine, which uses the libRevolt API to start up
// an interactive console system for environments that can compile C.
//
// On POSIX systems it uses <termios.h> to implement line editing:
//
// http://pubs.opengroup.org/onlinepubs/7908799/xbd/termios.html
//
// On Windows it uses the Console API:
//
// https://msdn.microsoft.com/en-us/library/ms682087.aspx
//
// Very little work is done in C.  For instance, the command line arguments
// are processed using PARSE by Rebol code that is embedded into the
// executable as compressed bytes.  And the majority of the console behavior
// is defined by Rebol code in %extensions/console.
//

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef TO_WINDOWS
    #undef _WIN32_WINNT  // https://forum.rebol.info/t/326/4
    #define _WIN32_WINNT 0x0501  // Minimum API target: WinXP
    #define WIN32_LEAN_AND_MEAN  // trim down the Win32 headers
    #include <windows.h>

    #include <shellapi.h>  // for CommandLineToArgvW()
#endif

#include "pstdint.h"  // stdint.h for builds w/pre-C99 compilers--see notes
#include "pstdbool.h"  // stdbool.h for builds w/pre-C99 compilers--see notes

// This file should only use the external API.  However, it can be helpful in
// debug situations to have access to PROBE() and other internal features.
//
#if !defined(DEBUG_MAIN_USING_SYS_CORE)
    #define REVOLT_EXPLICIT_END  // for building w/pre-C99 compilers--see notes
    #include "revolt.h"  // note: includes pstdint.h and pstdbool.h by default
#else
    #undef IS_ERROR  // windows.h has its own definition of this macro
    #include "sys-core.h"
#endif

#include "reb-c.h"  // provides cast(), UNUSED(), etc.


// Initialization done by rebStartup() is intended to be as basic as possible
// in order to get the Rebol series/values/array functions ready to be run.
// Once that's ready, the rest of the initialization can take advantage of
// a working evaluator.  This includes PARSE to process the command line
// parameters, or PRINT to output boot banners.
//
// The %prep-main.reb script bundles the %main-startup.reb with some other
// files, and turns the bundle into a compressed binary C literal.  That
// literal can be LOADed and executed to return the MAIN-STARTUP function,
// which takes the command line arguments as an array of STRING! and handles
// it from there.
//
#include "tmp-main-startup.inc"


#ifdef TO_WINDOWS
    //
    // Most Windows-specific code is expected to be run in extensions (or
    // in the interim, in "devices").  However, it's expected that all Windows
    // code be able to know its `HINSTANCE`.  This is usually passed in a
    // WinMain(), but since we don't use WinMain() in order to be able to
    // act as a console app -or- a GUI app some tricks are needed to capture
    // it, and then export it for other code to use.
    //
    // !!! This is not currently exported via EXTERN_C, because the core was
    // building in a dependency on the host.  This created problems for the
    // libRevolt, which needs to be independent of %host-main.c, and may be
    // used with clients that do not have the HINSTANCE easily available.
    // The best idea for exporting it is probably to have those clients who
    // provide it to inject it into the system object as a HANDLE!, so that
    // those extensions which need it have access to it, while not creating
    // problems for those that do not.
    //
    HINSTANCE App_Instance = 0;

    // For why this is done this way with a potential respawning, see the
    // StackOverflow question:
    //
    // "Can one executable be both a console and a GUI application":
    //
    //     http://stackoverflow.com/q/493536/
    //
    void Determine_Hinstance_May_Respawn(WCHAR *this_exe_path) {
        if (GetStdHandle(STD_OUTPUT_HANDLE) == 0) {
            //
            // No console to attach to, we must be the DETACHED_PROCESS which
            // was spawned in the below branch.
            //
            App_Instance = GetModuleHandle(nullptr);
        }
        else {
          #ifdef REB_CORE
            //
            // In "Core" mode, use a console but do not initialize graphics.
            // (stdio redirection works, blinking console window during start)
            //
            App_Instance = cast(HINSTANCE,
                GetWindowLongPtr(GetConsoleWindow(), GWLP_HINSTANCE)
            );
            UNUSED(this_exe_path);
          #else
            //
            // In the "GUI app" mode, stdio redirection doesn't work properly,
            // but no blinking console window during start.
            //
            if (not this_exe_path) { // argc was > 1
                App_Instance = cast(HINSTANCE,
                    GetWindowLongPtr(GetConsoleWindow(), GWLP_HINSTANCE)
                );
            }
            else {
                // Launch child as a DETACHED_PROCESS so that GUI can be
                // initialized, and exit.
                //
                STARTUPINFO startinfo;
                ZeroMemory(&startinfo, sizeof(startinfo));
                startinfo.cb = sizeof(startinfo);

                PROCESS_INFORMATION procinfo;
                if (not CreateProcess(
                    nullptr, // lpApplicationName
                    this_exe_path, // lpCommandLine
                    nullptr, // lpProcessAttributes
                    nullptr, // lpThreadAttributes
                    FALSE, // bInheritHandles
                    CREATE_DEFAULT_ERROR_MODE | DETACHED_PROCESS,
                    nullptr, // lpEnvironment
                    nullptr, // lpCurrentDirectory
                    &startinfo,
                    &procinfo
                )){
                    MessageBox(
                        nullptr, // owner window
                        L"CreateProcess() failed in %host-main.c",
                        this_exe_path, // title
                        MB_ICONEXCLAMATION | MB_OK
                    );
                }

                exit(0);
            }
          #endif
        }
    }
#endif


#ifdef TO_WINDOWS

    #undef _WIN32_WINNT  // https://forum.rebol.info/t/326/4
    #define _WIN32_WINNT 0x0501  // Minimum API target: WinXP
    #define WIN32_LEAN_AND_MEAN  // trim down the Win32 headers
    #include <windows.h>

    #undef IS_ERROR  // %windows.h defines this, but so does %sys-core.h

#elif defined(TO_EMSCRIPTEN)
    //
    // The emscripten build sets up the handling of rebHalt() differently
    //
    #error "%main.c included in Emscripten build - should not happen"
#else

    #include <signal.h>  // needed for SIGINT, SIGTERM, SIGHUP

#endif


//=//// USER-INTERRUPT/HALT HANDLING (Ctrl-C, Escape, etc.) ///////////////=//
//
// There's clearly contention for what a user-interrupt key sequence should
// be, given that "Ctrl-C" is copy in GUI applications.  Yet handling escape
// is not necessarily possible on all platforms and situations.
//
// For console applications, we assume that the program starts with user
// interrupting enabled by default...so we have to ask for it not to be when
// it would be bad to have the Revolt stack interrupted--during startup, or
// when in the "kernel" of the host console.
//
// (Note: If halting is done via Ctrl-C, technically it may be set to be
// ignored by a parent process or context, in which case conventional wisdom
// is that we should not be enabling it ourselves.  Review.)
//

bool halting_enabled = false;

#if defined(TO_WINDOWS)  //=//// WINDOWS ////////////////////////////////=//

// Windows handling is fairly simplistic--this is the callback passed to
// `SetConsoleCtrlHandler()`.  The most annoying thing about cancellation in
// windows is the limited signaling possible in the terminal's readline.
//
BOOL WINAPI Handle_Break(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
      case CTRL_C_EVENT:
      case CTRL_BREAK_EVENT:
        rebHalt();
        return TRUE;  // TRUE = "we handled it"

      case CTRL_CLOSE_EVENT:
        //
        // !!! Theoretically the close event could confirm that the user
        // wants to exit, if there is possible unsaved state.  As a UI
        // premise this is probably less good than persisting the state
        // and bringing it back.
        //
      case CTRL_LOGOFF_EVENT:
      case CTRL_SHUTDOWN_EVENT:
        //
        // They pushed the close button, did a shutdown, etc.  Exit.
        //
        // !!! Review arbitrary "100" exit code here.
        //
        exit(100);

      default:
        return FALSE;  // FALSE = "we didn't handle it"
    }
}

BOOL WINAPI Handle_Nothing(DWORD dwCtrlType)
{
    if (dwCtrlType == CTRL_C_EVENT)
        return TRUE;

    return FALSE;
}

void Disable_Halting(void)
{
    assert(halting_enabled);

    SetConsoleCtrlHandler(Handle_Break, FALSE);
    SetConsoleCtrlHandler(Handle_Nothing, TRUE);

    halting_enabled = false;
}

void Enable_Halting(void)
{
    assert(not halting_enabled);

    SetConsoleCtrlHandler(Handle_Break, TRUE);
    SetConsoleCtrlHandler(Handle_Nothing, FALSE);

    halting_enabled = true;
}

#else  //=//// POSIX, LINUX, MAC, etc. ////////////////////////////////////=//

// SIGINT is the interrupt usually tied to "Ctrl-C".  Note that if you use
// just `signal(SIGINT, Handle_Signal);` as R3-Alpha did, this means that
// blocking read() calls will not be interrupted with EINTR.  One needs to
// use sigaction() if available...it's a slightly newer API.
//
// http://250bpm.com/blog:12
//
// !!! What should be done about SIGTERM ("polite request to end", default
// unix kill) or SIGHUP ("user's terminal disconnected")?  Is it useful to
// register anything for these?  R3-Alpha did, and did the same thing as
// SIGINT.  Not clear why.  It did nothing for SIGQUIT:
//
// SIGQUIT is used to terminate a program in a way that is designed to
// debug it, e.g. a core dump.  Receiving SIGQUIT is a case where
// program exit functions like deletion of temporary files may be
// skipped to provide more state to analyze in a debugging scenario.
//
// SIGKILL is the impolite signal for shutdown; cannot be hooked/blocked

static void Handle_Signal(int sig)
{
    UNUSED(sig);
    rebHalt();
}

struct sigaction old_action;

void Disable_Halting(void)
{
    assert(halting_enabled);

    sigaction(SIGINT, nullptr, &old_action); // fetch current handler
    if (old_action.sa_handler != SIG_IGN) {
        struct sigaction new_action;
        new_action.sa_handler = SIG_IGN;
        sigemptyset(&new_action.sa_mask);
        new_action.sa_flags = 0;
        sigaction(SIGINT, &new_action, nullptr);
    }

    halting_enabled = false;
}

void Enable_Halting(void)
{
    assert(not halting_enabled);

    if (old_action.sa_handler != SIG_IGN) {
        struct sigaction new_action;
        new_action.sa_handler = &Handle_Signal;
        sigemptyset(&new_action.sa_mask);
        new_action.sa_flags = 0;
        sigaction(SIGINT, &new_action, nullptr);
    }

    halting_enabled = true;
}

#endif  //=///////////////////////////////////////////////////////////////=//



//=//// MAIN ENTRY POINT //////////////////////////////////////////////////=//
//
// Using a main() entry point for a console program (as opposed to WinMain())
// so we can connect to the console.  See Determine_Hinstance_May_Respawn().
//
int main(int argc, char *argv_ansi[])
{
    // Note: By default, Ctrl-C is not hooked or handled.  This is done by
    // the console extension (%extensions/console).  Halting should not be
    // possible while the mezzanine is loading.

    rebStartup();

    // With interpreter startup done, we want to turn the platform-dependent
    // argument strings into a block of Rebol strings as soon as possible.
    // That way the command line argument processing can be taken care of by
    // PARSE in the MAIN-STARTUP user function, instead of C code!
    //
    REBVAL *argv_block = rebValue("copy []", rebEND);

  #ifdef TO_WINDOWS
    //
    // Were we using WinMain we'd be getting our arguments in Unicode, but
    // since we're using an ordinary main() we do not.  However, this call
    // lets us slip out and pick up the arguments in Unicode form (UTF-16).
    //
    WCHAR **argv_ucs2 = CommandLineToArgvW(GetCommandLineW(), &argc);
    UNUSED(argv_ansi);

    Determine_Hinstance_May_Respawn(argc > 1 ? nullptr : argv_ucs2[0]);

    int i;
    for (i = 0; i != argc; ++i) {
        if (argv_ucs2[i] == nullptr)
            continue;  // !!! R3-Alpha commented here saying "shell bug" (?)

        // Note: rebTextWide() currently only supports UCS-2, so codepoints
        // needing more than two bytes to be represented will cause a failure.
        //
        rebElide(
            "append", argv_block, rebR(rebTextWide(argv_ucs2[i])),
        rebEND);
    }
  #else
    // Just take the ANSI C "char*" args...which should ideally be in UTF-8.
    //
    int i = 0;
    for (; i != argc; ++i) {
        if (argv_ansi[i] == nullptr)
            continue;  // !!! R3-Alpha commented here saying "shell bug" (?)

        rebElide("append", argv_block, rebT(argv_ansi[i]), rebEND);
    }
  #endif

    // Unzip the Gzip'd compressed startup code (embedded as bytes in a C
    // global variable) to make a BINARY!.  GUNZIP accepts a HANDLE! as input,
    // so pass it in here.
    //
    REBVAL *startup_bin = rebValue(
        "gunzip", rebR(rebHandle(
            m_cast(unsigned char*, &Main_Startup_Code[0]),
            MAIN_STARTUP_SIZE,
            nullptr
        )),
    rebEND);

    // !!! The startup code isn't really set up to run as a Module, though it
    // probably should be.  This is a carry-over of what some %sys-core.h
    // code was doing...adding top-level set-words to the lib context, while
    // binding everything to lib.  What you avoid by running this in the
    // user context is getting an importation of every word mentioned in the
    // host startup file...that's a lot of words, like CONSOLE.  And once the
    // word is imported from lib as NULL, if it gets added later (e.g. by
    // loading extensions) it won't update:
    //
    // https://forum.rebol.info/t/764
    //
    // R3-Alpha didn't resolve these issues, so for now the code is mostly
    // just following what it did before...yet pushing more and more of it
    // out through a user-friendly API.  But fundamental work is needed.
    //
    REBVAL *main_startup = rebValue(
        "use [code] [",
            "code: transcode", rebR(startup_bin),
            "bind/only/set code lib",  // only ADD top level set-word!s to lib
            "bind code lib",  // but BIND to anything else that exists in lib
            "do code",
        "]",
    rebEND);

    if (rebNot("action?", rebQ(main_startup), rebEND))
        rebJumps("PANIC-VALUE", rebQ(main_startup), rebEND);  // terminates

    Enable_Halting();

    // This runs the MAIN-STARTUP, which returns *requests* to execute
    // arbitrary code by way of its return results.  The ENTRAP is thus here
    // to intercept bugs *in MAIN-STARTUP itself*.
    //
    REBVAL *trapped = rebValue(
        "entrap [",  // MAIN-STARTUP action! takes one argument (argv[])
            main_startup, rebR(argv_block),
        "]",
    rebEND);
    rebRelease(main_startup);

    if (rebDid("error?", trapped, rebEND))  // error in MAIN-STARTUP itself
        rebJumps("PANIC", trapped, rebEND);  // terminates

    bool run_console = rebDid("first", trapped, rebEND);  // entrap's output
    rebRelease(trapped);  // don't need the outer block any more

    int exit_status;
    if (run_console) {
        REBVAL *result = rebValue("console", rebEND);
        exit_status = rebUnboxInteger(rebR(result), rebEND);
    }
    else
        exit_status = 0;

    const bool clean = false;  // process exiting, not necessary
    rebShutdown(clean);  // Note: debug build runs a clean shutdown anyway

    Disable_Halting();

    return exit_status;  // http://stackoverflow.com/q/1101957/
}

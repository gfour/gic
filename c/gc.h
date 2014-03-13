/** @file gc.h

  GC macros.

 */

#define UNW_LOCAL_ONLY
#define CONFIG_MSABI_SUPPORT
#include <libunwind.h>

/** Enables internal consistency check (used for debugging). */
#define DEBUG_GC 0
// Print (lots of) diagnostic messages
#define VERBOSE_GC 0
/** Print statistics. */
#define GC_STATS 0

/** GC assertion. */
#if DEBUG_GC
#include <assert.h>
#define ASSERT_GC(c, m)      assert((c) && (m))
#else
#define ASSERT_GC(n, m)      do { } while(0)
#endif
/** GC assertion (used as an expression). */
#define ASSERT_GC_EXPR(c, m) ({ ASSERT_GC(c, m); })

/** Check if a pointer belongs to space 1. */
#define ISSPACE1(x) ((x >= space1) && (x <= space1 + MAXMEMSPACE))
/** Check if a pointer belongs to space 2. */
#define ISSPACE2(x) ((x >= space2) && (x <= space2 + MAXMEMSPACE))

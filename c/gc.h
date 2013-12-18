/*
  GC macros.
*/

// Enables internal consistency check (used for debugging)
#define DEBUG_GC 0
// Print (lots of) diagnostic messages
#define VERBOSE_GC 0
// Print statistics
#define GC_STATS 1

// GC assertions
#if DEBUG_GC
#include <assert.h>
#define ASSERT_GC(c, m)      assert((c) && (m))
#else
#define ASSERT_GC(n, m)      do { } while(0)
#endif
#define ASSERT_GC_EXPR(c, m) ({ ASSERT_GC(c, m); })

// The size of the header prepended to every allocated memory area.
#define MEM_HEADER_SZ (sizeof(size_t))

// Check if a pointer belongs to one of the two spaces
#define ISSPACE1(x) ((x >= space1) && (x <= space1 + MAXMEMSPACE))
#define ISSPACE2(x) ((x >= space2) && (x <= space2 + MAXMEMSPACE))

#define MAGIC     ((unsigned long) 0xbad0bad1bad6bad7)
#define FORWARDED ((unsigned long) 0xbad0bad1bad6bad5)

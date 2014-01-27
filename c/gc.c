/*
 **********************************************************
 *  A copying semispace garbage collector.
 **********************************************************
 */

#include "c/gc.h"

#ifdef GC

// Function prototypes
static inline int MM_valid (TP_ t);
static void MM_gc ();
static void MM_process_stack ();
static TP_ MM_forward (TP_ lar);
static void MM_scan (TP_ lar);
static void MM_compare_heaps (byte* old_space);
static void MM_compare (TP_ lar);


#if GC_STATS
// Statistics: count the copied LARs & processed LAR pointers in the stack
static unsigned long bytes_copied, lar_count, st_count;
#endif

// Pointers to the memory spaces
static byte *from_space, *to_space;

// Checks if the given ptr could point to a valid LAR
static inline int MM_valid (TP_ t)
{
  return (byte*) t >= spaceStart
      && (byte*) t <= spaceEnd
      && (t->magic == MAGIC || t->magic == FORWARDED);
}

// Garbage collection entry point.
static void MM_gc ()
{
#if GC_STATS
  clock_t t1, t2;
  t1 = clock();
  bytes_copied = 0;
  lar_count = 0;
  st_count = 0;
#endif /* GC_STATS */

#if VERBOSE_GC
  printf("----------- Garbage Collection --------------\n");
#endif /* VERBOSE_GC */

  // Set up the from-space and to-space
  if (ISSPACE1(space))      { from_space = space1; to_space = space2; }
  else if (ISSPACE2(space)) { from_space = space2; to_space = space1; }
  else ASSERT_GC(0, "space does not belong to space1/space2");
#if DEBUG_GC
  byte* old_space = space;
#endif
  space = to_space;

  // Process the root set
  MM_process_stack();

  // Scan the to-space and forward any remaining pointers
  byte* scan = to_space;
  while (scan < space) {
    TP_ lar = (TP_) (scan + MEM_HEADER_SZ);
    MM_scan(lar);
    size_t sz = *((size_t*) scan);
    scan += sz;
  }
  ASSERT_GC(scan == space, "out of bounds when scanning");
  ASSERT_GC((long) (space-to_space) == (long) bytes_copied,
            "bytes copied by GC do not match the to_space used");

  // TODO Stack: unmark all LARs (when stack allocation is done)*/

#if DEBUG_GC
  MM_compare_heaps(old_space);
  printf("Clearing from-space...\n");
  memset(from_space, 0xbf, MAXMEMSPACE);
#endif

  // Reverse the two spaces
  spaceStart = to_space;
  spaceEnd = to_space + MAXMEMSPACE;

#if GC_STATS
  t2 = clock();
  printf("bytes copied (including mem headers): %lu/%lu (%f%%),\n"
         "LAR=%lu, stack roots=%lu, used=%lu, left=%lu,\n"
         "spaceStart=%p, space=%p, spaceEnd=%p\n",
         bytes_copied, MAXMEMSPACE,
         100*((float)bytes_copied/(float)MAXMEMSPACE),
         lar_count, st_count, (unsigned long) (space-spaceStart),
         (unsigned long) (spaceEnd-space),
         spaceStart, space, spaceEnd);
#if VERBOSE_GC
  printf("GC done, time = %.10f sec\n", ((double)(t2 - t1)/CLOCKS_PER_SEC));
#endif
#endif /* GC_STATS */
}

// Traverse the stack and forward everything that looks like a LAR pointer.
// If this is not exact, the program will crash.
static void MM_process_stack ()
{
  TP_* ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = *((TP_ *) ptr);
    if (MM_valid(lar)) {
#if VERBOSE_GC
      printf("rootset: ");
#endif
      *((TP_ *)ptr) = MM_forward(lar);
#if GC_STATS
      st_count++;
#endif
    }
  }
}

// Forwards a LAR pointer, possibly copying the LAR to the to-space.
static TP_ MM_forward (TP_ lar)
{
  ASSERT_GC(lar != NULL, "forwarding null");
#if VERBOSE_GC
  printf ("forwarding T0=%p, ", lar);
#endif
  // if already forwarded, just return it
  if (lar->magic == FORWARDED) {
#if VERBOSE_GC
    printf("already to %p\n", lar->prev);
#endif
    return lar->prev;
  }
  ASSERT_GC(lar->magic == MAGIC, "forwarding an invalid LAR");
  // copy this LAR
  size_t sz = *((size_t*) ((byte*) lar - MEM_HEADER_SZ));
  memcpy(space, (byte*) lar - MEM_HEADER_SZ, sz);
  lar->magic = FORWARDED;
  lar->prev = (TP_) (space + MEM_HEADER_SZ);
  space += sz;
#if GC_STATS
  lar_count++;
  bytes_copied += sz;
#endif
#if VERBOSE_GC
  printf("copied to %p\n", lar->prev);
#endif
  return lar->prev;
}

// Scans a LAR and rewrites its contexts using forwarding pointers
static void MM_scan (TP_ lar)
{
#if VERBOSE_GC
  printf("scanning T0=%p\n", lar);
#endif
  ASSERT_GC(lar->magic == MAGIC, "scanning an invalid LAR");
  if (MM_valid(lar->prev))
    lar->prev = MM_forward(lar->prev);
  int n;
  for (n=0; n<lar->arity; n++)
    if (ARGS(n, lar) == NULL && MM_valid(VALS(n, lar).ctxt))
      VALS(n, lar).ctxt = MM_forward(VALS(n, lar).ctxt);
  for (n=0; n<lar->nesting; n++)
    if (MM_valid(NESTED(n, lar)))
      NESTED(n, lar) = MM_forward(NESTED(n, lar));
}

// Compare the from-space to the to-space
static void MM_compare_heaps (byte* old_space)
{
  // Scan the to-space and forward any remaining pointers
  byte* scan = from_space;
  while (scan < old_space) {
    TP_ lar = (TP_) (scan + MEM_HEADER_SZ);
    MM_compare(lar);
    size_t sz = *((size_t*) scan);
    scan += sz;
  }
  ASSERT_GC(scan == old_space, "out of bounds when comparing");
}

// Compares a LAR with its copy

inline static int MM_ptr_eq (TP_ x, TP_ y)
{
  return x == y || (x->magic == FORWARDED && x->prev == y);
}

static void MM_compare (TP_ lar)
{
  if (lar->magic == MAGIC)
    return;
  ASSERT_GC(lar->magic == FORWARDED,
            "comparing an invalid LAR");
  TP_ copy = lar->prev;
  ASSERT_GC(copy->magic == MAGIC,
            "invalid forwarding pointer");
  ASSERT_GC(*((size_t*) lar) = *((size_t*) copy),
            "mismatch in sizes");
  ASSERT_GC(lar->arity == copy->arity,
            "mismatch in arity");
  ASSERT_GC(lar->nesting == copy->nesting,
            "mismatch in nesting");
  ASSERT_GC(lar->arity == copy->arity,
            "mismatch in arity");
  int n;
  for (n=0; n<lar->arity; n++)
    ASSERT_GC(ARGS(n, lar) == ARGS(n, copy),
              "mismatch in ARGS");
  for (n=0; n<lar->arity; n++) {
    ASSERT_GC(VALS(n, lar).constr == VALS(n, copy).constr,
              "mismatch in VALS constr");
    ASSERT_GC(MM_ptr_eq(VALS(n, lar).ctxt, VALS(n, copy).ctxt),
              "mismatch in VALS context");
  }
  for (n=0; n<lar->nesting; n++)
    ASSERT_GC(MM_ptr_eq(NESTED(n, lar), NESTED(n, copy)),
              "mismatch in NESTED");
}

#endif /* GC */


// Custom memory allocation routine

#define ALIGN(x) (((x) + (sizeof(int) - 1)) & ~(sizeof(int) - 1))

inline byte* MM_alloc (size_t bytes)
{
  // Properly align the size in bytes
#ifdef GC
  bytes = ALIGN(bytes + MEM_HEADER_SZ);
#else
  bytes = ALIGN(bytes);
#endif

  // If no space left
  if (space + bytes > spaceEnd) {
#ifndef GC
    // too bad...
    fprintf(stderr,
            "Heap overflow: more than %lu bytes needed. "
            "Use more memory with -mem, or turn on GC with -libgc or -semigc when compiling.\n",
            (unsigned long) MAXMEM);
    exit(1);
#else
    // or garbage collect
    MM_gc();
    if (space + bytes > spaceEnd) {
      fprintf(stderr,
              "Heap overflow: more than %lu bytes needed, "
              "even after GC. Use more memory with -mem when compiling.\n",
              (unsigned long) MAXMEM);
      exit(1);
    }
#endif
  }

#ifdef GC
  // Store the bytes count before the actual memory chunk
  *((size_t*) space) = bytes;
  byte* ret = space + MEM_HEADER_SZ;
#else
  byte* ret = space;
#endif
  space += bytes;
  return ret;
}

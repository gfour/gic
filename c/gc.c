/** @file gc.c

 A copying semispace garbage collector.

 Used by the compact LAR representation (lar_compact.h).

*/

#include "c/gc.h"

#ifdef GC

// Function prototypes
static inline int MM_heap_ptr (TP_ t);
static void MM_gc ();
static void MM_process_stack ();
static TP_ MM_forward (TP_ lar);
static void MM_scan (TP_ lar);
static void MM_compare_heaps (byte* old_space);
static void MM_compare (TP_ lar);

// The minimum amount of space that should be left after garbage collection.
#define GC_HEADROOM 1000000

#if GC_STATS
// Statistics: count the copied LARs & processed LAR pointers in the stack
static unsigned long bytes_copied, lar_count, st_count;
#endif

// Pointers to the memory spaces
static byte *from_space, *to_space;

// Temporary function, TODO: delete it.
void TODO(char *s) {
  printf("TODO in GC: %s\n", s);
  exit(-1);
}

/** Checks if the given ptr points to a LAR allocated on the heap.
    \param t The address of the LAR to check.
    \return 1 if the LAR is on the heap, 0 otherwise.
*/
static inline int MM_heap_ptr (TP_ t)
{
  // printf("MM_heap_ptr checks %p, spaceStart=%p, spaceEnd=%p\n", t, spaceStart, spaceEnd);
  return (byte*) t >= spaceStart
      && (byte*) t <= spaceEnd;
}

/** Garbage collection entry point. */
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
    TP_ lar = (TP_)scan;
    size_t sz = AR_SIZE(lar);
    MM_scan(lar);
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

/** Traverse the shadow stack and forward everything that looks like a pointer
    to a heap-allocated LAR. If this is not exact, the program will crash. */
static void MM_process_stack ()
{
  printf("Scanning the shadow stack (%p ... %p)\n", sstack_bottom, sstack_ptr);
  
  TP_* ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = *((TP_ *) ptr);

#if VERBOSE_GC
    printf("rootset: ");
#endif /* VERBOSE_GC */
    if (MM_heap_ptr(lar)) {
      *((TP_ *)ptr) = MM_forward(lar);
#if GC_STATS
    st_count++;
#endif
    }
  }
}

/** Forwards a LAR pointer, copying the LAR to the to-space.
    \param lar The LAR pointer that must be forwarded.
    \return The new address of the LAR in the to-space.
 */
static TP_ MM_forward (TP_ lar)
{
  ASSERT_GC(lar != NULL, "forwarding null");
#if VERBOSE_GC
  // printf ("forwarding T0=%p, ", lar);
#endif
  // if already forwarded, just return it
  if (IS_FORWARDED(lar)) {
    // We have stored the forwarded pointer in .prev
    TP_ fw_lar = CPTR(lar->prev);
#if VERBOSE_GC
    printf("already to %p\n", fw_lar);
#endif
    return fw_lar;
  }

  // copy this LAR
  char lar_a = AR_a(lar->prev);
  char lar_n = AR_n(lar->prev);
  // printf("found LAR %p with arity %d and nesting %d\n", lar, lar_a, lar_n);

  // size_t sz = 1 + lar_a + lar_n;
  size_t sz = AR_SIZE(lar);
  TP_ fw_lar = (TP_)memcpy(space, lar, sz);
  space += sz;
  // printf("Copied the LAR (%ld bytes) to %p\n", sz, fw_lar);

  lar->prev = ARINFO(lar_a, lar_n, FORWARDED_ADDR(fw_lar));

#if GC_STATS
  lar_count++;
  bytes_copied += sz;
#endif
#if VERBOSE_GC
  printf("copied to %p\n", lar->prev);
#endif
  return fw_lar;
}

/** Scans a LAR and rewrites its contexts using forwarding pointers
    \param lar The LAR to scan.
*/
static void MM_scan (TP_ lar) {
#if VERBOSE_GC
  printf("scanning T0=%p\n", lar);
#endif
  char lar_a = ARITY(lar);
  char lar_n = NESTING(lar);
  TP_ lar_prev = CPTR(lar->prev);
  // TODO: can this happen in a single byte-update?
  if (MM_heap_ptr(lar_prev))
    lar->prev = ARINFO(lar_a, lar_n, FORWARDED_ADDR(MM_forward(lar_prev)));

  int n;
  for (n=0; n<lar_a; n++) {
    Susp val = VALS(n, lar);
    if (ARGS_FLAG(n, lar) == NULL && IS_THUNK(val.ctxt) && MM_heap_ptr(val.ctxt))
      TODO("val.ctxt = MM_forward(VALS(n, lar).ctxt);");
  }
  for (n=0; n<lar_n; n++) {
#ifdef LAR_COMPACT
    if (MM_heap_ptr(NESTED(n, lar_a, lar))) {
#else
    if (MM_heap_ptr(NESTED(n, lar))) {
#endif /* LAR_COMPACT */
	TODO("NESTED(n, lar) = MM_forward(NESTED(n, lar));");
    }
  }
}

/** Compare the from-space to the to-space.
    \param old_space The old space.
 */
static void MM_compare_heaps (byte* old_space) {
  // Scan the to-space and forward any remaining pointers.
  byte* scan = from_space;
  while (scan < old_space) {
    TP_ lar = (TP_)scan;
    size_t sz = AR_SIZE(lar);
    MM_compare(lar);
    scan += sz;
  }
  ASSERT_GC(scan == old_space, "out of bounds when comparing");
}

// Compares a LAR with its copy

inline static int MM_ptr_eq (TP_ x, TP_ y)
{
  TODO("MM_ptr_eq");
  // return x == y || (x->magic == FORWARDED && x->prev == y);
  return 0;
}

/** Checks if a LAR in the from-space has been forwarded correctly.
    \param lar The from-space LAR.
*/
static void MM_compare (TP_ lar)
{
  ASSERT_GC(IS_FORWARDED(lar),
            "comparing an invalid LAR");
  TP_ copy = CPTR(lar->prev);
  ASSERT_GC(IS_FORWARDED(copy),
            "invalid forwarding pointer");
  ASSERT_GC(AR_SIZE(lar) == AR_SIZE(copy),
            "mismatch in sizes");
  ASSERT_GC(ARITY(lar) == ARITY(copy),
            "mismatch in arity");
  ASSERT_GC(NESTING(lar) == NESTING(copy),
            "mismatch in nesting");
/* These checks are disabled.
  int n;
  for (n=0; n<ARITY(lar); n++)
    ASSERT_GC(ARGS(n, lar) == ARGS(n, copy),
              "mismatch in ARGS");
  for (n=0; n<ARITY(lar); n++) {
    ASSERT_GC(VALS(n, lar).constr == VALS(n, copy).constr,
              "mismatch in VALS constr");
    ASSERT_GC(MM_ptr_eq(VALS(n, lar).ctxt, VALS(n, copy).ctxt),
              "mismatch in VALS context");
  }
  for (n=0; n<NESTING(lar); n++)
    ASSERT_GC(MM_ptr_eq(NESTED(n, lar), NESTED(n, copy)),
              "mismatch in NESTED");
  */
}

#endif /* GC */


/** Proper alignment of the bytes to be allocated. */
#define ALIGN(x) (((x) + (sizeof(int) - 1)) & ~(sizeof(int) - 1))

/** Custom memory allocation routine. If there is no more space, it
    triggers the semi-space garbage collector.
    \param bytes The number of bytes to allocate.
    \return The address of the newly allocated memory.
 */
inline byte* MM_alloc (size_t bytes)
{
  bytes = ALIGN(bytes);

  // If no space left
  if (space + bytes > spaceEnd) {
#ifdef OMP
    fprintf(stderr, "The OpenMP runtime is not supported by the semi-space "
                    " garbage collector.\n");
    exit(1);
#endif /* OMP */
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
    if (space + bytes > spaceEnd - GC_HEADROOM) {
      fprintf(stderr,
              "Heap overflow: more than %lu bytes needed, "
              "less than %d bytes free after GC. "
	      "Use more memory with -mem when compiling.\n",
              (unsigned long) MAXMEM, GC_HEADROOM);
      exit(1);
    }
#endif /* GC */
  }

  byte* ret = space;
  space += bytes;
  return ret;
}

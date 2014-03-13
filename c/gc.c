/** @file gc.c

 A copying semispace garbage collector.

 Used by the compact LAR representation (lar_compact.h).

*/

#include "c/gc.h"

#ifdef GC

// Function prototypes
static inline int MM_heap_ptr(TP_ t);
static void MM_gc(void);
static void MM_process_stack(void);
static TP_ MM_forward(TP_ lar);
static void MM_scan(TP_ lar);
static void MM_compare_heaps(byte* old_space);
static void MM_compare(TP_ lar);

// The minimum amount of space that should be left after garbage collection.
#define GC_HEADROOM 1000

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

/** Checks if the given pointer points to a LAR allocated on the heap.
    \param t The address of the LAR to check.
    \return 1 if the LAR is on the heap, 0 otherwise.
*/
static inline int MM_heap_ptr(TP_ t) {
  // printf("MM_heap_ptr checks %p, spaceStart=%p, spaceEnd=%p\n", t, spaceStart, spaceEnd);
  return (byte*)t >= spaceStart && (byte*)t <= spaceEnd;
}

/** Garbage collection entry point. */
static void MM_gc(void) {
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
#endif /* DEBUG_GC */
  space = to_space;

  // Process the root set.
  MM_process_stack();

  // Scan the to-space and forward any remaining pointers.
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

#if DEBUG_GC
  MM_compare_heaps(old_space);
  printf("Clearing from-space...\n");
  memset(from_space, 0xbf, MAXMEMSPACE);
#endif /* DEBUG_GC */

  // Reverse the two spaces
  spaceStart = to_space;
  spaceEnd = to_space + MAXMEMSPACE;

#if GC_STATS
  t2 = clock();
  printf("bytes copied: %lu/%lu (%f%%),\n"
         "forwarded LARs=%lu, stack roots=%lu, used=%lu bytes, left=%lu bytes,\n"
         "spaceStart=%p, space=%p, spaceEnd=%p\n",
         bytes_copied, MAXMEMSPACE, 100*((float)bytes_copied/(float)MAXMEMSPACE),
         lar_count, st_count, (unsigned long) (space-spaceStart),
         (unsigned long) (spaceEnd-space),
         spaceStart, space, spaceEnd);
#if VERBOSE_GC
  printf("GC done, time = %.10f sec\n", ((double)(t2 - t1)/CLOCKS_PER_SEC));
#endif
#endif /* GC_STATS */
}

/** Traverse the pointer stack and forward everything that looks like a pointer
    to a heap-allocated LAR. If this is not exact, the program will crash. */
static void MM_process_stack(void) {
  printf("Scanning the pointer stack (%p ... %p)\n", sstack_bottom, sstack_ptr);
  
  TP_* ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = *((TP_ *) ptr);

#if VERBOSE_GC
    //printf("rootset: ");
#endif /* VERBOSE_GC */
    if (MM_heap_ptr(lar)) {
      *((TP_ *)ptr) = MM_forward(lar);
#if GC_STATS
    st_count++;
#endif /* GC_STATS */
    }
  }
}

/** Forwards a LAR pointer, copying the LAR to the to-space.
    \param lar The LAR pointer that must be forwarded.
    \return The new address of the LAR in the to-space.
*/
static TP_ MM_forward(TP_ lar) {
  ASSERT_GC(lar != NULL, "forwarding null");
#if VERBOSE_GC
  // printf ("forwarding T0=%p, ", lar);
#endif
  // if already forwarded, just return it
  if (IS_FORWARDED(lar)) {
    // We have stored the forwarded pointer in .prev
    TP_ fw_lar = FORWARDED_ADDR(lar);
#if VERBOSE_GC
    printf("already to %p\n", fw_lar);
#endif /* VERBOSE_GC */
    return fw_lar;
  }

  // Calculate LAR layout to forward it.
  char lar_a = AR_a(lar->prev);
  char lar_n = AR_n(lar->prev);
  size_t sz = AR_SIZE(lar);
#if VERBOSE_GC
  printf("found LAR %p with arity %d and nesting %d, size=%ld bytes\n", 
	 lar, lar_a, lar_n, sz);
#endif /* VERBOSE_GC */

  int j;
  // poor man's memcpy
  if (space+sz<spaceEnd) {
    unsigned char *lar0 = (unsigned char*)lar;
    for (j=0; j<sz; j++)
      space[j] = lar0[j];
  }
  else {
    printf("Out of space.\n");
    exit(-1);
  }
  TP_ fw_lar = (TP_)space;
  // TODO: use memcpy
  // TP_ fw_lar = (TP_)memcpy(space, lar, sz);

#if DEBUG_GC
  if (ARITY(lar)   != ARITY(fw_lar))   printf("fw: arity mismatch");
  if (NESTING(lar) != NESTING(fw_lar)) printf("fw: nesting mismatch");
#endif /* DEBUG_GC */

  space += sz;
  // printf("Copied the LAR (%2ld bytes) to %p\n", sz, fw_lar);

  lar->prev = FW_ARINFO(lar_a, lar_n, fw_lar);

#if GC_STATS
  lar_count++;
  bytes_copied += sz;
#endif
#if VERBOSE_GC
  printf("copied to %p\n", FORWARDED_ADDR(lar));
#endif
  return fw_lar;
}

/** Scans a LAR and rewrites its contexts using forwarding pointers
    \param lar The LAR to scan.
*/
static void MM_scan(TP_ lar) {
#if VERBOSE_GC
  printf("scanning T0=%p\n", lar);
#endif
  char lar_a    = ARITY(lar);
  char lar_n    = NESTING(lar);
  TP_  lar_prev = AR_prev(lar);

  if (MM_heap_ptr(lar_prev))
    lar->prev = ARINFO(lar_a, lar_n, MM_forward(lar_prev));

  // Forward LAR pointers inside evaluated lazy constructors.
  int n;
  for (n=0; n<lar_a; n++) {
    Susp val = VALS(n, lar);
    if (IS_CONSTR(val)) {
      TP_ cptr = CPTR(val);
      if (MM_heap_ptr(cptr)) {
	int constrId = CONSTR(val);
	TP_ fwCtxt   = MM_forward(cptr);
	// VALS(n, lar) = THUNK(constrId, fwCtxt);
	VALS(n, lar) = THUNK(constrId, fwCtxt);
      }
      else if (cptr!=0) { printf("stack constructor: %p\n", cptr); exit(-1); }
    }
  }
  // Forward LAR pointers inside nested fields.
  for (n=0; n<lar_n; n++) {
#ifdef LAR_COMPACT
    TP_ nested = NESTED(n, lar_a, lar);
#else
    TP_ nested = NESTED(n, lar);
#endif /* LAR_COMPACT */
    if (MM_heap_ptr(nested)) {
      TP_ fwCtxt = MM_forward(nested);
#ifdef LAR_COMPACT
      NESTED(n, lar_a, lar) = fwCtxt;
#else
      TODO("NESTED in lar.h mode");
#endif /* LAR_COMPACT */
    }
  }
}

/** Compare the from-space to the to-space.
    \param old_space The old space.
 */
static void MM_compare_heaps(byte* old_space) {
  // Scan the from-space and compare forwarded LARs.
  byte* scan = from_space;
  while (scan < old_space) {
    TP_ lar = (TP_)scan;
    size_t sz = AR_SIZE(lar);
    MM_compare(lar);
    scan += sz;
  }
  ASSERT_GC(scan == old_space, "out of bounds when comparing");
}

static void MM_print_Susp(Susp s) {
  if (IS_VAL(s)) {
    if (IS_PVAL(s)) {
      printf("val(int=%ld)", PVAL_R(s));
    }
    else if (IS_CONSTR(s)) {
      TP_ c = CPTR(s);
      printf("val(constructor={%d, %p(fw=%d, from-heap=%d)})", 
	     CONSTR(s), c, IS_FORWARDED(c), MM_heap_ptr(c));
      if (IS_FORWARDED(c)) printf("(=>%p)", FORWARDED_ADDR(c));
    }
    else printf("Unknown Susp found.");
  } else {
    printf("code{%p}", (LarArg)CODE(s));
  }
}

/** Compares two thunks.
    \param  A thunk in from-space.
    \param  A thunk in to-space.
    \return If the thunks are the same, or if they are lazy constructors with
    the pointer field of the second be the forwarded pointer field of the first,
    returns 1. Otherwise, returns 0.
*/  
static int MM_compare_vals(Susp s_from, Susp s_to) {
  if (s_from==s_to) return 1;
  if (IS_CONSTR(s_from) && IS_CONSTR(s_to))
    return ((CONSTR(s_from)==CONSTR(s_to)) && 
	    (FORWARDED_ADDR(CPTR(s_from))==CPTR(s_to)));
  return 0;
}

/** Checks if a LAR in the from-space has been forwarded correctly.
    \param lar The from-space LAR.
*/
static void MM_compare(TP_ lar) {
  // printf("MM_compare(%p): ", lar);
  if (IS_FORWARDED(lar)) {
    // printf("LAR is forwarded to %p.\n", FORWARDED_ADDR(lar));
  }
  else {
    // printf("LAR is not forwarded.\n");
    return;
  }
  
  TP_ copy = FORWARDED_ADDR(lar);

  unsigned char lar_a  = AR_a(lar->prev);
  unsigned char lar_n  = AR_n(lar->prev);
  unsigned char copy_a = AR_a(copy->prev);
  unsigned char copy_n = AR_n(copy->prev);

  if (lar_a != copy_a) {
    printf("GC: arity mismatch between %p and %p: %d != %d\n",
	   lar, copy, lar_a, copy_a);
    exit(-1);
  }
  if (lar_n != copy_n) {
    printf("GC: nesting mismatch between %p and %p: %d != %d\n",
	   lar, copy, lar_n, copy_n);
    exit(-1);
  }
  if (AR_SIZE(lar) != AR_SIZE(copy)) {
    printf("GC: size mismatch between %p and %p: %ld != %ld\n",
	   lar, copy, AR_SIZE(lar), AR_SIZE(copy));
    exit(-1);
  }

  int n;
#ifndef LAR_COMPACT
  // TODO: write this for lar.h thunks
  for (n=0; n<lar_a; n++) {
    LarArg lar_arg  = (LarArg)(ARGS(n, lar ));
    LarArg copy_arg = (LarArg)(ARGS(n, copy));
    if (lar_arg != copy_arg) {
      printf("GC: arg[%d] mismatch between %p and %p: %p != %p, details:\n",
	     n, lar, copy, lar_arg, copy_arg);
      MM_print_Susp((Susp)lar_arg) ; printf(" != ");
      MM_print_Susp((Susp)copy_arg); printf("\n");
      exit(-1);
    }
  }
#endif
  for (n=0; n<lar_a; n++) {
    Susp lar_s  = VALS(n, lar );
    Susp copy_s = VALS(n, copy);
    // TODO: compare the structs for lar.h
    if (IS_VAL(lar_s) && IS_VAL(copy_s) && (!MM_compare_vals(lar_s, copy_s))) {
      printf("GC: val[%d] mismatch between %p and %p: %lx != %lx, details:\n",
	     n, lar, copy, lar_s, copy_s);
      MM_print_Susp((Susp)lar_s) ; printf(" != ");
      MM_print_Susp((Susp)copy_s); printf("\n");
      exit(-1);
    }
  }
  for (n=0; n<lar_n; n++) {
    TP_ lar_nested  = NESTED(n, lar_a , lar);
    TP_ copy_nested = NESTED(n, copy_a, copy);
    if (lar_nested != copy_nested) {
      printf("GC: nested[%d] mismatch between %p and %p: %p != %p, details:\n",
	     n, lar, copy, lar_nested, copy_nested);
      exit(-1);
    }
  }

  // printf("OK.\n");
}

#endif /* GC */


/** Proper alignment of the bytes to be allocated. */
#define ALIGN(x) (((x) + (sizeof(int) - 1)) & ~(sizeof(int) - 1))

/** Custom memory allocation routine. If there is no more space, it
    triggers the semi-space garbage collector.
    \param bytes The number of bytes to allocate.
    \return The address of the newly allocated memory.
 */
inline byte* MM_alloc(size_t bytes) {
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

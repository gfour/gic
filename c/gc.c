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
static TP_  MM_forward(TP_ lar);
static void MM_scan(TP_ lar);
static void MM_compare_heaps(byte* old_space);
static void MM_compare(TP_ lar);

// The minimum amount of space that should be left after garbage collection.
#define GC_HEADROOM 10

#if GC_STATS
// Statistics: count the copied LARs & processed LAR pointers in the stack
static unsigned long bytes_copied, lar_count, st_count;
#endif /* GC_STATS */

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
  return ((byte*)t >= spaceStart) && ((byte*)t <= spaceEnd);
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

  // #if VERBOSE_GC
  printf("----------- Garbage Collection --------------\n");
  // #endif /* VERBOSE_GC */

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

  //#if VERBOSE_GC
  printf("Forwarding all pointers in the to-space...\n");
  //#endif /* VERBOSE_GC */
  //exit(-1);

  // Scan the to-space and forward any remaining pointers.
  byte* scan = to_space;
  while (scan < space) {
    TP_ lar = (TP_)scan;
    size_t sz = AR_SIZE(lar);
    MM_scan(lar);
    scan += sz;
  }

#if DEBUG_GC
  if (scan!=space) {
    printf("Out of bounds when scanning, %p != %p.\n", scan, space);
    exit(EXIT_FAILURE);
  }
#endif /* DEBUG_GC */

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
  printf("bytes copied: %lu/%lu (%f%%), forwarded LARs=%lu,\n"
         "stack roots=%lu, used to-space=%lu bytes, left to-space=%lu bytes,\n"
         "spaceStart=%p, space=%p, spaceEnd=%p\n",
         bytes_copied, MAXMEMSPACE, 100*((float)bytes_copied/(float)MAXMEMSPACE),
         lar_count, st_count, (unsigned long) (space-spaceStart),
         (unsigned long) (spaceEnd-space),
         spaceStart, space, spaceEnd);
#if VERBOSE_GC
  printf("GC done, time = %.10f sec\n", ((double)(t2 - t1)/CLOCKS_PER_SEC));
#endif /* VERBOSE_GC */
#endif /* GC_STATS */
}

/** Traverse the pointer stack and forward everything that looks like a pointer
    to a heap-allocated LAR. If this is not exact, the program will crash. */
static void MM_process_stack(void) {
#if VERBOSE_GC
  printf("Scanning the pointer stack (%p ... %p, %ld pointers)\n",
	 sstack_bottom, sstack_ptr, sstack_ptr-sstack_bottom+1);
#endif /* VERBOSE_GC */

  TP_ *ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = *ptr;
    if (MM_heap_ptr(lar)) {
    //#if VERBOSE_GC
    //printf("heap root: %p (%s)\n", lar, (IS_FORWARDED(lar)? "forwarded" : "new"));
    //printf("flag=%d\n", MM_heap_ptr(lar));
    //#endif /* VERBOSE_GC */
      *ptr = MM_forward(lar);
#if GC_STATS
    st_count++;
#endif /* GC_STATS */
    }
  }
  //#if VERBOSE_GC
  printf("Forwarded %ld roots.\n", lar_count);
  //#endif /* VERBOSE_GC */

}

/** Forwards a LAR pointer, copying the LAR to the to-space.
    \param lar The LAR pointer that must be forwarded.
    \return The new address of the LAR in the to-space.
*/
static TP_ MM_forward(TP_ lar) {
  ASSERT_GC(lar != NULL, "forwarding null");
#if VERBOSE_GC
  printf ("forwarding T0=%p, ", lar);
#endif /* VERBOSE_GC */

  // if already forwarded, just return it
  if (IS_FORWARDED(lar)) {
    // We have stored the forwarded pointer in the "prev" field.
    TP_ fw_lar = FORWARDED_ADDR(lar);
#if VERBOSE_GC
    printf("already to %p\n", fw_lar);
#endif /* VERBOSE_GC */
    return fw_lar;
  }

  // Calculate LAR layout to forward it.
  char lar_a = ARITY(lar);
  char lar_n = NESTING(lar);
  size_t sz = AR_SIZE(lar);
#ifndef LAR_COMPACT
  TODO("Missing case: padding bytes.");
#endif /* LAR_COMPACT */
#if VERBOSE_GC
  printf("found LAR %p with arity %d and nesting %d, size=%ld bytes\n", 
	 lar, lar_a, lar_n, sz);
#endif /* VERBOSE_GC */

  /*
  if (!MM_heap_ptr((TP_)space)) {
    printf("MM_forward called for non-heap pointer %p.\n", space);
    exit(-1);
  }
  */

  int j;
  byte *to_space_limit = to_space+MAXMEMSPACE;
  if (space+sz < to_space_limit) {
    //  if ( (ISSPACE1(space) && ((space+sz)<(space1+MAXMEMSPACE))) ||
    //   (ISSPACE2(space) && ((space+sz)<(space2+MAXMEMSPACE)))) {
    memcpy(space, lar, sz);
    // poor man's memcpy, for debugging:
    // byte *lar0 = (byte*)lar;
    // for (j=0; j<sz; j++)
    //   space[j] = lar0[j];
  }
  else {
    printf("Out of to-space: %p + %ld = %p >= %p.\n",
	   space, sz, space+sz, to_space_limit);
    printf("Test MM_heap_ptr() returns %d.\n", MM_heap_ptr((TP_)space));
    printf("Spaces: space1 = [%p...%p], space2 = [%p...%p]\n",
	   space1, space1+MAXMEMSPACE, space2, space2+MAXMEMSPACE);
    printf("spaceStart=%p, spaceEnd=%p, 'belongs'=%d\n", spaceStart, spaceEnd,
	   (space >= spaceStart) && (space <= spaceEnd));
    printf("Type of this space: %s.\n",
	   (ISSPACE1(space)? "space1" : (ISSPACE2(space)? "space2" : "??")));
    exit(EXIT_FAILURE);
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
#endif /* GC_STATS */
#if VERBOSE_GC
    printf("%p was copied to %p\n", lar, fw_lar);
#endif /* VERBOSE_GC */
  return fw_lar;
}

/** Scans a LAR and rewrites its contexts using forwarding pointers
    \param lar The LAR to scan.
*/
static void MM_scan(TP_ lar) {
#if VERBOSE_GC
  printf("scanning T0=%p\n", lar);
#endif /* VERBOSE_GC */
  char lar_a    = ARITY(lar);
  char lar_n    = NESTING(lar);
  TP_  lar_prev = AR_prev(lar);

  if (MM_heap_ptr(lar_prev))
    lar->prev = ARINFO(lar_a, lar_n, MM_forward(lar_prev));

  // Forward LAR pointers inside evaluated lazy constructors.
  int n;
  for (n=0; n<lar_a; n++) {
    Susp val = VALS(n, AR_REF(lar));
    if (IS_VAL(n, AR_REF(lar)) && IS_CONSTR(val)) {
      TP_ cptr = CPTR(val);
      if (MM_heap_ptr(cptr)) {
	int constrId = CONSTR(val);
	TP_ fwCtxt   = MM_forward(cptr);
#ifdef LAR_COMPACT
	VALS(n, AR_REF(lar)) = THUNK(constrId, fwCtxt);
#else
	TODO("VALS(n, lar) = THUNK(constrId, fwCtxt);");
#endif /* LAR_COMPACT */
      }
      else if (cptr != 0) { printf("stack constructor: %p\n", cptr); exit(EXIT_FAILURE); }
    }
  }
  // Forward LAR pointers inside nested fields.
  for (n=0; n<lar_n; n++) {
#ifdef LAR_COMPACT
    TP_ nested = NESTED(n, lar_a, AR_REF(lar));
#else /* LAR_COMPACT */
    TP_ nested = NESTED(n, AR_REF(lar));
#endif /* LAR_COMPACT */
    if (MM_heap_ptr(nested)) {
      TP_ fwCtxt = MM_forward(nested);
#ifdef LAR_COMPACT
      NESTED(n, lar_a, AR_REF(lar)) = fwCtxt;
#else
      NESTED(n, AR_REF(lar)) = fwCtxt;
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
    if (sz % 8 != 0) {
      printf("Non-aligned LAR of size %ld found at %p.\n", sz, lar);
      exit(EXIT_FAILURE);
    }
    MM_compare(lar);
    scan += sz;
  }

#if DEBUG_GC
  if (scan!=old_space) {
    printf("Out of bounds when comparing, %p != %p.\n", scan, old_space);
    exit(EXIT_FAILURE);
  }
#endif /* DEBUG_GC */

}

#ifdef LAR_COMPACT
/** Compares two thunks.
    \param  A thunk in from-space.
    \param  A thunk in to-space.
    \return If the thunks are the same, or if they are lazy constructors with
    the pointer field of the second be the forwarded pointer field of the first,
    returns 1. Otherwise, returns 0.
*/  
static int MM_compare_vals(int n, TP_ lar, TP_ copy) {
  Susp s_from = VALS(n, AR_REF(lar ));
  Susp s_to   = VALS(n, AR_REF(copy));
  if (s_from==s_to) return 1;
  if (IS_CONSTR(s_from) && IS_CONSTR(s_to))
    return ((CONSTR(s_from)==CONSTR(s_to)) && 
	    (FORWARDED_ADDR(CPTR(s_from))==CPTR(s_to)));
  return 0;
}
#endif /* LAR_COMPACT */

/** Checks if a LAR in the from-space has been forwarded correctly.
    \param lar The from-space LAR.
*/
static void MM_compare(TP_ lar) {
  // printf("MM_compare(%p): ", lar);
  if (IS_FORWARDED(lar)) {
    // printf("LAR %p is forwarded to %p.\n", lar, FORWARDED_ADDR(lar));
  }
  else {
    // printf("LAR is not forwarded.\n");
    return;
  }
  
  TP_ copy = FORWARDED_ADDR(lar);

  unsigned char lar_a  = ARITY(lar);
  unsigned char lar_n  = NESTING(lar);
  unsigned char copy_a = ARITY(copy);
  unsigned char copy_n = NESTING(copy);

  if (lar_a != copy_a) {
    printf("GC: arity mismatch between %p and %p: %d != %d\n",
	   lar, copy, lar_a, copy_a);
    exit(EXIT_FAILURE);
  }
  if (lar_n != copy_n) {
    printf("GC: nesting mismatch between %p and %p: %d != %d\n",
	   lar, copy, lar_n, copy_n);
    exit(EXIT_FAILURE);
  }
  if (AR_SIZE(lar) != AR_SIZE(copy)) {
    printf("GC: size mismatch between %p and %p: %ld != %ld\n",
	   lar, copy, AR_SIZE(lar), AR_SIZE(copy));
    exit(EXIT_FAILURE);
  }

  int n;
  for (n=0; n<lar_a; n++) {
#ifdef LAR_COMPACT
    if (IS_VAL(n, AR_REF(lar)) && IS_VAL(n, AR_REF(copy)) && 
	(!MM_compare_vals(n, lar, copy))) {
      printf("GC: val[%d] mismatch between %p and %p: %lx != %lx, details:\n",
	     n, lar, copy, VALS(n, AR_REF(lar)), VALS(n, AR_REF(copy)));
      MM_printThunk(n, lar) ; printf(" != ");
      MM_printThunk(n, copy); printf("\n");
      exit(EXIT_FAILURE);
    }
#else
  TODO("thunk comparison for lar.h thunks");
#endif /* LAR_COMPACT */
  }
  for (n=0; n<lar_n; n++) {
#ifdef LAR_COMPACT
    TP_ lar_nested  = NESTED(n, lar_a , AR_REF(lar ));
    TP_ copy_nested = NESTED(n, copy_a, AR_REF(copy));
#else
    TP_ lar_nested  = NESTED(n, AR_REF(lar ));
    TP_ copy_nested = NESTED(n, AR_REF(copy));
#endif /* LAR_COMPACT */

    if (lar_nested != copy_nested) {
      printf("GC: nested[%d] mismatch between %p and %p: %p != %p, details:\n",
	     n, lar, copy, lar_nested, copy_nested);
      exit(EXIT_FAILURE);
    }
  }

  // printf("OK.\n");
}

#endif /* GC */

/** Pretty printer for LAR thunks.
    \param n   The position of a thunk in a LAR.
    \param lar The LAR. */
#ifdef LAR_COMPACT
static void MM_printThunk(int n, TP_ lar) {
  if (IS_VAL(n, AR_REF(lar))) {
    Susp s = VALS(n, AR_REF(lar));
    if (IS_PVAL(s)) {
      printf("val(int=%ld)", PVAL_R(s));
    }
    else if (IS_CONSTR(s)) {
      TP_ c = CPTR(s);
      printf("val(constructor={%d, %p})", CONSTR(s), c);
#ifdef GC
      printf("(fw=%d, from-heap=%d)})", IS_FORWARDED(c), MM_heap_ptr(c));
#endif /* GC */
      if (IS_FORWARDED(c)) printf("(=>%p)", FORWARDED_ADDR(c));
    }
    else printf("Unknown Susp found.");
  } else {
    LarArg symbolAddr = (LarArg)CODE(n, AR_REF(lar));
    printf("code{");
    DEBUG_printSymbol(symbolAddr);
    printf("::%p}", symbolAddr);
  }
}
#else
static void MM_printThunk(int n, TP_ lar) {
  TODO("MM_printThunk missing");
}
#endif /* LAR_COMPACT */

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
    exit(EXIT_FAILURE);
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

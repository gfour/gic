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
  printf("bytes copied: %lu/%lu (%f%%), forwarded LARs=%lu,\n"
         "stack roots=%lu, used to-space=%lu bytes, left to-space=%lu bytes,\n"
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

static void MM_check_fw_reg(unw_cursor_t *cursor, unw_regnum_t regname, TP_ reg_tp) {
  reg_tp = CPTR(reg_tp);
  if (MM_heap_ptr(reg_tp) && (!(IS_FORWARDED(reg_tp)))) {
    if (regname>=UNW_X86_64_XMM0) {
      printf("Cannot handle XMM registers yet.");
      exit(-1);
    }
    printf("forwarding reg %2d=%p\n", regname, reg_tp);
    TP_ fw_tp = MM_forward(reg_tp);
    int r = unw_set_reg(cursor, regname, (unw_word_t)fw_tp);
    if (r!=0) {
      printf("Error modifying register %d (%p => %p)\n", regname, reg_tp, fw_tp);
      exit(-1);
    }
#if GC_STATS
    st_count++;
#endif /* GC_STATS */
  }
}

/** Traverse the pointer stack and forward everything that looks like a pointer
    to a heap-allocated LAR. If this is not exact, the program will crash. */
static void MM_process_stack(void) {
#if VERBOSE_GC
  printf("Scanning the pointer stack (%p ... %p, %ld pointers)\n",
	 sstack_bottom, sstack_ptr, sstack_ptr-sstack_bottom+1);
#endif /* VERBOSE_GC */

  TP_** ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = **ptr;
#if VERBOSE_GC
    printf("candidate root: %p\n", lar);
#endif /* VERBOSE_GC */
    if (MM_heap_ptr(lar)) {
      **((TP_ **)ptr) = MM_forward(lar);
#if GC_STATS
    st_count++;
#endif /* GC_STATS */
    }
  }
#if VERBOSE_GC
  printf("Forwarded %ld roots.\n", lar_count);
#endif /* VERBOSE_GC */

  // -------------------------
  /*
  unw_cursor_t cursor; unw_context_t uc;
  unw_word_t rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, 
             r8,  r9,  r10, r11, r12, r13, r14, r15;
  unw_word_t xmm0 , xmm1 , xmm2 , xmm3 , xmm4 , xmm5 , xmm6 , xmm7 ,
             xmm8 , xmm9 , xmm10, xmm11, xmm12, xmm13, xmm14, xmm15;
  unw_word_t ip, sp, sp_prev=0, offp;
  int frames = 0;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  while (unw_step(&cursor) > 0) {
    const int len = 100;
    char fname[len];
    unw_get_reg(&cursor, UNW_X86_64_RAX, &rax);
    unw_get_reg(&cursor, UNW_X86_64_RBX, &rbx);
    unw_get_reg(&cursor, UNW_X86_64_RCX, &rcx);
    unw_get_reg(&cursor, UNW_X86_64_RDX, &rdx);
    unw_get_reg(&cursor, UNW_X86_64_RSI, &rsi);
    unw_get_reg(&cursor, UNW_X86_64_RDI, &rdi);
    unw_get_reg(&cursor, UNW_X86_64_RBP, &rbp);
    unw_get_reg(&cursor, UNW_X86_64_RSP, &rsp);
    unw_get_reg(&cursor, UNW_X86_64_R8 , &r8 );
    unw_get_reg(&cursor, UNW_X86_64_R9 , &r9 );
    unw_get_reg(&cursor, UNW_X86_64_R10, &r10);
    unw_get_reg(&cursor, UNW_X86_64_R11, &r11);
    unw_get_reg(&cursor, UNW_X86_64_R12, &r12);
    unw_get_reg(&cursor, UNW_X86_64_R13, &r13);
    unw_get_reg(&cursor, UNW_X86_64_R14, &r14);
    unw_get_reg(&cursor, UNW_X86_64_R15, &r15);
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);
    unw_get_reg(&cursor, UNW_X86_64_XMM0 , &xmm0 );
    unw_get_reg(&cursor, UNW_X86_64_XMM1 , &xmm1 );
    unw_get_reg(&cursor, UNW_X86_64_XMM2 , &xmm2 );
    unw_get_reg(&cursor, UNW_X86_64_XMM3 , &xmm3 );
    unw_get_reg(&cursor, UNW_X86_64_XMM4 , &xmm4 );
    unw_get_reg(&cursor, UNW_X86_64_XMM5 , &xmm5 );
    unw_get_reg(&cursor, UNW_X86_64_XMM6 , &xmm6 );
    unw_get_reg(&cursor, UNW_X86_64_XMM7 , &xmm7 );
    unw_get_reg(&cursor, UNW_X86_64_XMM8 , &xmm8 );
    unw_get_reg(&cursor, UNW_X86_64_XMM9 , &xmm9 );
    unw_get_reg(&cursor, UNW_X86_64_XMM10, &xmm10);
    unw_get_reg(&cursor, UNW_X86_64_XMM11, &xmm11);
    unw_get_reg(&cursor, UNW_X86_64_XMM12, &xmm12);
    unw_get_reg(&cursor, UNW_X86_64_XMM13, &xmm13);
    unw_get_reg(&cursor, UNW_X86_64_XMM14, &xmm14);
    unw_get_reg(&cursor, UNW_X86_64_XMM15, &xmm15);
    int r = unw_get_proc_name(&cursor, fname, len, &offp);
    if (r==0) {
      frames++;
      //printf("%5d. %s(): ", frames, fname);      
      if (strncmp(fname, "main", 4)==0) {
	printf("Top-level function frame found, returning from stack walk.\n");
	return;
      }
      MM_check_fw_reg(&cursor, UNW_X86_64_RAX, (TP_)rax);
      MM_check_fw_reg(&cursor, UNW_X86_64_RBX, (TP_)rbx);
      MM_check_fw_reg(&cursor, UNW_X86_64_RCX, (TP_)rcx);
      MM_check_fw_reg(&cursor, UNW_X86_64_RDX, (TP_)rdx);
      MM_check_fw_reg(&cursor, UNW_X86_64_RSI, (TP_)rsi);
      MM_check_fw_reg(&cursor, UNW_X86_64_RDI, (TP_)rdi);
      MM_check_fw_reg(&cursor, UNW_X86_64_R8 , (TP_)r8 );
      MM_check_fw_reg(&cursor, UNW_X86_64_R9 , (TP_)r9 );
      MM_check_fw_reg(&cursor, UNW_X86_64_R10, (TP_)r10);
      MM_check_fw_reg(&cursor, UNW_X86_64_R11, (TP_)r11);
      MM_check_fw_reg(&cursor, UNW_X86_64_R12, (TP_)r12);
      MM_check_fw_reg(&cursor, UNW_X86_64_R13, (TP_)r13);
      MM_check_fw_reg(&cursor, UNW_X86_64_R14, (TP_)r14);
      MM_check_fw_reg(&cursor, UNW_X86_64_R15, (TP_)r15);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM0 , (TP_)xmm0 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM1 , (TP_)xmm1 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM2 , (TP_)xmm2 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM3 , (TP_)xmm3 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM4 , (TP_)xmm4 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM5 , (TP_)xmm5 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM6 , (TP_)xmm6 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM7 , (TP_)xmm7 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM8 , (TP_)xmm8 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM9 , (TP_)xmm9 );
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM10, (TP_)xmm10);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM11, (TP_)xmm11);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM12, (TP_)xmm12);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM13, (TP_)xmm13);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM14, (TP_)xmm14);
      MM_check_fw_reg(&cursor, UNW_X86_64_XMM15, (TP_)xmm15);
      if (MM_heap_ptr((TP_)ip)) { printf("Found heap IP.\n"); exit(-1); }
      if (MM_heap_ptr((TP_)sp)) { printf("Found heap SP.\n"); exit(-1); }
      if (sp_prev!=0) {
	TP_* sptr;
	for (sptr = (TP_*)sp_prev; sptr < (TP_*)sp; sptr++) {
	  // printf("Searching sp range (%p ... %p).\n", , );
	  TP_ candidate = *sptr;
	  if (MM_heap_ptr(candidate) && (IS_FORWARDED(candidate))) {
	    TP_ fw_tp = MM_forward(candidate);
	    printf("Rogue forwarded pointer found: %p => %p\n", candidate, fw_tp);
	    *sptr = fw_tp;
	    // exit(-1);
	  }
	}
      }
      sp_prev = sp;
    }
    else { printf("Could not retrieve function name.\n"); exit(-1); }
    // printf ("rax = %lx, rbx = %lx, rcx = %lx, rdx = %lx, ",
    // 	    (long) rax, (long) rbx, (long) rcx, (long) rdx);
    // printf ("ip = %lx, sp = %lx\n", (long) ip, (long) sp);
  }
  */

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
  if ( (ISSPACE1(space) && (space+sz<space1+MAXMEMSPACE)) ||
       (ISSPACE2(space) && (space+sz<space2+MAXMEMSPACE))) {
    unsigned char *lar0 = (unsigned char*)lar;
    for (j=0; j<sz; j++)
      space[j] = lar0[j];
  }
  else {
    printf("Out of space: %p + %ld = %p >= %p.\n", space, sz, space+sz, spaceEnd);
    printf("Spaces: space1 = [%p...%p], space2 = [%p...%p]\n",
	   space1, space1+MAXMEMSPACE, space2, space2+MAXMEMSPACE);
    printf("Type of this space: %s.\n",
	   (ISSPACE1(space)? "space1" : (ISSPACE2(space)? "space2" : "??")));
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
    printf("%p was copied to %p\n", lar, fw_lar);
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
    // printf("LAR %p is forwarded to %p.\n", lar, FORWARDED_ADDR(lar));
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

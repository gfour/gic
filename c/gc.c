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

// Looks like a LAR.
static int ll_lar(TP_ lar) {
  return (((unsigned char)ARITY(lar) + (unsigned char)NESTING(lar)) != 0);
}

/** Takes the result of a saved register update and shows an error message.
    \param r   The result value from the register update operation.
    \param reg The register (according to libunwind).
*/
static void MM_reg_upd_err(int r, unw_regnum_t reg) {
  if (r != 0) {
    switch (r) {
    case UNW_EUNSPEC:
      printf("An unspecified error occurred.\n"); break;
    case UNW_EBADREG:
      printf("An attempt was made to write a register that is either invalid or not accessible in the current frame.\n"); break;
    default:
      printf("Unknown error (%d).\n", r);
    }
    exit(EXIT_FAILURE);
  }
}

/** Checks if a x86-64 XMM register saved in a stack frame contains a LAR
    pointer; if yes, the pointers is forwarded.
    \param The libunwind cursor pointing to the stack frame.
    \param The XMM register name (see header "libunwind-x86_64.h").
    \param The 128-bit register contents.
 */
static void MM_check_fw_fpreg(unw_cursor_t *cursor, unw_regnum_t reg, unw_fpreg_t reg_tp) {
  TP_ *reg_tp_pair = (TP_*)(&reg_tp);
  int i;
  int modified = 0, pbody = 0;
  for (i=0; i<2; i++) {
    TP_ reg_tp_i = reg_tp_pair[i];
#ifdef LAR_COMPACT
    TP_ reg_tp_iC = CPTR(reg_tp_i);
#endif /* LAR_COMPACT */
    if (MM_heap_ptr(CPTR(reg_tp_i)) && ll_lar(CPTR(reg_tp_i))) {
      if (!unw_is_fpreg(reg)) {
	printf("MM_check_fw_fpreg cannot handle non-XMM register %d.\n", reg);
	exit(EXIT_FAILURE);
      }
      if (reg_tp_i != reg_tp_iC) {
#if VERBOSE_GC
	printf("X:pointer body, a=%d, n=%d, constr=%d, is_val=%d, is_pval=%d\n",
	       AR_a(reg_tp_i), AR_n(reg_tp_i), CONSTR(reg_tp_i),
	       (((intptr_t)(reg_tp_i) & 1) == 0), IS_PVAL(reg_tp_i));
#endif /* VERBOSE_GC */
	pbody = 1;
      }
#ifdef LAR_COMPACT
#if VERBOSE_GC
      printf("Forwarding XMM register %2d[%d] = %p (pointer body = %p) => ",
      	     reg, i, reg_tp_pair[i], reg_tp_iC);
#endif /* VERBOSE_GC */
      if (pbody == 0)
	reg_tp_pair[i] = MM_forward(reg_tp_i);
      else
	reg_tp_pair[i] = (TP_)(((intptr_t)MM_forward(reg_tp_iC) & PTRMASK) |
			       ((intptr_t)reg_tp_i & ~PTRMASK));
#if VERBOSE_GC
      printf("%p\n", reg_tp_pair[i]);
#endif /* VERBOSE_GC */
#else
      TODO("Register update for lar.h.");
#endif /* LAR_COMPACT */
      modified = 1;
    }
  }
  // If a word was modified, write back the register.
  if (modified) {
    unw_fpreg_t new_val = *((unw_fpreg_t*)reg_tp_pair);
    int r = unw_set_fpreg(cursor, reg, new_val);
    if (r != 0) {
      printf("Error modifying register %d: ", reg);
      MM_reg_upd_err(r, reg);
    }
  }
}

/** Checks if a x86 register saved in a stack frame contains a LAR pointer;
    if yes, the pointer is forwarded.
    \param The libunwind cursor pointing to the stack frame.
    \param The x86 register name.
    \param The register contents cast to a LAR pointer.
 */
static void MM_check_fw_reg(unw_cursor_t *cursor, unw_regnum_t reg, TP_ reg_tp) {
#ifdef LAR_COMPACT
  int pbody = 0;
  TP_ reg_tp_C = CPTR(reg_tp);
#endif /* LAR_COMPACT */
  if (MM_heap_ptr(reg_tp) && ll_lar(reg_tp)) {
    if (unw_is_fpreg(reg)) {
      printf("MM_check_fw_reg cannot handle XMM register %d.\n", reg);
      exit(EXIT_FAILURE);
    }    
    if (reg_tp != reg_tp_C) {
      printf("R:pointer body, a=%d, n=%d, constr=%d, is_val=%d, is_pval=%d\n",
	     AR_a(reg_tp), AR_n(reg_tp), CONSTR(reg_tp), 
	     (((intptr_t)(reg_tp) & 1) == 0), IS_PVAL(reg_tp));
      pbody = 1;
    }
    TP_ fw_tp;
    if (pbody == 1)
      fw_tp = (TP_)(((intptr_t)MM_forward(reg_tp_C) &  PTRMASK) |
		    ((intptr_t)reg_tp               & ~PTRMASK));
    else
      fw_tp = MM_forward(reg_tp);
    printf("Forwarding register %2d = %p => %p.\n", reg, reg_tp, fw_tp);
    int r = unw_set_reg(cursor, reg, (unw_word_t)fw_tp);
    if (r != 0) {
      printf("Error modifying register %d (%p => %p): ", reg, reg_tp, fw_tp);
      MM_reg_upd_err(r, reg);
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

  TP_* ptr;
  for (ptr = sstack_bottom; ptr <= sstack_ptr; ptr++) {
    TP_ lar = *ptr;
#if VERBOSE_GC
    printf("candidate root: %p\n", lar);
#endif /* VERBOSE_GC */
    if (MM_heap_ptr(lar)) {
      *((TP_ *)ptr) = MM_forward(lar);
#if GC_STATS
    st_count++;
#endif /* GC_STATS */
    }
  }
#if VERBOSE_GC
  printf("Forwarded %ld roots.\n", lar_count);
#endif /* VERBOSE_GC */

  // -------------------------

  unw_cursor_t cursor; unw_context_t uc;
  unw_word_t rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, 
             r8,  r9,  r10, r11, r12, r13, r14, r15;
  unw_fpreg_t xmm0 , xmm1 , xmm2 , xmm3 , xmm4 , xmm5 , xmm6 , xmm7 ,
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
    /*
    unw_get_fpreg(&cursor, UNW_X86_64_XMM0 , &xmm0 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM1 , &xmm1 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM2 , &xmm2 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM3 , &xmm3 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM4 , &xmm4 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM5 , &xmm5 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM6 , &xmm6 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM7 , &xmm7 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM8 , &xmm8 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM9 , &xmm9 );
    unw_get_fpreg(&cursor, UNW_X86_64_XMM10, &xmm10);
    unw_get_fpreg(&cursor, UNW_X86_64_XMM11, &xmm11);
    unw_get_fpreg(&cursor, UNW_X86_64_XMM12, &xmm12);
    unw_get_fpreg(&cursor, UNW_X86_64_XMM13, &xmm13);
    unw_get_fpreg(&cursor, UNW_X86_64_XMM14, &xmm14);
    unw_get_fpreg(&cursor, UNW_X86_64_XMM15, &xmm15);
    */
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
      /*
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM0 , xmm0 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM1 , xmm1 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM2 , xmm2 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM3 , xmm3 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM4 , xmm4 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM5 , xmm5 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM6 , xmm6 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM7 , xmm7 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM8 , xmm8 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM9 , xmm9 );
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM10, xmm10);
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM11, xmm11);
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM12, xmm12);
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM13, xmm13);
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM14, xmm14);
      MM_check_fw_fpreg(&cursor, UNW_X86_64_XMM15, xmm15);
      */
      if (MM_heap_ptr((TP_)ip)) { printf("Found heap IP.\n"); exit(-1); }
      if (MM_heap_ptr((TP_)sp)) { printf("Found heap SP.\n"); exit(-1); }
      if (sp_prev!=0) {
	TP_* sptr;
	for (sptr = (TP_*)sp_prev; sptr < (TP_*)sp; sptr++) {
	  // printf("Searching sp range (%p ... %p).\n", , );
	  TP_ candidate = *sptr;
	  TP_ candidateC = CPTR(candidate);
	  if (MM_heap_ptr(candidateC) &&
	      (ISSPACE1((byte*)CPTR(candidateC->prev)) || 
	       ISSPACE2((byte*)CPTR(candidateC->prev)))) {
	    int pbody = 0;
	    if (candidate != candidateC) {
#if VERBOSE_GC
	      printf("sptr:pointer body, a=%d, n=%d, constr=%d, is_val=%d, is_pval=%d\n",
	      	     AR_a(candidate), AR_n(candidate), CONSTR(candidate),
	      	     (((intptr_t)(candidate) & 1) == 0), IS_PVAL(candidate));
#endif /* VERBOSE_GC */
	      pbody = 1;
	    }
	    TP_ fw_tp;
	    if (pbody==0)
	      fw_tp = MM_forward(candidate);
	    else
	      fw_tp = (TP_)(((intptr_t)MM_forward(candidateC) &  PTRMASK) |
			    ((intptr_t)candidate              & ~PTRMASK));
#if VERBOSE_GC
	    printf("Rogue forwarded pointer found: %p => %p\n", candidate, fw_tp);
#endif /* VERBOSE_GC */
	    *sptr = fw_tp;
	  }
	}
      }
      sp_prev = sp;
    }
    else { printf("Could not retrieve function name.\n"); exit(EXIT_FAILURE); }
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
#endif /* VERBOSE_GC */
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
static void MM_print_Susp(int n, TP_ lar) {
  if (IS_VAL(n, AR_REF(lar))) {
    Susp s = VALS(n, AR_REF(lar));
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
    printf("code{%p}", (LarArg)CODE(n, AR_REF(lar)));
  }
}
#else
static void MM_print_Susp(int n, TP_ lar) {
  TODO("MM_print_Susp missing");
}
#endif /* LAR_COMPACT */

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
      MM_print_Susp(n, lar) ; printf(" != ");
      MM_print_Susp(n, copy); printf("\n");
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

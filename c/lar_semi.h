/**
   Macros used for the LAR representation using the semi-space collector.
   This file contains common macros for both the standard and the compact
   representation.
*/

#ifdef GC
/* LAR wrapper (*-operator). */
#define AR_TP(tp)  *(tp)
/* LAR wrapper (&-operator). */
#define AR_REF(tp) &(tp)
#else
/* Dummy LAR wrapper (*-operator). */
#define AR_TP(tp)  (tp)
/* Dummy LAR wrapper (&-operator). */
#define AR_REF(tp) (tp)
#endif /* GC */

/** The start of the ARGS fields, holding the code pointers of the thunks.
    \param T The LAR. */
#ifdef GC
#define THE_ARGS(T)   ((byte *) &((*(T))->data))
#else
#define THE_ARGS(T)   ((byte *) &((T)->data))
#endif /* GC */

/** Printer for activation records (used for debugging).
    \param a The LAR. This is used many times, it should not be a macro. */
#ifdef GC
#define DEBUG_PRINT_AR(a) { printf("%p@{ prev=%p, a=%d, n=%d } [size=%ld] ", a, AR_prev(a), ARITY(a), NESTING(a), AR_SIZE(a)); printf("thunks: "); int i; for (i=0; i<ARITY(a); i++) { printf("<"); MM_printThunk(i, a); ; printf("> "); }; }
#else
#define DEBUG_PRINT_AR(a) { }
#endif /* GC */

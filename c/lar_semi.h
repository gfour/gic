/**
   Macros used for the LAR representation using the semi-space collector.
   This file contains common macros for both the standard and the compact
   representation.
*/

#ifdef GC
/* LAR wrapper (*-operator). */
#define AR_TP(tp)  *tp
/* LAR wrapper (&-operator). */
#define AR_REF(tp) &(tp)
#else
/* Dummy LAR wrapper (*-operator). */
#define AR_TP(tp)  tp
/* Dummy LAR wrapper (&-operator). */
#define AR_REF(tp) tp
#endif /* GC */

/** The start of the ARGS fields, holding the code pointers of the thunks.
    \param T The LAR. */
#ifdef GC
#define THE_ARGS(T)   ((byte *) &((*(T))->data))
#else
#define THE_ARGS(T)   ((byte *) &((T)->data))
#endif /* GC */

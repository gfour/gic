/*
   LAR infrastructure, compact representation for use with the semi-space
   garbage collector. Only works on the x86-64 architecture due to heavy
   use of tagged pointers and assumptions about the use of bits in pointers.
*/

// Header files

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gc.h"

/* Give an error on non-x86-64-bit systems. */
#ifndef __x86_64__
#error "The compact LAR representation does not support non-x86-64 platforms yet."
#endif /* __x86_64__ */

/* Tags are not supported. */
#ifdef USE_TAGS
#error "The compact LAR representation does not support data type tags."
#endif /* USE_TAGS */

/* The OpenMP runtime is not supported. */
#ifdef OMP
#error "The compact LAR representation does not support the OpenMP runtime."
#endif /* OMP */

/* The shadow stack should only be used when GC is on. */
#ifdef GC
#ifndef SSTACK
#error Garbage collection in compact mode requires the shadow stack, #define SSTACK.
#endif /* SSTACK */
#endif /* GC */

/* Give a warning when compiling without garbage collection. */
#ifndef GC
#warning Garbage collection is disabled for this build.
#endif /* GC */

/** Set macro used by LAR-specific C code (such as the garbage collector). */
#define LAR_COMPACT

// Types

typedef unsigned char byte;

typedef struct T_* TP_;

typedef struct Susp {
  //  int constr;              // constructor id
  //  union {
  TP_ ctxt;                    // lazy constructor context
  //    LarArg arg;            // unevaluated argument
  //}
} Susp;

typedef Susp (*LarArg)(TP_);

typedef struct T_ {
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

#define LAR_STRUCT(n_arity, n_nesting)             \
  struct {                                         \
    TP_  prev;                                     \
    Susp the_vals[n_arity];                        \
    TP_  the_nested[n_nesting];                    \
  }

/* Macros */

#define True 1
#define False 0

#define ZEROIFTAG(x)                   0

/* The mask that singles out with AND the important bits in a pointer (3:47). */
#define PTRMASK                        0xfffffffffff8

#define THE_ARGS(T)                    ((byte *) &((T)->data))
#define VALS(x, T)                     (((Susp*) (THE_ARGS(T)))[x])
#define ARGS(x, T)                     (VALS(x, T).ctxt)
#define ARGS_FLAG(x, T)                ((LarArg)((uintptr_t)ARGS(x, T) & (uintptr_t)0x1))
#define INIT_ARG_LOCKS(arity_a)        { }

#define THE_NESTED(VALSARITY, T)       (THE_ARGS(T) + VALSARITY * sizeof(Susp))
#define NESTED(x, VALSARITY, T)        (((TP_*) THE_NESTED(VALSARITY, T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = (TP_)(AR_prev(T0))

#define GETARG(x, T)  ({                  \
      if (ARGS_FLAG(x, T) != NULL) {	  \
        Susp val = CODE(x, T)(T);         \
        VALS(x, T) = val;                 \
      }                                   \
      VALS(x, T);                         \
    })

/* A strict argument is already evaluated, just return its value. */
#define GETSTRICTARG(x, T)                 VALS(x, T)

/* A call-by-name argument calls directly the argument, does no memoization. */
#define GETCBNARG(x, T)                    (CODE(x, T)(T))

#define AR_CAT(A, B) AR_CAT_AUX(A, B)
#define AR_CAT_AUX(A, B) A ## B

#define AR(n_arity, n_nesting, ...) ({                          \
      TP_ lar = (TP_) MM_alloc(sizeof(T_) +      		\
                               n_arity * sizeof(Susp) +         \
                               n_nesting * sizeof(TP_));        \
      lar->prev = ARINFO(n_arity, n_nesting, T0);		\
      AR_CAT(AR_COPY_, n_arity)(lar, 0, ## __VA_ARGS__);        \
      AR_CAT(AR_CLEAR_, n_nesting)(lar, n_arity, 0);		\
      lar;                                                      \
    })
#define AR_S(n_arity, n_nesting, ...)                   \
  ((TP_) &((LAR_STRUCT(n_arity, n_nesting))             \
    { ARINFO(n_arity, n_nesting, T0), { __VA_ARGS__ } }))

/* *********** Macros of the LAR API *********** */

/* shift right, fill with 0s */
#define CONSTR(p)     ((int)(((uintptr_t)(p).ctxt) >> 48))
#define CPTR(p)       ((TP_)((((intptr_t)(((uintptr_t)(p)) << 16)) >> 16) & (~7)))
#define ARGC(arg)     ((TP_)((uintptr_t)arg | (uintptr_t)0x1))
#define CODE(x, T)    ((LarArg)((uintptr_t)ARGS(x, T) & ~1))

/* Primitive value read/create macros. These values use all high 62 bits. */
#define PVAL_R(p)     ((signed long)(((intptr_t)((p).ctxt)) >> 2))
#define PVAL_C(i)     ((Susp) { (TP_)(((intptr_t)(i)) << 2) } )

/* Thunk constructor, ignores the data type tag 't'. */
#define SUSP(c, t, p) ((Susp) { ((TP_)((((uintptr_t)(c)) << 48) | (((uintptr_t)(p)) & PTRMASK) | 2)) } )

/* ********** Fast integer handling ***** */

/* These operators are enabled when -fop is passed in the command line.
   They should only be used for evaluation purposes. */

#define PVAL_ADD(p1, p2)  ((Susp) { (TP_)(((intptr_t)((p1).ctxt)) + ((intptr_t)((p2).ctxt))) })
#define PVAL_SUB(p1, p2)  ((Susp) { (TP_)(((intptr_t)((p1).ctxt)) - ((intptr_t)((p2).ctxt))) })
#define PVAL_MUL(p1, p2)  ((Susp) { (TP_)((((intptr_t)((p1).ctxt)) >> 2) * ((intptr_t)((p2).ctxt))) })
#define PVAL_DIV(p1, p2)  ((Susp) { (TP_)((((intptr_t)((p1).ctxt)) / ((intptr_t)((p2).ctxt))) << 2) })
#define PVAL_MOD(p1, p2)  ((Susp) { (TP_)(((intptr_t)((p1).ctxt)) % ((intptr_t)((p2).ctxt))) })
#define PVAL_EQU(p1, p2)  ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) == ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_NEQ(p1, p2)  ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) != ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_LT(p1, p2)   ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) <  ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_LE(p1, p2)   ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) <= ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_GT(p1, p2)   ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) >  ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_GE(p1, p2)   ((Susp) { (TP_)((intptr_t)((((intptr_t)((p1).ctxt)) >= ((intptr_t)((p2).ctxt))) << 2 )) })
#define PVAL_AND(p1, p2)  ((Susp) { (TP_)(((intptr_t)((p1).ctxt)) & ((intptr_t)((p2).ctxt))) })
#define PVAL_OR(p1, p2)   ((Susp) { (TP_)(((intptr_t)((p1).ctxt)) | ((intptr_t)((p2).ctxt))) })
#define PVAL_NEG(p)       ((Susp) { (TP_)(~((intptr_t)((p).ctxt) - 4) & ~3) })

#define IS_PVAL(p)        (((intptr_t)((p).ctxt) & 2) == 0)

/* ********** Garbage collection ********** */

#define IS_FORWARDED(ar)   ((uintptr_t)((ar)->prev) & 1)
#define FORWARDED_ADDR(p)  ((TP_)(((uintptr_t)p) & ~1))

/** Embeds arity/nesting/previous-pointer information in a single word.
    \param a    the arity of the LAR
    \param n    the nesting depth of the LAR
    \param prev the access-link pointer to the parent LAR
    \return     the constructed "prev" field
*/
#define ARINFO(a, n, prev) (TP_)((((uintptr_t)(a)) << 56) \
                               | (((uintptr_t)(n)) << 48) \
                          | (((uintptr_t)prev) & PTRMASK))

/** Returns the access-link pointer of a LAR. */
#define AR_prev(T0)   ((TP_)(((intptr_t)(T0->prev) << 16) >> 16))
/** Returns the "arity" field of a LAR. */
#define AR_a(p)       ((unsigned char)(((uintptr_t)p) >> 56))
/** Returns the "nesting" field of a LAR. */
#define AR_n(p)       ((unsigned char)((((uintptr_t)p) >> 48) & 0xff))

#define ARITY(lar)    AR_a(((TP_)lar)->prev)
#define NESTING(lar)  AR_n(((TP_)lar)->prev)

#define AR_SIZE(ar)   ((1 + AR_a(ar->prev) + AR_n(ar->prev))*sizeof(TP_))
#define IS_THUNK(p)   ((((uintptr_t)p) & 0x4) != (uintptr_t)NULL)

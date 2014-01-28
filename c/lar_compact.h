/*
   LAR infrastructure, compact representation for use with the semi-space
   garbage collector. Only works on the x86-64 architecture due to heavy
   use of tagged pointers.
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

/* The shadow stack should only be used when GC is on. */
#ifdef SSTACK
#ifndef GC
#error Garbage collection in compact mode requires the shadow stack, #define SSTACK.
#endif /* GC */
#endif /* SSTACK */

/* Give a warning when compiling without garbage collection enabled. */
#ifndef GC
#warning Garbage collection is disabled for this build.
#endif /* GC */

// Types

typedef unsigned char byte;

typedef struct T_* TP_;

typedef struct Susp {
  int constr;              // constructor id
  //  union {
    TP_ ctxt;              // lazy constructor context
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

// Macros

#define True 1
#define False 0

#define ZEROIFTAG(x)                   0

#define THE_ARGS(T)                    ((byte *) &((T)->data))
#define VALS(x, T)                     (((Susp*) (THE_ARGS(T)))[x])
#define ARGS(x, T)                     (VALS(x, T).ctxt)
#define ARGS_FLAG(x, T)                ((LarArg)((uintptr_t)ARGS(x, T) & (uintptr_t)0x1))
#define INIT_ARG_LOCKS(arity_a)        { }

#define THE_NESTED(VALSARITY, T)       (THE_ARGS(T) + VALSARITY * sizeof(Susp))
#define NESTED(x, VALSARITY, T)        (((TP_*) THE_NESTED(VALSARITY, T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = (TP_)(GETPREV(T0))

#define GETARG(x, T)  ({                  \
      if (ARGS_FLAG(x, T) != NULL) {	  \
        Susp val = ARGS_FUNC(x, T)(T);    \
        VALS(x, T) = val;                 \
      }                                   \
      VALS(x, T);                         \
    })

/* A strict argument is already evaluated, just return its value. */
#define GETSTRICTARG(x, T)                 VALS(x, T)

/* A call-by-name argument calls directly the argument, does no memoization. */
#define GETCBNARG(x, T)                    (ARGS_FUNC(x, T)(T))

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

// Embeds arity/nesting/previous-pointer information in a single word.
#define ARINFO(n_arity, n_nesting, prev) (TP_)((((uintptr_t)n_arity) << 56) | ((uintptr_t)n_nesting << 48) | (((uintptr_t)prev) & 0xfffffffffff8))

// Macros for compatibility with the LAR API.
#define GETPREV(T0) GETTPTR(T0->prev)
#define GETTPTR(p) (TP_)(((intptr_t)(((uintptr_t)(p)) << 16) >> 16) & (~7))
// AR info field (prev): arity (63...57), nesting (56...48), pointer (47...3)
#define ARITY(lar) (unsigned char)(((uintptr_t)((TP_)lar)->prev) >> 56)
#define NESTING(lar) (unsigned char)((((uintptr_t)((TP_)lar)->prev) & 0xffffffffffff) >> 48)
#define ARGC(arg)                      (TP_)((uintptr_t)arg | (uintptr_t)0x1)
#define ARGS_FUNC(x, T)                ((LarArg)((uintptr_t)ARGS(x, T) & (uintptr_t)(~1)))
/*
   LAR infrastructure, optimized representation for use with the libgc
   garbage collector.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include "gc.h"

#ifdef USE_OMP
#include <omp.h>
#endif /* USE_OMP */

// Types

typedef unsigned char byte;

typedef struct T_* TP_;

typedef struct Susp {
  int constr;              // constructor id
#ifdef USE_TAGS
  int tag;                 // data type tag
#endif /* USE_TAGS */
  TP_ ctxt;                // lazy constructor context
} Susp;

#ifndef USE_OMP
typedef Susp (*LarArg)(TP_);
#else
typedef struct LarArg {
  Susp (*volatile larArg)(TP_);
  omp_lock_t larArgLock;
} LarArg;
#endif /* USE_OMP */

typedef struct T_ {
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

// Macros

#define True 1
#define False 0

#ifndef __x86_64__
#warning "The compiler has only been tested on the x86-64 architecture."
#endif /* __x86_64__ */

#ifndef USE_OMP
#define LAR_STRUCT(n_arity_a, n_arity_v, n_nesting)\
  struct {                                         \
    TP_  prev;                                     \
    Susp the_vals[n_arity_v];	                   \
    TP_  the_nested[n_nesting];                    \
  }
#else
#define LAR_STRUCT(n_arity_a, n_arity_v, n_nesting)\
  struct {                                         \
    TP_    prev;                                   \
    LarArg the_args[n_arity_a];                    \
    Susp   the_vals[n_arity_v];	                   \
    TP_    the_nested[n_nesting];                  \
  }
#endif

#define THE_ARGS(T)                         ((byte *) &((T)->data))
#define THE_NESTED(VARSARITY, VALSARITY, T) (THE_VALS(VARSARITY, T) + VALSARITY * sizeof(Susp))

// single-threaded runtime
#ifndef USE_OMP
#define ZEROIFSEQ(x)                        0
#define THE_VALS(VARSARITY, T)              (THE_ARGS(T))
#define ARGS(x, T)                          ((((Susp*) THE_VALS(0, T))[x])).ctxt
#define ARGS_FLAG(x, T)                     ((LarArg)((unsigned long)ARGS(x, T) & 0x1))
#define ARGC(arg)                           (TP_)((unsigned long)arg | 0x1)
#define ARGS_FUNC(x, T)                     ((LarArg)((unsigned long)ARGS(x, T) & (UINTPTR_MAX-1)))
#define INIT_ARG_LOCKS(arity_a)                { }
#define GETARG(x, ARGSARITY, T)  ({            \
      if (ARGS_FLAG(x, T) != NULL) {		       \
        Susp val = ARGS_FUNC(x, T)(T);                     \
        VALS(x, ARGSARITY, T) = val;           \
      }                                        \
      VALS(x, ARGSARITY, T);                   \
    })
#else
// parallel runtime
#define ZEROIFSEQ(x)                        x
#define THE_VALS(VARSARITY, T)              (THE_ARGS(T) + VARSARITY * sizeof(LarArg))
#define ARGS(x, T)                          ((((LarArg*) THE_ARGS(T))[x]).larArg)
#define ARGS_FUNC(x, T)                     ARGS(x, T)
#define ARGC(arg)                           arg
#define LOCKS(x, T)                         (omp_lock_t*)(&((((LarArg*) THE_ARGS(T))[x]).larArgLock))
#define INIT_ARG_LOCKS(arity_a)             {int a; for (a=0; a<arity_a; a++) { omp_init_lock(LOCKS(a, T0)); }}
#define GETARG(x, ARGSARITY, T)  ({            \
      if (ARGS(x, T) != NULL) {                \
        omp_set_lock(LOCKS(x, T));             \
        if (ARGS(x, T) != NULL) {              \
          Susp val = ARGS(x, T)(T);            \
          VALS(x, ARGSARITY, T) = val;         \
          ARGS(x, T) = NULL;                   \
        }                                      \
        omp_unset_lock(LOCKS(x, T));	       \
      }                                        \
      VALS(x, ARGSARITY, T);                   \
    })
#endif /* USE_OMP */

#define VALS(x, VARSARITY, T)               (((Susp*) THE_VALS(VARSARITY, T))[x])
#define NESTED(x, VARSARITY, VALSARITY, T)  (((TP_*) THE_NESTED(VARSARITY, VALSARITY, T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = T0->prev

#define GETSTRICTARG(x, VARSARITY, T)      VALS(x, VARSARITY, T)
#define GETCBNARG(x, T)                    (ARGS_FUNC(x, T)(T))

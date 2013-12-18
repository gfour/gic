/*
   LAR infrastructure, optimized representation for use with the libgc
   garbage collector.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gc.h"

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

typedef Susp (*LarArg)(TP_);

typedef struct T_ {
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

#define LAR_STRUCT(n_arity_a, n_arity_v, n_nesting)\
  struct {                                         \
    TP_ prev;                                      \
    LarArg the_args[n_arity_a];                    \
    Susp the_vals[n_arity_v];                      \
    TP_ the_nested[n_nesting];                     \
  }

// Macros

#define True 1
#define False 0

#define THE_ARGS(T)                         ((byte *) &((T)->data))
#define THE_VALS(VARSARITY, T)              (THE_ARGS(T) + VARSARITY * sizeof(LarArg))
#define THE_NESTED(VARSARITY, VALSARITY, T) (THE_VALS(VARSARITY, T) + VALSARITY * sizeof(Susp))
#define ARGS(x, T)                          (((LarArg*) THE_ARGS(T))[x])
#define VALS(x, VARSARITY, T)               (((Susp*) THE_VALS(VARSARITY, T))[x])
#define NESTED(x, VARSARITY, VALSARITY, T)  (((TP_*) THE_NESTED(VARSARITY, VALSARITY, T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = T0->prev

#define GETARG(x, ARGSARITY, T)  ({            \
      if (ARGS(x, T) != NULL) {                \
        Susp val = ARGS(x, T)(T);              \
        VALS(x, ARGSARITY, T) = val;           \
        ARGS(x, T) = NULL;                     \
      }                                        \
      VALS(x, ARGSARITY, T);                   \
    })
#define GETSTRICTARG(x, VARSARITY, T)  VALS(x, VARSARITY, T)
#define GETCBNARG(x, T)     (ARGS(x, T)(T))


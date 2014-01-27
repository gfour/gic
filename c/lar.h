/*
   LAR infrastructure, standard representation for use with the semispace
   garbage collector.
*/

// Header files

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
  unsigned long magic;   // the 'magic' marker for GC
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  byte arity;            // the number of arguments in this LAR
  byte nesting;          // the number of nesting links
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

#define LAR_STRUCT(n_arity, n_nesting)             \
  struct {                                         \
    unsigned long magic;                           \
    TP_ prev;                                      \
    byte arity, nesting;                           \
    LarArg the_args[n_arity];                      \
    Susp the_vals[n_arity];                        \
    TP_ the_nested[n_nesting];                     \
  }

// Macros

#define True 1
#define False 0
#define THE_ARGS(T)   ((byte *) &((T)->data))
#define THE_VALS(T)   (THE_ARGS(T) + (ARITY(T)) * sizeof(LarArg))
#define THE_NESTED(T) (THE_VALS(T) + (ARITY(T)) * sizeof(Susp))

#define ARGS(x, T)    ((ASSERT_GC_EXPR((x) < (ARITY(T)),		\
                                       "ARGS bounds violation"),        \
                        (LarArg*) THE_ARGS(T))[x])
#define VALS(x, T)    ((ASSERT_GC_EXPR((x) < (ARITY(T)),	        \
                                       "VALS bounds violation"),        \
                        (Susp*) THE_VALS(T))[x])
#define NESTED(x, T)  ((ASSERT_GC_EXPR((x) < (NESTING(T)),		\
                                       "NESTED bounds violation"),      \
                        (TP_*) THE_NESTED(T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = (TP_)(GETPREV(T0))

#define GETARG(x, T)  ({                       \
      if (ARGS(x, T) != NULL) {                \
        Susp val = ARGS(x, T)(T);              \
        VALS(x, T) = val;                      \
        ARGS(x, T) = NULL;                     \
      }                                        \
      VALS(x, T);                              \
    })

/* A strict argument is already evaluated, just return its value. */
#define GETSTRICTARG(x, T)  VALS(x, T)

/* A call-by-name argument calls directly the argument, does no memoization. */
#define GETCBNARG(x, T)     (ARGS(x, T)(T))

#define AR_CAT(A, B) AR_CAT_AUX(A, B)
#define AR_CAT_AUX(A, B) A ## B

#define AR(n_arity, n_nesting, ...) ({                          \
      TP_ lar = (TP_) MM_alloc(sizeof(T_) +      		\
                               sizeof(unsigned long) +          \
                               n_arity * sizeof(LarArg) +       \
                               n_arity * sizeof(Susp) +         \
                               n_nesting * sizeof(TP_));        \
      lar->prev = T0;				                \
      lar->arity = n_arity;                                     \
      lar->nesting = n_nesting;                                 \
      lar->magic = MAGIC;                                       \
      AR_CAT(AR_COPY_, n_arity)(lar, 0, ## __VA_ARGS__);        \
      AR_CAT(AR_CLEAR_, n_nesting)(lar, 0);                     \
      lar;                                                      \
    })
#define AR_S(n_arity, n_nesting, ...)                   \
  ((TP_) &((LAR_STRUCT(n_arity, n_nesting))             \
    { MAGIC, T0, n_arity, n_nesting, { __VA_ARGS__ } }))

#define GETPREV(T0)      T0->prev
#define GETPTR(p)        p
#define ARITY(lar)       lar->arity
#define NESTING(lar)     lar->nesting

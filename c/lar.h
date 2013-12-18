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

typedef struct T_* volatile TP_;

typedef struct Susp {
  int constr;              // constructor id
#ifdef USE_TAGS
  int tag;                 // data type tag
#endif /* USE_TAGS */
  TP_ ctxt;                // lazy constructor context
} Susp;

typedef Susp (*LarArg)(TP_);

typedef struct T_ {
#ifdef GC
  unsigned long magic;   // the 'magic' marker for GC
#endif /* GC */
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  byte arity;            // the number of arguments in this LAR
  byte nesting;          // the number of nesting links
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

#ifdef GC
#define LAR_STRUCT(n_arity, n_nesting)             \
  struct {                                         \
    unsigned long magic;                           \
    TP_ prev;                                      \
    byte arity, nesting;                           \
    LarArg the_args[n_arity];                      \
    Susp the_vals[n_arity];                        \
    TP_ the_nested[n_nesting];                     \
  }
#else
#define LAR_STRUCT(n_arity, n_nesting)             \
  struct {                                         \
    TP_ prev;                                      \
    byte arity, nesting;                           \
    LarArg the_args[n_arity];                      \
    Susp the_vals[n_arity];                        \
    TP_ the_nested[n_nesting];                     \
  }
#endif /* GC */

// Macros

#define True 1
#define False 0
#define THE_ARGS(T)   ((byte *) &((T)->data))
#define THE_VALS(T)   (THE_ARGS(T) + (T)->arity * sizeof(LarArg))
#define THE_NESTED(T) (THE_VALS(T) + (T)->arity * sizeof(Susp))

#define ARGS(x, T)    ((ASSERT_GC_EXPR((x) < (T)->arity,                \
                                       "ARGS bounds violation"),        \
                        (LarArg*) THE_ARGS(T))[x])
#define VALS(x, T)    ((ASSERT_GC_EXPR((x) < (T)->arity,              \
                                       "VALS bounds violation"),      \
                        (Susp*) THE_VALS(T))[x])
#define NESTED(x, T)  ((ASSERT_GC_EXPR((x) < (T)->nested,               \
                                       "NESTED bounds violation"),      \
                        (TP_*) THE_NESTED(T))[x])

#define VAR(x)        FUNC(x)
#define FUNC(x)       Susp x(TP_ T0)
#define ACTUAL        T0 = T0->prev

#define GETARG(x, T)  ({                       \
      if (ARGS(x, T) != NULL) {                \
        Susp val = ARGS(x, T)(T);              \
        VALS(x, T) = val;                      \
        ARGS(x, T) = NULL;                     \
      }                                        \
      VALS(x, T);                              \
    })
#define GETSTRICTARG(x, T)  VALS(x, T)
#define GETCBNARG(x, T)     (ARGS(x, T)(T))

#define AR_CAT(A, B) AR_CAT_AUX(A, B)
#define AR_CAT_AUX(A, B) A ## B
#ifdef GC
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
#else
#define AR(n_arity, n_nesting, ...) ({                          \
      TP_ lar = (TP_) MM_alloc(sizeof(T_) +      		\
                               n_arity * sizeof(LarArg) +       \
                               n_arity * sizeof(Susp) +         \
                               n_nesting * sizeof(TP_));        \
      lar->prev = T0;				                \
      lar->arity = n_arity;                                     \
      lar->nesting = n_nesting;                                 \
      AR_CAT(AR_COPY_, n_arity)(lar, 0, ## __VA_ARGS__);        \
      AR_CAT(AR_CLEAR_, n_nesting)(lar, 0);                     \
      lar;                                                      \
    })
#define AR_S(n_arity, n_nesting, ...)                   \
  ((TP_) &((LAR_STRUCT(n_arity, n_nesting))             \
    { T0, n_arity, n_nesting, { __VA_ARGS__ } }))
#endif /* GC */


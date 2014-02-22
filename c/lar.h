/** @file lar.h
   LAR infrastructure, standard representation for use with the semi-space
   garbage collector.
*/

// Header files

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gc.h"

/** Set macro used by LAR-specific C code (such as the garbage collector). */
#define LAR

// Types

typedef unsigned char byte;

typedef struct T_* TP_;

/** The representation of a lazy (suspended) constructor. */
typedef struct Susp {
  /** Constructor ID. */
  int constr;
#ifdef USE_TAGS
  /** Optional data type tag. */
  int tag;
#endif /* USE_TAGS */
  /** Lazy constructor context. */
  TP_ ctxt;
} Susp;

typedef Susp (*LarArg)(TP_);

typedef struct T_ {
  TP_ prev;              // link to parent LAR (also GC forwarded pointer)
  byte arity;            // the number of arguments in this LAR
  byte nesting;          // the number of nesting links
  void* data[];          // the rest of this struct contains:
                         //   - array of args to evaluate (ARGS)
                         //   - computed thunk values (VALS)
                         //   - nested contexts (NESTED)
} T_;

/** A LAR holding a number of thunks and a number of nested fields. */
#define LAR_STRUCT(n_arity, n_nesting)             \
  struct {                                         \
    TP_ prev;                                      \
    byte arity, nesting;                           \
    LarArg the_args[n_arity];                      \
    Susp the_vals[n_arity];                        \
    TP_ the_nested[n_nesting];                     \
  }

// Macros

/** Boolean True. */
#define True 1
/** Boolean False. */
#define False 0
/** The start of the ARGS fields, holding the code pointers of the thunks.
    \param T The LAR. */
#define THE_ARGS(T)   ((byte *) &((T)->data))
/** The start of the VALS fields, holding the results of thunk evaluation.
    \param T The LAR. */
#define THE_VALS(T)   (THE_ARGS(T) + (ARITY(T)) * sizeof(LarArg))
/** The start of the NESTED fields, holding the scrutinee contexts. */
#define THE_NESTED(T) (THE_VALS(T) + (ARITY(T)) * sizeof(Susp))
/** A code pointer for a thunk argument (or 0 is the thunk is evaluated.
    \param x The position of the thunk in the LAR.
    \param T The LAR. */
#define ARGS(x, T)    ((ASSERT_GC_EXPR((x) < (ARITY(T)),		\
                                       "ARGS bounds violation"),        \
                        (LarArg*) THE_ARGS(T))[x])
/** The value of a thunk.
    \param x The position of the thunk in the LAR.
    \param T The LAR. */
#define VALS(x, T)    ((ASSERT_GC_EXPR((x) < (ARITY(T)),	        \
                                       "VALS bounds violation"),        \
                        (Susp*) THE_VALS(T))[x])
/** The value of a nested field.
    \param x The position of the field in the LAR "nested" area.
    \param T The LAR. */
#define NESTED(x, T)  ((ASSERT_GC_EXPR((x) < (NESTING(T)),		\
                                       "NESTED bounds violation"),      \
                        (TP_*) THE_NESTED(T))[x])
/** The C prototype of a LAR formal variable definition. */
#define VAR(x)        FUNC(x)
/** The C prototype of a LAR function definition. */
#define FUNC(x)       Susp x(TP_ T0)
/** The intensional "actuals" operator. */
#define ACTUAL        T0 = (TP_)(GETPREV(T0))

/** Return the value of a thunk (forcing it if it is not evaluated).
    \param x The position of the thunk in the LAR.
    \param T The LAR. */
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

/** LAR constructor for stack allocation.
    \param n_arity The number of thunks passed to the LAR.
    \param n_nesting The nesting depth of the called function.
    \param ... The thunks passed to the called function. */
#define AR(n_arity, n_nesting, ...) ({                          \
      TP_ lar = (TP_) MM_alloc(sizeof(T_) +      		\
                               sizeof(unsigned long) +          \
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
/** LAR constructor for stack allocation.
    \param n_arity The number of thunks passed to the LAR.
    \param n_nesting The nesting depth of the called function.
    \param ... The thunks passed to the called function. */
#define AR_S(n_arity, n_nesting, ...)                   \
  ((TP_) &((LAR_STRUCT(n_arity, n_nesting))             \
    { T0, n_arity, n_nesting, { __VA_ARGS__ } }))

/* *********** Macros of the LAR API *********** */

/** The previous LAR link. */
#define GETPREV(T0)      T0->prev
/** Reads a pointer from a field. */
#define GETPTR(p)        p
/** The arity of a LAR. */
#define ARITY(lar)       lar->arity
/** The nesting depth of a LAR. */
#define NESTING(lar)     lar->nesting
/** Reads the code pointer of a LAR thunk.
    \param x The position of the thunk in the LAR.
    \param T The LAR. */
#define ARGS_FUNC(x, T)  ARGS(x, T)
/** Creates the code pointer of a LAR thunk.
    \param The code pointer to store. */
#define ARGC(arg)        arg
/** Reads a pointer. */
#define GETTPTR(p)         p
/** Returns the constructor part of a thunk. */
#define CONSTR(p)          p.constr

/** Primitive value read/create macros. Isomorphic to nullary constructors. */
#define PRIMVAL_R(p)                   p.constr

#ifdef USE_TAGS
/** Creates a primitive value.
    \param i The integer to store.
    \param tag The data type tag to embed. */
#define PRIMVAL_C(i, tag)              ((Susp) { i, tag, NULL })
#else
/** Creates a primitive value.
    \param i The integer to store. */
#define PRIMVAL_C(i)                   ((Susp) { i, NULL })
#endif /* USE_TAGS */

#ifdef USE_TAGS
/** Thunk constructor: (constructor, tag, ctxt). */
#define SUSP(c, t, p)      ((Susp) {c, t, p})
#else
/** Thunk constructor, ignores the tag 't'. */
#define SUSP(c, t, p)      ((Susp) {c, p})
#endif /* USE_TAGS */

/* ********** Garbage collection ********** */

/** For compatibility with the "compact" representation. */
#define ARINFO(n_arity, n_nesting, prev) prev
#define FORWARDED(p)     ({ printf("FORWARDED(p) missing"); exit(-1); (TP_)0; })
#define IS_FORWARDED(ar) ({ printf("IS_FORWARDED(ar) missing"); exit(-1); (TP_)0; })
#define ARGS_FLAG(x, T)  ARGS(x, T)

#define AR_SIZE(ar)   (sizeof(TP_) + 2 + (ar->arity*(sizeof(LarArg)+sizeof(Susp))) + (ar->nesting*sizeof(TP_)))
#define IS_THUNK(p)      ({ printf("IS_THUNK(ar) missing"); exit(-1); 0; })

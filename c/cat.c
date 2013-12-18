/* 
   The Context Allocation Table (CAT) that each node has in distributed eduction.

   A CAT is a linked list of context descriptors. To save space, no actual descriptor is
   kept in each node, but its address in memory is used instead (cast to the long data type).

   A CAT entry is a tuple {int remove, free_list *next, int i, ErlNifPid whPid, long *tailCtxtId}.
   The first is the GC flag, the second is the linked list link, and the last 3 are the payload
   (call index, warehouse PID, tail context).

   The following functions are implemented:
   - initCAT(): initializes a CAT
   - getNextID(): returns the next free context ID
   - markID(): marks a context as live (to be used during GC)
   - removeIDs(): removes all contexts that are not live
   - getCtxt(id): given a context id, get its contents as a tuple {i, warehouse, tail}

 */

// TODO: do we need the type checking of the Erlang NIFs?
// TODO: wh==0 is never used by the erlang code, can we use it as a special warehouse?

// enable extra debugging code
// #define DEBUG_CAT

#include <stdio.h>
#include <stdlib.h>

#ifdef ERLANG_NIF_C

#include "erl_nif.h"

extern void initCAT(int maxSize);
extern long getNextID(void);
extern void markID(long id);
extern void removeIDs(void);
extern long allocCtxt(int i, ErlNifPid whPid, long tailCtxtId);
extern void* getCtxt(long id);

#else

#define ErlNifEnv    long
#define ERL_NIF_TERM long
// from erl_nif.h:
typedef struct
{
    ERL_NIF_TERM pid;  /* internal, may change */
}ErlNifPid;

int enif_get_int(ErlNifEnv* env , ERL_NIF_TERM i, int  *ip) { *ip=i; return 1; }
int enif_get_local_pid(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifPid* pid) { pid->pid = term; return 1; }
int enif_get_long(ErlNifEnv* env, ERL_NIF_TERM l, long *lp) { *lp=l; return 1; }
ERL_NIF_TERM enif_make_long(ErlNifEnv* env, long int i) { return i; }
ERL_NIF_TERM enif_make_int(ErlNifEnv* env, int i) { return i; }
ERL_NIF_TERM enif_make_pid(ErlNifEnv* env, const ErlNifPid* pid) { return pid->pid; }
ERL_NIF_TERM enif_make_tuple3(ErlNifEnv* env, ERL_NIF_TERM e1, ERL_NIF_TERM e2, ERL_NIF_TERM e3) { printf("enif_make_tuple3 is not usable from the test C code\n"); return 0; }

#endif /* ERLANG_NIF_C */

// The free list structure that keeps all the context entries.
struct free_list {
  int    remove;             // flag used for garbage collection
  struct free_list *next;    // the pointer to the next element in the list
  int    i;                  // payload: call index i
  ErlNifPid whPid;           // payload: warehouse PID
  long   tailId;             // payload: context tail ID
};

typedef struct free_list free_list;

#define MAX_WH 1000

static free_list** cat = 0;
static free_list** catNextFreeID = 0;
static int* catSize;

// Initialize the CAT free list
void c_initCAT(int wh, int maxSize) {
  int i;

  // guard against bad casts
  if (sizeof(long) != sizeof(free_list *)){
    printf("Unsupported platform, sizeof(long) != sizeof(free_list *)\n");
    exit(1);
  }

  if (wh>=MAX_WH) {
    printf("Too many warehouses, cannot create CAT for warehouse %d.\n", wh);
    exit(1);
  }

  printf("===============\n");

  // if this is the first time we are called, initialize pointers
  if (cat==0) {
    cat           = (free_list**)(malloc(MAX_WH * sizeof(free_list*)));
    catNextFreeID = (free_list**)(malloc(MAX_WH * sizeof(free_list*)));
    catSize       = (int*)(malloc(MAX_WH * sizeof(int)));
    printf("Created CAT for wh=%d.\n", wh);
  }

  cat[wh] = (free_list*)malloc(maxSize * sizeof(free_list));
  catSize[wh] = maxSize;
  catNextFreeID[wh] = cat[wh];
  for (i=0; i<maxSize; i++) {
    cat[wh][i].remove = 1;
    if (i==maxSize-1)
      cat[wh][i].next = 0;
    else
      cat[wh][i].next = &cat[wh][i+1];
  }

#ifdef DEBUG_CAT
  printf("Allocated CAT table %d of size %d\n", wh, maxSize);
#endif /* DEBUG_CAT */
  
}

long c_getNextID(int wh) {
  free_list* tmp = catNextFreeID[wh];
  if (catNextFreeID[wh] == 0) {
    printf("No more IDs in warehouse %d, must call garbage collection for the CAT table!\n", wh);
    return 0; // exit(1);
  }
  catNextFreeID[wh] = catNextFreeID[wh]->next;
  return (long)tmp;
}

// Allocate a new context {i, warehouse, tail} in the CAT.
long c_allocCtxt(int wh, int i, ErlNifPid whPid, long tailId) {
  free_list *id = (free_list *)c_getNextID(wh);

  // if no memory was available return 0 to the warehouse, to trigger garbage collection
  if (id==0)
    return 0;
  else {
    id->i = i;
    id->whPid = whPid;
    id->tailId = tailId;
  
#ifdef DEBUG_CAT
    printf("Allocated new context: %ld {%d, %ld, %ld}\n", (long)id, i, whPid.pid, tailId);
#endif /* DEBUG_CAT */
  
    return (long)id;
  }
}

/* Marks a context ID as "not to remove". Called by the garbage collector
   each time it finds a Context that is visible from the roots. */
void c_markID(long id) {
  ((free_list*)id)->remove = 0;
}

/* Recycles all elements of the list that have the 'remove' flag set.
   Should be used after using markID() to mark the elements to preserve.
   Scans the list to find blocks of contiguous elements that must be removed;
   these blocks are moved to the end of the list and are considered free. */
void c_removeIDs(int wh) {
  // the last node that is not to be removed
  free_list *last_kept_node = 0;
  // the node counter and the free/last node of sets of free nodes
  free_list *node, *first_set_node = 0, *last_set_node;
  // the free list that is formed from removed elements
  free_list *freeHead = 0, *freeTail = 0;
  int i;

  // for (node = cat; node!=0; node=node->next) {
  for (i=0; i<catSize[wh]; i++) {
    node = &cat[wh][i];
    // printf("Checking node %p\n", node);
    // node will not be removed, remember it
    if (!(node->remove))
      last_kept_node = node;
    // node belongs to a (contiguous) block of nodes to be removed
    else {
      // check if it is the first node of the free block
      if ((last_kept_node == NULL) || (last_kept_node->next == node))
	first_set_node = node;
      // check if it is the last node of the set (or the list)
      if ((node->next==0) || (!(node->next->remove))) {
	last_set_node = node;
	printf("To-free block found: [%p..%p] after %p\n", (void *)first_set_node, (void *)last_set_node, (void *)last_kept_node);
        // connect the node before the set (if it exists) with the node after the set
        if (last_kept_node!=0)
	  last_kept_node->next = last_set_node->next;
        // add the set to the end of the free list
	if (freeHead==0)
	  freeHead = first_set_node;
	else
	  freeTail->next = first_set_node;
	freeTail = last_set_node;
	freeTail->next = 0;
#ifdef DEBUG_CAT
	free_list *elm;
	printf("Zeroing out the free block...\n");
	// zero out the free block
	for (elm = freeHead; elm->next !=0; elm=elm->next) {
	  elm->i      = 0;
	  elm->whPid.pid  = 0;
	  elm->tailId = 0;
	}
#endif /* DEBUG_CAT */
      }
    }
  }

  // if a list of free ids was created, then add it to the list
  if (freeHead!=0) {
    // if no nodes were kept, the free list is the full list and becomes the CAT
    if (last_kept_node==0)
      cat[wh] = freeHead;
    else
      // else, append it to the current CAT
      last_kept_node->next = freeHead;
    catNextFreeID[wh] = freeHead;
  }
  else {
    printf("ERROR: no local context descriptors could be deallocated, continuing execution might be dangerous.\n");
  }
    
  // in the end, tag all nodes as remove = true to prepare for the next recycling
  for (i=0; i<catSize[wh]; i++)
    cat[wh][i].remove = 1;

}

void c_printList(int wh) {
  free_list* tmp;
  for (tmp = cat[wh]; tmp!=0; tmp=tmp->next)
    printf("%p: {%d, %p, <%d, %ld, %ld>}\n", (void *)tmp, tmp->remove, (void *)tmp->next, tmp->i, tmp->whPid.pid, tmp->tailId);
  printf("\n");
}

#ifdef ERLANG_NIF_C

// Erlang wrapper for allocCtxt
static ERL_NIF_TERM allocCtxt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int i, wh;
  ErlNifPid whpid;
  long tail;

  if (enif_get_int(env, argv[0], &wh)) {
    if (enif_get_int(env, argv[1], &i)) {
      if (enif_get_local_pid(env, argv[2], &whpid)) {
	if (enif_get_long(env, argv[3], &tail))
	  return enif_make_long(env, c_allocCtxt(wh, i, whpid, tail));
	else { printf("ERROR: allocCtxt(), tail malformed\n"); exit(1); }
      }
      else { printf("ERROR: allocCtxt(), whPid malformed\n"); exit(1); }
    }
    else { printf("ERROR: allocCtxt(), index i malformed\n"); exit(1); }
  }
  else { printf("ERROR: allocCtxt(), warehouse wh malformed\n"); exit(1); }
}

// Erlang wrapper for getCtxt
static ERL_NIF_TERM getCtxt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int wh;
  long id;

  if (enif_get_int(env, argv[0], &wh)) {
    if (enif_get_long(env, argv[1], &id)) {
      free_list *ctxt = (free_list *)id;
      return enif_make_tuple3(env, enif_make_int(env, ctxt->i), enif_make_pid(env, &(ctxt->whPid)), enif_make_long(env, ctxt->tailId));
    }
    else { printf("ERROR: getCtxt(), id malformed\n"); exit(1); }
  }
  else { printf("ERROR: getCtxt(), warehouse wh malformed\n"); exit(1); }
}

// Erlang wrapper for initCAT
static ERL_NIF_TERM initCAT_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int maxSize, wh;
  if (enif_get_int(env, argv[0], &wh)) {
    if (enif_get_int(env, argv[1], &maxSize)) {
      c_initCAT(wh, maxSize);
    }
    else { printf("ERROR: initCat(), maxSize malformed\n"); exit(1); }
  }
  else { printf("ERROR: initCat(), warehouse wh malformed\n"); exit(1); }
  return enif_make_int(env, 0);
}

// Erlang wrapper for getNextID
static ERL_NIF_TERM getNextID_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int wh;
  if (enif_get_int(env, argv[0], &wh)) {
    return enif_make_long(env, c_getNextID(wh));
  }
  else { printf("ERROR: getNextID(), warehouse wh malformed\n"); exit(1); }
}

// Erlang wrapper for markID
static ERL_NIF_TERM markID_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  long id;
  if (!enif_get_long(env, argv[0], &id)) {
    return enif_make_badarg(env);
  }
  c_markID(id);
  return enif_make_int(env, 0);
}

// Erlang wrapper for removeIDs
static ERL_NIF_TERM removeIDs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int wh;
  if (enif_get_int(env, argv[0], &wh)) {
    c_removeIDs(wh);
  }
  else { printf("ERROR: removeIDs(), warehouse wh malformed\n"); exit(1); }  
  return enif_make_int(env, 0);
}

static ErlNifFunc nif_funcs[] = { 
    {"allocCtxt" , 4, allocCtxt_nif}
  , {"getCtxt"   , 2, getCtxt_nif}
  , {"getNextID" , 1, getNextID_nif}
  , {"initCAT"   , 2, initCAT_nif}
  , {"markID"    , 2, markID_nif}
  , {"removeIDs" , 1, removeIDs_nif}
};

ERL_NIF_INIT(warehouse, nif_funcs, NULL, NULL, NULL, NULL)

#endif /* ERLANG_NIF_C */

/* main() function for testing */
#ifndef ERLANG_NIF_C
int main(int argc, char **argv) {
  int i;
  long tmp[100];
  int wh = 0;

  printf("test starts\n");

  c_initCAT(wh, 10);
  for (i=0; i<10; i++) {
    tmp[i] = c_getNextID(wh);
    printf("getNextID() returns: %p\n", (void*)tmp[i]);
  }

  c_markID(tmp[0]);
  c_markID(tmp[1]);
  c_markID(tmp[4]);
  c_markID(tmp[6]);
  c_markID(tmp[7]);
  c_markID(tmp[9]);

  printf("Before:\n"); c_printList(wh);
  c_removeIDs(wh);

  printf("After:\n"); c_printList(wh);

  for (i=0; i<4; i++) {
    tmp[i] = c_getNextID(wh);
    printf("getNextID() returns: %p\n", (void*)tmp[i]);
  }

  printf("test ends\n");
  return 0;
}
#endif /* ERLANG_NIF_C */

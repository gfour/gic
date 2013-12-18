-module(warehouse_redis).
-export([spawn_warehouses/2, winit/2, initCAT/2, allocCtxt/4, 
	 getCtxt/2, getNextID/1, markID/2, removeIDs/1]).
-on_load(c_init/0).

% Uncomment for debug messages.
% -define(DEB(Msg, Args), io:fwrite(string:concat("**", Msg), Args)).
-define(DEB(Msg, Args), ok).

% Loads the library of the CAT functions.
c_init() -> ok.
%   ok = erlang:load_nif("/home/gfour/workspace/bencherl/bencherl/bench/queens/data/libcat", 0).
%   ok = erlang:load_nif("./libcat", 0).

% Spawns all the warehouses (to be used at the beginning).
spawn_warehouses(N, MAX_SZ_CTXTS) ->
    case N of
	0 -> [];
	_ ->
	    WPID = spawn(warehouse_redis, winit, [N, MAX_SZ_CTXTS]),
	    [WPID|spawn_warehouses(N-1, MAX_SZ_CTXTS)]
    end.

% The warehouse entry point.
winit(ID, MAX_SZ_CTXTS) ->
    % create warehouse data tables
    WValuesDB = 0,
    % PendingSlotsDB = 1,    
    {ok, WValues} = eredis:start_link("127.0.0.1", 6379, WValuesDB),
    % {ok, PendingSlots} = eredis:start_link("127.0.0.1", 6379, PendingSlotsDB),
    PendingSlots = dict:new(),
    {ok, _} = eredis:q(WValues, ["FLUSHDB"]),
    % {ok, _} = eredis:q(PendingSlots, ["FLUSHDB"]),
    % initCAT(ID, MAX_SZ_CTXTS),
    Ctxts = array:new(MAX_SZ_CTXTS, {fixed, true}),
    ?DEB("Warehouse ~p (~p) started\n", [ID, self()]),
    wrun(ID, Ctxts, 0, WValues, PendingSlots).


% TODO: do we need the ID formal?

%
% The Warehouse process.
%
% The warehouse contains 2 tables: WValues, which stores the computed
% values, and PendingSlots, which contains the pending slots for
% current computations.
% Dictionaries format:
% - WValues: {{identifier, context}, value}
% - PendingSlots: {pid, {identifier, context}}
%
wrun(ID, Ctxts, CID, WValues, PendingSlots) ->
    receive
	% the {gcLock} message has the highest priority
	{gcLock} ->
	    lockWH(ID),
	    gcMode(ID, PendingSlots),
	    io:fwrite("Warehouse ~p finishes GC\n", [ID]),
	    wrun(ID, Ctxts, CID, WValues, PendingSlots);
	{demand, PID, V, C} ->
	    ?DEB("Warehouse ~p: demand from ~p for ~s in ~p...\n", [self(), PID, V, C]),
	    case eredis:q(WValues, ["GET", {V, C}]) of
	    % case dict:find({V, C}, WValues) of
		% TODO: can pattern matching be done faster?
		% if not found, create a new pending slot in both tables
		{ok, undefined} ->
		    ?DEB("not found, creating new slot (~p, {~p, ~p}).\n", [PID, V, C]),
		    {ok, <<"OK">>} = eredis:q(WValues, ["SET", {V, C}, term_to_binary({pending, PID})]),
		    % WValues2 = dict:append({V, C}, {pending, PID}, WValues),
		    PendingSlots2 = dict:append(PID, {wait, V, C}, PendingSlots),
		    % and tell the requesting process to continue
		    ?DEB("...and the original node continues.\n", []),
		    PID ! {continue},
		    wrun(ID, Ctxts, CID, WValues, PendingSlots2);
		{ok, RVal} ->
		    % io:fwrite("RVal=~p\n", [RVal]),
		    case binary_to_term(RVal) of
			{pending, PID2} ->
			    % TODO: DEBUG, remove for better performance
			    if  PID==PID2 ->
				    io:fwrite("*** ERROR: Cycle detected.\n"), erlang:exit(-1);
				true ->
				    ?DEB("Blocked: ~p demands a computation (~p, ~p) of ~p, creating pending {~p, ~p}.\n", [PID, V, C, PID2, PID2, PID]),
				    % create a pending slot for new node (to get unblocked later)
				    PendingSlots2 = dict:append(PID2, {pid, PID}, PendingSlots),
				    wrun(ID, Ctxts, CID, WValues, PendingSlots2)
			    end;
                        % if a value is found, return it to the demanding node
			{val, Val} ->
			    ?DEB("*** Warehouse ~p has a value ~p for ~p.\n", [self(), Val, PID]),
			    % io:fwrite("*** Warehouse ~p has a value ~p for ~p.\n", [self(), Val, PID]),
			    PID ! {notify, leftNode, Val},
			    wrun(ID, Ctxts, CID, WValues, PendingSlots)
		    end
	    end;
	{regval, PID, Val} ->
	    ?DEB("Warehouse: Received a registration demand for (~p, ~p).\n", [PID, Val]),
	    % A message for value registration
	    % case ets:lookup(PendingSlots, PID) of
	    case dict:find(PID, PendingSlots) of
		error ->
		    ?DEB("Warehouse ~p: no pending slots for ~p found.\n", [self(), PID]),
		    wrun(ID, Ctxts, CID, WValues, PendingSlots);
		{ok, PIDSlots} ->
		    ?DEB("Pending slots found: ~p\n", [PIDSlots]),
		    % delete the pending slots
		    % ets:match_delete(PendingSlots, {PID, '_'}),
		    PendingSlots2 = dict:erase(PID, PendingSlots),
		    % ets:select(TAB,[{{'$1', '$2'}, [], ['$$']}]).
		    % deletePending(PendingSlots, PIDSlots),
		    updateWH(PID, WValues, PIDSlots, Val),
		    wrun(ID, Ctxts, CID, WValues, PendingSlots2)
	    end;
	{createCtxt, PID, J, C} ->
	    % call(J) under context C
	    ?DEB("Warehouse ~p: received {createCtxt, ~p, ~p, ~p}\n", [self(), PID, J, C]),
	    % the new context is the triplet of the call index, the 
	    % warehouse PID and the new context index
	    if CID == 2000000 ->  % TODO: hard-coded limit
		    io:fwrite("Must do garbage collection!"),
		    exit(1);
	       true -> true
	    end,
	    % allocate the context in the next slot in the Ctxts table
	    Ctxts2 = array:set(CID, {PID, J, C}, Ctxts),
	    CID2 = CID + 1,
	    PID ! {ctxt, {J, self(), CID}},
	    ?DEB("Context created: {~p, ~p, ~p}\n", [J, self(), CID2]),
	    wrun(ID, Ctxts2, CID2, WValues, PendingSlots);
	%% {createCtxt, PID, J, C} ->
	%%     % call(J) under context C
	%%     ?DEB("warehouse ~p: received {createCtxt, ~p, ~p, ~p}\n", [self(), PID, J, C]),
	%%     % the new context is the triplet of the call index, the 
	%%     % warehouse PID and the new context index
	%%     {J2, PID2, C2} = C,
	%%     I2 = allocCtxt(ID, J2, PID2, C2),
	%%     if I2 == 0 -> theGarbageCollector ! {gcStart, C},
	%% 		  gcMode(ID, PendingSlots),
	%% 		  io:fwrite("***** Garbage collection finished, now what?\n"),
	%% 		  exit(1);
	%%        true    -> PID ! {ctxt, {J, self(), I2}},
	%% 		  ?DEB("Context created: {~p, ~p, ~p}\n", [J, self(), I2])
	%%     end,
	%%     wrun(ID, Ctxts, CID, WValues, PendingSlots);
	{getCtxtTail, PID, CtxtID} ->
	    ?DEB("Looking in the warehouse CAT for ctxt=~p\n", [CtxtID]),
	    {_, _, CtxtTail} = array:get(CtxtID, Ctxts),
	    % io:fwrite("CtxtTail = ~p\n", [CtxtTail]),
	    PID ! {ctxtTail, CtxtTail},
	    wrun(ID, Ctxts, CID, WValues, PendingSlots)
	%% {getCtxtTail, PID, CtxtID} ->
	%%     CtxtTail = getCtxt(ID, CtxtID),
	%%     % io:fwrite("CtxtTail = ~p\n", [CtxtTail]),
	%%     PID ! {ctxtTail, CtxtTail},
	%%     wrun(ID, Ctxts, CID, WValues, PendingSlots);
    end.

% Updates the warehouse memoization dictionary and returns the updated dictionary.
updateWH(PID, WValues, PIDSlots, Val) ->
    case PIDSlots of
	[] ->
	    ?DEB("No more Pid slots.\n", []),
	    WValues;
	% A slot waiting for the value of <V, C> from PID
	[{wait, V, C} | XS] ->
	    ?DEB("updated waiting slot ([~p, ~p] -> ~p)\n", [V, C, Val]),
	    eredis:q(WValues, ["SET", {V, C}, term_to_binary({val, Val})]),
	    % WValues2 = dict:update({V, C}, fun (_) -> Val end, WValues),
	    % unblock updated node
	    PID ! {notify, leftNode, Val},
	    updateWH(PID, WValues, XS, Val);
	% A slot of PID waiting for some value from another node
	[{pid, PIDB} | XS] ->
	    ?DEB("Unblocking node ~p.\n", [PIDB]),
	    PIDB ! {notify, leftNode, Val},
	    updateWH(PID, WValues, XS, Val)
        end.

initCAT(_, _) ->
    io:fwrite("ERROR: initCAT stub version reached\n"),
    exit(nif_library_not_loaded).

getCtxt(_, _) ->
    io:fwrite("ERROR: getCtxt stub version reached\n"),
    exit(nif_library_not_loaded).

getNextID(_) ->
    io:fwrite("ERROR: getNextID stub version reached\n"),
    exit(nif_library_not_loaded).

markID(_, _) ->
    io:fwrite("ERROR: markID stub version reached\n"),
    exit(nif_library_not_loaded).

removeIDs(_) ->
    io:fwrite("ERROR: removeIDs stub version reached\n"),
    exit(nif_library_not_loaded).

allocCtxt(_, _, _, _) ->
    io:fwrite("ERROR: allocCtxt stub version reached\n"),
    exit(nif_library_not_loaded).

% The event loop of garbage collection in warehouse processes.
gcMode(ID, PendingSlots) ->
    receive
	{gcLock} -> 
	    lockWH(ID),
	    gcMode(ID, PendingSlots);
	{gcFindRoots} ->
	    io:fwrite("Scanning PendingSlots of wh#~p for roots...\n", [ID]),
	    Roots = ets:match(PendingSlots, {'_', {'_', '$1'}}),
	    io:fwrite("wh#~p roots: ~p\n", [ID, Roots]),
	    theGarbageCollector ! {gcRoots, self(), Roots},
	    gcMode(ID, PendingSlots);
	{gcMark, Roots} ->
	    io:fwrite("TODO: marking phase in warehouse ~p\n", [ID]),
	    gcMode(ID, PendingSlots)
	% M        -> io:fwrite("ERROR: gcMode received ~p\n", [M])
    end.
    
lockWH(ID) ->
    io:fwrite("wh#~p: locked\n", [ID]),
    theGarbageCollector ! {gcLockOK, self()}.

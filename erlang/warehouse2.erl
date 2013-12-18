% The warehouse processes of the parallel eduction interpreter.

-module(warehouse2).
-export([magic/1, spawn_warehouses/1, warehouses_num/0, winit/0]).

% The number of the warehouses during execution.
warehouses_num() -> 20.

% Spawns all the warehouses (to be used at the beginnig).
spawn_warehouses(N) ->
    case N of
	0 -> [];
	_ ->
	    WPID = spawn(warehouse2, winit, []),
	    [WPID|spawn_warehouses(N-1)]
    end.

% The warehouse entry point.
winit() ->
    % create warehouse data tables
    WValues = ets:new(w_values, []),
    PendingSlots = ets:new(pending_slots, [bag]),
    deb("Warehouse ~p started\n", [self()]),
    wrun(WValues, PendingSlots).

deb(_, _) -> true.
% deb(Msg, Args) -> io:fwrite(string:concat("**", Msg), Args).

%
% The Warehouse actor.
%
% The warehouse contains 2 tables: WValues, which stores the computed
% values, and PendingSlots, which contains the pending slots for
% current computations.
% Tables format:
% - WValues: {{identifier, context}, value}
% - PendingSlots: {pid, {identifier, context}}
%
wrun(WValues, PendingSlots) ->
    % deb("Warehouse: waiting for messages...\n", []),
    receive
	{demand, PID, V, C} ->
	    deb("Warehouse: got a demand from ~p for ~s under ~p...\n", [PID, V, C]),
	    case ets:lookup(WValues, {V, C}) of
		% if not found, create a new pending slot in both tables
		[] ->
		    deb("not found, creating new slot (~p, {~p, ~p}).\n", [PID, V, C]),
		    ets:insert(WValues, {{V, C}, {pending, PID}}),
		    ets:insert(PendingSlots, {PID, {V, C}}),
		    % and tell the requesting process to continue
		    deb("...and the original node continues.\n", []),
		    PID ! {continue};
		% if already pending, let the demanding node wait
		% (but record the nodes dependency in the pending slots)
		[{_, {pending, PID2}}] ->
		    % DEBUG, remove for better performance
		    %% if
		    %% 	PID==PID2 -> io:fwrite("*** Cycle detected.\n"), erlang:exit(-1);
		    %% 	true ->
			    deb("Blocked: ~p demands a computation (~p, ~p) of ~p, creating pending {~p, ~p}.\n", [PID, V, C, PID2, PID2, PID]),
			    % create a pending slot for this new node (so that it will get unblocked later)
			    ets:insert(PendingSlots, {PID2, PID});
		    %% end;
		% if a value is found, return it to the demanding node
		[{_, Val}] ->
		    deb("*** Warehouse has a value ~p for ~p.\n", [Val, PID]),
		    PID ! {notify, leftNode, Val}
	    end;
	{regval, PID, Val} ->
	    deb("Warehouse: Received a registration demand for (~p, ~p).\n", [PID, Val]),
	    % A message for value registration
	    case ets:lookup(PendingSlots, PID) of
		[] -> deb("No pending slots for ~p found.\n", [PID]);
		PIDSlots ->
		    deb("Pending slots found: ~p\n", [PIDSlots]),
		    % delete the pending slots
		    ets:match_delete(PendingSlots, {PID, '_'}),
		    % ets:select(TAB,[{{'$1', '$2'}, [], ['$$']}]).
		    % deletePending(PendingSlots, PIDSlots),
		    updateWH(WValues, PIDSlots, Val)
	    end
    end,
    wrun(WValues, PendingSlots).

updateWH(WValues, PIDSlots, Val) ->
    deb("WH: UPDATE-START (Val: ~p)... \n", [Val]),
    case PIDSlots of
	[] -> deb("No more Pid slots.\n", []);
	% A slot waiting for the value of <V, C> from PID
	[{PID, {V, C}} | XS] ->
	    deb("updated waiting slot ([~p, ~p] -> ~p)\n", [V, C, Val]),
	    ets:insert(WValues, {{V, C}, Val}),
	    % unblock updated node
	    PID ! {notify, leftNode, Val},
	    % case ets:lookup(WValues, {V, C}) of
	    %	X2 -> deb("DEBUG: Does it exist? ~p", [X2])
	    % end,
	    updateWH(WValues, XS, Val);
	% A slot of PID waiting for some value from another node
	[{_, PID} | XS] ->
	    deb("Unblocking node ~p.\n", [PID]),
	    PID ! {notify, leftNode, Val},
	    updateWH(WValues, XS, Val)
        end,
    deb("UPDATE-END.\n", []).

% Chooses the warehouse to hit, according to the context.
% The (+1) is due to the 1-based Erlang list indices.
magic(C) ->
    (length(C) rem warehouse2:warehouses_num()) + 1.

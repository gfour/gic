% -module(main).
% -export([eval/2, p/0, lookup/3, run/5, init/0]).

%% TODO: Dictionary lookup
%% lookupCtxt(D, C) ->
%%     case D of
%% 	[] ->
%% 	    io:fwrite("Dictionary lookup failed for context ~p \n", [C]),
%% 	    erlang:error("unrecoverable error");
%% 	[{C, SC1}|_] -> 
%% 	    % io:fwrite("Dict lookup succeeded for context ~p \n", [C]),
%% 	    SC1;
%% 	[_|D2] -> lookupCtxt(D2, C)
%%     end.
	     
leq(I, J)   -> if I=<J -> 1; true -> 0 end.
geq(I, J)   -> if I>=J -> 1; true -> 0 end.
lthan(I, J) -> if I<J -> 1; true -> 0 end.
gthan(I, J) -> if I>J -> 1; true -> 0 end.
equ(I, J)   -> if I==J -> 1; true -> 0 end .

% From: http://erlang.org/pipermail/erlang-questions/2008-April/034596.html
floorDiv(A, B) -> 
    Quotient = A / B,
    Truncated = trunc(Quotient),
    case Truncated > Quotient of
	true -> Truncated - 1;
	false -> Truncated
    end.

% boink() -> erlang:exit(12345).

runLookup(V, C, Parent, Branch, W, Wpend) ->
    case p(V) of
	{def, Expr} -> run(Expr, C, Parent, Branch, W, Wpend);
        Actual ->
	    [{M, Idx}|C2] = C,
	    run(lists:nth(Idx+1, Actual(M)), C2, Parent, Branch, W, Wpend)
    end.

% The main message handler of expression nodes.	
run(E, C, Parent, Branch, W, Wpend) ->
    case E of
	{call, I, E1} -> run(E1, [I|C], Parent, Branch, W, Wpend);
	%% {actuals, E1} ->
	%%     io:fwrite("Invalid actuals() reached, ~p\n", [E1]);
	%%     %% run(E1, tl(C), Parent, Branch, W, Wpend);
	{id, V} ->
	    W_idx = warehouse2:magic(C),
	    % io:fwrite("Demanding (~p, ~p) from (WH#~p): \n", [V, C, W_idx]),
	    % Find the warehouse to use
	    W_used = lists:nth(W_idx, W),
	    % Send a demand to the warehouse
	    W_used ! {demand, self(), V, C},
	    % io:fwrite("Waiting for warehouse reply...\n"),
	    receive
		{notify, _, Val} ->
		    %% case Branch of
		    %% 	rightNode ->
		    % io:fwrite("~p (~p) notifies parent ~p for value ~p\n", [self(), Branch, Parent, Val]),
		    regval_wh(Wpend, self(), Val),
		    Parent ! {notify, Branch, Val};
		    %% 	_ -> Val
		    %% end;
		{continue} ->
		    % Add the warehouse to update to the pending warehouses
		    Wpend2 = [W_used|Wpend],
		    runLookup(V, C, Parent, Branch, W, Wpend2)
	    end;
%% TODO: Data types handling
%% 	{bid, V} ->
%% 	    % io:fwrite("bound variable ~p at context ~p \n", [V, C]),
%% 	    C1 = lookupCtxt(D, C),
%% 	    run({id, V}, C1, Parent, Branch, W, Wpend);
%% OB	{match, E1, Pats} ->
%% 	    S = self(),
%% 	    % io:fwrite("entered pattern matching at ~p\n", [C]),
%% 	    spawn(main, run, [E1, C, D, C, S, leftNode, W, []]),
%% 	    receive
%% 		{notify, _, {Constr, Dict}} ->
%% 		    BranchE = Pats(Constr),
%% 		    spawn(main, run, [BranchE, C, Dict, SC, S, rightNode, W, Wpend]),
%% 		    receive 
%% 			{notify, _, ValE} -> 
%% 			    regval_wh(Wpend, self(), ValE),
%% 			    Parent ! {notify, Branch, ValE}
%% 		    end
%% 	    end;
%% 	{thunk, Constr} ->
%%             % register value in the warehouse(s)
%% 	    NewThunk = {Constr, [{SC, C}|D]},
%% 	    regval_wh(Wpend, self(), NewThunk),
%% 	    % io:fwrite("~p: reached thunk ~p\n", [self(), NewThunk]),
%% 	    % tell the parent execution is over with a thunk value
%% 	    Parent ! {notify, Branch, NewThunk};
	{val, Val} ->
            % register value in the warehouse(s)
	    regval_wh(Wpend, self(), Val),
	    % io:fwrite("~p: found value ~p \n", [self(), Val]),
	    % tell the parent execution is over with a value
	    Parent ! {notify, Branch, Val};
	{'+', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receivePlusMerge(Parent, Branch, Wpend);
	{'-', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveMinusMerge(Parent, Branch, Wpend);
	{'*', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveMultMerge(Parent, Branch, Wpend);
	{'==', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveEquMerge(Parent, Branch, Wpend);
	{'<=', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveLeMerge(Parent, Branch, Wpend);
	{'>=', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveGeMerge(Parent, Branch, Wpend);
	{'>', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveGtMerge(Parent, Branch, Wpend);
	{'<', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveLtMerge(Parent, Branch, Wpend);
	{'div', E1, E2} ->
	    S = self(),
	    spawn(main, run, [E1, C, S, leftNode, W, []]),
	    spawn(main, run, [E2, C, S, rightNode, W, []]),
	    receiveDivMerge(Parent, Branch, Wpend);
	{'iff', E0, E1, E2} ->
	    S = self(),
	    spawn(main, run, [E0, C, S, leftNode, W, []]),
	    receive
		{notify, _, Val} ->
		    case Val of
			1 -> spawn(main, run, [E1, C, S, rightNode, W, Wpend]),
			     receive 
				 {notify, _, ValE} -> 
				     regval_wh(Wpend, self(), ValE),
				     Parent ! {notify, Branch, ValE}
			     end;
			0 -> spawn(main, run, [E2, C, S, rightNode, W, Wpend]),
			     receive 
				 {notify, _, ValE} -> 
				     regval_wh(Wpend, self(), ValE),
				     Parent ! {notify, Branch, ValE}
			     end
		    end
	    end
    end.

% go into receive loop, the nested receive clauses
% and the "if" statement take care of message order
receivePlusMerge(Parent, Branch, Wpend) -> 
    receive
	{notify, _, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = V1 + V2,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveMinusMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val =
V1 - V2,
%			case B1 of
%			    leftNode -> V1 - V2; % received in order
%			    rightNode -> V2 - V1
%			end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.
	
receiveMultMerge(Parent, Branch, Wpend) -> 
    receive
	{notify, _, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = V1 * V2,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveLeMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = leq(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveGeMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = geq(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveEquMerge(Parent, Branch, Wpend) ->
    receive
	{notify, _, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = equ(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveGtMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = gthan(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveLtMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = lthan(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

receiveDivMerge(Parent, Branch, Wpend) ->
    receive
	{notify, leftNode, V1} ->
	    receive
		{notify, _, V2} ->
		    Val = floorDiv(V1, V2),
			%% case B1 of
			%%     leftNode -> leq(V1, V2); % received in order
			%%     rightNode -> leq(V2, V1)
			%% end,
		    Parent ! {notify, Branch, Val},
		    regval_wh(Wpend, self(), Val)
	    end
    end.

% a notification from an actor to the warehouses waiting
% for its value
regval_wh(Wpend, PID, Val) ->
    case Wpend of
	[] ->
	     true;
	[W|WS] ->
	    W ! {regval, self(), Val},
	    regval_wh(WS, PID, Val)
    end.

init() ->
    {_, T1, S1} = erlang:now(),
    WPIDs = warehouse2:spawn_warehouses(warehouse2:warehouses_num()),
    Wpend = [], %wh_pend_empty(),
    % start evaluation
    run({id, resultName()}, [], self(), rightNode, WPIDs, Wpend),
    % wait for result and show statistics
    receive
	{notify, _, Val} ->
	    {_, T2, S2} = erlang:now(),
	    io:fwrite("result = ~p\n", [Val]),
	    io:fwrite("time = ~p\n", [(T2 - T1)+((S2-S1)/1000000)])
    end.

%% % The main eval() function (sequential version, jfp1).
%% % For reference, not used in the actors implementation.
%% eval(E, C) ->
%%   case E of
%% 	{id, V} -> eval(lookup(V, C), C);
%% 	{call, I, E1} -> eval(E1, [I|C]);
%% 	{actuals, _, E1} -> eval(E1, tl(C));
%% 	{val, Val} -> Val;
%% 	{'+', E1, E2} -> eval(E1, C) + eval(E2, C);
%% 	{'-', E1, E2} -> eval(E1, C) - eval(E2, C);
%% 	{'*', E1, E2} -> eval(E1, C) * eval(E2, C);
%% 	{'<=', E1, E2} -> leq(eval(E1, C), eval(E2, C));
%% 	{iff, E0, E1, E2} ->
%% 		case eval(E0, C) of
%% 			1 -> eval(E1, C);
%% 			0 -> eval(E2, C)
%% 		end
%% 	end.

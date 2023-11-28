-module(normal_node).
-export([start/0, init/1]).

start() -> spawn(node(), fun() -> waiting_loop() end).

waiting_loop() ->
	receive
		{init, Type, PID_List} ->
			if
				Type == 1 -> builder_node:init(PID_List);
				Type == 0 -> normal_node:init(PID_List)
			end
	end.

init(PID_List) -> normal_loop(PID_List, []).

normal_loop(Nodes, BlockChain) ->
	receive
		{block, NewBlock} ->
			io:format("[normal] ~p has received block #~p!~n", [self(), length(BlockChain)+1]),
			normal_loop(Nodes, lists:append(BlockChain, [NewBlock]));
		{stop} ->
			utils:print_all_blocks(BlockChain),
			io:format("[normal] ~p finished.~n", [self()])
	end.


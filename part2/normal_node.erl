-module(normal_node).
-export([start/0, init/2]).

-record(nodes, {builder, validators, proposers, normal}).

start() -> spawn(node(), fun() -> waiting_loop() end).

waiting_loop() ->
	receive
		{init, Type, PID_List, Nodes} -> % Nodes is of type #nodes{validators,proposers,builder,normal}
			if
				Type == 0 -> normal_node:init(PID_List, Nodes);
				Type == 1 -> builder_node:init(PID_List, Nodes);
				Type == 2 -> validator_node:init(PID_List, Nodes)
			end
	end.

init(PID_List, Nodes) -> normal_loop(PID_List, Nodes, []).

normal_loop(PID_List, Nodes, BlockChain) ->
	Builder = Nodes#nodes.builder,
	receive
		{block, NewBlock, From} ->
			if
				From == Builder ->
					normal_loop(PID_List, Nodes, lists:append(BlockChain, [NewBlock]));
				true ->
					normal_loop(PID_List, Nodes, BlockChain)
			end;
		{stop, From} ->
			if
				From == Builder ->
					utils:print_all_blocks(BlockChain),
					io:format("[~p] (normal) finished~n", [self()]);
				true ->
					normal_loop(PID_List, Nodes, BlockChain)
			end
	end.


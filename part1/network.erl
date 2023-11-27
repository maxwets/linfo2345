-module(network).
-export([init/1]).

init(N) ->
	Nodes = init_nodes(N),
	start_nodes(Nodes).

init_nodes(N) ->
	tail_init_nodes(N, []).

tail_init_nodes(N, Acc) ->
	if
		N == 0 -> Acc;
		N == 1 -> NewNode = pos_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc));
		N > 1 -> NewNode = pos_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc))
	end.

start_nodes(Nodes) -> tail_start_nodes(Nodes, Nodes).

tail_start_nodes(NodesToInitialize, NodeList) ->
	case NodesToInitialize of
		[H] ->
			H ! {init, 1, NodeList};
		[H|T] ->
			H ! {init, 0, NodeList},
			tail_start_nodes(T, NodeList)
	end.

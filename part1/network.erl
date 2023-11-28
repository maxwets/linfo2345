-module(network).
-export([init/1]).

init(N) ->
	NormalNodes = init_nodes(N-1),
	BuilderNode = init_node(),
	PID_List = lists:append(NormalNodes, [BuilderNode]),
	start_builder(BuilderNode, PID_List),
	start_normal(NormalNodes, PID_List).

init_node() -> normal_node:start().
init_nodes(N) -> tail_init_nodes(N, []).
tail_init_nodes(N, Acc) ->
	if
		N == 0 -> Acc;
		N == 1 -> NewNode = normal_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc));
		N > 1 -> NewNode = normal_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc))
	end.

start_builder(BuilderNode, PID_List) ->
		BuilderNode ! {init, 1, PID_List}.

start_normal(NormalNodes, PID_List) ->
	case NormalNodes of
		[H] -> H ! {init, 0, PID_List};
		[H|T] -> H ! {init, 0, PID_List}, start_normal(T, PID_List)
	end.

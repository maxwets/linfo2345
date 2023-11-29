-module(network).
-export([init/2]).

-record(nodes, {builder, validators, proposers, normal}).

init(V, N) ->
	ValidatorNodes = init_nodes(V),
	NormalNodes = init_nodes(N-1),
	BuilderNode = init_node(),
	ProposerNodes = utils:f10p(ValidatorNodes),
	Nodes = #nodes{
						 builder=BuilderNode,
						 validators=ValidatorNodes,
						 proposers=ProposerNodes,
						 normal=NormalNodes
						},
	io:format("[network] Validator nodes are ~w~n", [ValidatorNodes]),
	Temp = lists:append([BuilderNode], NormalNodes),
	PID_List = lists:append(ValidatorNodes, Temp),
	start_validators(ValidatorNodes, PID_List, Nodes),
	start_builder(BuilderNode, PID_List, Nodes),
	start_normal(NormalNodes, PID_List, Nodes).

start_validators(ValidatorNodes, PID_List, Nodes) ->
	case ValidatorNodes of
		[H] -> H ! {init, 2, PID_List, Nodes};
		[H|T] -> H ! {init, 2, PID_List, Nodes}, start_validators(T, PID_List, Nodes)
	end.

start_builder(BuilderNode, PID_List, Nodes) ->
		BuilderNode ! {init, 1, PID_List, Nodes}.

start_normal(NormalNodes, PID_List, Nodes) ->
	case NormalNodes of
		[H] -> H ! {init, 0, PID_List, Nodes};
		[H|T] -> H ! {init, 0, PID_List, Nodes}, start_normal(T, PID_List, Nodes)
	end.

init_node() -> normal_node:start().
init_nodes(N) -> tail_init_nodes(N, []).
tail_init_nodes(N, Acc) ->
	if
		N == 0 -> Acc;
		N == 1 -> NewNode = normal_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc));
		N > 1 -> NewNode = normal_node:start(), tail_init_nodes(N-1, lists:append([NewNode], Acc))
	end.



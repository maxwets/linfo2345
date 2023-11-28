-module(validator_node).
-export([init/2, validator_loop/3, validator_change_epoch_loop/3]).

-record(nodes, {builder, validators, proposers, normal}).

init(PID_List, Nodes) ->
	IsMainProposer = lists:nth(1, Nodes#nodes.proposers) == self(),
	OtherProposers = lists:nthtail(1, [Nodes#nodes.proposers]),
	IsProposer = lists:member(self(), OtherProposers),
	if
		IsMainProposer -> proposer_node:main_proposer_loop(PID_List, Nodes, 0, []);
		IsProposer -> proposer_node:proposer_loop(PID_List, Nodes, []);
		true -> validator_loop(PID_List, Nodes, [])
	end.

validator_loop(PID_List, Nodes, BlockChain) ->
	MainProposer = lists:nth(1, Nodes#nodes.proposers),
	Builder = Nodes#nodes.builder,
	receive
		{block, NewBlock, From} ->
			if
				From == Builder ->
					validator_loop(PID_List, Nodes, lists:append(BlockChain, [NewBlock]));
				true ->
					validator_loop(PID_List, Nodes, BlockChain)
			end;

		{epoch_end, From} ->
			if
				From == MainProposer ->
					io:format("[~p] (validator) received an {epoch_end} message from ~p~n", [self(), From]),
					validator_change_epoch_loop(PID_List, Nodes, BlockChain);
				true ->
					validator_loop(PID_List, Nodes, BlockChain)
			end;

		{stop, From} ->
			if
				From == Builder ->
					%io:format("[~p] (validator) finished~n", [self()]),
					utils:print_all_blocks(BlockChain);
				true ->
					validator_loop(PID_List, Nodes, BlockChain)
			end
	end.

validator_change_epoch_loop(PID_List, Nodes, BlockChain) ->
	PreviousMainProposer = lists:nth(1, Nodes#nodes.proposers),
	receive
		{validators, List, From} ->
			ShuffledValidators = utils:shuf(List),
			NextInList = lists:nth(1, lists:delete(self(), ShuffledValidators)),
			io:format("[~p] (validator) received shuffled list ~w from ~p and will now send ~w to ~p~n", [self(), List, From, ShuffledValidators, NextInList]),
			NextInList ! {validators, ShuffledValidators, self()},
			validator_change_epoch_loop(PID_List, Nodes, BlockChain);

		{proposers, List, From} ->
			if
				From == PreviousMainProposer ->
					io:format("[~p] (validator) received proposer list ~w from ~p~n", [self(), List, From]),
					IsNewMainProposer = lists:nth(1, List) == self(),
					OtherProposers = lists:nthtail(1, List),
					IsInProposers = lists:member(self(), OtherProposers),
					if
						IsNewMainProposer ->
							io:format("[~p] (validator) will now become the new main proposer~n", [self()]),
							proposer_node:main_proposer_loop(PID_List,
																	#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
																	0,
																	BlockChain
																);

						IsInProposers ->
							io:format("[~p] (validator) will now become part of the proposer group~n", [self()]),
							proposer_node:proposer_loop(PID_List,
														#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														BlockChain
													 );

						true ->
							io:format("[~p] (validator) was not elected as part of the proposer group~n", [self()]),
							validator_loop(PID_List,
														 #nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														 BlockChain
														)

					end;

				true ->
					validator_change_epoch_loop(PID_List, Nodes, BlockChain)
			end
	end.

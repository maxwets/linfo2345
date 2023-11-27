-module(validator_node).
-export([init/2]).

-record(nodes, {builder, validators, proposers, normal}).

init(PID_List, Nodes) ->
	IsMainProposer = lists:nth(1, Nodes#nodes.proposers) == self(),
	OtherProposers = lists:nthtail(1, [Nodes#nodes.proposers]),
	IsProposer = lists:member(self(), OtherProposers),
	if
		IsMainProposer -> main_proposer_loop(PID_List, Nodes, 0, []);
		IsProposer -> proposer_loop(PID_List, Nodes, []);
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
					validator_change_epoch_loop(PID_List, Nodes, BlockChain);
				true ->
					validator_loop(PID_List, Nodes, BlockChain)
			end;
		{stop, From} ->
			if
				From == Builder ->
					io:format("Node ~p finishing...~n", [self()]),
					utils:print_all_blocks(BlockChain);
				true ->
					validator_loop(PID_List, Nodes, BlockChain)
			end
	end.

proposer_loop(PID_List, Nodes, BlockChain) ->
	MainProposer = lists:nth(1, Nodes#nodes.proposers),
	Builder = Nodes#nodes.builder,
	receive
		{block, NewBlock, From} ->
			if
				From == Builder ->
					proposer_loop(PID_List, Nodes, lists:append(BlockChain, [NewBlock]));
				true ->
					proposer_loop(PID_List, Nodes, BlockChain)
			end;
		{epoch_end, From} ->
			if
				From == MainProposer ->
					validator_change_epoch_loop(PID_List, Nodes, BlockChain);
				true ->
					proposer_loop(PID_List, Nodes, BlockChain)
			end;
		{stop, From} ->
			if
				From == Builder ->
					io:format("Node ~p finishing...~n", [self()]),
					utils:print_all_blocks(BlockChain);
				true ->
					proposer_loop(PID_List, Nodes, BlockChain)
			end
	end.

main_proposer_loop(PID_List, Nodes, EpochCount, BlockChain) ->
	Builder = Nodes#nodes.builder,
	if
		EpochCount < 10 ->
			receive
				{block, NewBlock, From} ->
					io:format("[main_proposer] ~p received block, epoch = ~p~n", [self(), EpochCount]),
					if
						From == Builder ->
							io:format("[main_proposer] ~p got block from correct builder~n", [self()]),
							Builder ! {continue, self()},
							main_proposer_loop(PID_List, Nodes, EpochCount+1, lists:append(BlockChain, [NewBlock]));
						true ->
							io:format("[main_proposer] ~p got block from incorrect builder~n", [self()]),
							main_proposer_loop(PID_List, Nodes, EpochCount,BlockChain)
					end
			end;

		EpochCount == 10 ->
			utils:broadcast(PID_List, {epoch_end, self()}),
			ShuffledValidators = utils:shuf(Nodes#nodes.validators),
			NextInList = lists:nth(1, lists:delete(self(), ShuffledValidators)),
			io:format("[main_proposer_loop] ~p is going to send the shuffled list: ~w to ~p~n", [self(), ShuffledValidators, NextInList]),
			NextInList ! {validators, ShuffledValidators, self()},
			main_proposer_change_epoch_loop(PID_List, Nodes, BlockChain)
	end.

validator_change_epoch_loop(PID_List, Nodes, BlockChain) ->
	PreviousMainProposer = lists:nth(1, Nodes#nodes.proposers),
	receive

		{validators, List, From} ->
			ShuffledValidators = utils:shuf(List),
			NextInList = lists:nth(1, lists:delete(self(), ShuffledValidators)),
			io:format("[validator_change_epoch_loop] ~p received this shuffled list ~w from ~p and will now send ~w to ~p~n", [self(), List, From, ShuffledValidators, NextInList ]),
			NextInList ! {validators, ShuffledValidators, self()},
			validator_change_epoch_loop(PID_List, Nodes, BlockChain);

		{proposers, List, From} ->
			io:format("[validator_change_epoch_loop] ~p received this list of proposers: ~w from ~p~n", [self(), List, From]),
			if
				From == PreviousMainProposer ->
					IsNewMainProposer = lists:nth(1, List) == self(),
					OtherProposers = lists:nthtail(1, List),
					IsInProposers = lists:member(self(), OtherProposers),
					if
						IsNewMainProposer ->
							io:format("[validator_change_epoch_loop] ~p will now become the main proposer~n", [self()]),
							main_proposer_loop(PID_List,
																	#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
																	0,
																	BlockChain
																);

						IsInProposers ->
							io:format("[validator_change_epoch_loop] ~p will now become a member of the proposer group~n", [self()]),
							proposer_loop(PID_List,
														#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														BlockChain
													 );

						true ->
							validator_loop(PID_List,
														 #nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														 BlockChain
														)

					end;

				true ->
					io:format("[validator_change_epoch_loop] ~p received ~w from ~p while the previous main proposer is ~p~n", [self(), List, From, PreviousMainProposer]),
					validator_change_epoch_loop(PID_List, Nodes, BlockChain)
			end
	end.

main_proposer_change_epoch_loop(PID_List, Nodes, BlockChain) ->
	receive
		{validators, List, From} ->
			io:format("[main_proposer_change_epoch_loop] ~p received this list of validators: ~w from ~p~n", [self(), List, From]),
			NewProposers = utils:f10p(List),
			utils:broadcast(PID_List, {proposers, NewProposers, self()}),
			validator_change_epoch_loop(PID_List, Nodes, BlockChain)
	end.


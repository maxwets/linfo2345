-module(proposer_node).
-export([proposer_loop/3, main_proposer_loop/4]).

-record(nodes, {builder, validators, proposers, normal}).

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
					io:format("[~p] (proposer) received an {epoch_end} message from ~p~n", [self(), From]),
					proposer_change_epoch_loop(PID_List, Nodes, BlockChain);
				true ->
					proposer_loop(PID_List, Nodes, BlockChain)
			end;

		{stop, From} ->
			if
				From == Builder ->
					%io:format("[~p] (proposer) finished~n", [self()]),
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
					if
						From == Builder ->
							io:format("[~p] (main_proposer) received a block from ~p (epoch=~p)~n", [self(), From, EpochCount]),
							Builder ! {continue, self()},
							main_proposer_loop(PID_List, Nodes, EpochCount+1, lists:append(BlockChain, [NewBlock]));
						true ->
							main_proposer_loop(PID_List, Nodes, EpochCount,BlockChain)
					end;
				{stop, From} ->
					if
						From == Builder ->
							%io:format("[~p] (main_proposer) finished~n", [self()]),
							utils:print_all_blocks(BlockChain);
						true ->
							main_proposer_loop(PID_List, Nodes, BlockChain, EpochCount)
					end
			end;

		EpochCount == 10 ->
			io:format("[~p] (main_proposer) broadcasting {epoch_end}~n", [self()]),
			utils:broadcast(PID_List, {epoch_end, self()}),
			ShuffledValidators = utils:shuf(Nodes#nodes.validators),
			NextInList = lists:nth(1, lists:delete(self(), ShuffledValidators)),
			io:format("[~p] (main_proposer) sending shuffled list ~w to ~p~n", [self(), ShuffledValidators, NextInList]),
			NextInList ! {validators, ShuffledValidators, self()},
			main_proposer_change_epoch_loop(PID_List, Nodes, BlockChain)
	end.

proposer_change_epoch_loop(PID_List, Nodes, BlockChain) ->
	PreviousMainProposer = lists:nth(1, Nodes#nodes.proposers),
	receive
		{validators, List, From} ->
			ShuffledValidators = utils:shuf(List),
			NextInList = lists:nth(1, lists:delete(self(), ShuffledValidators)),
			io:format("[~p] (proposer) received shuffled list ~w from ~p and will now send ~w to ~p~n", [self(), List, From, ShuffledValidators, NextInList]),
			NextInList ! {validators, ShuffledValidators, self()},
			proposer_change_epoch_loop(PID_List, Nodes, BlockChain);

		{proposers, List, From} ->
			if
				From == PreviousMainProposer ->
					io:format("[~p] (proposer) received proposer list ~w from ~p~n", [self(), List, From]),
					IsNewMainProposer = lists:nth(1, List) == self(),
					OtherProposers = lists:nthtail(1, List),
					IsInProposers = lists:member(self(), OtherProposers),
					if
						IsNewMainProposer ->
							io:format("[proposer_change_epoch_loop] ~p will now become the main proposer~n", [self()]),
							main_proposer_loop(PID_List,
																	#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
																	0,
																	BlockChain
																);

						IsInProposers ->
							io:format("[~p] (proposer) will now become the new main proposer~n", [self()]),
							proposer_loop(PID_List,
														#nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														BlockChain
													 );

						true ->
							io:format("[~p] (proposer) was not elected as part of the proposer group~n", [self()]),
							validator_node:validator_loop(PID_List,
														 #nodes{ validators = Nodes#nodes.validators, builder = Nodes#nodes.builder, proposers = List, normal = Nodes#nodes.normal },
														 BlockChain
														)

					end;

				true ->
					proposer_change_epoch_loop(PID_List, Nodes, BlockChain)
			end
	end.

main_proposer_change_epoch_loop(PID_List, Nodes, BlockChain) ->
	receive
		{validators, List, From} ->
			NewProposers = utils:f10p(List),
			io:format("[~p] (main_proposer) received shuffled list ~w from ~p and will now elect ~w as the proposer group~n", [self(), List, From, NewProposers]),
			utils:broadcast(PID_List, {proposers, NewProposers, self()}),
			validator_node:validator_change_epoch_loop(PID_List, Nodes, BlockChain)
	end.


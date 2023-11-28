-module(builder_node).
-export([init/2]).

-record(block, {hash, tree, prev_hash, number, builder}).
-record(nodes, {builder, validators, proposers, normal}).


%%%% INIT FUNCTIONS %%%%
init(PID_List, Nodes) ->
	TransactionList = utils:readlines("../etc/transactions.csv"),
	FirstBlock = #block{hash = crypto:hash(md5, ""), number = 0},
	utils:broadcast(PID_List, {block, FirstBlock, self()}),
	builder_loop(PID_List, Nodes, TransactionList, [FirstBlock]).

builder_loop(PID_List, Nodes, TxRemaining, BlockChain) ->
	%timer:sleep(round(timer:seconds(rand:uniform()))),
	MainProposer = lists:nth(1, Nodes#nodes.proposers),

			receive
				{continue, From} ->
					io:format("[~p] (builder) received {continue} from ~p~n", [self(), From]),
					if
						length(TxRemaining) == 0 ->
							io:format("[~p] (builder) has finished processing transactions, broadcasting {stop}~n", [self()]),
							utils:broadcast(PID_List, {stop, self()}),
							utils:print_all_blocks(BlockChain);

						true ->
							if
								From == MainProposer ->
									{NewTxRemaining, NextBlock} = build_new_block(TxRemaining, BlockChain),
									utils:broadcast(PID_List, {block, NextBlock, self()}),
									builder_loop(PID_List, Nodes, NewTxRemaining, lists:append(BlockChain, [NextBlock]));
							 true ->
									builder_loop(PID_List, Nodes, TxRemaining, BlockChain)
							end
					end;

				{epoch_end, From} ->
					if
						From == MainProposer ->
							builder_change_epoch_loop(PID_List, Nodes, TxRemaining, BlockChain);
						true ->
							builder_loop(PID_List, Nodes, TxRemaining, BlockChain)
					end
	end.

builder_change_epoch_loop(PID_List, Nodes, TxRemaining, BlockChain) ->
	PreviousMainProposer = lists:nth(1, Nodes#nodes.proposers),
	receive
		{proposers, List, From} ->
			io:format("[~p] (builder) received ~w as the new list of proposers from ~p~n", [self(), List, From]),
			if
				From == PreviousMainProposer ->
					builder_loop(
						PID_List,
						#nodes{
							 builder = self(),
							 validators = Nodes#nodes.validators,
							 proposers = List,
							 normal = Nodes#nodes.normal
							},
						TxRemaining,
						BlockChain
					 );
				true ->
					builder_change_epoch_loop(PID_List, Nodes, TxRemaining, BlockChain)
			end
	end.


build_new_block(TxRemaining, BlockChain) ->
	N = length(TxRemaining),
	SizeOfBlock = (10 rem N)+1,
	{Transactions, NewTxRemaining} = lists:split(SizeOfBlock, TxRemaining),
	MerkleTree = merkle_tree:init(Transactions),
	PreviousBlock = lists:last(BlockChain),
	NextBlock = #block{
								 hash = merkle_tree:root(MerkleTree),
								 tree = MerkleTree,
								 prev_hash = PreviousBlock#block.hash,
								 number = PreviousBlock#block.number+1,
								 builder = self()
								},
	{NewTxRemaining, NextBlock}.

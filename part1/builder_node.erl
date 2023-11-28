-module(builder_node).
-export([init/1]).

-record(block, {hash, tree, prev_hash, number, builder}).

init(PID_List) ->
	io:format("[builder] ~p initialized~n", [self()]),
	TransactionList = utils:readlines("../etc/transactions.csv"),
	FirstBlock = #block{hash = crypto:hash(md5, ""), number = 0},
	utils:broadcast(PID_List, {block, FirstBlock}),
	builder_loop(PID_List, TransactionList, [FirstBlock]).

builder_loop(PID_List, TxRemaining, BlockChain) ->
	%timer:sleep(round(timer:seconds(rand:uniform()))),
	io:format("[builder] ~p with ~p transactions remaining.~n", [self(), length(TxRemaining)]),
	if
		length(TxRemaining) == 0 ->
			io:format("[builder] ~p finished. Terminating all nodes.~n", [self()]),
			utils:broadcast(PID_List, {stop}),
			utils:print_all_blocks(BlockChain);

		length(TxRemaining) > 0 ->
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
			io:format("[builder] ~p broadcasting block #~p~n", [self(), length(BlockChain)+1]),
			utils:broadcast(PID_List, {block, NextBlock}),
			builder_loop(PID_List, NewTxRemaining, lists:append(BlockChain, [NextBlock]))
	end.

-module(pos_node).
-export([start/0]).

-record(block, {hash, tree, prev_hash, number, builder}).

start() -> spawn(node(), fun() -> waiting_loop() end).

waiting_loop() ->
	receive
		{init, Type, PID_List} ->
			if
				Type == 1 -> builder_init(PID_List);
				Type == 0 -> normal_init(PID_List)
			end
	end.

%%%% INITIALIZATION FUNCTIONS %%%%
builder_init(Nodes) ->
	io:format("~p was initialized as a builder node!~n", [self()]),
	%timer:sleep(round(timer:seconds(rand:uniform() * 3))),
	TransactionList = readlines("transactions.csv"),
	FirstBlock = #block{hash = crypto:hash(md5, ""), number = 0},
	broadcast_block(Nodes, FirstBlock),
	builder_loop(Nodes, TransactionList, [FirstBlock]).

normal_init(Nodes) ->
	io:format("~p was initialized as a normal node!~n", [self()]),
	normal_loop(Nodes, []).

%%%% MAIN LOOPS %%%%
normal_loop(Nodes, BlockChain) ->
	%timer:sleep(round(timer:seconds(rand:uniform()))),
	receive
		{block, NewBlock} ->
			io:format("~p has received a new block !~n", [self()]),
			normal_loop(Nodes, lists:append(BlockChain, [NewBlock]));
		{stop} ->
			print_all_blocks(BlockChain),
			io:format("Node ~p finished...~n", [self()])
	end.

builder_loop(Nodes, TxRemaining, BlockChain) ->
	%timer:sleep(round(timer:seconds(rand:uniform()))),
	io:format("Builder is ~p, with ~p transactions remaining.~n", [self(), length(TxRemaining)]),
	if
		length(TxRemaining) == 0 ->
			stop_all_nodes(Nodes),
			print_all_blocks(BlockChain);

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
			broadcast_block(Nodes, NextBlock),
			builder_loop(Nodes, NewTxRemaining, lists:append(BlockChain, [NextBlock]))
	end.

%%%% UTILITARY FUNCTIONS %%%%
readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

stop_all_nodes(Nodes) ->
	case Nodes of
		[H] ->
			io:format("Stopping node ~p ~n", [H]),
			H ! {stop};
		[H|T] ->
			io:format("Stopping node ~p ~n", [H]),
			H ! {stop},
			stop_all_nodes(T)
	end.

broadcast_block(Nodes, Block) ->
	case Nodes of
		[H] ->
			H ! {block, Block};
		[H|T] ->
			H ! {block, Block},
			broadcast_block(T, Block)
	end.

print_all_blocks(BlockChain) ->
	{ok, File} = file:open("log/" ++ "blocks_" ++ io_lib:format("~p", [self()]) ++ ".log", [write]),
	tail_print_all_blocks(BlockChain, File).

tail_print_all_blocks(BlockChain, File) ->
	case BlockChain of
		[H] ->
			print_block(H, File);
		[H|T] ->
			print_block(H, File),
			tail_print_all_blocks(T, File)
	end.

print_block(Block, File) ->
	io:format(File, "~w,~w,~w,~w,~w~n", [ Block#block.number, Block#block.builder, Block#block.hash, Block#block.prev_hash, Block#block.tree ]).


-module(utils).

-record(block, {hash, tree, prev_hash, number, builder}).

-export([readlines/1, broadcast/2, print_all_blocks/1]).
readlines(FileName) -> {ok, Data} = file:read_file(FileName), binary:split(Data, [<<"\n">>], [global]).

broadcast(Nodes, M) ->
	case Nodes of
		[H] -> H ! M;
		[H|T] -> H ! M, broadcast(T, M)
	end.

print_all_blocks(BlockChain) ->
	{ok, File} = file:open("log/" ++ "blocks_" ++ io_lib:format("~p", [self()]) ++ ".log", [write]),
	tail_print_all_blocks(BlockChain, File).

print_block(Block, File) -> io:format(File, "~w,~w,~w,~w,~w~n", [ Block#block.number, Block#block.builder, Block#block.hash, Block#block.prev_hash, Block#block.tree ]).

tail_print_all_blocks(BlockChain, File) ->
	case BlockChain of
		[H] -> print_block(H, File);
		[H|T] -> print_block(H, File), tail_print_all_blocks(T, File)
	end.



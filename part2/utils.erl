-module(utils).
-export([shuf/1, f10p/1, print_all_blocks/1, readlines/1, broadcast/2]).

-record(block, {hash, tree, prev_hash, number, builder}).

shuf(L) -> [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].
f10p(L) -> N = round(length(L)/10)+1, R = lists:reverse(L), lists:nthtail(length(R) - N, R).
broadcast(L, M) -> case L of [H] -> H ! M; [H|T] -> H ! M, broadcast(T, M) end.
readlines(FileName) -> {ok, Data} = file:read_file(FileName), binary:split(Data, [<<"\n">>], [global]).

print_all_blocks(Blocks) ->
	{ok, File} = file:open("log/" ++ "blocks_" ++ io_lib:format("~p", [self()]) ++ ".log", [write]),
	tail_print_all_blocks(Blocks, File).

tail_print_all_blocks(Blocks, File) ->
	case Blocks of
		[H] ->
			print_block(H, File);
		[H|T] ->
			print_block(H, File),
			tail_print_all_blocks(T, File)
	end.

print_block(Block, File) ->
	io:format(File, "~w,~w,~w,~w,~w~n", [ Block#block.number, Block#block.builder, Block#block.hash, Block#block.prev_hash, Block#block.tree ]).



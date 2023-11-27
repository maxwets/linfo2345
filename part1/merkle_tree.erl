-module(merkle_tree).
-export([init/1, root/1, main/0]).

-record(tree, {hash, left, right, tx}).

init(Data) ->
	Hashes = create_list_hashes(Data, []),
	build_tree(Hashes).

root(Tree) -> Tree#tree.hash.

create_list_hashes(Data, Acc) ->
	case Data of
		[] -> Acc;
		[H|T] ->
			Leaf = #tree{hash=crypto:hash(md5, iolist_to_binary(io_lib:write(H))), tx=H},
			create_list_hashes(T, Acc ++ [Leaf])
	end.

build_tree(Hashes) ->
	case Hashes of
		[Root] -> Root;
		[_|_] ->
			NextLayer = tail_build_tree(Hashes, []),
			build_tree(NextLayer)
	end.

tail_build_tree(HashesList, Tree) ->
	if
		length(HashesList) == 0 -> Tree;
		length(HashesList) == 1 ->
			[H|_] = HashesList,
			NextTree = Tree ++ [#tree{hash = H#tree.hash, left=H}],
			NextTree;
		length(HashesList) >= 2 ->
			{[H1, H2], T} = lists:split(2, HashesList),
			Hash1 = H1#tree.hash,
			Hash2 = H2#tree.hash,
			NextTree = Tree ++ [#tree{hash = crypto:hash(md5, <<Hash1/binary, Hash2/binary>>), left=H1, right=H2}],
			tail_build_tree(T, NextTree)
	end.

main() ->
	Tree = init(["Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel"]),
	root(Tree).


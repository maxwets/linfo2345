# Report LINFO2345 Project
Maxime Wets
---

## PART 1

### Design Choices

```text
$ tree part1/
    part1
    |-- network.erl
    |-- builder_node.erl
    |-- normal_node.erl
    |-- merkle_tree.erl
    `-- utils.erl
```

#### Merkle Tree
The merkle tree is implemented in `./part1/merkle_tree.erl`.
It exports 2 functions:
- `init/1`: requires a list of transactions as input, and will calculate the merkle tree;
- `root/1`: requires a merkle tree as input, andwill return the hash of the top of the merkle tree.

To compute the Merkle Tree of a list of transactions, the algorithm first calculates the MD5 hash of each transaction,
then, transforms each element of that list into a tree record definded as:
```erlang
-record(tree, {hash, left, right, tx})
```
After the first layer has been computed (i.e. all transactions are transformed into leaf nodes), the algorithm will build the tree layer by layer, until one node is left (this will be the root node).

The hash represents the MD5 hash of the transaction (if the node is a leaf) or the hash of its left and right children (if the node is a tree).

#### Node implementation
The nodes are implemented in `./part1/pos_node.erl`.
For this first task, I renamed `Validator` and `Nonvalidator` nodes to `Normal`.
The module only export one function, `start/0`, that will spawn a node and run the `waiting_loop/0` function while waiting to be initialized:

```erlang
waiting_loop() ->
    receive
        {init, Type, PID_List} ->
            if
                Type == 1 -> builder_loop(PID_List);
                Type == 0 -> normal_loop(PID_List)
        end
    end.
```

Once a node receives an `init` message, it will run the corresponding init function (`builder_init` or `normal_init`).
This function will then be responsible to prepare the function parameters and then call the corresponding loop (`builder_loop` or `normal_loop`).

The `builder_init` function is responsible for
- reading the CSV file containing all pending transactions;
- creating the first block and broadcast it to everyone;
- run the tail-recursive `builder_loop` function that will run until the last pending transaction.

The `normal_init` function simply prints a message indicating that the normal node has started, and then runs a loop waiting for incoming messages:
- `{block, NewBlock}`: this indicates that a new block has been pushed, the node will append the block to its internal list and recursively run the loop;
- `{stop}`: this indicates that the builder has finished, the node will then create a CSV file and write its version of the blockchain to that file.

---
## PART 2
```
$ tree part2/
    part2
    |-- network.erl
    |-- normal_node.erl
    |-- builder_node.erl
    |-- validator_node.erl
    |-- proposer_node.erl
    |-- merkle_tree.erl
    `-- utils.erl
```

---
## PART 3
TODO

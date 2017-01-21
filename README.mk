Erlang Tripeaks Solver
======================

This is a fast tripeaks solver written in Erlang. 

## How to use?

1. Edit `cards.erl` to represent your particular deal you want to solve.
2. Compile `./rebar3 compile`
3. Run `erl -pa _build/default/lib/tripeaks/ebin/ -run tripeaks main` 

## How to tweak the algorithm

The current algorithm will solve most easy to hard games in a few seconds
or less, but extremely hard games make take it as long as 30 minutes. 
Tweaking the algorithm is super easy. Just open `tripeaks.erl` and modify
`buffer_score()` and `buffer_sorted_insert()` to change the heuristic used
to decide which moves are more valuable.

## Room for improvement

* The format for representing games is kind of a pain.
* It'd be nice if we could just read in game files from the command line.
* **It could always be faster!**
%%%-------------------------------------------------------------------
%%% @author stefanrusek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2016 11:51 AM
%%%-------------------------------------------------------------------
-module(cards).
-author("stefanrusek").

%% API
-export([start_game/0, next_moves/1, apply_move/2, printable_moves/1, parse_card/1]).

-record(card, {number, suit}).
-record(node, {card, parents}).
-record(game, {deck, tree, current}).

start_game() ->
  [Current | Deck] = initial_deck(),
  GameSource = initial(),
  _53 = sets:size(sets:from_list(lists:flatten([Current, Deck, GameSource]))),
   io:format("53 == ~b~n", [_53]),
  #game{deck = Deck, tree = make_trees(GameSource), current = Current}.

next_moves(#game{tree = []}) -> [win];
next_moves(#game{tree = T, deck = Deck, current = Current}) ->
  Acc = case Deck of
          [] -> [];
          _ -> [draw]
        end,
  next_moves(T, Current, Acc).
next_moves([], _, Acc) ->
  Acc;
next_moves([N | T], C, Acc) when N#node.card#card.number == (C#card.number rem 13) + 1 ->
  next_moves(T, C, [N | Acc]);
next_moves([N | T], C, Acc) when (N#node.card#card.number rem 13) + 1 == C#card.number ->
  next_moves(T, C, [N | Acc]);
next_moves([_ | T], C, Acc) ->
  next_moves(T, C, Acc).

apply_move(Game, win) ->
  Game;
apply_move(#game{deck = [Current | Deck], tree = Tree}, draw) ->
  #game{deck = Deck, tree = Tree, current = Current};
apply_move(Game, N) ->
  #game{tree = Tree} = Game,
  Pruned = prune(Tree, N),
  NewTree = find_free_parents(Pruned, N) ++ Pruned,
  Game#game{tree = NewTree, current = N#node.card}.


printable_moves(Moves) ->
  printable_moves(Moves, []).
printable_moves([], Acc) -> lists:reverse(Acc);
printable_moves([A | Moves], Acc) when is_atom(A) -> printable_moves(Moves, [A | Acc]);
printable_moves([#node{card = Card} | Moves], Acc) -> printable_moves(Moves, [Card | Acc]).


%% private

parse_card([$1, D, S]) ->
  #card{number = 10 + D - $0, suit = list_to_atom([S])};
parse_card([$k, S]) ->
  parse_card([$1, $3, S]);
parse_card([$q, S]) ->
  parse_card([$1, $2, S]);
parse_card([$j, S]) ->
  parse_card([$1, $1, S]);
parse_card([$a, S]) ->
  parse_card([$1, S]);
parse_card([D, S]) ->
  #card{number = D - $0, suit = list_to_atom([S])};
parse_card(u) ->
  undefined.

parse_list([]) -> [];
parse_list([H | T]) ->
  [parse_card(H) | parse_list(T)].

initial_deck(Deck) ->
  parse_list(Deck).

initial_deck() ->
  initial_deck(["5s", "9d", "8s", "6c", "5h", "7h", "7s", "4h", "js", "4c", "8c", "kh", "10c", "qd", "jc", "kd",
    "10h", "3c", "7d", "7c", "as", "9h", "ad", "9c"]).

initial() -> [
  parse_list(["4s", "6d", "5d", "jd", "ah", "6s", "qs", "jh", "3s", "2h"]),
  parse_list([u,    "ac", "3h", "2s", "8d", "5c", "ks", "2c", "6h", "2d", u]),
  parse_list([u,       u, "9s", "qc",   u, "10d", "kc",    u, "8h", "10s", u, u]),
  parse_list([u, u, u, "3d", u, u, "qh", u, u, "4d", u, u, u])
].


%%initial() -> [
%%  [#card{number = 13, suit = d}, #card{number = 12, suit = h}, #card{number = 5, suit = h}, #card{number = 1, suit = d}, #card{number = 9, suit = c}, #card{number = 13, suit = c}, #card{number = 4, suit = h}, #card{number = 7, suit = c}, #card{number = 2, suit = c}, #card{number = 11, suit = h}],
%%  [undefined, #card{number = 2, suit = s}, #card{number = 10, suit = d}, #card{number = 6, suit = d}, #card{number = 10, suit = c}, #card{number = 6, suit = c}, #card{number = 3, suit = h}, #card{number = 1, suit = s}, #card{number = 12, suit = s}, #card{number = 11, suit = c}, undefined],
%%  [undefined, undefined, #card{number = 5, suit = s}, #card{number = 10, suit = s}, undefined, #card{number = 1, suit = h}, #card{number = 12, suit = s}, undefined, #card{number = 11, suit = d}, #card{number = 4, suit = c}, undefined, undefined],
%%  [undefined, undefined, undefined, #card{number = 4, suit = s}, undefined, undefined, #card{number = 10, suit = h}, undefined, undefined, #card{number = 3, suit = d}, undefined, undefined, undefined]
%%].
%%
%%initial_deck() ->
%%  [#card{number = 9, suit = d}, #card{number = 5, suit = d}, #card{number = 6, suit = s}, #card{number = 8, suit = h}, #card{number = 3, suit = s}, #card{number = 6, suit = h}, #card{number = 8, suit = d}, #card{number = 12, suit = d}, #card{number = 11, suit = s}, #card{number = 13, suit = s}, #card{number = 9, suit = s}, #card{number = 5, suit = c}, #card{number = 8, suit = c}, #card{number = 1, suit = c}, #card{number = 3, suit = c}, #card{number = 13, suit = h}, #card{number = 7, suit = s}, #card{number = 2, suit = h}, #card{number = 7, suit = d}, #card{number = 7, suit = h}, #card{number = 8, suit = s}, #card{number = 4, suit = d}, #card{number = 9, suit = h}, #card{number = 2, suit = d}].

make_trees([R1, R2, R3, R4]) ->
  S1 = make_nodes(lists:duplicate(100, undefined), R4),
  S2 = make_nodes(S1, R3),
  S3 = make_nodes(S2, R2),
  make_nodes(S3, R1).

make_nodes(Parents, Cards) ->
  make_nodes(Parents, Cards, []).

make_nodes(_, [], Acc) ->
  lists:reverse(Acc);
make_nodes([_ | Parents], [undefined | Cards], Acc) ->
  make_nodes(Parents, Cards, [undefined | Acc]);
make_nodes([P1, P2 | Parents], [C | Cards], Acc) ->
  make_nodes([P2 | Parents], Cards, [#node{card = C, parents = clean(P1, P2)} | Acc]).


clean(undefined, undefined) -> [];
clean(A, undefined) -> [A];
clean(undefined, B) -> [B];
clean(A, B) -> [A, B].


prune(Tree, N) ->
  lists:delete(N, Tree).

find_free_parents(Tree, #node{parents = Nodes}) ->
  find_free_nodes(Tree, Nodes, []).
find_free_nodes(_, [], Acc) -> Acc;
find_free_nodes(Tree, [N | Nodes], Acc) ->
  case is_in_tree(Tree, N) of
    exists -> find_free_nodes(Tree, Nodes, Acc);
    no -> find_free_nodes(Tree, Nodes, [N | Acc])
  end.

is_in_tree([], _) -> no;
is_in_tree([#node{parents = Parents} | Tree], N) ->
  case is_in_list(Parents, N) of
    exists -> exists;
    _ -> is_in_tree(Parents ++ Tree, N)
  end.

is_in_list([], _) -> no;
is_in_list([N | _], N) -> exists;
is_in_list([_ | List], N) -> is_in_list(List, N).

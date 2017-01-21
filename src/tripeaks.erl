%%%-------------------------------------------------------------------
%%% @author stefanrusek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2016 11:46 AM
%%%-------------------------------------------------------------------
-module(tripeaks).
-author("stefanrusek").

%% API
-export([play/1, main/0, buffer_status/0, buffer/0]).

buffer_score({_, Moves}) ->
  buffer_score(Moves);
buffer_score([]) -> {0, 0, 0};
buffer_score([draw | Moves]) ->
  {A, B, C} = buffer_score(Moves),
  {A, B + 1, C + 1};
buffer_score([_ | Moves]) ->
  {A, B, C} = buffer_score(Moves),
  {A + 1, B, C + 1}.

%%buffer_sorted_insert([], Item) -> [Item];
%%buffer_sorted_insert(List, Item) -> [Item | List].

buffer_sorted_insert([], Item) ->
  [Item];
buffer_sorted_insert([First | Rest], Item) ->
  {{FirstA, FirstB, FirstC}, _} = First,
  {{A, B, C}, _} = Item,
  if
    FirstA < A -> [Item, First | Rest];
    (FirstA =:= A) and (FirstB < B) -> [Item, First | Rest];
    (FirstA =:= A) and (FirstB =:= B) and (FirstC < C) -> [Item, First | Rest];
    true -> [First | buffer_sorted_insert(Rest, Item)]
  end.

buffer() ->
  io:format("Starting LIFO~n"),
  buffer([], 0),
  io:format("Shutdown LIFO~n").
buffer(Data, Failed) ->
  receive
    {next, Pid} ->
      case Data of
        [{_, Next} | Rest] ->
          Pid ! Next,
          buffer(Rest, Failed);
        [] ->
          Pid ! done,
          buffer([], Failed)
      end;
    {store, Next} ->
      buffer(buffer_sorted_insert(Data, {buffer_score(Next), Next}), Failed);
    status ->
      [{Score, _} | _] = Data,
      io:format("Considering ~b, Discarded ~b, Closeness ~p.~n", [length(Data), Failed, Score]),
      buffer(Data, Failed);
    failed ->
      buffer(Data, Failed + 1);
    stop -> ok
  end.

buffer_status() ->
  buffer ! status.


main_loop() ->
  receive
    {win, Solution} ->
      io:format("~n~p~n", [Solution]),
      buffer ! stop;
    {failed} ->
      buffer ! failed,
      main_loop()
  end.

main() ->
  register(buffer, spawn(tripeaks, buffer, [])),
  buffer ! {store, {cards:start_game(), []}},
  timer:apply_interval(5000, tripeaks, buffer_status, []),

  play_init(20),
  spawn_init(20),
  main_loop().

play_init(0) -> ok;
play_init(N) ->
  play_once(self()),
  buffer_status(),
  play_init(N - 1).

spawn_init(0) -> ok;
spawn_init(N) ->
  spawn(tripeaks, play, [self()]),
  spawn_init(N - 1).

play(Parent) ->
  case play_once(Parent) of
    done -> ok;
    more -> play(Parent)
  end.

play_once(Parent) ->
  buffer ! {next, self()},
  receive
    done -> done;
    {Game, MoveAcc} ->
      Moves = cards:next_moves(Game),
      case cards:next_moves(Game) of
        [] -> Parent ! {failed};
        Moves -> play_moves(Parent, Game, Moves, MoveAcc)
      end,
      more
  end.

play_moves(_, _, [], _) ->
  ok;
play_moves(Parent, _, [win], Moves) ->
  Parent ! {win, cards:printable_moves(lists:reverse(Moves))};
play_moves(Parent, OldGame, [Move | Moves], MoveAcc) ->
  NewGame = cards:apply_move(OldGame, Move),
  buffer ! {store, {NewGame, [Move | MoveAcc]}},
  play_moves(Parent, OldGame, Moves, MoveAcc).


%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(logger).

%% API
-export([error/4, info/4]).

error(Str, Arge, Mode, Line)->
  io:format("~ts~n", [format("error", Str, Arge, Mode, Line)]).

info(Str, Arge, Mode, Line)->
  io:format("~ts~n", [format("info", Str, Arge, Mode, Line)]).

format(T, F, A, Mod, Line) ->
  Now = reptile_lib:now_str(),
  IOList = io_lib:format(lists:concat([
    "## ", T, " ", Now,
    "[", atom_to_list(Mod),":", integer_to_list(Line),"]", F
  ]), A),
  unicode:characters_to_binary(IOList).

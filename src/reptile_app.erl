-module(reptile_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-include("reptile.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    [Num, Url, File|_] = Arge = init:get_plain_arguments(),
    ?ERR("启动参数~w", [Arge]),
    reptile_sup:start_link({Num, Url, File}).

stop(_State) ->
    ok.

start()->
    application:start(inets),
    application:start(reptile).
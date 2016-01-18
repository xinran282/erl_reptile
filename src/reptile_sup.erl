-module(reptile_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
%% Arge = {ProcessNum, Url}
start_link(Arge) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Arge]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Arge]) ->
    Child = [
        {reptile_mgr, {reptile_mgr, start_link, [Arge]}, permanent, 5000, worker, [reptile_mgr]}
    ],
    {ok, { {one_for_one, 5, 10}, Child} }.


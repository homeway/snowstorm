-module(ss_model_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% start a model server
start_child(Id, Module, Args) ->
    Child = ?CHILD(Id, Module, Args, worker),
    supervisor:start_child(self(), Child).
stop_child(Id) ->
    supervisor:terminate_child(self(), Id).
-module(ss_model_sup).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start() -> {ok, Pid} = start_link(), unlink(Pid).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% start a child
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).
start_child(Id, SsMod, Args) ->
    Child = ?CHILD(Id, SsMod, Args, worker),
    supervisor:start_child(?MODULE, Child).

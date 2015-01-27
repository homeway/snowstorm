%% -*- mode: nitrogen -*-
-module(ss_server_sup).

-behaviour(supervisor).

%% API
-export([start/1, start_link/1, start_child/4]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start(Name) ->
    Pid1 = case start_link(Name) of
        {ok, Pid} -> Pid;
        {error,{already_started, Pid}} -> Pid
    end,
    unlink(Pid1).
start_link(Name) -> supervisor:start_link({local, Name}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% start a child
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).
start_child(Name, Id, SsMod, Args) ->
    Child = ?CHILD(Id, SsMod, Args, worker),
    supervisor:start_child(Name, Child).

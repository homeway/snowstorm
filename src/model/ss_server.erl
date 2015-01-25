%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start/1, start/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type args() :: term().
-type state() :: term().
-type msg() :: term().
-type reply() :: term().
-callback init(args()) -> {ok, state()}.
-callback handle_call(msg(), {pid(), reference()}, state()) -> {reply(), state()}.

start(Mod) -> start(Mod, []).
start(Mod, Args) -> {ok, Pid} = start_link(Mod, Args), unlink(Pid).
start_link(Mod, Args) -> gen_server:start_link(?MODULE, {Mod, Args}, []).

init({Mod, Args}) -> {ok, {Mod, Mod:init(Args)}}.
handle_call(Msg, From, {Mod, State}) ->
  {reply, Reply, NewState} = Mod:handle_call(Msg, From, State),
  {reply, Reply, {Mod, NewState}}.
handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.
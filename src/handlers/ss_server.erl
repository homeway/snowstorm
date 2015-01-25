%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start/2, start/3, start_link/2, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type args() :: term().
-type state() :: term().
-type msg() :: term().
-type reply() :: term().
-callback init(args()) -> {ok, state()}.
-callback handle_call(msg(), {pid(), reference()}, state()) -> {reply(), state()}.

start(Name, Mod) -> gen_server:start(?MODULE, {Name, Mod, []}, []).
start(Name, Mod, Args) -> gen_server:start(?MODULE, {Name, Mod, Args}, []).
start_link(Name, Mod) -> gen_server:start_link(?MODULE, {Name, Mod, []}, []).
start_link(Name, Mod, Args) -> gen_server:start_link(?MODULE, {Name, Mod, Args}, []).

init({Name, Mod}) -> init({Name, Mod, []});
init({Name, Mod, Args}) -> ss_world:reg(Name), {ok, {Mod, Mod:init(Args)}}.
handle_call(Msg, From, {Mod, State}) ->
  {reply, Reply, NewState} = Mod:handle_call(Msg, From, State),
  {reply, Reply, {Mod, NewState}}.
handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ss_world:unreg(), ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.
%% -*- mode: nitrogen -*-
-module(ss_live).
-behaviour(gen_server).
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Name) -> gen_server:start(?MODULE, Name, []).
start_link(Name) -> gen_server:start_link(?MODULE, Name, []).

init(Name) -> ss_world:reg(Name), {ok, #{}}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ss_world:unreg(), ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.
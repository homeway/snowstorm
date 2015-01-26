%% -*- mode: nitrogen -*-
-module([[[Name]]]).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link()      -> gen_server:start_link(?MODULE, [], []).
start_link(Color) -> gen_server:start_link(?MODULE, [Color], []).

init([])      -> {ok, #{}};
init([Color]) -> {ok, #{color=>Color}}.

handle_call(_Msg, _From, S) -> {reply, ok, S}.
handle_cast({_Msg, _From}, S) -> {noreply, S}.

handle_info(undefined_info, S) ->  {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.
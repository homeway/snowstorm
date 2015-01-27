%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([hello/1, who/1, model/2]).

-define(db, ss_nosqlite).
-define(m, model_user).
-define(res, user).

start_link()       -> gen_server:start_link(?MODULE, [], []).
start_link(UserId) -> gen_server:start_link(?MODULE, [UserId], []).

init(_) -> {ok, #{id=>undefined}}.
handle_call(Fun, _From, S) when is_atom(Fun) -> apply(?MODULE, Fun, [S]);
handle_call([Fun|Args], _From, S) when is_atom(Fun) -> apply(?MODULE, Fun, Args++[S]).
handle_cast({_Msg, _From}, S) -> {noreply, S}.
handle_info(undefined_info, S) ->  {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.

%% helper info
hello(#{id:=undefined}=S) -> {reply, #{}, S};
hello(#{id:=Id}=S) -> {reply, Id, S}.
who(#{id:=Id}=S) -> {reply, Id, S}.

%% model action
model(Name, S) -> {reply, ?m:Name(), S}.


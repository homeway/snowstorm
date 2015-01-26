%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link()       -> gen_server:start_link(?MODULE, [], []).
start_link(UserId) -> gen_server:start_link(?MODULE, [UserId], []).

init(_)        -> {ok, #{id=>anonymouse}}.

handle_call(hello, _From, #{id:=anonymouse}=S) ->  {reply, #{}, S};
handle_call(hello, _From, #{id:=Id}=S) -> {reply, model_user:get(Id), S};

handle_call({login, Id}, _From, #{id:=anonymouse}=S) ->
    case model_user:login(Id) of
        ok -> {reply, ok, S#{id=>Id}};
        Error -> {reply, Error, S}
    end;

handle_call({login, Id}, _From, S) -> {reply, already_login, S};
handle_call(logout, _From, S) -> {reply, ok, S#{id=>anonymouse}}.

handle_cast({_Msg, _From}, S) -> {noreply, S}.

handle_info(undefined_info, S) ->  {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.

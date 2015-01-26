%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([model/2]).
-export([create/2, update/2, delete/2, find/2, all/1]).
-export([hello/1, who/1, login/3, logout/1]).

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

%% priv -------------------------------------------------

%% helper info
hello(#{id:=undefined}=S) -> {reply, #{}, S};
hello(#{id:=Id}=S) -> find(Id, S).
who(#{id:=Id}=S) -> {reply, Id, S}.

%% model action
model(Name, S) -> {reply, ?m:Name(), S}.

%% db action
create(M, S)   -> {reply, ?db:create(?res, M), S}.
update(M, S)   -> {reply, ?db:patch (?res, M), S}.
delete(Key, S) -> {reply, ?db:delete(?res, Key), S}.
find(Key, S)   -> {reply, ?db:get   (?res, Key, ?m:show()), S}.
all(S)         -> {reply, ?db:all   (?res, ?m:all()), S}.

%% login and logout
login(Id, Pass, #{id:=undefined}=S) ->
    case check_password(Id, Pass) of
        true -> {reply, ok, S#{id=>Id}};
        false -> {reply, invalid_user_or_pass, S}
    end;
login(_Id, _Pass, S) -> {reply, already_login, S}.
logout(S) -> {reply, ok, S#{id=>undefined}}.

check_password(UserId, Pass) ->
    Info = ?db:get(?res, UserId, ?m:show()),
    ss_model:equal("密码", Info, Pass).

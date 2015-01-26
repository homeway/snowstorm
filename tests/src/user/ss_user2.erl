%% -*- mode: nitrogen -*-
-module(ss_user2).
-behaviour(ss_server).
-export([init/1, model/1, db/0, resource/0]).
-export([hello/1, who/1, login/3, logout/1]).

-define(db, ss_nosqlite).
-define(m, model_user).
-define(res, user).

init(_) -> {ok, #{id=>undefined}}.
db() -> ?db.
resource() -> ?res.
model(Name) -> model_user:Name().

%% helper info
hello(#{id:=undefined}=S) -> {#{}, S};
hello(#{id:=Id}=S) -> {?db:get(?res, Id, ?m:show()), S}.
who(#{id:=Id}=S) -> {Id, S}.

%% login and logout
login(Id, Pass, #{id:=undefined}=S) ->
    case check_password(Id, Pass) of
        true -> {ok, S#{id=>Id}};
        false -> {invalid_user_or_pass, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>undefined}}.

check_password(UserId, Pass) ->
    Info = ?db:get(?res, UserId, ?m:show()),
    ss_model:equal("密码", Info, Pass).
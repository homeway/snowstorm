%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([hello/1, status/1, status/2, who/1, login/3, logout/1, notify/2]).

%% ss_server api
init(_) -> {ok, #{db=>ss_nosqlite, res=>user, id=>not_login}}.

model(all) -> ss_model:confirm_model([
    {account, #{validate=>[required]}},
    {password, #{type=>password, validate=>[required, {min, 6}]}},
    {email, #{}},
    {nickname, #{}},
    {contacts, #{type=>tags}}
]);
model(show) -> ss_model:drop([password], model(all));
model(password) -> ss_model:filter([account, password], model(all));
model(_) -> [].

%% helper info
who(#{id:=Id}=S) -> {Id, S}.
hello(#{db:=Db, res:=Res, id:=Id}=S) ->
    force_login(fun() ->
        {Db:find(Res, Id), S}
    end, S).

%% user state and sign string
%% 读取状态
status(S) ->
    force_login(fun() ->
        {maps:get(status, S, "online"), S}
    end, S).
%% 设置状态
status(Status, S) ->
    force_login(fun() ->
        {ok, S#{status=>Status}}
    end, S).

%% login and logout
login(User, Pass, #{db:=Db, res:=Res, id:=not_login}=S) ->
    case check_password(Db, Res, User, Pass) of
        {true, Id} -> {ok, S#{id=>Id}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>not_login}}.

check_password(Db, Res, Account, Pass) ->
    User = Db:find(Res, account, Account),
    case User of
        notfound -> {error, "account not exist"};
        #{<<"_key">> :=Id} ->
            case ss_model:equal(password, User, Pass) of
                true -> {true, Id};
                false -> {error, "invalid password"}
            end
    end.

%% force login
force_login(Fun, S) ->
    case maps:get(id, S, not_login) of
        not_login -> {not_login, S};
        _ -> Fun()
    end.

%% 收通知
notify(Content, S) ->
    force_login(fun() ->
        Content
    end, S).

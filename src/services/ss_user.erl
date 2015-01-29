%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(ss_server).
-export([init/1, model/1]).
%% call callback
-export([hello/1, status/1, status/2, who/1, login/3, logout/1]).
%% cast callback
-export([hello/2, notify/2]).

%% ss_server api
init([Config]) when is_map(Config) ->
    Default = #{db=>ss_nosqlite, res=>user, id=>not_login},
    {ok, maps:merge(Default, Config)};
init([]) ->
    Default = #{db=>ss_nosqlite, res=>user, id=>not_login},
    {ok, Default}.

model(all) -> ss_model:confirm_model([
    {account, #{validate=>[required, uniq]}},
    {password, #{type=>password, validate=>[required, {min, 6}]}},
    {email, #{}},
    {nickname, #{}},
    {pub_to, #{type=>list}},  % 订阅者列表
    {contacts, #{type=>tags}} % 联系人
]);
model(show) -> ss_model:drop([password, pub_to], model(all));
model(password) -> ss_model:filter([account, password], model(all));
%% message stored along with table message_{res()}
model(message) -> ss_model:confirm_model([
    {from, #{}},
    {to, #{}},
    {status, #{type=>select, options=>[offline, confirm], value=>offline}},
    {content, #{type=>textarea}}
]);
model(_) -> [].

%% call who
who(#{id:=Id}=S) -> {Id, S}.

%% call hello
hello(#{db:=Db, res:=Res, id:=Id}=S) ->
    force_login(fun() ->
        {Db:find(Res, Id), S}
    end, S).

%% cast hello
hello(From, S) ->
    force_login(fun() ->
        hi
    end, From, S),
    {ok, S}.

%% cast notify
notify({From, Content}, S) ->
    force_login(fun() ->
        Content
    end, From, S),
    {ok, S}.

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
force_login(Fun, From, S) ->
    case maps:get(id, S, not_login) of
        not_login -> From ! not_login;
        _ -> From ! Fun()
    end.

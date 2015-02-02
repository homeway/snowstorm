%% -*- mode: nitrogen -*-
-module(ss_user2).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([hello/1, status/1, status/2, who/1, login/3, logout/1, notify/2]).

%% ss_server api
init(_) -> {ok, #{db=>ss:nosqlite(ss_user2), id=>not_login}}.

model(all) -> ss_model:confirm_model([
    {"类型", #{value=>"普通会员", validate=>[required]}},
    {"账户名", #{value=>"yifan", validate=>[required]}},
    {"性别", #{value=>"女"}},
    {"密码", #{type=>password, value=>"123456", validate=>[required, {min, 6}]}},
    {"EMail", #{}},
    {"电话", #{}},
    {"姓名", #{}},
    {"昵称", #{}},
    {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {"生日", #{type=>date}},
    {"联系人", #{type=>tags}}
]);
model(show) -> ss_model:drop(["密码"], model(all));
model(password) -> ss_model:filter(["账户名", "密码"], model(all));
model(_) -> [].

%% helper info
who(#{id:=Id}=S) -> {Id, S}.
hello(#{db:=Db, id:=Id}=S) ->
    force_login(fun() ->
        {Db:find(Id), S}
    end, S).

%% user state and sign string
%% 读取状态
status(S) ->
    force_login(fun() ->
        {maps:get(status, S, "已登录"), S}
    end, S).
%% 设置状态
status(Status, S) ->
    force_login(fun() ->
        {ok, S#{status=>Status}}
    end, S).

%% login and logout
login(User, Pass, #{db:=Db, id:=not_login}=S) ->
    case check_password(Db, User, Pass) of
        {true, Id} -> {ok, S#{id=>Id}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>not_login}}.

check_password(Db, Account, Pass) ->
    User = Db:find("账户名", Account),
    case User of
        notfound -> {error, "账户不存在"};
        #{<<"_key">> :=Id} ->
            case ss_model:equal("密码", User, Pass) of
                true -> {true, Id};
                false -> {error, "密码不正确"}
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

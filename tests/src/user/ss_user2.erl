%% -*- mode: nitrogen -*-
-module(ss_user2).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([hello/1, who/1, login/3, logout/1]).

%% ss_server api
init(_) -> {ok, #{db=>ss_nosqlite, res=>user, id=>undefined}}.

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
model(password) -> ss_model:filter(["账户", "密码"], model(all)).

%% helper info
hello(#{id:=undefined}=S) -> {#{}, S};
hello(#{db:=Db, res:=Res, id:=Id}=S) -> {Db:get(Res, Id, model(show)), S}.
who(#{id:=Id}=S) -> {Id, S}.

%% login and logout
login(Id, Pass, #{db:=Db, res:=Res, id:=undefined}=S) ->
    case check_password(Db, Res, Id, Pass) of
        true -> {ok, S#{id=>Id}};
        false -> {invalid_user_or_pass, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>undefined}}.

check_password(Db, Res, UserId, Pass) ->
    Info = Db:get(Res, UserId, model(password)),
    ss_model:equal("密码", Info, Pass).
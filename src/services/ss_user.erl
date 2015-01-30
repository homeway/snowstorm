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
    {contacts, #{type=>list}}  % 联系人列表
]);
model(show) -> ss_model:drop([password, pub_to], model(all));
model(password) -> ss_model:filter([account, password], model(all));

%% contacts保存联系人的格式为 [{account(), #{rel=>rel()}}]
%%     rel() :: single|double
model(contacts) -> ss_model:filter([contacts], model(all));

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
login(User, Pass, #{id:=not_login}=S) ->
    #{world:=World, db:=Db, res:=Res}=S,
    case check_password(Db, Res, User, Pass) of
        {true, Data} ->
            Contacts = ss_model:value(contacts, Data, []),
            Subs = [{user, Account} || {Account, _} <- Contacts],
            [ss_world:send2(World, Sub, [notify, {online, User}]) || Sub <- Subs], 
            {ok, S#{id=>ss_model:value('_key', Data)}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>not_login}}.

check_password(Db, Res, Account, Pass) ->
    User = Db:find(Res, account, Account),
    case User of
        notfound -> {error, "account not exist"};
        _ ->
            case ss_model:equal(password, User, Pass) of
                true -> {true, User};
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

%% cast hello
hello(From, S) ->
    force_login(fun() ->
        hi
    end, From, S),
    {ok, S}.

%% 通知接收
%%
%% 上线出席通知
%% 若contact关系为double, 则发送出席回执
%% 若存在slots, 则转发erlang消息给连接者
notify({online, User}, #{world:=World, db:=Db, res:=Res, id:=Id}=S) ->
    case maps:get(id, S, not_login) of
        not_login -> nothing;
        Id ->
            User = Db:find(Res, Id),
            Contacts = ss_model:value(contacts, User),
            Sub = {user, User},
            case lists:keyfind(User, 1, Contacts) of
                true -> ss_world:send2(World, Sub, [notify, {confirm, online, User}]);
                _ -> nothing
            end
    end,
    Slots = maps:get(slots, S, []),
    [P ! {online, User} || P <- Slots],
    {ok, S};
notify({confirm, online, User}, #{world:=World, db:=Db, res:=Res, id:=Id}=S) ->
    Slots = maps:get(slots, S, []),
    [P ! {online, User} || P <- Slots],
    {ok, S}.

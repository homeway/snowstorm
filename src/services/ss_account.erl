%% -*- mode: nitrogen -*-
-module(ss_account).
-behaviour(ss_server).
-export([init/1, model/1]).
%% call callback
-export([hello/1, status/1, status/2, contact/2, contacts/1, invite/2, who/1, login/3, logout/1]).
%% cast callback
-export([hello/2, notify/2]).

%% ss_server api
init([Config]) when is_map(Config) ->
    Default = #{db=>ss_nosqlite, res=>account, account=>not_login, id=>not_login},
    {ok, maps:merge(Default, Config)};
init([]) ->
    Default = #{db=>ss_nosqlite, res=>account, account=>not_login, id=>not_login},
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
who(#{account:=Account}=S) -> {Account, S}.

%% call hello
hello(#{db:=Db, res:=Res, account:=Account}=S) ->
    force_login(fun() ->
        {Db:find(Res, Account), S}
    end, S).

%% account state and sign string
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
login(UserName, Pass, #{account:=not_login, id:=not_login}=S) ->
    #{world:=World, db:=Db, res:=Res}=S,
    case check_password(Db, Res, UserName, Pass) of
        {true, Data} ->
            % 从数据库读取联系人信息
            Contacts = ss_model:value(contacts, Data, []),
            % 广播出席通知
            Subs = [{account, Account} || {Account, _} <- Contacts],
            [ss_world:send2(World, Sub, [notify, {online, UserName}]) || Sub <- Subs], 
            % 初始化联系人列表, 活跃map为空
            {ok, S#{id=>ss_model:value('_key', Data), account=>UserName, contacts=>Contacts, living=>#{}}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{account=>not_login, id=>not_login}}.

check_password(Db, Res, Account, Pass) ->
    Data = Db:find(Res, account, Account),
    case Data of
        notfound -> {error, "account not exist"};
        _ ->
            case ss_model:equal(password, Data, Pass) of
                true -> {true, Data};
                false -> {error, "invalid password"}
            end
    end.

%% force login
force_login(Fun, S) ->
    case maps:get(account, S, not_login) of
        not_login -> {not_login, S};
        _ -> Fun()
    end.
force_login(Fun, From, S) ->
    case maps:get(account, S, not_login) of
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
notify({online, From}, #{world:=World}=S) ->
    % 收到出席通知
    force_login(fun() ->
        Account = maps:get(account, S),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true ->
                % 发送出席回执
                ss_world:send2(World, {account, From}, [notify, {confirm, online, Account}]),
                % 发给slot连接者
                Slots = maps:get(slots, S, []),
                [P ! {online, From} || P <- Slots],
                % 仅加入到living状态
                Living = maps:get(living, S, []), 
                {ok, S#{living=>maps:put(From, "online", Living)}};
            _ ->
                {single_contact, S}
        end
    end, S);
notify({confirm, online, From}, #{db:=Db, res:=Res, id:=Id}=S) ->
    % 收到出席通知的回执
    force_login(fun() ->
        Contacts = maps:get(contacts, S, []),
        case lists:keyfind(From, 1, Contacts) of
            false ->
                {single_contact, S};
            {K, Info} ->
                % 将联系人状态从single修改为double
                erlang:display("notify received ..........."),
                erlang:display(Contacts),
                New = lists:keyreplace(K, 1, Contacts, {K, Info#{rel=>double}}),
                erlang:display(New),
                % 存储
                ok=Db:update(Res, Id, #{<<"contacts">> =>New}),
                % 发给slot连接者
                Slots = maps:get(slots, S, []),
                [P ! {online, From} || P <- Slots],
                % 仅加入到living状态
                Living = maps:get(living, S, []), 
                {ok, S#{contacts=>New, living=>maps:put(From, "online", Living)}}
        end
    end, S).

%% 读取联系人, 合并在线状态
contacts(S) ->
    force_login(fun() ->
        Contacts = maps:get(contacts, S, []),
        Living = maps:get(living, S, #{}),
        {[{K, I#{status=>maps:get(K, Living, "offline")}} || {K, I} <- Contacts], S}
    end, S).

%% 设置联系人, 成为single类型联系人
%%   1) 联系人已存在, 直接返回
%%   2) 联系人不存在, 创建数据记录，并添加到进程状态
%%   3) 未登录
contact(To, #{db:=Db, res:=Res, id:=Id}=S) ->
    force_login(fun() ->
        Data = Db:find(Res, Id),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(To, 1, Contacts) of
            true -> {already_contact, S};
            false ->
                New = [{To, #{rel=>single}}|Contacts],
                ok=Db:update(Res, Id, Data#{<<"contacts">> =>New}),
                {ok, S#{contacts=>New}}
        end
    end, S).

%% 邀请成为联系人的请求
invite(From, #{world:=World}=S) ->
    % 更新对方为自己的联系人
    case contact(From, S) of
        {ok, S1} ->
            case maps:get(account, S, undefined) of
                undefined -> nothing;
                Account -> ss_world:send2(World, {account, From}, [notify, {online, Account}])
            end,
            {ok, S1};
        _ -> {error, S}
    end.


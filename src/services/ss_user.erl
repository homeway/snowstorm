%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(ss_server).
-export([init/1, model/1]).
%% call callback
-export([hello/1, status/1, status/2, contact/2, contacts/1, invite/2, who/1, login/3, logout/1]).
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

%% 读取联系人
contacts(S) ->
    force_login(fun() ->
        {maps:get(contacts, S, []), S}
    end, S).

%% 设置联系人, 成为single类型联系人
%%   1) 联系人已存在, 直接返回
%%   2) 联系人不存在, 创建数据记录，并添加到进程状态
%%   3) 未登录
contact(To, #{db:=Db, res:=Res, id:=Id}=S) ->
    force_login(fun() ->
        Data = Db:find(Res, Id),
        Contacts = ss_model:value(contacts, Data, []),
        case lists:keymember(To, 1, Contacts) of
            true -> {ok, S};
            false ->
                New = [{To, #{rel=>single}}|Contacts],
                ok=Db:update(Res, Id, Data#{<<"contacts">> =>New}),
                {ok, S#{contacts=>reload_contacts(S)}}
        end
    end, S).
reload_contacts(#{db:=Db, res:=Res, id:=Id}=S) ->
    Data = Db:find(Res, Id),
    Contacts0 = ss_model:value(contacts, Data, []),
    Contacts1 = maps:get(contacts, S, []),
    lists:map(fun({K, V}) ->
        Info = proplists:get_value(K, Contacts1, #{}),
        case maps:get(status, Info, "offline") of
            "offline" -> {K, V#{status=>"offline"}};
            Status ->    {K, V#{status=>Status}}
        end
    end, Contacts0).

%% 邀请成为联系人
%% 1) 更新自己的联系人列表
%% 2) 发送出席消息
invite(From, #{world:=World}=S) ->
    % 更新对方为自己的联系人
    case contact(From, S) of
        {R1, S1} ->
            case maps:get(user, S, undefined) of
                undefined -> nothing;
                User -> ss_world:send2(World, {user, From}, [notify, {online, User}])
            end,
            {R1, S1};
        _ -> {error, S}
    end.

%% login and logout
login(User, Pass, #{id:=not_login}=S) ->
    #{world:=World, db:=Db, res:=Res}=S,
    case check_password(Db, Res, User, Pass) of
        {true, Data} ->
            % 从数据库读取联系人信息
            Contacts = ss_model:value(contacts, Data, []),
            % 广播出席通知
            Subs = [{user, Account} || {Account, _} <- Contacts],
            [ss_world:send2(World, Sub, [notify, {online, User}]) || Sub <- Subs], 
            % 初始化联系人状态为离线
            ContactsS = [{Account, Info#{status=>"offline"}} || {Account, Info} <- Contacts],
            {ok, S#{id=>ss_model:value('_key', Data), user=>User, contacts=>ContactsS}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(S) -> {ok, S#{id=>not_login}}.

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
notify({online, From}, #{world:=World, db:=Db, res:=Res, id:=Id}=S) ->
    Contacts = maps:get(contacts, S, []),
    case maps:get(id, S, not_login) of
        not_login -> not_login;
        Id ->
            Data = Db:find(Res, Id),
            MyAccount = ss_model:value(account, Data),
            case lists:keymember(From, 1, Contacts) of
                true -> ss_world:send2(World, {user, From}, [notify, {confirm, online, MyAccount}]);
                _ -> single_contact
            end
    end,
    Slots = maps:get(slots, S, []),
    [P ! {online, From} || P <- Slots],
    {ok, S#{contacts=>update_contact(From, "online", Contacts)}};
notify({confirm, online, From}, S) ->
    % 更新联系人关系, 从single到double
    % 更新联系人状态
    Contacts = maps:get(contacts, S, []),
    Slots = maps:get(slots, S, []),
    [P ! {online, From} || P <- Slots],
    {ok, S#{contacts=>update_contact(From, "online", Contacts)}}.
update_contact(From, Status, Contacts) ->
    case lists:keyfind(From, 1, Contacts) of
        false -> [];
        {From, Info} when is_map(Info) ->
            New = {From, Info#{status=>Status}},
            lists:keyreplace(From, 1, Contacts, New);
        Contacts1 -> Contacts1
    end.

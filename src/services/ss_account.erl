%% -*- mode: nitrogen -*-
-module(ss_account).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([hello/1, hello/2, status/1, status/2, who/1, notify/2, login/3, logout/1]).
-export([contacts/1, invite/2, invite_to_accept/3, invite_to_refuse/3,
        invite_from/3, invite_accept/3, invite_refuse/3,
        chat/3, message/4]).

-define(offline, {service, offline}).

%% ss_server api
init([Config]) when is_map(Config) ->
    Default = #{db=>ss:db(ss_nosqlite, snowstorm_account), account=>not_login, id=>not_login},
    {ok, maps:merge(Default, Config)};
init([]) ->
    Default = #{db=>ss:db(ss_nosqlite, snowstorm_offline), account=>not_login, id=>not_login},
    {ok, Default}.

%% ss_account 所有关键字段
model(all) -> ss_model:confirm_model([
    {account, #{validate=>[required, uniq]}},
    {password, #{type=>password, validate=>[required, {min, 6}]}},
    {contacts, #{type=>list}}  % 联系人列表
]);

%% password
model(password) -> ss_model:filter([account, password], model(all));

%% contacts保存联系人的格式为 [{account(), #{rel=>rel()}}]
%%     rel() :: single|double
model(contacts) -> ss_model:filter([contacts], model(all));

model(_) -> [].

%% call who
who(#{account:=Account}=S) -> {Account, S}.

%% call hello
hello(#{db:=Db, account:=Account}=S) ->
    force_login(fun() ->
        {Db:find(Account), S}
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
    #{world:=W, db:=Db}=S,
    case check_password(Db, UserName, Pass) of
        {true, Data} ->
            % 从数据库读取联系人信息
            Contacts = ss_model:value(contacts, Data, []),
            % 广播出席通知
            Subs = [?offline|[{account, Account} || {Account, _} <- Contacts]],
            %erlang:display("login ...."),
            %erlang:display(Subs),
            [W:send(Sub, [notify, {online, UserName}]) || Sub <- Subs], 
            % 初始化联系人列表, 活跃map为空
            {ok, S#{id=>ss_model:value('_key', Data), account=>UserName, contacts=>Contacts, living=>#{}}};
        {error, Reason} -> {{error, Reason}, S}
    end;
login(_Id, _Pass, S) -> {already_login, S}.
logout(#{world:=W}=S) ->
    force_login(fun() ->
        % 广播出席通知
        MyAccount = maps:get(account, S),
        Contacts = maps:get(contacts, S, []),
        Subs = [{account, Account} || {Account, _} <- Contacts],
        %erlang:display("login ...."),
        %erlang:display(Subs),
        [W:send(Sub, [notify, {offline, MyAccount}]) || Sub <- Subs], 

        {ok, S#{account=>not_login, id=>not_login}}
    end, S).

check_password(Db, Account, Pass) ->
    Data = Db:find(account, Account),
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
notify({online, From}, #{world:=W}=S) ->
    % 收到出席通知
    force_login(fun() ->
        Account = maps:get(account, S),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true ->
                % 发送出席回执
                W:send({account, From}, [notify, {confirm, online, Account}]),
                % 分发给连接者
                ss_server:dispatch({online, From}, S),
                % 仅加入到living状态
                Living = maps:get(living, S, []), 
                {ok, S#{living=>maps:put(From, "online", Living)}};
            _ ->
                {single_contact, S}
        end
    end, S);
notify({offline, From}, S) ->
    % 收到出席通知
    force_login(fun() ->
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true ->
                % 分发给连接者
                ss_server:dispatch({offline, From}, S),
                % 从living中剔除
                Living = maps:get(living, S, []), 
                {ok, S#{living=>maps:remove(From, Living)}};
            _ ->
                {single_contact, S}
        end
    end, S);
notify({confirm, online, From}, #{db:=Db, id:=Id}=S) ->
    % 收到出席通知的回执
    force_login(fun() ->
        Contacts = maps:get(contacts, S, []),
        case lists:keyfind(From, 1, Contacts) of
            false ->
                {single_contact, S};
            {K, Info} ->
                % 将联系人状态从single修改为double
                New = lists:keyreplace(K, 1, Contacts, {K, Info#{rel=>double}}),
                % 存储
                %erlang:display("notify confirm ........"),
                %erlang:display(New),
                ok=Db:update(Id, #{<<"contacts">> =>New}),
                % 发给slot连接者
                ss_server:dispatch({online, From}, S),
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

%% 添加联系人
%%   1) 设置single联系人
%%   2) 发送double联系人邀请
invite(To, #{world:=W, db:=Db, id:=Id}=S) ->
    force_login(fun() ->
        case maps:get(account, S, undefined) of
            undefined ->
                {ok, S};
            Account ->
                % 更新对方为自己的联系人
                Old = Db:find(Id),
                Contacts = list_append(To, {To, #{rel=>single}}, maps:get(<<"contacts">>, Old, [])),
                New = Old#{<<"contacts">> => Contacts},
                ok=Db:update(Id, New),
                % 发送邀请
                W:send(?offline, [invite, Account, To]),
                {ok, S#{contacts=>Contacts}}
        end
    end, S).

%% 收到联系人邀请, 请求连接者同意
invite_from(TrackId, From, S) ->
    force_login(fun() ->
        ss_server:dispatch({invite_from, TrackId, From}, S),
        {ok, S}
    end, S).

%% 接受联系人邀请
invite_to_accept(TrackId, From,  #{world:=W, db:=Db, account:= Account, id:=Id}=S) ->
    force_login(fun() ->
        W:send(?offline, [delete, TrackId]),
        Data = Db:find(Id),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true -> {already_contact, S};
            false ->
                %erlang:display("to accept ......"),
                %erlang:display(From),
                % 更新对方为自己的联系人
                New = [{From, #{rel=>double}}|Contacts],
                ok=Db:update(Id, Data#{<<"contacts">> =>New}),
                % 发送邀请通过的通知
                W:send(?offline, [invite_accept, Account, From]),
                {ok, S#{contacts=>New}}
        end
    end, S).

%% 拒绝联系人邀请
invite_to_refuse(TrackId, From,  #{world:=W, account:= Account}=S) ->
    force_login(fun() ->
        W:send(?offline, [delete, TrackId]),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true -> {already_contact, S};
            false ->
                %erlang:display("to refuse ......"),
                %erlang:display(From),
                % 发送拒绝邀请的通知
                W:send(?offline, [invite_refuse, Account, From]),
                {ok, S}
        end
    end, S).

%% 收到接受邀请的确认
invite_accept(TrackId, From, #{world:=W, db:=Db, account:= Account, id:=Id}=S) ->
    force_login(fun() ->
        W:send(?offline, [delete, TrackId]),
        Data = Db:find(Id),
        Contacts = maps:get(contacts, S, []),
        case lists:keyfind(From, 1, Contacts) of
            false ->
                {not_invited, S};
            _ ->
                % 通知连接者
                ss_server:dispatch({invite_accept, From}, S),
                % 清理
                W:send(?offline, [delete, TrackId]),
                %erlang:display("invite_confirm ......"),
                %erlang:display(From),
                % 更新联系人关系
                New = lists:keyreplace(From, 1, Contacts, {From, #{rel=>double}}),
                ok=Db:update(Id, Data#{<<"contacts">> =>New}),
                % 发送在线通知
                W:send({account, From}, [notify, {online, Account}]),
                {ok, S#{contacts=>New}}
        end
    end, S).

%% 收到接受邀请的确认
invite_refuse(TrackId, From, #{world:=W}=S) ->
    force_login(fun() ->
        % 通知连接者
        ss_server:dispatch({invite_accept, From}, S),
        % 清理
        W:send(?offline, [delete, TrackId]),
        {ok, S}
    end, S).

%% 在线聊天消息
chat(Message, To, #{world:=W}=S) ->
    force_login(fun() ->
        Account = maps:get(account, S),
        W:send({account, To}, [message, chat, Message, Account]),
        {ok, S}
    end, S).

%% 接收在线消息
message(chat, Message, From, S) ->
    force_login(fun() ->
        _Account = maps:get(account, S),
        ss_server:dispatch({chat, Message, From}, S),
        {ok, S}
    end, S).

%% (避免重复)添加元素到列表
list_append(To, Item, List) ->
    case lists:keymember(To, 1, List) of
        true -> List;
        false -> [Item|List]
    end.


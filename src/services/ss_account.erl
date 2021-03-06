%% -*- mode: nitrogen -*-
-module(ss_account).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([create/2, create/3, update/3, update/4, delete/2, find/2, all/1, drop/1]).
-export([hello/1, hello/2, status/1, status/2, who/1, notify/2, login/3, logout/1]).
-export([contacts/1, invite/2, invite_to_accept/3, invite_to_refuse/3,
        invite_from/3, invite_accept/3, invite_refuse/3,
        chat_to/3, chat_from/4, chat_online_confirm/2, chat_offline/4, chat_history/1, notify_history/1]).

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
model(create) -> model(all);
model(update) -> model(all);

%% password
model(password) -> ss_model:filter([account, password], model(all));

%% contacts保存联系人的格式为 [{account(), #{rel=>rel()}}]
%%     rel() :: single|double
model(contacts) -> ss_model:filter([contacts], model(all));

%% message
model(chat) -> ss_model:confirm_model([
    {message, #{}},
    {from, #{}},
    {to, #{}},
    {type, #{}}
]);

model(_) -> [].

%% db action ------------------------------------------------------
create(Data, S) -> create(Data, create, S).
create(Data, Model, #{db:=D}=S) ->
    ss_server:validate(Data, Model, S, fun() ->
        D:create(Data)
    end).
update(K, Data, S) -> update(K, Data, update, S).
update(K, Data, Model, #{db:=D}=S) ->
    ss_server:validate(Data, Model, S, fun() ->
        D:update(K, Data)
    end).
delete(K, #{db:=D}=S) -> {D:delete(K), S}.
find  (K, #{db:=D}=S) -> {D:find(K), S}.
all      (#{db:=D}=S) -> {D:all(), S}.
drop     (#{db:=D}=S) -> {D:drop(), S}.

%% other action ---------------------------------------------------
%%

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
            [W:cast(Sub, notify, {online, UserName}) || Sub <- Subs], 
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
        [W:cast(Sub, notify, {offline, MyAccount}) || Sub <- Subs], 

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
                W:cast({account, From}, notify, {confirm, online, Account}),
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
                W:cast(?offline, invite, [Account, To]),
                % 保存邀请记录
                save_invite(invite_to, Account, To, S),
                {ok, S#{contacts=>Contacts}}
        end
    end, S).

%% 收到联系人邀请, 请求连接者同意
invite_from(TrackId, From, S) ->
    force_login(fun() ->
        % 保存通知
        save_invite(invite_from, From, maps:get(account, S), S),
        ss_server:dispatch({invite_from, TrackId, From}, S),
        {ok, S}
    end, S).

%% 接受联系人邀请
invite_to_accept(TrackId, From,  #{world:=W, db:=Db, account:= Account, id:=Id}=S) ->
    force_login(fun() ->
        W:cast(?offline, delete, TrackId),
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
                W:cast(?offline, invite_accept, [Account, From]),
                {ok, S#{contacts=>New}}
        end
    end, S).

%% 拒绝联系人邀请
invite_to_refuse(TrackId, From,  #{world:=W, account:= Account}=S) ->
    force_login(fun() ->
        W:cast(?offline, delete, TrackId),
        Contacts = maps:get(contacts, S, []),
        case lists:keymember(From, 1, Contacts) of
            true -> {already_contact, S};
            false ->
                %erlang:display("to refuse ......"),
                %erlang:display(From),
                % 发送拒绝邀请的通知
                W:cast(?offline, invite_refuse, [Account, From]),
                {ok, S}
        end
    end, S).

%% 收到接受邀请的确认
invite_accept(TrackId, From, #{world:=W, db:=Db, id:=Id}=S) ->
    force_login(fun() ->
        Account = maps:get(account, S),
        W:cast(?offline, delete, TrackId),
        Data = Db:find(Id),
        Contacts = maps:get(contacts, S, []),
        case lists:keyfind(From, 1, Contacts) of
            false ->
                {not_invited, S};
            _ ->
                % 通知连接者
                ss_server:dispatch({invite_accept, From}, S),
                % 保存通知
                save_invite(accept, From, Account, S),
                % 清理
                W:cast(?offline, delete, TrackId),
                %erlang:display("invite_confirm ......"),
                %erlang:display(From),
                % 更新联系人关系
                New = lists:keyreplace(From, 1, Contacts, {From, #{rel=>double}}),
                ok=Db:update(Id, Data#{<<"contacts">> =>New}),
                % 发送在线通知
                W:cast({account, From}, notify, {online, Account}),
                {ok, S#{contacts=>New}}
        end
    end, S).

%% 收到接受邀请的确认
invite_refuse(TrackId, From, #{world:=W}=S) ->
    force_login(fun() ->
        % 通知连接者
        ss_server:dispatch({invite_accept, From}, S),
        % 保存通知
        save_invite(refuse, From, maps:get(account, S), S),
        % 清理
        W:cast(?offline, delete, TrackId),
        {ok, S}
    end, S).

%% 发送在线聊天消息
chat_to(Content, To, #{world:=W}=S) ->
    force_login(fun() ->
        Account = maps:get(account, S),
        %% 首先判断是否在线
        case maps:is_key(To, maps:get(living, S, #{})) of
            true ->
                %% 要求在3秒内接收确认, 否则转发到离线请求
                Ref = make_ref(),
                Pid = spawn(fun() ->
                    receive {confirm, Ref} -> ok
                    after 1000 -> W:cast(?offline, chat_to, [Content, Account, To]) end
                end),
                %% 发送消息
                W:cast({account, To}, chat_from, [{Pid, Ref}, Content, Account]);
            false ->
                W:cast(?offline, chat_to, [Content, Account, To])
        end,
        %% 保存发送记录
        save_chat(Content, Account, To, S),
        {ok, S}
    end, S).

%% 接收在线聊天消息
chat_from(Confirm, Content, From, #{world:=W}=S) ->
    force_login(fun() ->
        %% 发送在线确认
        W:cast({account, From}, chat_online_confirm, Confirm),
        %% 通知连接者
        ss_server:dispatch({chat_from, Content, From}, S),
        %% 保存接收记录
        save_chat(Content, From, maps:get(account, S), S),
        {ok, S}
    end, S).

%% 接收离线消息
chat_offline(TrackId, Content, From, #{world:=W}=S) ->
    force_login(fun() ->
        W:cast(?offline, delete, TrackId),
        ss_server:dispatch({chat_offline, Content, From}, S),
        %% 保存接收记录
        save_chat(Content, From, maps:get(account, S), S),
        {ok, S}
    end, S).

%% 聊天消息确认
chat_online_confirm({Pid, Ref}, S) ->
    force_login(fun() ->
        erlang:display("chat confirm ...."),
        Pid ! {confirm, Ref},
        {ok, S}
    end, S).

%% 查看聊天记录
chat_history(S) ->
    force_login(fun() ->
        Db = chat_db(S),
        {Db:all(), S}
    end, S).

%% 查看通知记录
notify_history(S) ->
    force_login(fun() ->
        Db = notify_db(S),
        {Db:all(), S}
    end, S).

%% (避免重复)添加元素到列表
list_append(To, Item, List) ->
    case lists:keymember(To, 1, List) of
        true -> List;
        false -> [Item|List]
    end.

%% 
chat_db(S) ->
    Res = io_lib:format("~ts_chat", [maps:get(account, S)]),
    ss:nosqlite(Res).

%% 保存聊天记录
save_chat(Content, From, To, S) ->
    save_message(Content, From, To, chat, S).

%% 保存消息历史
save_message(Content, From, To, Type, S) ->
    force_login(fun() ->
        Own = maps:get(account, S),
        Item = #{<<"content">> => Content,
                 <<"from">> => From,
                 <<"to">> => To,
                 <<"type">> => Type,
                 <<"received_at">> => ss_time:now_to_iso()},
        case Own =:= From of
            true -> Key = To;
            _ -> Key = From
        end,
        Db = chat_db(S),
        case Db:find(Key) of
            notfound ->
                Db:create(Key, #{<<"items">> => [Item]});
            #{<<"items">> := Items} ->
                Db:update(Key, #{<<"items">> => [Item|Items]})
        end
    end, S).

%% 
notify_db(S) ->
    Res = io_lib:format("~ts_notify", [maps:get(account, S)]),
    ss:nosqlite(Res).

%% 保存邀请记录
save_invite(Type, From, To, S) ->
    save_notify("invite", From, To, Type, S).

%% 保存通知历史
save_notify(Content, From, To, Type, S) ->
    force_login(fun() ->
        Db = notify_db(S),
        Data = #{<<"content">> => Content,
                 <<"from">> => From,
                 <<"to">> => To,
                 <<"type">> => Type,
                 <<"received_at">> => ss_time:now_to_iso()},
        Db:create(Data)
    end, S).

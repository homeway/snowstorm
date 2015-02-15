%% -*- mode: nitrogen -*-
%% @doc 联系人邀请队列
-module(ss_offline).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([invite/3, invite_accept/3, invite_refuse/3, chat_to/4, notify/2]).

%% ss_server api
init([Config]) when is_map(Config) ->
    Default = #{db=>ss:db(ss_nosqlite, snowstorm_offline)},
    {ok, maps:merge(Default, Config)};
init([]) ->
    Default = #{db=>ss:db(ss_nosqlite, snowstorm_offline)},
    {ok, Default}.

%% 
model(all) -> ss_model:confirm_model([
    {receiver, #{}},
    {message, #{type=>list}}
]).

%% 邀请联系人, 支持离线处理
invite(From, To, S) ->
    do(From, To, invite_from, S),
    {ok, S}.
invite_accept(From, To, S) ->
    do(From, To, invite_accept, S),
    {ok, S}.
invite_refuse(From, To, S) ->
    do(From, To, invite_refuse, S),
    {ok, S}.

do(From, To, Action, #{world:=W, db:=Db}) ->
    % 保存离线邀请, 收到确认后清除
    Message = [Action, From],
    Item = #{<<"receiver">> =>To, <<"message">> =>Message},
    {ok, TrackId}=Db:create(Item),
    % 发送邀请消息
    % erlang:display("do from invite first........."),
    % erlang:display(Message),
    W:cast({account, To}, Action, [TrackId, From]).

%% 离线消息
chat_to(Content, From, To, #{world:=W, db:=Db}=S) ->
    % 保存离线邀请, 收到确认后清除
    Message = [chat_offline, Content, From],
    Item = #{<<"receiver">> =>To, <<"message">> =>Message},
    {ok, TrackId}=Db:create(Item),
    W:cast({account, To}, chat_offline, [Content, TrackId, From]),
    {ok, S}.

%% 转发离线通知
%%
%% 要求接收方从离线服务删除的消息包括:
%%   1) invite_from
%%   2) offline_message
notify({online, From}, #{world:=W, db:=Db}=S) ->
    % 收到出席通知
    % erlang:display("notify from offline....."),
    Messages = Db:search(fun(#{<<"receiver">> := R}) ->
        From =:= R
    end, []),
    % erlang:display(Messages),
    [W:cast({account, From}, Action, [TrackId|Msg]) || #{<<"_key">> := TrackId, <<"message">> := [Action|Msg]} <- Messages],
    {ok, S}.

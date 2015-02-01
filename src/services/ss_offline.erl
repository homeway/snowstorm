%% -*- mode: nitrogen -*-
%% @doc 联系人邀请队列
-module(ss_offline).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([invite/3, notify/2]).

%% ss_server api
init([Config]) when is_map(Config) ->
    Default = #{db=>ss_nosqlite, res=>contact_invite},
    {ok, maps:merge(Default, Config)};
init([]) ->
    Default = #{db=>ss_nosqlite, res=>contact_invite},
    {ok, Default}.

%% 
model(all) -> ss_model:confirm_model([
    {receiver, #{}},
    {message, #{type=>list}}
]).

%% 邀请联系人, 支持离线处理
invite(From, To, #{world:=World, db:=Db, res:=Res}=S) ->
    % 保存离线邀请, 收到确认后清除
    Message = [invite_from, From],
    Item = #{<<"receiver">> =>To, <<"message">> =>Message},
    {ok, TrackId}=Db:create(Res, Item),
    % 发送邀请消息
    erlang:display("invite from invite first........."),
    erlang:display(Message),
    ss_world:send2(World, {account, To}, [invite_from, TrackId, From]),
    {ok, S}.

%% 发送离线消息
%%
%% 要求接收方从离线服务删除的消息包括:
%%   1) invite_from
%%   2) offline_message
notify({online, From}, #{world:=World, db:=Db, res:=Res}=S) ->
    % 收到出席通知
    erlang:display("notify from offline....."),
    Messages = Db:search(Res, fun(#{<<"receiver">> := R}) ->
        From =:= R
    end, []),
    erlang:display(Messages),
    [ss_world:send2(World, {account, From}, [Action,TrackId|Msg]) || #{<<"_key">> := TrackId, <<"message">> := [Action|Msg]} <- Messages],
    {ok, S}.

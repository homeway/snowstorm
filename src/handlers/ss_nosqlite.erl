%% -*- mode:nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2014, homeway
%%% @doc
%%% a KV storage joy with dets : )
%%%
%%% data saved as maps in fact, but access it with ss_model style
%%% @end
%%% Created : 24 Dec 2014 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(ss_nosqlite).
-export([init/1, from_model/1, to_model/1, create/2, update/2, patch/2, get/3, delete/2, search/3]).
-export([all/1, all/2]).

%% 表初始化
init(Table) ->
    Filename = io_lib:format("data/~s.nosqlite", [ss:to_binary(Table)]),
    filelib:ensure_dir(Filename),
    dets:open_file(Table, [{type, set}, {file, Filename}, {repair, true}]).
close(Table) ->
    dets:close(Table).

%% 创建数据，返回{ok, Id}
%% Data应为maps类型
%% 自动插入创建和最后修改时间时间戳, 使用ISO标准格式
create(Table, M1) ->
    M = ss_model:confirm_model(M1),
    case ss_model:validate(ss_model:confirm_model(M)) of
        {ok, _} -> create2(Table, M);
        {error, M2} -> {error, M2}
    end.
create2(Table, M1) ->
    M = ss_model:confirm_model(M1),
    Id1 = maps:get(value, proplists:get_value(<<"_key">>, M, #{}), undefined),
    if
        Id1 =:= undefined ->
            {_, S, Ms} = now(),
            Id = ss:to_binary(io_lib:format("~B-~6..0B", [S, Ms]));
        true ->
            Id = ss:to_binary(Id1)
    end,
    init(Table),
    Time = ss_time:now_to_iso(),
    Data1 = from_model(M),
    Data = Data1#{<<"_created_at">> => Time, <<"_lastmodified_at">> => Time},
    dets:insert(Table, {Id, Data}),
    close(Table),
    {ok, Id}.

%% 查看
%% 直接返回所查到的maps类型，方便函数级联操作
get(Table, Key, M1) ->
    M = ss_model:confirm_model(M1),
    Id = ss:to_binary(Key),
    init(Table),
    R = case dets:lookup(Table, Id) of
        [{_K, Result}|_] -> Result;
        Error -> erlang:display(Error), #{}
    end,
    close(Table),
    to_model(R#{<<"_key">> => Id}, M).

%% 更新数据，返回ok | notfound
update(Table, M) ->
    case ss_model:validate(M) of
        {ok, _} -> update2(Table, M);
        {error, M2} -> {error, M2}
    end.
update2(Table, M1) ->
    M = ss_model:confirm_model(M1),
    Key = maps:get(value, proplists:get_value(<<"_key">>, M), []),
    Id = ss:to_binary(Key),
    init(Table),
    case get(Table, Id, []) of
        #{} ->
            close(Table),{error, invalid_key};
        Data -> 
            OldData = from_model(Data),
            Time = ss_time:now_to_iso(),
            Data1 = from_model(M),
            Data = maps:merge(OldData, Data1#{<<"_lastmodified_at">> => Time}),
            dets:insert(Table, {Id, Data}),
            close(Table),
            ok
    end.

%% 部分更新
patch(Table, M1) ->
    M = ss_model:confirm_model(M1),
    Id = maps:get(value, proplists:get_value(<<"_key">>, M), []),
    Old = from_model(get(Table, Id, [])),
    New = maps:merge(Old, from_model(M)),
    update(Table, to_model(New)).

%% 删除
delete(Table, Key) ->
    Id = ss:to_binary(Key),
    init(Table),
    dets:delete(Table, Id),
    close(Table).

%% 搜索
%% Cond 条件
%% Option 选项: [{start, Start}, {row, Row}, {sort, Sort}]
search(Table, Fun, _Options) ->
    init(Table),
    Result = search_acc(Table, dets:first(Table), Fun, []),
    close(Table),
    Result.

%% 在查询结果中插入<<"_key">>，实际上数据库并不保存这个字段
%% 但其他两个系统字段<<"_lastmodified_at">>和<<"_created_at">>是要保存的
search_acc(_Table, '$end_of_table', _Fun, Acc) ->
    Acc;
search_acc(Table, K, Fun, Acc) ->
    [{_, Obj1}|_T] = dets:lookup(Table, K),
    Obj = maps:put(<<"_key">>, K, Obj1),
    case Fun(Obj) of
        true ->
            search_acc(Table, dets:next(Table, K), Fun, [Obj|Acc]);
        false ->
            search_acc(Table, dets:next(Table, K), Fun, Acc)
    end.

%% 遍历所有数据
all(Table) ->
    L = search(Table, fun(_K) -> true end, []),
    [to_model(I) || I <- L].
all(Table, M) ->
    L = search(Table, fun(_K) -> true end, []),
    [to_model(I, M) || I <- L].

%% convert model from nosqlite maps -------------------------------------
from_model(M) ->
    L1 = lists:map(fun({K, V}) ->
        {ss:to_binary(K), maps:get(value, V, <<>>)}
    end, M),
    maps:from_list(L1).

to_model(D) -> to_model(D, []).
to_model(D, M1) ->
    M = [{ss:to_binary(K), V} || {K, V} <- M1],
    lists:map(fun({K, V}) ->
        Field = proplists:get_value(K, M, #{}),
        {K, Field#{value => V}}
    end, maps:to_list(D)).

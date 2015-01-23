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
-export([init/1, from_model/1, to_model/1, create/2, update/2, patch/2, get/2, delete/2, search/3]).
-export([all/1]).

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
from_model(M) ->
    L1 = lists:map(fun({K, V}) ->
        {K, maps:get(value, V, <<>>)}
    end, M),
    maps:from_list(L1).
to_model(D) ->
    lists:map(fun({K, V}) ->
        {K, #{value => V}}
    end, maps:to_list(D)).

create(Table, #{<<"_key">> := Id}=Data1) when is_map(Data1) ->
    init(Table),
    Time = ss_time:now_to_iso(),
    Data2 = from_model(Data1),
    Data = Data2#{<<"_created_at">> => Time, <<"_lastmodified_at">> => Time},
    dets:insert(Table, {Id, Data}),
    close(Table);
create(Table, Data1) when is_map(Data1) ->
    Data = from_model(Data1),
    {_, S, M} = now(),
    Id = ss:to_binary(io_lib:format("~B-~6..0B", [S, M])),
    ok = create(Table, Data#{<<"_key">> => Id}),
    {ok, Id}.

%% 查看
%% 直接返回所查到的maps类型，方便函数级联操作
get(Table, Id) ->
    init(Table),
    Result = case dets:lookup(Table, Id) of
        [] -> #{<<"_error">> => <<"notfound">>};
        [{_K, V}|_T] -> V
    end,
    close(Table),
    to_model(Result#{<<"_key">> => Id}).

%% 更新数据，返回ok | notfound
update(Table, #{<<"_key">> := Id}=Data1) when is_map(Data1) ->
    init(Table),
    OldData = get(Table, Id),
    Time = ss_time:now_to_iso(),
    Data2 = from_model(Data1),
    Data = maps:merge(OldData, Data2#{<<"_lastmodified_at">> => Time}),
    dets:insert(Table, {Id, Data}),
    close(Table).
%% 部分更新
patch(Table, #{<<"_key">> := Id}= Data) when is_map(Data) ->
    {ok, Old} = get(Table, Id),
    New = maps:merge(Old, from_model(Data)),
    update(Table, to_model(New)).

%% 删除
delete(Table, #{<<"_key">> := Id}) ->
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
    search(Table, fun(_K) -> true end, []).

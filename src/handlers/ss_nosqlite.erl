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
-behaviour(ss_db).
-export([create/2, create/3, update/3, find/2, find/3, delete/2, all/1, drop/1]).
-export([init/1, search/3]).

%% 表初始化
init({?MODULE, Table}) -> init(Table);
init(Table) ->
    Filename = io_lib:format("data/~s.nosqlite", [ss:to_binary(Table)]),
    filelib:ensure_dir(Filename),
    dets:open_file(Table, [{type, set}, {file, Filename}, {repair, true}]).

close({?MODULE, Table}) -> close(Table);
close(Table) ->
    dets:close(Table).

%% 删除所有数据
drop({?MODULE, Table}) -> drop(Table);
drop(Table) ->
    init(Table),
    R = dets:delete_all_objects(Table),
    close(Table),
    R.

%% 创建数据，返回{ok, Id}
%% Data应为maps类型
%% 自动插入创建和最后修改时间时间戳, 使用ISO标准格式
create(Data, {?MODULE, Table}) -> create(Table, Data);
create(Table, Data) ->
    {_, S, Ms} = now(),
    Id = ss:to_binary(io_lib:format("~B-~6..0B", [S, Ms])),
    create(Table, Id, Data).
create(Key, Data, {?MODULE, Table}) -> create(Table, Key, Data);
create(Table, Key, Data1) ->
    Id = ss:to_binary(Key),
    init(Table),
    Time = ss_time:now_to_iso(),
    Data = Data1#{<<"_created_at">> => Time, <<"_lastmodified_at">> => Time},
    L = [{ss:to_binary(K), V} || {K, V} <- maps:to_list(Data)],
    dets:insert(Table, {Id, maps:from_list(L)}),
    close(Table),
    {ok, Id}.

%% 查看
%% 由于使用了set类型，仅返回一个结果
%% 直接返回所查到的maps类型，方便函数级联操作
find_bare(Table, Id) ->
    case dets:lookup(Table, Id) of
        [{_K, Result}|_] -> Result;
        [] -> notfound
    end.
find(Key, {?MODULE, Table}) -> find(Table, Key);
find(Table, Key) ->
    Id = ss:to_binary(Key),
    init(Table),
    R = find_bare(Table, Id),
    close(Table),
    R.
%% 使用任意字段查询
find(Key, Value, {?MODULE, Table}) -> find(Table, Key, Value);
find(Table, Key1, Value) ->
    Key = ss:to_binary(Key1),
    init(Table),
    R1 = search(Table, fun(Obj) ->
        maps:get(Key, Obj, no_this_value) =:= Value
    end, [{count, 1}]),
    close(Table),
    case R1 of
        [R|_] -> R;
        [] -> notfound
    end.

%% 部分更新数据，返回ok | notfound
update(Key, Data, {?MODULE, Table}) -> update(Table, Key, Data);
update(Table, Key, Data1) ->
    Id = ss:to_binary(Key),
    init(Table),
    case find_bare(Table, Id) of
        notfound ->
            close(Table),{error, notfound};
        OldData ->
            Time = ss_time:now_to_iso(),
            L = [{ss:to_binary(K), V} || {K, V} <- maps:to_list(Data1)],
            Data2 = maps:from_list(L),
            Data = maps:merge(OldData, Data2#{<<"_lastmodified_at">> => Time}),
            dets:insert(Table, {Id, Data}),
            close(Table),
            ok
    end.

%% 删除
delete(Key, {?MODULE, Table}) -> delete(Table, Key);
delete(Table, Key) ->
    Id = ss:to_binary(Key),
    init(Table),
    dets:delete(Table, Id),
    close(Table).

%% 搜索
%% Cond 条件
%% Option 选项: [{start, Start}, {count, Count}, {sort, Sort}]
search(Fun, Options, {?MODULE, Table}) -> search(Table, Fun, Options);
search(Table, Fun, Options) ->
    init(Table),
    Count = proplists:get_value(count, Options, -1),
    R = search_acc(Table, dets:first(Table), Fun, [], Count),
    close(Table),
    R.

%% 在查询结果中插入<<"_key">>，实际上数据库并不保存这个字段
%% 但其他两个系统字段<<"_lastmodified_at">>和<<"_created_at">>是要保存的
search_acc(_Table, '$end_of_table', _Fun, Acc, _Count) ->
    Acc;
search_acc(Table, K, Fun, Acc, Count) ->
    [{_, Obj1}|_T] = dets:lookup(Table, K),
    Obj = maps:put(<<"_key">>, K, Obj1),
    case Fun(Obj) of
        true when (Count =:= -1) or (length(Acc) < Count) ->
            search_acc(Table, dets:next(Table, K), Fun, [Obj|Acc], Count);
        true -> Acc;
        false ->
            search_acc(Table, dets:next(Table, K), Fun, Acc, Count)
    end.

%% 遍历所有数据
all({?MODULE, Table}) -> all(Table);
all(Table) ->
    search(Table, fun(_Obj) -> true end, []).

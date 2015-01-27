%% -*- mode: nitrogen -*-
-module(ss_nosqlite_test).
-export([]).
-include_lib("eunit/include/eunit.hrl").

-define(db, ss_nosqlite).
-define(tab, test_user).

drop(Tab) -> ?db:drop(Tab).
create(Tab, Data) -> ?db:create(Tab, Data).
create(Tab, Id, Data) -> ?db:create(Tab, Id, Data).
find(Tab, Id) -> ?db:find(Tab, Id).
update(Tab, Id, Data) -> ?db:update(Tab, Id, Data).
delete(Tab, Id) -> ?db:delete(Tab, Id).
all(Tab) -> ?db:all(Tab).

db_test() ->
    %% 清理
    ?assertEqual(ok, drop(?tab)),

    %% 正确的CRUD操作
    D1 = #{<<"name">> => "yifan", <<"age">> => 10},
    D2 = #{<<"age">> => 11},
    %% create
    {ok, Id1} = create(?tab, D1),
    {ok, Id2} = create(?tab, <<"001">>, D1),
    ?assertEqual(true, is_binary(Id1)),
    ?assertEqual(<<"001">>, Id2),
    %% find
    ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 10}, find(?tab, Id1)),
    %% update
    ?assertEqual(ok, update(?tab, Id1, D2)),
    ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 11}, find(?tab, Id1)),
    %% all
    ?assertEqual(2, length(all(?tab))),
    %% delete
    ?assertEqual(ok, delete(?tab, Id1)),
    ?assertEqual(1, length(all(?tab))),
    ?assertEqual(notfound, find(?tab, Id1)).


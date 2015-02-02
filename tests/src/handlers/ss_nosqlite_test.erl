%% -*- mode: nitrogen -*-
-module(ss_nosqlite_test).
-include_lib("eunit/include/eunit.hrl").

db_test() ->
    T = ss:nosqlite(test_user),
    %% 清理
    ?assertEqual(ok, T:drop()),

    %% 正确的CRUD操作
    D1 = #{<<"name">> => "yifan", <<"age">> => 10},
    D2 = #{<<"age">> => 11},
    %% create
    {ok, Id1} = T:create(D1),
    {ok, Id2} = T:create(<<"001">>, D1),
    ?assertEqual(true, is_binary(Id1)),
    ?assertEqual(<<"001">>, Id2),
    %% find
    ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 10}, T:find(Id1)),
    ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 10}, T:find("name", "yifan")),
    %% update
    ?assertEqual(ok, T:update(Id1, D2)),
    ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 11}, T:find(Id1)),
    %% all
    ?assertEqual(2, length(T:all())),
    %% delete
    ?assertEqual(ok, T:delete(Id1)),
    ?assertEqual(1, length(T:all())),
    ?assertEqual(notfound, T:find(Id1)).


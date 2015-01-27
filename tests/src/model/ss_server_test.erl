%% -*- mode: nitrogen -*-
-module(ss_server_test).
-include_lib("eunit/include/eunit.hrl").

-define(world, to_test_ss_server).
to_test() ->
    %% 启动world
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% regss ss_user1
    P1 = test_user1,
    ss_world:regss2(?world, P1, ss_user2),
    ?assertEqual(true, is_pid(ss_world:find2(?world, P1))),

    %% call info
    ?assertMatch(#{db:=_Db, res:=_Res}, ss_world:call2(?world, P1, info)),

    %% call model
    ?assertMatch([], ss_world:call2(?world, P1, [model, undefined])),
    ?assertMatch([{_K, #{}}|_], ss_world:call2(?world, P1, [model, all])),

    %% call db methods
    D1 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "123456"},
    D2 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "1234"},
    D3 = #{<<"密码"/utf8>> => "654321"},

    %% clear data
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),
    ?assertEqual([], ss_world:call2(?world, P1, all)),
    
    %% create
    M1 = ss_world:call2(?world, P1, [model, password]),
    ?assertMatch([{<<"账户名"/utf8>>, _}, {<<"密码"/utf8>>, _}], M1),
    {ok, Id1} = ss_world:call2(?world, P1, [create, D1, M1]),
    ?assertEqual(true, is_binary(Id1)),

    %% create with invalidte min length
    {error, M2} = ss_world:call2(?world, P1, [create, D2, M1]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),

    %% find Id1
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "123456"}, ss_world:call2(?world, P1, [find, Id1])),

    %% update Id1
    ?assertEqual(ok, ss_world:call2(?world, P1, [update, Id1, D3, M1])),
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "654321"}, ss_world:call2(?world, P1, [find, Id1])),

    %% all
    ?assertEqual(1, length(ss_world:call2(?world, P1, all))),
    {ok, _Id2} = ss_world:call2(?world, P1, [create, D1, M1]),
    {ok, Id3} = ss_world:call2(?world, P1, [create, D1, M1]),
    ?assertEqual(3, length(ss_world:call2(?world, P1, all))),

    %% delete Id3
    ?assertEqual(ok, ss_world:call2(?world, P1, [delete, Id3])),
    ?assertEqual(2, length(ss_world:call2(?world, P1, all))),
    ?assertMatch(notfound, ss_world:call2(?world, P1, [find, Id3])),

    %% clear data
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),

    %% stop the world
    ss_world:stop2(?world).

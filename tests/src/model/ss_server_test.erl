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
    %D3 = #{<<"密码"/utf8>> => "654321"},

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

    %% find
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "123456"}, ss_world:call2(?world, P1, [find, Id1])),
    % %% update
    % ?assertEqual(ok, update(?tab, Id1, D2)),
    % ?assertMatch(#{<<"name">> := "yifan", <<"age">> := 11}, find(?tab, Id1)),
    % %% all
    % ?assertEqual(2, length(all(?tab))),
    % %% delete
    % ?assertEqual(ok, delete(?tab, Id1)),
    % ?assertEqual(1, length(all(?tab))),
    % ?assertEqual(notfound, find(?tab, Id1)).

    %% stop the world
    ss_world:stop2(?world).

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
    ss_world:reg_server2(?world, P1, ss_user2),
    ?assertEqual(true, is_pid(ss_world:find2(?world, P1))),

    %% call info
    ?assertMatch(#{db:=_Db, res:=_Res}, ss_world:call2(?world, P1, info)),

    %% call model
    ?assertMatch([], ss_world:call2(?world, P1, [model, undefined])),
    ?assertMatch([{_K, #{}}|_], ss_world:call2(?world, P1, [model, all])),

    %% call db methods

    %% clear data
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),
    ?assertEqual([], ss_world:call2(?world, P1, all)),
    
    %% create
    M1 = ss_world:call2(?world, P1, [model, password]),
    D1 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "123456"},
    ?assertMatch([{<<"账户名"/utf8>>, _}, {<<"密码"/utf8>>, _}], M1),
    {ok, Id1} = ss_world:call2(?world, P1, [create, D1, M1]),
    ?assertEqual(true, is_binary(Id1)),

    %% create with invalidte min length
    D2 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "1234"},
    {error, M2} = ss_world:call2(?world, P1, [create, D2, M1]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),
    {error, _} = ss_world:call2(?world, P1, [create, D2, password]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),

    %% find Id1
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "123456"}, ss_world:call2(?world, P1, [find, Id1])),

    %% update Id1
    M_PassOnly = ss_model:filter(["密码"], ss_user2:model(all)),
    D3 = #{<<"密码"/utf8>> => "654321"},
    ?assertEqual(ok, ss_world:call2(?world, P1, [update, Id1, D3, M_PassOnly])),
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

    %% other methods
    %%
    %% hello
    ?assertEqual(not_login, ss_world:call2(?world, P1, hello)),

    %% login
    ?assertEqual(ok, ss_world:call2(?world, P1, [login, Id1, "654321"])),

    %% status
    ?assertMatch("已登录", ss_world:call2(?world, P1, status)),
    ?assertMatch(ok, ss_world:call2(?world, P1, [status, "忙碌"])),
    ?assertMatch("忙碌", ss_world:call2(?world, P1, status)),

    %% who
    ?assertMatch(Id1, ss_world:call2(?world, P1, who)),
    ?assertMatch(ok, ss_world:call2(?world, P1, logout)),
    ?assertMatch(not_login, ss_world:call2(?world, P1, who)),

    %% stop the world
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),
    ss_world:stop2(?world).
